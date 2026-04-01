
import os
import shutil
import tempfile
import contextlib
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Optional, List
import time, random
import warnings

import ee

from google.oauth2 import service_account
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from googleapiclient.http import MediaIoBaseDownload

# GDAL bindings
from osgeo import gdal

gdal.UseExceptions() #get GDAL tracebacks


def list_assets(parent: str) -> list[str]:
    """
    Recursively list asset IDs under an Earth Engine folder/collection.

    Args:
        parent: e.g. "projects/ee-tymc5571-goodfire/assets/"
    Returns:
        A list of child asset IDs under the given parent.
    Requires:
        import ee
        from typing import List
    """
    assets = []
    def _list(parent_id):
        response = ee.data.listAssets({'parent': parent_id})
        for item in response.get('assets', []):
            asset_id = item.get('name')
            asset_type = item.get('type')
            if asset_type in ('FOLDER', 'IMAGE_COLLECTION'):
                _list(asset_id)
            else:
                assets.append(asset_id)

    _list(parent)
    return assets


def list_all_files_in_folder(
    service,
    folder_id: str,
    description: Optional[str] = None,
    *,
    include_all_drives: bool = True,
    drive_id: Optional[str] = None,
    page_size: int = 1000,
) -> List[dict]:
    """
    List ALL files in a Drive folder, with pagination.
    Filters by description substring and .tif/.TIF extension.
    Returns a list of dicts with at least {'id','name'}.
    """
    # Query: in folder, not trashed, optional name contains 'description',
    # and extension contains '.tif' or '.TIF'
    name_filter = []
    if description:
        name_filter.append(f"(name contains '{description}')")
    ext_filter = "((name contains '.tif') or (name contains '.TIF'))"
    q_parts = [f"'{folder_id}' in parents", "trashed=false", ext_filter]
    if name_filter:
        q_parts.append(" and ".join(name_filter))
    q = " and ".join(q_parts)

    kwargs = dict(
        q=q,
        pageSize=page_size,
        fields="nextPageToken, files(id, name)",
        orderBy="name",
    )
    if include_all_drives:
        kwargs.update(dict(supportsAllDrives=True, includeItemsFromAllDrives=True))
        if drive_id:
            kwargs.update(dict(corpora="drive", driveId=drive_id))
        else:
            kwargs.update(dict(corpora="allDrives"))
    else:
        kwargs.update(dict(corpora="user"))

    files, page_token = [], None
    while True:
        resp = service.files().list(pageToken=page_token, **kwargs).execute()
        files.extend(resp.get("files", []))
        page_token = resp.get("nextPageToken")
        if not page_token:
            break
    return files

def _download_file_from_drive(file_id, file_name, temp_dir, service_account_file):
    try:
        # Authenticate service account
        SCOPES = ['https://www.googleapis.com/auth/drive.readonly']
        credentials = service_account.Credentials.from_service_account_file(
            os.path.abspath(service_account_file), scopes=SCOPES
        )
        service = build('drive', 'v3', credentials=credentials)

        request = service.files().get_media(fileId=file_id)
        local_path = os.path.join(temp_dir, file_name)
        with open(local_path, 'wb') as f:
            downloader = MediaIoBaseDownload(f, request)
            done = False
            while not done:
                status, done = downloader.next_chunk()
        time.sleep(random.uniform(0.5, 1.5))  # Random delay
        print(f"Downloaded {file_name}")
        return local_path
    except Exception as e:
        print(f"Error downloading {file_name}: {e}")
        return None


def _get_band_descriptions(path: str) -> List[str]:
    ds = gdal.Open(path, gdal.GA_ReadOnly)
    if ds is None:
        raise RuntimeError(f"Could not open raster to read band descriptions: {path}")
    desc = [(ds.GetRasterBand(i).GetDescription() or "") for i in range(1, ds.RasterCount + 1)]
    ds = None
    return desc

def _set_band_descriptions(path: str, descriptions: List[str]) -> None:
    ds = gdal.Open(path, gdal.GA_Update)
    if ds is None:
        raise RuntimeError(f"Could not open raster for update to set band descriptions: {path}")
    out_n = ds.RasterCount
    if out_n != len(descriptions):
        ds = None
        raise ValueError(
            f"Output band count ({out_n}) does not match source band count ({len(descriptions)}) "
            f"when setting descriptions on: {path}"
        )
    for i, d in enumerate(descriptions, start=1):
        ds.GetRasterBand(i).SetDescription(d)
    ds = None  # flush/close

def _validate_same_band_descriptions(downloaded_paths: List[str], *, sample_n: int = 5) -> List[str]:
    if not downloaded_paths:
        raise RuntimeError("No downloaded files to validate band descriptions.")

    ref_path = downloaded_paths[0]
    ref_desc = _get_band_descriptions(ref_path)

    if any(d.strip() == "" for d in ref_desc):
        warnings.warn(
            f"Reference tile {os.path.basename(ref_path)} has empty GDAL band descriptions; "
            "output will have unnamed bands (terra will show lyr.1, lyr.2, …).",
            RuntimeWarning
        )

    mismatches = []
    for p in downloaded_paths[1:]:
        d = _get_band_descriptions(p)
        if d != ref_desc:
            mismatches.append((p, d))

    if mismatches:
        lines = []
        lines.append("Downloaded tiles do not all share identical GDAL band descriptions.")
        lines.append(f"Reference file: {os.path.basename(ref_path)}")
        lines.append(f"Reference descriptions ({len(ref_desc)} bands): {ref_desc}")
        lines.append("")
        lines.append("Example mismatches:")
        for p, d in mismatches[:sample_n]:
            lines.append(f"  - {os.path.basename(p)}")
            lines.append(f"    descriptions ({len(d)} bands): {d}")
        if len(mismatches) > sample_n:
            lines.append(f"  ... and {len(mismatches) - sample_n} more mismatching file(s).")
        raise ValueError("\n".join(lines))

    return ref_desc


def _format_band_names_for_gdal(descriptions: List[str]) -> str:
    # GDAL wants comma-separated list; escape commas defensively
    return ",".join([d.replace(",", "_") for d in descriptions])

def _set_band_descriptions_vrt(vrt_path: str, descriptions: List[str]) -> None:
    ds = gdal.Open(vrt_path, gdal.GA_Update)
    if ds is None:
        raise RuntimeError(f"Could not open VRT for update: {vrt_path}")
    if ds.RasterCount != len(descriptions):
        ds = None
        raise ValueError("Band count mismatch when setting VRT descriptions.")
    for i, d in enumerate(descriptions, start=1):
        ds.GetRasterBand(i).SetDescription(d)
    ds = None


def download_merge_from_drive(
    description: str,
    local_filename: str,
    drive_folder: str,
    service_account_file: str,
    *,
    # Output format control
    as_cog: bool = False,               # if True, write Cloud-Optimized GeoTIFF (COG); else GeoTIFF
    # Output & mosaic controls
    compress: str = "DEFLATE",          # DEFLATE|LZW|ZSTD|JPEG
    predictor: Optional[int] = None,    # 2 for integers, 3 for floats; None = let GDAL decide
    blocksize: int = 512,               # GeoTIFF: BLOCKXSIZE/BLOCKYSIZE; COG: BLOCKSIZE
    bigtiff: str = "YES",               # YES|IF_SAFER
    dst_srs: Optional[str] = None,      # e.g., "EPSG:5070" (if reprojection needed)
    xres: Optional[float] = None,       # set both xres & yres to enforce pixel size (e.g., 30)
    yres: Optional[float] = None,
    dst_nodata: Optional[float] = None, # set if you need a specific nodata in output
    # Drive / listing controls
    drive_id: Optional[str] = None,     # if folder is in a Shared Drive, you can pass the Drive ID
    include_all_drives: bool = True,    # search across Shared Drives
    # Behavior controls
    check_existing: bool = True,
    n_workers: int = 4,
    verbose: bool = True,
    error_if_no_descriptions: bool = False,
) -> str:
    """
    Download all Drive .tif/.TIF files whose names contain `description` from `drive_folder`,
    build a VRT mosaic, then stream out either a tiled+compressed GeoTIFF or a COG.

    Ensures the final output's GDAL band descriptions match the downloaded tiles and
    errors if tiles disagree.
    """

    local_filename = os.path.abspath(local_filename)
    if check_existing and os.path.exists(local_filename):
        if verbose:
            print(f"File already exists: {local_filename}")
        return local_filename

    temp_dir = tempfile.mkdtemp()
    vrt_path = os.path.join(temp_dir, "mosaic.vrt")
    downloaded_paths: List[str] = []

    SCOPES = ["https://www.googleapis.com/auth/drive.readonly"]
    cred_path = os.path.abspath(service_account_file)
    credentials = service_account.Credentials.from_service_account_file(cred_path, scopes=SCOPES)
    service = build("drive", "v3", credentials=credentials)

    list_kwargs = dict(
        fields="nextPageToken, files(id, name, parents)",
        supportsAllDrives=True,
        includeItemsFromAllDrives=True
    ) if include_all_drives else dict(fields="nextPageToken, files(id, name, parents)")

    try:
        folder_q = f"name='{drive_folder}' and mimeType='application/vnd.google-apps.folder' and trashed=false"
        folder_kwargs = dict(
            q=folder_q,
            pageSize=1000,
            corpora=("drive" if drive_id else "allDrives") if include_all_drives else "user",
            driveId=drive_id,
            **list_kwargs
        )

        folders, page_token = [], None
        while True:
            resp = service.files().list(pageToken=page_token, **folder_kwargs).execute()
            folders.extend(resp.get("files", []))
            page_token = resp.get("nextPageToken")
            if not page_token:
                break

        if not folders:
            raise FileNotFoundError(
                f"Folder '{drive_folder}' not found. "
                "Ensure it exists and is shared with the service account "
                f"({credentials.service_account_email}). "
                "If it is on a Shared Drive, also ensure the service account has at least Viewer."
            )
        folder_id = folders[0]["id"]

        files = list_all_files_in_folder(
            service,
            folder_id,
            description=description,
            include_all_drives=include_all_drives,
            drive_id=drive_id,
            page_size=1000
        )
        if not files:
            raise FileNotFoundError(
                f"No matching .tif/.TIF files found for '{description}' in '{drive_folder}'. "
                "Check filenames and sharing."
            )

        if verbose:
            print(f"Found {len(files)} file(s). Starting download to {temp_dir} ...")

        if n_workers <= 1:
            for f in files:
                p = _download_file_from_drive(f["id"], f["name"], temp_dir, service_account_file)
                if p:
                    downloaded_paths.append(p)
        else:
            with ThreadPoolExecutor(max_workers=n_workers) as ex:
                futures = [
                    ex.submit(_download_file_from_drive, f["id"], f["name"], temp_dir, service_account_file)
                    for f in files
                ]
                for fut in as_completed(futures):
                    p = fut.result()
                    if p:
                        downloaded_paths.append(p)

        if not downloaded_paths:
            raise RuntimeError("Downloads finished but no files were saved.")

        ref_band_desc = _validate_same_band_descriptions(downloaded_paths)

        if error_if_no_descriptions:
            if all((d is None) or (str(d).strip() == "") for d in ref_band_desc):
                raise ValueError(
                    "All GDAL band descriptions are empty for the downloaded raster(s). "
                    "Set band names upstream (preferred), or run with error_if_no_descriptions=False "
                    "to allow unnamed bands."
                )

        print("Confirmed all band descriptions exist and are consistent across downloaded tiles.")

        needs_warp = any(v is not None for v in (dst_srs, xres, yres, dst_nodata))

        # ---------------------------------------------------------
        # Fast path: only one tile -> translate/warp directly
        # ---------------------------------------------------------
        if len(downloaded_paths) == 1:
            src_path = downloaded_paths[0]
            if verbose:
                print("Single tile detected; skipping VRT mosaic step.")

            if not as_cog:
                co = [
                    f"BIGTIFF={bigtiff}",
                    "TILED=YES",
                    f"COMPRESS={compress.upper()}",
                    f"BLOCKXSIZE={int(blocksize)}",
                    f"BLOCKYSIZE={int(blocksize)}",
                    "OVERVIEWS=AUTO",
                    "RESAMPLING=AVERAGE",
                    "NUM_THREADS=ALL_CPUS",
                ]
                if predictor is not None:
                    co.append(f"PREDICTOR={int(predictor)}")

                if needs_warp:
                    if verbose:
                        print("Warping single tile (streamed) to target grid...")
                    warp_kwargs = dict(
                        format="GTiff",
                        creationOptions=co,
                        multithread=True,
                    )
                    if dst_srs:
                        warp_kwargs["dstSRS"] = dst_srs
                    if xres and yres:
                        warp_kwargs["xRes"] = float(xres)
                        warp_kwargs["yRes"] = float(yres)
                        warp_kwargs["targetAlignedPixels"] = True
                    if dst_nodata is not None:
                        warp_kwargs["dstNodata"] = dst_nodata

                    out_ds = gdal.Warp(local_filename, src_path, **warp_kwargs)
                    if out_ds is None:
                        raise RuntimeError("gdal.Warp returned None (no GDAL exception was raised).")
                    out_ds = None
                else:
                    if verbose:
                        print("Translating single tile to tiled, compressed GeoTIFF...")
                    out_ds = gdal.Translate(local_filename, src_path, creationOptions=co)
                    if out_ds is None:
                        raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                    out_ds = None

            else:
                co_cog = [
                    f"COMPRESS={compress.upper()}",
                    f"BLOCKSIZE={int(blocksize)}",
                    "NUM_THREADS=ALL_CPUS",
                    f"BIGTIFF={bigtiff}",
                    "OVERVIEW_RESAMPLING=AVERAGE",
                    f"BAND_NAMES={_format_band_names_for_gdal(ref_band_desc)}",
                ]
                if predictor is not None:
                    co_cog.append(f"PREDICTOR={int(predictor)}")

                if needs_warp:
                    if verbose:
                        print("Warping single tile to target grid (VRT) and writing COG...")
                    warped_vrt = os.path.join(temp_dir, "warped_single.vrt")
                    warp_kwargs = dict(
                        format="VRT",
                        multithread=True,
                    )
                    if dst_srs:
                        warp_kwargs["dstSRS"] = dst_srs
                    if xres and yres:
                        warp_kwargs["xRes"] = float(xres)
                        warp_kwargs["yRes"] = float(yres)
                        warp_kwargs["targetAlignedPixels"] = True
                    if dst_nodata is not None:
                        warp_kwargs["dstNodata"] = dst_nodata

                    out_warp = gdal.Warp(warped_vrt, src_path, **warp_kwargs)
                    if out_warp is None:
                        raise RuntimeError("gdal.Warp returned None (no GDAL exception was raised).")
                    out_warp = None

                    _set_band_descriptions_vrt(warped_vrt, ref_band_desc)

                    src_vrt = os.path.join(temp_dir, "src_single.vrt")

                    tmp = gdal.Translate(src_vrt, src_path, format="VRT")
                    if tmp is None:
                        raise RuntimeError("gdal.Translate to VRT returned None.")
                    tmp = None

                    _set_band_descriptions_vrt(src_vrt, ref_band_desc)

                    out_ds = gdal.Translate(local_filename, src_vrt, format="COG", creationOptions=co_cog)
                    if out_ds is None:
                        raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                    out_ds = None

                    if out_ds is None:
                        raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                    out_ds = None
                else:
                    if verbose:
                        print("Translating single tile directly to COG (internal overviews)...")
                    out_ds = gdal.Translate(local_filename, src_path, format="COG", creationOptions=co_cog)
                    if out_ds is None:
                        raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                    out_ds = None
                    
            if not as_cog:
                _set_band_descriptions(local_filename, ref_band_desc)

            if verbose:
                print(f"Final output saved to: {local_filename}")
            return local_filename

        # ---------------------------------------------------------
        # Otherwise: multi-tile path
        # ---------------------------------------------------------
        if verbose:
            print(f"Building VRT from {len(downloaded_paths)} tile(s) ...")

        vrt_path = os.path.join(temp_dir, "mosaic.vrt")
        vrt_options = {}
        if dst_nodata is not None:
            vrt_options.update(dict(VRTNodata=dst_nodata))

        vrt = gdal.BuildVRT(vrt_path, downloaded_paths, **vrt_options)
        if vrt is None:
            raise RuntimeError("gdal.BuildVRT returned None (no GDAL exception was raised).")
        vrt = None  # flush

        _set_band_descriptions_vrt(vrt_path, ref_band_desc)


        # -------------------------
        # Write output (GeoTIFF vs COG)
        # -------------------------
        if not as_cog:
            co = [
                f"BIGTIFF={bigtiff}",
                "TILED=YES",
                f"COMPRESS={compress.upper()}",
                f"BLOCKXSIZE={int(blocksize)}",
                f"BLOCKYSIZE={int(blocksize)}",
                "OVERVIEWS=AUTO",
                "RESAMPLING=AVERAGE",
                "NUM_THREADS=ALL_CPUS",
            ]
            if predictor is not None:
                co.append(f"PREDICTOR={int(predictor)}")

            if needs_warp:
                if verbose:
                    print("Warping (streamed) to target grid...")
                warp_kwargs = dict(
                    format="GTiff",
                    creationOptions=co,
                    multithread=True,
                )
                if dst_srs:
                    warp_kwargs["dstSRS"] = dst_srs
                if xres and yres:
                    warp_kwargs["xRes"] = float(xres)
                    warp_kwargs["yRes"] = float(yres)
                    warp_kwargs["targetAlignedPixels"] = True
                if dst_nodata is not None:
                    warp_kwargs["dstNodata"] = dst_nodata

                out_ds = gdal.Warp(local_filename, vrt_path, **warp_kwargs)
                if out_ds is None:
                    raise RuntimeError("gdal.Warp returned None (no GDAL exception was raised).")
                out_ds = None
            else:
                if verbose:
                    print("Translating VRT to tiled, compressed GeoTIFF (streamed)...")
                out_ds = gdal.Translate(local_filename, vrt_path, creationOptions=co)
                if out_ds is None:
                    raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                out_ds = None

        else:
            co_cog = [
                f"COMPRESS={compress.upper()}",
                f"BLOCKSIZE={int(blocksize)}",
                "NUM_THREADS=ALL_CPUS",
                f"BIGTIFF={bigtiff}",
                "OVERVIEW_RESAMPLING=AVERAGE",
                f"BAND_NAMES={_format_band_names_for_gdal(ref_band_desc)}",
            ]
            if predictor is not None:
                co_cog.append(f"PREDICTOR={int(predictor)}")

            if needs_warp:
                if verbose:
                    print("Warping to target grid (VRT) and writing COG...")
                warped_vrt = os.path.join(temp_dir, "warped.vrt")
                warp_kwargs = dict(
                    format="VRT",
                    multithread=True,
                )
                if dst_srs:
                    warp_kwargs["dstSRS"] = dst_srs
                if xres and yres:
                    warp_kwargs["xRes"] = float(xres)
                    warp_kwargs["yRes"] = float(yres)
                    warp_kwargs["targetAlignedPixels"] = True
                if dst_nodata is not None:
                    warp_kwargs["dstNodata"] = dst_nodata

                out_warp = gdal.Warp(warped_vrt, vrt_path, **warp_kwargs)
                if out_warp is None:
                    raise RuntimeError("gdal.Warp returned None (no GDAL exception was raised).")
                out_warp = None

                _set_band_descriptions_vrt(warped_vrt, ref_band_desc)

                out_ds = gdal.Translate(local_filename, warped_vrt, format="COG", creationOptions=co_cog)
                if out_ds is None:
                    raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                out_ds = None
            else:
                if verbose:
                    print("Translating VRT directly to COG (internal overviews)...")
                out_ds = gdal.Translate(local_filename, vrt_path, format="COG", creationOptions=co_cog)
                if out_ds is None:
                    raise RuntimeError("gdal.Translate returned None (no GDAL exception was raised).")
                out_ds = None

        if not as_cog:
            _set_band_descriptions(local_filename, ref_band_desc)

        if verbose:
            print(f"Final output saved to: {local_filename}")
        return local_filename

    except HttpError as e:
        msg = str(e)
        if "accessNotConfigured" in msg or "has not been used in project" in msg:
            raise RuntimeError(
                "Google Drive API is disabled for this service account’s project.\n"
                "Enable the **Google Drive API** in GCP → APIs & Services → Library, then retry."
            ) from e
        raise
    except KeyboardInterrupt:
        print("Interrupted by user. Cleaning up and exiting.")
        raise
    finally:
        try:
            shutil.rmtree(temp_dir)
            if verbose:
                print(f"Cleaned up temporary directory: {temp_dir}")
        except Exception as cleanup_err:
            print(f"Error during cleanup: {cleanup_err}")