import os
from pathlib import Path
from typing import Union, List
import time
import ee
import os
from google.oauth2 import service_account
from googleapiclient.discovery import build
from googleapiclient.http import MediaIoBaseDownload
import io
import tempfile
import rasterio
from rasterio.merge import merge

def dir_ensure(paths: Union[str, List[str]]) -> List[Path]:
    """
    Ensure that one or more directories exist. Create them if they do not.

    Parameters:
    - paths: A single path or a list of paths (str or Path)

    Returns:
    - List of Path objects that were checked/created.
    """
    if isinstance(paths, (str, Path)):
        paths = [paths]
    elif not isinstance(paths, (list, tuple)):
        raise TypeError("`paths` must be a string, Path, or list of strings/Paths.")

    created_paths = []

    for p in paths:
        path = Path(p).expanduser().resolve()
        try:
            if not path.exists():
                path.mkdir(parents=True, exist_ok=True)
                print(f"📁 Directory created: {path}")
            else:
                print(f"✅ Directory already exists: {path}")
            created_paths.append(path)
        except Exception as e:
            print(f"⚠️ Failed to create directory: {path} — {e}")

    return created_paths


# def export_image_to_drive_and_download(
#     image: ee.Image,
#     region: ee.Geometry,
#     description: str,
#     local_filename: str,
#     drive_folder: str = "EarthEngineExports",
#     service_account_file: str = "your-service-account.json",
#     scale: int = 30,
#     wait_interval: int = 30
# ) -> str:
#     """
#     Export an Earth Engine image to Drive, then download and merge the result using a service account.

#     Parameters:
#     - image: Earth Engine image to export.
#     - region: Geometry defining the export region.
#     - description: Prefix for exported files.
#     - local_filename: Final local merged filename.
#     - drive_folder: Google Drive folder shared with the service account.
#     - service_account_file: Path to service account key JSON.
#     - scale: Export scale in meters.
#     - wait_interval: Seconds between task status checks.

#     Returns:
#     - Path to merged GeoTIFF.
#     """

#     # Start EE export
#     task = ee.batch.Export.image.toDrive(
#         image=image.clip(region),
#         description=description,
#         folder=drive_folder,
#         fileNamePrefix=description,
#         region=region.bounds().getInfo()["coordinates"],
#         scale=scale,
#         maxPixels=1e13
#     )
#     task.start()
#     print(f"🚀 Started Earth Engine export: {description}")

#     # Wait for completion
#     while task.active():
#         print("⏳ Waiting for Earth Engine export to finish...")
#         time.sleep(wait_interval)

#     status = task.status()
#     if status["state"] != "COMPLETED":
#         raise RuntimeError(f"❌ Export failed: {status}")

#     print("✅ Earth Engine export complete. Downloading from Drive...")

#     # Authenticate service account
#     SCOPES = ['https://www.googleapis.com/auth/drive.readonly']
#     credentials = service_account.Credentials.from_service_account_file(
#         service_account_file, scopes=SCOPES
#     )
#     service = build('drive', 'v3', credentials=credentials)

#     # Find the Drive folder
#     folder_results = service.files().list(
#         q=f"name='{drive_folder}' and mimeType='application/vnd.google-apps.folder' and trashed=false",
#         fields="files(id, name)"
#     ).execute()

#     folders = folder_results.get('files', [])
#     if not folders:
#         raise FileNotFoundError(f"❌ Folder '{drive_folder}' not found or not shared with service account.")

#     folder_id = folders[0]['id']

#     # Find all .tif files with matching prefix
#     file_results = service.files().list(
#         q=f"'{folder_id}' in parents and trashed=false and name contains '{description}' and name contains '.tif'",
#         fields="files(id, name)"
#     ).execute()

#     files = file_results.get('files', [])
#     if not files:
#         raise FileNotFoundError(f"❌ No matching .tif files found for '{description}' in '{drive_folder}'.")

#     print(f"📁 Found {len(files)} files. Downloading...")

#     # Download files
#     temp_dir = tempfile.mkdtemp()
#     downloaded_paths = []

#     for file in files:
#         file_id = file['id']
#         file_name = file['name']
#         local_path = os.path.join(temp_dir, file_name)

#         request = service.files().get_media(fileId=file_id)
#         with open(local_path, 'wb') as f:
#             downloader = MediaIoBaseDownload(f, request)
#             done = False
#             while not done:
#                 status, done = downloader.next_chunk()
#                 print(f"⬇️ Downloaded {file_name} - {int(status.progress() * 100)}%")

#         downloaded_paths.append(local_path)

#     # Merge GeoTIFFs
#     src_files_to_mosaic = [rasterio.open(f) for f in downloaded_paths]
#     mosaic, out_transform = merge(src_files_to_mosaic)

#     out_meta = src_files_to_mosaic[0].meta.copy()
#     out_meta.update({
#         "driver": "GTiff",
#         "height": mosaic.shape[1],
#         "width": mosaic.shape[2],
#         "transform": out_transform
#     })

#     with rasterio.open(local_filename, "w", **out_meta) as dest:
#         dest.write(mosaic)

#     for src in src_files_to_mosaic:
#         src.close()

#     print(f"✅ Final merged GeoTIFF saved to: {local_filename}")
#     return local_filename




def download_merge_from_drive(
    description: str,
    local_filename: str,
    drive_folder: str,
    service_account_file: str,
    compress: str = "deflate"
) -> str:
    """
    Download GeoTIFF files from Google Drive and merge into one file.

    Parameters:
    - description: File name prefix to search for in Drive.
    - local_filename: Full path for the final merged file.
    - drive_folder: Folder in Google Drive containing the exports.
    - service_account_file: Full path to service account JSON.
    - compress: Compression method for output GeoTIFF (e.g., "deflate", "lzw").

    Returns:
    - Path to the merged GeoTIFF file.
    """
    SCOPES = ['https://www.googleapis.com/auth/drive.readonly']
    credentials = service_account.Credentials.from_service_account_file(
        os.path.abspath(service_account_file), scopes=SCOPES
    )
    service = build('drive', 'v3', credentials=credentials)

    folder_results = service.files().list(
        q=f"name='{drive_folder}' and mimeType='application/vnd.google-apps.folder' and trashed=false",
        fields="files(id, name)"
    ).execute()

    folders = folder_results.get('files', [])
    if not folders:
        raise FileNotFoundError(f"❌ Folder '{drive_folder}' not found or not shared with service account.")

    folder_id = folders[0]['id']

    file_results = service.files().list(
        q=f"'{folder_id}' in parents and trashed=false and name contains '{description}' and name contains '.tif'",
        fields="files(id, name)"
    ).execute()

    files = file_results.get('files', [])
    if not files:
        raise FileNotFoundError(f"❌ No matching .tif files found for '{description}' in '{drive_folder}'.")

    print(f"📁 Found {len(files)} files. Downloading...")

    temp_dir = tempfile.mkdtemp()
    downloaded_paths = []

    for file in files:
        file_id = file['id']
        file_name = file['name']
        local_path = os.path.join(temp_dir, file_name)

        request = service.files().get_media(fileId=file_id)
        with open(local_path, 'wb') as f:
            downloader = MediaIoBaseDownload(f, request)
            done = False
            while not done:
                status, done = downloader.next_chunk()
                print(f"⬇️ Downloaded {file_name} - {int(status.progress() * 100)}%")

        downloaded_paths.append(local_path)

    src_files_to_mosaic = [rasterio.open(f) for f in downloaded_paths]
    mosaic, out_transform = merge(src_files_to_mosaic)

    out_meta = src_files_to_mosaic[0].meta.copy()
    out_meta.update({
        "driver": "GTiff",
        "height": mosaic.shape[1],
        "width": mosaic.shape[2],
        "transform": out_transform,
        "compress": compress,
        "tiled": True
    })

    local_filename = os.path.abspath(local_filename)
    with rasterio.open(local_filename, "w", **out_meta) as dest:
        dest.write(mosaic)

    for src in src_files_to_mosaic:
        src.close()

    print(f"✅ Final merged GeoTIFF saved to: {local_filename}")
    return local_filename


def export_image_to_drive_and_download(
    image: ee.Image,
    region: ee.Geometry,
    description: str,
    local_filename: str,
    drive_folder: str = "EarthEngineExports",
    service_account_file: str = "your-service-account.json",
    scale: int = 30,
    wait_interval: int = 30,
    compress: str = "deflate"
) -> str:
    """
    Export an Earth Engine image to Drive, then download and merge the result.

    Parameters:
    - image: Earth Engine image to export.
    - region: Geometry defining the export region.
    - description: Prefix for exported files.
    - local_filename: Path to final merged file (can be full path).
    - drive_folder: Google Drive folder shared with service account.
    - service_account_file: Path to service account JSON (can be full path).
    - scale: Export resolution in meters.
    - wait_interval: Seconds to wait between checking task status.
    - compress: Compression type for the final GeoTIFF.

    Returns:
    - Path to merged GeoTIFF.
    """
    task = ee.batch.Export.image.toDrive(
        image=image.clip(region),
        description=description,
        folder=drive_folder,
        fileNamePrefix=description,
        region=region.bounds().getInfo()["coordinates"],
        scale=scale,
        maxPixels=1e13
    )
    task.start()
    print(f"🚀 Started Earth Engine export: {description}")

    while task.active():
        print("⏳ Waiting for Earth Engine export to finish...")
        time.sleep(wait_interval)

    status = task.status()
    if status["state"] != "COMPLETED":
        raise RuntimeError(f"❌ Export failed: {status}")

    print("✅ Earth Engine export complete. Downloading from Drive...")

    return download_merge_from_drive(
        description=description,
        local_filename=local_filename,
        drive_folder=drive_folder,
        service_account_file=service_account_file,
        compress=compress
    )
