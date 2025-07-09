import os
from pathlib import Path
from typing import Union, List
import time
import ee
import os
import shutil
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


def download_merge_from_drive(
    description: str,
    local_filename: str,
    drive_folder: str,
    service_account_file: str,
    compress: str = "deflate",
    check_existing: bool = True
) -> str:
    local_filename = os.path.abspath(local_filename)
    if check_existing and os.path.exists(local_filename):
        print(f"✅ File already exists, skipping download and merge: {local_filename}")
        return local_filename

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

    try:
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

        with rasterio.open(local_filename, "w", **out_meta) as dest:
            dest.write(mosaic)

        for src in src_files_to_mosaic:
            src.close()

        print(f"✅ Final merged GeoTIFF saved to: {local_filename}")
        return local_filename

    finally:
        shutil.rmtree(temp_dir, ignore_errors=True)
        print(f"🧹 Cleaned up temporary files: {temp_dir}")

def export_image_to_drive_and_download(
    image: ee.Image,
    region: ee.Geometry,
    description: str,
    local_filename: str,
    drive_folder: str = "EarthEngineExports",
    service_account_file: str = "your-service-account.json",
    scale: int = 30,
    wait_interval: int = 30,
    compress: str = "deflate",
    check_existing: bool = True
) -> str:
    local_filename = os.path.abspath(local_filename)
    if check_existing and os.path.exists(local_filename):
        print(f"✅ File already exists, skipping export: {local_filename}")
        return local_filename

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
        compress=compress,
        check_existing=check_existing
    )
