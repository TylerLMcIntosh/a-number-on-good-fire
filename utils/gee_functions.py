import os
import shutil
import tempfile
import time
import random
import contextlib
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Union, List
from pathlib import Path


import ee
import rioxarray as rxr
from rioxarray.merge import merge_arrays

import xarray as xr
from google.oauth2 import service_account
from googleapiclient.discovery import build
from googleapiclient.http import MediaIoBaseDownload



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
        print(f"⬇Downloaded {file_name}")
        return local_path
    except Exception as e:
        print(f"Error downloading {file_name}: {e}")
        return None


def download_merge_from_drive(
    description: str,
    local_filename: str,
    drive_folder: str,
    service_account_file: str,
    compress: str = "deflate",
    check_existing: bool = True,
    n_workers: int = 1
) -> str:
    local_filename = os.path.abspath(local_filename)
    if check_existing and os.path.exists(local_filename):
        print(f"File already exists: {local_filename}")
        return local_filename

    temp_dir = tempfile.mkdtemp()
    datasets = []

    try:
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
            raise FileNotFoundError(f"Folder '{drive_folder}' not found or not shared with service account.")
        folder_id = folders[0]['id']

        file_results = service.files().list(
            q=f"'{folder_id}' in parents and trashed=false and name contains '{description}' and name contains '.tif'",
            fields="files(id, name)"
        ).execute()
        files = file_results.get('files', [])
        if not files:
            raise FileNotFoundError(f"No matching .tif files found for '{description}' in '{drive_folder}'.")

        print(f"Found {len(files)} files. Starting download...")

        downloaded_paths = []
        if n_workers == 1:
            for file in files:
                path = _download_file_from_drive(file['id'], file['name'], temp_dir, service_account_file)
                if path:
                    downloaded_paths.append(path)
        else:
            with ThreadPoolExecutor(max_workers=n_workers) as executor:
                futures = [
                    executor.submit(_download_file_from_drive, file['id'], file['name'], temp_dir, service_account_file)
                    for file in files
                ]
                for future in as_completed(futures):
                    result = future.result()
                    if result:
                        downloaded_paths.append(result)

        for f in downloaded_paths:
            ds = rxr.open_rasterio(f, masked=True, chunks=True)
            datasets.append(ds)

        mosaic = merge_arrays(datasets)
        mosaic.rio.to_raster(local_filename, compress=compress)
        print(f"Final merged GeoTIFF saved to: {local_filename}")
        return local_filename

    except KeyboardInterrupt:
        print("Interrupted by user. Cleaning up and exiting.")
        raise

    finally:
        for ds in datasets:
            with contextlib.suppress(Exception):
                ds.close()
        try:
            shutil.rmtree(temp_dir)
            print(f"Cleaned up temporary directory: {temp_dir}")
        except Exception as cleanup_err:
            print(f"Error during cleanup: {cleanup_err}")

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
    check_existing: bool = True,
    n_workers: int = 1
) -> str:
    local_filename = os.path.abspath(local_filename)
    if check_existing and os.path.exists(local_filename):
        print(f"File already exists: {local_filename}")
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
    print(f"Started Earth Engine export: {description}")

    while task.active():
        print("Waiting for Earth Engine export to finish...")
        time.sleep(wait_interval)

    status = task.status()
    if status["state"] != "COMPLETED":
        raise RuntimeError(f"Export failed: {status}")

    print("Earth Engine export complete. Downloading from Drive...")

    return download_merge_from_drive(
        description=description,
        local_filename=local_filename,
        drive_folder=drive_folder,
        service_account_file=service_account_file,
        compress=compress,
        check_existing=check_existing,
        n_workers=n_workers
    )