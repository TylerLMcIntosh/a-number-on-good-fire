#' Install and Load Required Packages with Automatic pak Fallback
#'
#' Installs and loads CRAN and GitHub packages automatically.
#' Uses pak if available and functional, falls back to base installers otherwise.
#'
#' @param package_list A character vector of package names. GitHub packages should be "user/repo".
#' @return NULL
#' @examples
#' install_and_load_packages(c("dplyr", "sf", "user/repo"))
install_and_load_packages <- function(package_list) {
  # Ensure remotes is available for GitHub fallback
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  
  # Classify packages
  missing_cran <- c()
  missing_github <- c()
  
  for (pkg in package_list) {
    is_github <- grepl("/", pkg)
    pkg_name <- if (is_github) strsplit(pkg, "/")[[1]][2] else pkg
    
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      if (is_github) {
        missing_github <- c(missing_github, pkg)
      } else {
        missing_cran <- c(missing_cran, pkg)
      }
    }
  }
  
  # pak installer fallback
  try_pak_install <- function(pkgs) {
    tryCatch({
      if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
      pak::pkg_install(pkgs, upgrade = TRUE)
      TRUE
    }, error = function(e) {
      message("pak::pkg_install() failed: ", conditionMessage(e))
      FALSE
    })
  }
  
  fallback_cran <- function(pkgs) {
    install.packages(pkgs)
  }
  
  fallback_github <- function(pkgs) {
    for (repo in pkgs) {
      tryCatch({
        remotes::install_github(repo)
      }, error = function(e) {
        message("Failed to install GitHub repo ", repo, ": ", conditionMessage(e))
      })
    }
  }
  
  # Install CRAN packages
  if (length(missing_cran) > 0) {
    if (!try_pak_install(missing_cran)) fallback_cran(missing_cran)
  }
  
  # Install GitHub packages
  if (length(missing_github) > 0) {
    if (!try_pak_install(missing_github)) fallback_github(missing_github)
  }
  
  # Load all packages
  for (pkg in package_list) {
    pkg_name <- if (grepl("/", pkg)) strsplit(pkg, "/")[[1]][2] else pkg
    tryCatch({
      library(pkg_name, character.only = TRUE)
    }, error = function(e) {
      message("Failed to load ", pkg_name, ": ", conditionMessage(e))
    })
  }
  
  invisible(NULL)
}


#' 
#' #' Install and Load Required Packages Using pak
#' #'
#' #' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' #' If any packages are missing, it installs them automatically.
#' #' It uses the `pak` package for faster and more efficient package installation.
#' #'
#' #' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' #' GitHub packages should be specified as `username/repo` in strings.
#' #' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' #' without asking for user permission. If "n", asks for permission from the user.
#' #' @return No return value. Installs and loads the specified packages as needed.
#' #' @examples
#' #' \dontrun{
#' #' install_and_load_packages(c(dplyr, here, "username/repo"))
#' #' }
#' #' @importFrom pak pkg_install
#' #' @export
#' install_and_load_packages <- function(package_list, auto_install = "n") {
#'   # Convert non-string package names to strings
#'   package_list <- lapply(package_list, function(pkg) {
#'     if (is.symbol(pkg)) {
#'       deparse(substitute(pkg))
#'     } else {
#'       pkg
#'     }
#'   })
#'   
#'   # # Check if 'renv' is installed; if not, skip the 'renv' check
#'   # if (requireNamespace("renv", quietly = TRUE) && renv::is_active()) {
#'   #   cat("renv is active. Only loading packages...\n")
#'   #   for (pkg in package_list) {
#'   #     package_name <- if (grepl("/", pkg)) unlist(strsplit(pkg, "/"))[2] else pkg
#'   #     if (!require(package_name, character.only = TRUE)) {
#'   #       cat("Failed to load package:", package_name, "\n")
#'   #     }
#'   #   }
#'   #   return(invisible())
#'   # }
#'   
#'   # Check if pak is installed; install if not
#'   if (!requireNamespace("pak", quietly = TRUE)) {
#'     cat("The 'pak' package is required for fast installation of packages, installing now.\n")
#'     install.packages("pak")
#'   }
#'   
#'   # Initialize lists to store missing CRAN and GitHub packages
#'   missing_cran_packages <- c()
#'   missing_github_packages <- c()
#'   
#'   # # Helper function to get user input
#'   # get_user_permission <- function(prompt_msg) {
#'   #   if (auto_install == "y") {
#'   #     return("y")
#'   #   } else {
#'   #     return(tolower(readline(prompt = prompt_msg)))
#'   #   }
#'   # }
#'   
#'   # Check for missing packages
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
#'     } else { # CRAN package
#'       package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
#'     }
#'     if (!package_loaded) {
#'       if (grepl("/", pkg)) {
#'         missing_github_packages <- c(missing_github_packages, pkg)
#'       } else {
#'         missing_cran_packages <- c(missing_cran_packages, pkg)
#'       }
#'     }
#'   }
#'   
#'   # Install missing CRAN packages using pak::pkg_install
#'   if (length(missing_cran_packages) > 0) {
#'     # cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
#'     # response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
#'     # if (response == "y") {
#'       pak::pkg_install(missing_cran_packages, upgrade = TRUE)
#'     # } else {
#'     #   cat("Skipping installation of missing CRAN packages.\n")
#'     # }
#'   }
#'   
#'   # Install missing GitHub packages using pak::pkg_install
#'   if (length(missing_github_packages) > 0) {
#'     # cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
#'     # response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
#'     # if (response == "y") {
#'       pak::pkg_install(missing_github_packages, upgrade = TRUE)
#'     # } else {
#'     #   cat("Skipping installation of missing GitHub packages.\n")
#'     # }
#'   }
#'   
#'   # Load all packages after checking for installation
#'   for (pkg in package_list) {
#'     if (grepl("/", pkg)) { # GitHub package
#'       package_name <- unlist(strsplit(pkg, "/"))[2]
#'       if (!require(package_name, character.only = TRUE)) {
#'         cat("Failed to load GitHub package:", package_name, "\n")
#'       }
#'     } else { # CRAN package
#'       if (!require(pkg, character.only = TRUE)) {
#'         cat("Failed to load CRAN package:", pkg, "\n")
#'       }
#'     }
#'   }
#'   
#'   cat("All specified packages installed and loaded.\n")
#' }


#' Access MTBS CONUS Polygons
#'
#' This function accesses the MTBS (Monitoring Trends in Burn Severity) CONUS (Continental United States) polygons by downloading and reading the MTBS perimeter shapefile directly from the USGS website. The shapefile is accessed via a URL and read into an `sf` object.
#'
#' @return An `sf` object containing the MTBS CONUS polygons.
#' @examples
#' \dontrun{
#' mtbs_data <- access_data_mtbs_conus()
#' print(mtbs_data)
#' }
#' 
#' @importFrom sf st_read 
#' @export
access_data_mtbs_conus <- function() {
  mtbs <- paste0(
    "/vsizip/vsicurl/",
    "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip",
    "/mtbs_perims_DD.shp"
  ) |>
    sf::st_read()
  
  return(mtbs)
}

#' Read a CSV File from Google Drive
#'
#' Downloads and reads a CSV file from a specified Google Drive folder using the file name.
#' This function is SIGNIFICANTLY faster than the original function
#'
#' This function locates a file by name within a Google Drive folder, retrieves its file ID,
#' reads its content as a string, and converts it into a data frame.
#'
#' @param drive_folder A character string. The path to the Google Drive folder (can be a name or ID).
#' @param file_name A character string. The exact name of the file to be read from the Drive folder.
#'
#' @return A data frame containing the contents of the CSV file.
#' @export
#'
#' @importFrom googledrive drive_ls drive_get drive_read_string as_id
#' @importFrom dplyr filter pull
#'
#' @examples
#' \dontrun{
#' df <- read_csv_from_gdrive_v2("my-folder", "my-file.csv")
#' }
read_csv_from_gdrive_v2 <- function(drive_folder, file_name) {
  # Get file ID
  folder_contents <- googledrive::drive_ls(path = drive_folder)
  id <- folder_contents |>
    dplyr::filter(name == file_name) |>
    dplyr::pull(id)
  
  if (length(id) == 0) {
    stop("File not found in specified Drive folder.")
  } else {
    message("File found: ", file_name)
  }
  
  # Get file metadata
  f <- googledrive::drive_get(googledrive::as_id(id))
  
  # Read the content of the file as a string and convert to data frame
  csv <- f |>
    googledrive::drive_read_string() %>%
    read.csv(text = .)
  
  return(csv)
}



#' Download a file from a Google Drive folder (fast listing)
#'
#' Lists the specified Google Drive folder, finds the file by exact name, and downloads it.
#'
#' @param drive_folder Character. Path or ID of the Google Drive folder.
#' @param file_name Character. Exact file name inside the folder.
#' @param local_path Character. Local path where the file will be saved.
#' @param overwrite Logical. Overwrite local file if it exists? Default TRUE.
#'
#' @return Invisibly returns the local path.
#' @export
#'
#' @importFrom googledrive drive_ls drive_download as_id
#' @importFrom dplyr filter pull
download_data_from_gdrive_v2 <- function(drive_folder, file_name, local_path, overwrite = TRUE) {
  # Validate inputs
  if (missing(drive_folder) || missing(file_name) || missing(local_path)) {
    stop("'drive_folder', 'file_name', and 'local_path' must be provided.")
  }
  
  # List folder contents once
  folder_contents <- googledrive::drive_ls(path = drive_folder)
  file_id <- folder_contents |>
    dplyr::filter(.data$name == file_name) |>
    dplyr::pull(.data$id)
  
  if (length(file_id) == 0) {
    stop("File not found in specified Drive folder.")
  } else if (length(file_id) > 1) {
    warning("Multiple matches for file name; using the first one.")
    file_id <- file_id[1]
  }
  
  message("Downloading: ", file_name)
  
  # Ensure local directory exists
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  
  # Download directly by ID
  googledrive::drive_download(
    file = googledrive::as_id(file_id),
    path = local_path,
    overwrite = isTRUE(overwrite)
  )
  
  invisible(local_path)
}

#' #' Read CSV from Google Drive Path - DEPRECATED, USE V2
#' #'
#' #' This function reads a CSV file directly from a specified Google Drive path using the `googledrive` package. It first retrieves the file using the provided path and then reads the content into a data frame.
#' #'
#' #' @param path A character string specifying the Google Drive path to the CSV file. The path can be a file ID, URL, or a full path to the file.
#' #' @return A data frame containing the contents of the CSV file.
#' #' @details The function uses the `googledrive` package to access Google Drive files. Ensure that you have authenticated with Google Drive using `googledrive::drive_auth()` before using this function.
#' #' @examples
#' #' \dontrun{
#' #' # Example usage:
#' #' csv_data <- access_data_read_csv_from_gdrive("your-file-id-or-url")
#' #' head(csv_data)
#' #' }
#' #' @importFrom googledrive drive_get drive_read_string
#' #' @export
#' read_csv_from_gdrive <- function(path) {
#'   # Retrieve the file metadata from Google Drive
#'   f <- googledrive::drive_get(path)
#'   
#'   # Read the content of the file as a string and convert it to a data frame
#'   csv <- f |>
#'     googledrive::drive_read_string() %>%
#'     read.csv(text = .)
#'   
#'   return(csv)
#' }

#' Write Shapefile to a New Directory and Create a Zipped Version
#'
#' This function writes an `sf` object to a shapefile in a new, file-specific directory and optionally creates a zipped version of the shapefile.
#' It also allows for the removal of the original unzipped files and handles overwriting existing files.
#'
#' @param shp An `sf` object to write as a shapefile.
#' @param location A character string specifying the path of the directory to create the new file-specific subdirectory in.
#' @param filename A character string specifying the name of the file without the `.shp` extension.
#' @param zip_only A logical value indicating whether the original (unzipped) files should be removed after zipping. Defaults to `FALSE`.
#' @param overwrite A logical value indicating whether existing files should be overwritten. Defaults to `FALSE`.
#' @return No return value. The function writes a shapefile to a specified directory, optionally zips the files, and manages file cleanup based on user input.
#' @examples
#' \dontrun{
#' # Example usage
#' st_write_shp(shp = prepped_for_parks_etal,
#'              location = here::here("data/derived"),
#'              filename = "career_lba_for_parks_v1",
#'              zip_only = TRUE,
#'              overwrite = TRUE)
#' }
#' @importFrom sf st_write
#' @importFrom zip zip
#' @export
st_write_shp <- function(shp, location, filename, zip_only = FALSE, overwrite = FALSE) {
  
  # Define paths
  out_dir <- file.path(location, filename)
  zip_file <- file.path(out_dir, paste0(filename, ".zip"))
  zip_file_dest <- file.path(location, paste0(filename, ".zip"))
  
  # Manage overwriting and directory creation
  if (dir.exists(out_dir)) {
    if (overwrite) {
      unlink(out_dir, recursive = TRUE)
    } else {
      stop("Directory '", out_dir, "' already exists and overwrite is set to FALSE.")
    }
  }
  
  if (file.exists(zip_file_dest) && zip_only) {
    if (overwrite) {
      unlink(zip_file_dest)
    } else {
      stop("Zip file '", zip_file_dest, "' already exists and overwrite is set to FALSE.")
    }
  }
  
  # Create the directory if not there
  dir_ensure(out_dir)
  
  # Write the shapefile
  shapefile_path <- file.path(out_dir, paste0(filename, ".shp"))
  sf::st_write(shp, shapefile_path, append = FALSE)
  
  # Get all shapefile components
  all_shp_files <- list.files(out_dir, pattern = paste0(filename, ".*"), full.names = TRUE)
  
  # Create zip file
  zip::zip(zipfile = zip_file, files = all_shp_files, mode = "cherry-pick")
  
  # Remove raw files if zip_only is TRUE
  if (zip_only) {
    file.copy(zip_file, zip_file_dest)
    unlink(out_dir, recursive = TRUE)
  }
}


#' Ensure Directories Exist
#'
#' This function checks if one or more directories exist at the specified paths,
#' and creates any that do not exist.
#'
#' @param path A character string or a vector of strings specifying directory paths.
#' @return A character vector of all directory paths that were checked/created.
#' @examples
#' # Ensure a single directory
#' dir_ensure("data")
#'
#' # Ensure multiple directories
#' dir_ensure(c("data", "output", "logs"))
#'
#' @export
dir_ensure <- function(path) {
  if (!is.character(path)) {
    stop("`path` must be a character string or a vector of character strings.")
  }
  
  created_paths <- character()
  
  for (p in path) {
    if (!dir.exists(p)) {
      tryCatch({
        dir.create(p, recursive = TRUE)
        message("Directory created: ", p)
        created_paths <- c(created_paths, p)
      }, error = function(e) {
        warning("Failed to create directory: ", p, " — ", conditionMessage(e))
      })
    } else {
      message("Directory already exists: ", p)
    }
  }
  
  return(invisible(path))
}


#' Safely Extract a ZIP or TAR Archive
#'
#' Handles both .zip and .tar(.gz) files. Supports skipping if files/folders exist,
#' recursive extraction of nested archives, and optional cleanup.
#'
#' @param archive_path Character. Path to a .zip, .tar, or .tar.gz file.
#' @param extract_to Character. Directory for extraction. Defaults to archive's directory.
#' @param recursive Logical. Recursively extract nested archives? Defaults to FALSE.
#' @param keep_archive Logical. Keep original and nested archives after extraction? Defaults to TRUE.
#' @param full_contents_check Logical. If TRUE, skip extraction only if all files exist.
#' @param return_all_paths Logical. If TRUE, return all extracted file paths;
#'                          if FALSE, return all top-level files and directories.
#'
#' @return Character vector of extracted paths.
#' @export
safe_extract <- function(archive_path,
                         extract_to = dirname(archive_path),
                         recursive = FALSE,
                         keep_archive = TRUE,
                         full_contents_check = FALSE,
                         return_all_paths = FALSE) {
  # --- Validate inputs ---
  if (!file.exists(archive_path)) stop("Archive does not exist: ", archive_path)
  if (!dir.exists(extract_to)) dir.create(extract_to, recursive = TRUE)
  
  ext <- tolower(tools::file_ext(archive_path))
  is_zip <- ext == "zip"
  is_tar <- ext %in% c("tar", "gz", "tgz", "tar.gz")
  
  if (!is_zip && !is_tar) stop("Unsupported archive type: ", ext)
  
  # --- List archive contents ---
  contents <- if (is_zip) {
    utils::unzip(archive_path, list = TRUE)$Name
  } else {
    utils::untar(archive_path, list = TRUE)
  }
  
  # Determine top-level items
  top_level_items <- unique(sub("^([^/]+).*", "\\1", contents))
  top_level_paths <- file.path(extract_to, top_level_items)
  
  # --- Skip logic ---
  skip_extract <- if (full_contents_check) {
    all(file.exists(file.path(extract_to, contents)))
  } else {
    all(file.exists(top_level_paths))
  }
  
  if (!skip_extract) {
    tryCatch({
      if (is_zip) {
        unzip(archive_path, exdir = extract_to)
      } else {
        utils::untar(archive_path, exdir = extract_to)
      }
    }, error = function(e) stop("Extraction failed: ", e$message))
    
    # --- Recursive extraction ---
    if (recursive) {
      nested_archives <- list.files(extract_to, pattern = "\\.(zip|tar|gz|tgz)$", recursive = TRUE, full.names = TRUE)
      nested_archives <- setdiff(nested_archives, archive_path)
      for (na in nested_archives) {
        safe_extract(na, dirname(na), recursive = recursive, keep_archive = keep_archive,
                     full_contents_check = FALSE, return_all_paths = FALSE)
        if (!keep_archive) unlink(na)
      }
    }
    
    if (!keep_archive) unlink(archive_path)
  } else {
    message("Skipping extract: Targets already exist in ", extract_to)
  }
  
  # --- Return paths ---
  if (return_all_paths) {
    # Get full paths of extracted files
    extracted_paths <- file.path(extract_to, contents)
    extracted_files <- extracted_paths[file.exists(extracted_paths) & !file.info(extracted_paths)$isdir]
    return(invisible(normalizePath(extracted_files, winslash = "/", mustWork = FALSE)))
  } else {
    paths <- file.path(extract_to, top_level_items)
    return(invisible(normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = FALSE)))
  }
}



#' Safely Download a File to a Directory
#'
#' Downloads a file from a URL to a specified directory, only if it doesn't already exist there.
#'
#' @param url Character. The URL to download from.
#' @param dest_dir Character. The directory where the file should be saved.
#' @param mode Character. Mode passed to `download.file()`. Default is "wb" (write binary).
#' @param timeout Integer. Optional timeout in seconds. Will be reset afterward.
#'
#' @return A character string with the full path to the downloaded file.
#'
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \dontrun{
#' path <- safe_download("https://example.com/data.zip", "data/")
#' }
safe_download <- function(url,
                          dest_dir,
                          mode = "wb",
                          timeout = NA) {
  # Validate input
  if (!is.character(url) || length(url) != 1) stop("`url` must be a single character string.")
  if (!is.character(dest_dir) || length(dest_dir) != 1) stop("`dest_dir` must be a single character string.")
  
  # Ensure destination directory exists
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  
  # Derive destination file path from URL and directory
  filename <- basename(url)
  destfile <- file.path(dest_dir, filename)
  
  # Skip download if file already exists
  if (file.exists(destfile)) {
    message("Skipping download: File already exists at ", destfile)
    return(normalizePath(destfile, winslash = "/", mustWork = FALSE))
  }
  
  # Handle optional timeout
  original_timeout <- getOption("timeout")
  if (!is.na(timeout) && timeout > original_timeout) {
    options(timeout = timeout)
    on.exit(options(timeout = original_timeout), add = TRUE)
  }
  
  # Attempt to download
  tryCatch({
    download.file(url, destfile, mode = mode)
    message("Downloaded: ", destfile)
  }, error = function(e) {
    stop("Failed to download file from URL: ", e$message)
  })
  
  return(normalizePath(destfile, winslash = "/", mustWork = FALSE))
}



#' Convert R Color to Hexadecimal
#'
#' This function converts a standard R color name (e.g., 'red', 'steelblue') to its corresponding hexadecimal color code.
#'
#' @param color A character string specifying a standard R color name.
#' @return A character string representing the hexadecimal color code of the specified R color.
#' @examples
#' # Convert the color 'red' to its hexadecimal equivalent
#' col2hex("red")
#'
#' # Convert the color 'steelblue' to its hexadecimal equivalent
#' col2hex("steelblue")
#'
#' @export
col2hex <- function(color) {
  rgb_values <- col2rgb(color)
  hex_color <- rgb(rgb_values[1], rgb_values[2], rgb_values[3], maxColorValue=255)
  return(hex_color)
}


#' Download a file from Google Drive to a local directory
#'
#' This function downloads a file from a Google Drive path to a specified local path.
#'
#' @param gDrivePath A character string. The path or name of the file on Google Drive.
#' @param localPath A character string. The local path where the file will be saved.
#' @param overwrite A logical value indicating whether to overwrite the file if it already exists at the local path. Defaults to `TRUE`.
#'
#' @details This function retrieves a file's ID from Google Drive using the provided `gDrivePath` and downloads it to the local directory specified by `localPath`. The file will be overwritten if `overwrite` is set to `TRUE` (default).
#' 
#' @return The downloaded file will be saved to the specified `localPath`.
#' 
#' @note You must be authenticated with Google Drive via the `googledrive` package for this function to work.
#' 
#' @importFrom googledrive drive_get drive_download as_id
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' download_data_from_gdrive("path/to/file/on/drive", "path/to/local/file.csv")
#' }
#' 
#' @export
download_data_from_gdrive <- function(gDrivePath, localPath) {
  # Validate inputs
  if (missing(gDrivePath) || missing(localPath)) {
    stop("Both 'gDrivePath' and 'localPath' must be provided.")
  }
  if (!is.character(gDrivePath) || !nzchar(gDrivePath)) {
    stop("'gDrivePath' must be a non-empty string.")
  }
  if (!is.character(localPath) || !nzchar(localPath)) {
    stop("'localPath' must be a non-empty string.")
  }
  
  # Retrieve file ID from GDrive
  f <- googledrive::drive_get(gDrivePath)
  id <- f$id
  nm <- f$name
  
  googledrive::drive_download(googledrive::as_id(id), path = localPath, overwrite = TRUE)
}


#' Save Kable Output as PNG with Workaround
#'
#' This function provides a workaround for an issue (as of 3/20/24) with `kableExtra::save_kable`, which fails to export tables as `.png` files. It first saves the table as an HTML file and then converts it to a PNG using `webshot2`.
#'
#' @param k An output object from the `kable` function.
#' @param file_path A character string specifying the full desired file path (e.g., 'myDir/figs/myTable.png') for the output PNG file.
#' @return No return value. The function saves the PNG file to the specified location.
#' @examples
#' # Save a kable output as a PNG file
#' \dontrun{
#' k <- knitr::kable(head(mtcars))
#' save_kable_workaround(k, "myDir/figs/myTable.png")
#' }
#'
#' @importFrom webshot2 webshot
#' @importFrom kableExtra save_kable
#' @export
save_kable_workaround <- function(k, file_path) {
  html_path <- paste0(tools::file_path_sans_ext(file_path), ".html")
  kableExtra::save_kable(x = k, file = html_path)
  webshot2::webshot(html_path, file = file_path)
  file.remove(html_path)
}


#' Fit MBLM (Median-Based Linear Model) Estimator and Visualize
#'
#' This function fits a Theil-Sen or Siegel estimator using median-based linear 
#' modeling (MBLM) and visualizes the results with a scatter plot and fitted 
#' regression line. The type of estimator (Theil-Sen or Siegel) can be controlled 
#' via the `repeated` argument.
#'
#' @param dats A data frame containing the data.
#' @param x The predictor variable (unquoted column name) from the data frame.
#' @param y The response variable (unquoted column name) from the data frame.
#' @param repeated Logical, if `TRUE`, uses the Siegel estimator which allows 
#' for repeated medians; if `FALSE`, uses the Theil-Sen estimator (default: `FALSE`).
#'
#' @return A `ggplot` object showing the scatter plot of `x` vs `y` with the 
#' fitted regression line overlaid.
#'
#' @details
#' The function uses the `mblm` package to fit the linear model using either 
#' the Theil-Sen or Siegel estimator based on the value of the `repeated` argument.
#' It visualizes the fit using `ggplot2` by plotting the data points and 
#' adding a dashed regression line based on the model's coefficients.
#'
#' Non-standard evaluation (NSE) is used to allow unquoted column names for `x` 
#' and `y`. The variables are converted to strings using `deparse(substitute())`, 
#' which allows them to be used in the formula for model fitting and in the plot labels.
#'
#' @examples
#' # Example with Theil-Sen estimator
#' mblm_fit_estimator_and_visualize(mtcars, mpg, disp, repeated = FALSE)
#' 
#' # Example with Siegel estimator
#' mblm_fit_estimator_and_visualize(mtcars, mpg, disp, repeated = TRUE)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs
#' @importFrom mblm mblm
#' @export
mblm_fit_estimator_and_visualize <- function(dats, x, y, repeated = FALSE) {
  
  # Determine which estimator to use
  estimator <- ifelse(repeated, "siegel estimator", "thiel-sen estimator")
  
  # Convert x and y to string names for formula creation
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Create the formula dynamically
  formula <- as.formula(paste(y_name, "~", x_name))
  
  # Fit the MBLM model
  fit <- mblm::mblm(formula, data = dats, repeated = repeated)
  
  # Create the plot with ggplot2
  p <- ggplot2::ggplot(dats, ggplot2::aes(x = {{x}}, y = {{y}})) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = fit$coefficients["(Intercept)"],
                         slope = fit$coefficients[x_name],  # Access slope using variable name
                         linetype = "dashed",
                         linewidth = 0.8) +
    ggplot2::labs(
      title = paste("MBLM Fit for", y_name, "vs", x_name),
      x = x_name,
      y = y_name,
      caption = paste0("Coefficient: ", fit$coefficients[x_name], "\nUsing ", estimator)
    )
  
  return(p)
}


#' Get MBLM Coefficients by Group
#'
#' This function estimates the slope coefficients of a linear relationship between two variables (`x` and `y`)
#' for each group in a dataset, using either the Siegel or Theil-Sen estimator (from the `mblm` package).
#'
#' @param dats A data frame containing the variables.
#' @param x The independent variable.
#' @param y The dependent variable.
#' @param group The grouping variable. The function will estimate coefficients for each unique value of this group.
#' @param repeated Logical, if `TRUE`, the Siegel estimator is used, otherwise the Theil-Sen estimator is applied. Defaults to `FALSE`.
#'
#' @return A tibble with three columns: `group` (unique group values), `coefficient` (estimated slope coefficients), 
#' and `estimator` (the name of the estimator used).
#'
#' @importFrom dplyr pull filter tibble group_split
#' @importFrom purrr map
#' @importFrom mblm mblm
#'
#' @examples
#' # Example usage
#' df <- data.frame(group = rep(c("A", "B"), each = 10), x = rnorm(20), y = rnorm(20))
#' mblm_get_coefficients_by_group(df, x, y, group, repeated = FALSE)
#'
#' @export
mblm_get_coefficients_by_group <- function(dats, x, y, group, repeated = FALSE) {
  
  # Determine which estimator to use
  estimator <- ifelse(repeated, "siegel estimator", "thiel-sen estimator")
  
  # Convert x and y to string names for formula creation
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  # Get unique values for the group variable
  unique_groups <- dats |> dplyr::pull({{group}}) |> unique()
  
  # Create a list of data frames, one for each group
  subset_list <- dats |> dplyr::group_split({{group}})
  
  # Estimate coefficients for each group
  estimators <- subset_list |> purrr::map(function(subset_data) {
    formula <- as.formula(paste(y_name, "~", x_name))
    fit <- mblm::mblm(formula, data = subset_data, repeated = repeated)
    coef <- fit$coefficients[x_name]
    return(coef)
  })
  
  # Combine unique groups and estimators into a data frame
  results <- dplyr::tibble(
    group = unique_groups,
    coefficient = unlist(estimators),
    estimator = estimator
  )
  
  return(results)
}


## ArcGIS REST Data access

#' Robustly fetch data from ArcGIS REST API with pagination and partial result handling
#'
#' This version preserves partial data if an error occurs mid-fetch.
#'
#' @export
access_data_get_x_from_arcgis_rest_api_geojson <- function(base_url, query_params, max_record, n, timeout) {
  if (!is.character(base_url) || length(base_url) != 1) stop("Parameter 'base_url' must be a single character string.")
  if (!is.list(query_params)) stop("Parameter 'query_params' must be a list.")
  if (!is.numeric(max_record) || max_record <= 0) stop("Parameter 'max_record' must be a positive integer.")
  if (!is.numeric(timeout) || timeout <= 0) stop("Parameter 'timeout' must be a positive integer.")
  
  total_features <- list()
  offset <- 0
  total_fetched <- 0
  fetch_all <- identical(n, "all")
  
  if (!fetch_all && (!is.numeric(n) || n <= 0)) {
    stop("Parameter 'n' must be a positive integer or 'all'.")
  }
  
  repeat {
    query_params$resultOffset <- offset
    query_params$resultRecordCount <- max_record
    
    message(sprintf("Requesting records %d to %d...", offset + 1, offset + max_record))
    
    response <- tryCatch(
      httr::GET(url = base_url, query = query_params, httr::timeout(timeout)),
      error = function(e) {
        warning(sprintf("Request failed at offset %d: %s", offset, e$message))
        return(NULL)
      }
    )
    
    if (is.null(response)) break
    
    # Check content type
    resp_type <- httr::headers(response)[["content-type"]]
    if (!grepl("geo\\+json|application/json", resp_type)) {
      warning(sprintf("Non-GeoJSON response at offset %d. Skipping this batch.", offset))
      break
    }
    
    # Try to read the GeoJSON content
    data <- tryCatch({
      sf::st_read(httr::content(response, as = "text", encoding = "UTF-8"), quiet = TRUE)
    }, error = function(e) {
      warning(sprintf("Failed to parse GeoJSON at offset %d: %s", offset, e$message))
      return(NULL)
    })
    
    if (is.null(data) || nrow(data) == 0) {
      message("No more data returned.")
      break
    }
    
    total_features[[length(total_features) + 1]] <- data
    total_fetched <- total_fetched + nrow(data)
    message(sprintf("Fetched %d records so far...", total_fetched))
    
    if ((nrow(data) < max_record) || (!fetch_all && total_fetched >= n)) break
    
    offset <- offset + max_record
  }
  
  if (length(total_features) == 0) {
    warning("No data was successfully fetched.")
    return(NULL)
  }
  
  result <- do.call(rbind, total_features)
  
  if (!fetch_all) {
    result <- result[1:min(n, nrow(result)), ]
  }
  
  return(result)
}

