[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![license](https://img.shields.io/badge/license-BY--NC--ND--4.0-blue)](https://creativecommons.org/licenses/by-nc-nd/4.0/) [![DOI](https://img.shields.io/badge/DOI-doi.org%2F10.1101%2F2024.12.06.627082-blue)](https://doi.org/10.1101/2024.12.06.627082)
![graphical abstract showing data layers integrated and software used in workflow](/images/gf_overview.jpg)


# Good fire in western U.S. forests: estimating the beneficial ecosystem work of wildfires

This repository contains code, data, and graphics to accompany {ADD CITATION & DOI HERE ONCE PUBLISHED}. This repository is intended to support research reproduction according to principles of Open Science and FAIR data principles.

## Requirements

Analyses require use of Google Earth Engine (GEE) and R.

This codebase has been tested on the following operating systems:

Linux:
Mac OSX:
Windows: Windows 11 Pro

R version 4.3.2 and Google Earth Engine as of 2024 were used for code development and testing.

Users will need to install and set up several packages from CRAN.

## Code access

To clone this repo to your workspace: 
``` git clone https://github.com/TylerLMcIntosh/a-number-on-good-fire ```

Cloning time should be negligible.

## Code demo
A small demo dataset is available in the "demo_data" directory. This directory contains three demo datasets, one each for MTBS, Welty, and NFPORS data in the state of Colorado. To test analyses on this smaller spatial area, copy the datasets into a data/raw directory and change the associated file paths in the scripts.

Run time for the full workflow should be within a few hours, dependant on GEE resources.

## Contents

Scripts in the /code directory are ordered from 00 on up, and are meant to be run in order. Scripts with 'a' before the number manage wildfire event data manipulation and summarization prior to data merge, while scripts with 'b' before the number manage prescribed burn event data manipulation and summarization. Scripts with neither 'a' or 'b' are the core analysis scripts used after all data are prepared for analysis.

Javascript scripts (.js) are meant to be run on Google Earth Engine (GEE); each contains a link to the online version of the working script.

R scripts (.R) are used for data management.

Quarto documents (.qmd) are used for creation of final publication figures.

A list and description of all scripts is included below.

## Script descriptions

### Wildfire event manipulation and severity workflow
- a00_create_goodfire_event_dataset.R : This script merges data from the MTBS & Welty&Jeffries fire event datasets into a single main dataset
- a01_generate_gf_cbi.js: This script uses GEE to calculate region-wide CBI layers for the goodfire event dataset created in a00
- a02_full_streamlined_good_fire.js : This GEE script generates good fire layers from the CBI layers derived in the last script and summarizes the data for any set of summary polygons. It also outputs per-event statistics. All outputs go to user's Google Drive in the specified folder.
- a03_merge_gf_gdrive_data.R : This script pulls outputs from GEE (GDrive) to the user's local machine and merges the annualized data for further use. It also accesses the remote per-event statistics. All data are written out locally for analysis using core scripts.

### Prescribed burn event manipulation workflow
- 00b_prep_rx_for_gee.R : This script prepares NFPORS RX data for use in GEE
- 01b_add_gee_data_to_rx.js : This GEE script adds Fire Regime Group and landcover data to the prescribed burn event data
- 02b_merge_summarize_rx_data.R : This script pulls in outputs from GEE and merges the new data with the raw spatial NFPORS data. It also cleans the data and then writes out summaries.

### Core analysis workflow
- 00_create_plots.qmd : This quarto document creates and exports all figures for the publication


