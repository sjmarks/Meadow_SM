# Meadow_SM

##### Processing functions for soil moisture data collected at Rock Creek, Marian, Control, and Childs meadows near Chester, CA. Supports the montane meadow hydrologic data aquisition and monitoring research effort currently conducted by Cal Poly SLO. 

The **functions** contained in this repo:

**1.** Read raw files containing soil moisture (SM) data retrieved from Onset Hobo (`.csv` files) and Campbell Scientific (`.dat` files) data loggers.

**2.** Compile SM data from a meadow site's respective network of data loggers in chronological order. Data is compiled in a "tidy" format, with each column representing a volumetric water content value for a soil moisture sensor at the depth below the surface it is currently installed. Each row corresponds to the time and date the data was collected.

**3.** Update a previous compilation for a given meadow site with newly collected raw data files from the field.

All functions are contained in the file `SM_Processing.Rmd`. This document describes each function's purpose and usage in greater detail. The SM data processing procedure is performed in the `.Rmd` files with file names indicating the month and year SM data were collected in the field from research meadows. Raw data files are contained in sub-folders to this repo with nomenclature indicative of where and when files were collected in the field.

**NOTE: Most recent compilation was done for data files collected in November 2020- `SM_Processing_Nov2020.Rmd` . . . Previous compilations in this repo (July and Aug 2020) do not feature the most up to date functions in their respective `.Rmd` files. Please reference `SM_Processing.Rmd` or `SM_Processing_Nov2020.Rmd` for the next compilation process.**
