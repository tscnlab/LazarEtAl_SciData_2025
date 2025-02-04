<img src="https://github.com/tscnlab/Templates/blob/main/logo/logo_with_text-01.png" width="400"/>

# Code Repository
## Joint infrared pupil images and near-corneal-plane spectral light exposure data in natural conditions across the adult lifespan.

This code repository is part of the data descriptor publication: 
**“Joint infrared pupil images and near-corneal-plane spectral light exposure data in natural conditions across the adult lifespan.”**

It is publicly accessible under the MIT license (see `LICENSE.md` file). The dataset accompanying this publication is available on FigShare under the CC-BY 4.0 license: **Joint infrared pupil images and near-corneal-plane spectral light exposure data in natural conditions across the adult lifespan - Dataset.** [https://doi.org/10.6084/m9.figshare.28176839](https://doi.org/10.6084/m9.figshare.28176839) 
Processed data and supporting material based on the same raw data is publicly accessible in the following locations:

- **Registered report:** [https://doi.org/10.1098/rsos.191613](https://doi.org/10.1098/rsos.191613)
- **Processed dataset used in registered report:** [https://doi.org/10.6084/m9.figshare.24230848.v3](https://doi.org/10.6084/m9.figshare.24230848.v3) 
- **R code repository for registered report:** [https://doi.org/10.5281/zenodo.14678627](https://doi.org/10.5281/zenodo.14678627) 
- **Additional supporting materials:** [https://doi.org/10.6084/m9.figshare.24230890.v1](https://doi.org/10.6084/m9.figshare.24230890.v1) 

If you have any comments or queries, please reach out to **manuel.spitschan@tum.de**.

---

## Code Structure

The described code in this repository is subdivided into Matlab, Python, and R code.

### 1. Matlab Code

The Matlab code contains the full dark and thermal noise calibration procedure, including spectral irradiance samples recorded in darkness. These samples are stored in the folder `Calibration_files`.

#### Key Files:
- **`C01_Calibration_import.m`**: Imports and prepares the baseline training dataset (`Calibration_files/baseline_data`). This dataset consists of spectral measurements in darkness collected with a plastic cap covering the spectroradiometer’s light sensor.
- **`C02_Calibration_fitplot.m`**: Uses Matlab’s Curve Fitting Toolbox to fit polynomial models to baseline data. Five models are compared across 874 pixels in the 380-780 nm range, with the 3x1 model selected.
- **`C03_Calibration_Select.m`**: Applies the selected model to a validation dataset (`Calibration_files/pixelselection_dataset`). Dysfunctional pixels are excluded based on RMSE values.
- **`C04_Calibration_Export.m`**: Exports the calibration results into a text file, including a short README description.

### 2. Python Code

The Python script `rename.py` (located in the `Python` folder) was used to rename recorded files based on estimated start times due to an issue with the Raspberry Pi’s internal clock. 

- The first sample of each trial is renamed to a timestamp **10 minutes after** the start of the calibration video.
- Subsequent files are renamed at **10-second intervals**.
- The corrected filenames appear in both calibrated and uncalibrated spectral irradiance samples and pupil images.

### 3. R Code

#### **Subfolder `01_processed_data`**
Contains code and data from the pre-processed dataset ([https://doi.org/10.6084/m9.figshare.24230848.v3](https://doi.org/10.6084/m9.figshare.24230848.v3)) and R code repository ([https://doi.org/10.5281/zenodo.14678627](https://doi.org/10.5281/zenodo.14678627)).

- **`create_subdatasets.R`**: Uses `merged_data_conf.rda` to create sub-datasets for table and figure generation.

#### **Subfolder `02_figs_tabs_code`**
Contains scripts for generating figures and tables for the data descriptor.

- **`demographics.R`**: Generates participant characteristics table and exports metadata (`demographic_data.csv`).
- **`figures&tables.R`**: Generates R-based figures and three metadata files:
  - `sat_files.csv`: List of saturated/erroneous spectrometer samples (410 samples).
  - `sample_phase_tab.pdf`: Summary table of experiment phase conditions (`dark`, `laboratory`, `field`, `mixed`).
  - `sum_tab.pdf`: Summary of participant demographics and number of observations per experiment phase.
- **`ggplot_functions.R`**: Functions for plotting used in `figures&tables.R`.

#### **Subfolder `03_output`**
Contains output from the above scripts, structured into subfolders for figures and tables.

---
