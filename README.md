# YuvaIntern Week 1: Data Cleaning and Preliminary Analysis with R

**Intern:** Varun Kumar Damagatla
**Internship:** Virtual R Data Analyst Intern — YuvaIntern
**Task:** Week 1 — Data Cleaning and Preliminary Analysis with R

## 1. Project Overview

This project focuses on data cleaning, preprocessing, descriptive statistics, and exploratory data analysis (EDA) using R. The Hotel Booking Demand dataset is used to investigate booking patterns, cancellation behaviour, missing values, and numerical relationships.

## 2. Dataset

* **Dataset:** Hotel Booking Demand
* **Format:** CSV
* **Source:** [Hotel Booking Demand dataset — Kaggle](https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand)
* **Original dataset size:** 119,390 rows and 32 columns
* **Analysis environment:** R 4.6.1 and RStudio

The dataset contains hotel booking information, including hotel type, lead time, length of stay, number of guests, average daily rate (ADR), and cancellation status.

## 3. Tools and Libraries

* R and RStudio
* tidyverse
* janitor
* naniar
* ggplot2
* corrplot
* scales

## 4. Data Cleaning and Preprocessing

The analysis includes:

1. Inspecting the dataset using `dim()`, `names()`, `str()`, and `summary()`.
2. Checking missing values in each column.
3. Cleaning column names and handling missing values.
4. Removing duplicate records from the working dataset.
5. Detecting potential outliers in `lead_time` and `adr` using the Interquartile Range (IQR) method.
6. Applying min-max normalization to selected numerical variables.
7. Preparing categorical variables for encoding.
8. Performing final data-quality checks.

The cleaned dataset contains 87,396 rows and 34 columns, with zero missing cells and zero exact duplicate rows remaining, according to the final R output.

## 5. Exploratory Data Analysis

The analysis covers:

* Booking cancellation status
* Distribution of bookings by hotel type
* Lead-time distribution
* Average Daily Rate (ADR) distribution
* Cancellation rate by hotel type
* Descriptive statistics
* Pearson correlation analysis

## 6. Key Findings

* Total records in the cleaned dataset: 87,396.
* Not-canceled bookings: 63,371 (72.5%).
* Canceled bookings: 24,025 (27.5%).
* City Hotel bookings: 53,428 (61.1%).
* Resort Hotel bookings: 33,968 (38.9%).
* City Hotel cancellation rate: approximately 30.0%.
* Resort Hotel cancellation rate: approximately 23.5%.
* Mean booking lead time: approximately 79.9 days.
* Mean ADR: approximately 106.

These findings describe the analyzed records and do not establish causal relationships.

## 7. Repository Contents

* `README.md` — Project overview and findings.
* `week1_analysis.R` — R analysis script.
* `YuvaIntern_Week1_R_Data_Analysis_Varun.docx` — Internship report.
* `outputs/` — Statistical tables and visualizations.
* `data/` — Dataset location, if included.

## 8. How to Run the Project

1. Install R and RStudio.
2. Download the dataset from the source listed above.
3. Place the CSV file at `data/hotel_bookings.csv`.
4. Open `week1_analysis.R` in RStudio.
5. Run the script from top to bottom.

The script installs required packages if they are not already available. An internet connection may be required for package installation.

## 9. Conclusion

This task provided practical experience in R programming, data cleaning, missing-value handling, outlier detection, normalization, descriptive statistics, and visualization. The results demonstrate how exploratory analysis can help summarize hotel booking patterns and cancellation behaviour.

**Note:** Results depend on the dataset version and the cleaning decisions used in the analysis.
