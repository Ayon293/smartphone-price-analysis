# Smartphone Product Price Scraping and Analysis Using R

## Overview
This project demonstrates a complete data science pipeline for analyzing smartphone prices in Bangladesh. It involves web scraping, data cleaning, exploratory data analysis, and clustering using R.

## Objective
- Scrape smartphone product data from Pickaboo e-commerce website.
- Analyze price distribution, brand-wise trends, and feature-based pricing (RAM, Storage).
- Segment smartphones into budget, mid-range, and premium clusters using K-Means clustering.

## Tools & Libraries
- R packages: httr, jsonlite, stringr, dplyr, ggplot2
- Data preprocessing: missing value handling, feature engineering, outlier detection, scaling
- Analysis: EDA, chi-square test, K-Means clustering, visualizations

## How to Run
1. Open `scripts/Finalterm_dataset.R` in RStudio.
2. Run the script to preprocess, analyze, and visualize data.
3. Output CSV saved in `data/`.

## Future Work
- Include additional features like battery, camera, processor
- Explore DBSCAN or hierarchical clustering
- Build predictive models for smartphone pricing
