 Scripts, inputs, and outputs
Script
Input(s)
Output(s)
Purpose
data_cleaning.R
raw/working data (repo data folder)
cleaned_data.csv (and/or cleaned files in claened_data/)
Produces a consistent cleaned dataset used by all later steps.
data_visualization.R
cleaned_data.csv
figures saved into figuers_from_data/
Generates the plots used in the report’s visualisation section.
data_correlation_test.R
cleaned_data.csv
outputs saved into correlation_test_results/
Runs the correlation test(s) and saves the statistical results for reporting.

