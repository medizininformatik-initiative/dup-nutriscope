### Objective

The aim of this retrospective study is to analyze the prevalence and severity 
of disease-related malnutrition in hospitalized patients in university hospitals, as this is associated with considerable clinical and economic consequences. 
Medical documentation data recorded in various clinics will be used for this purpose.<br/> 
The goal is to clarify <br/>
a) whether the use of nutritional counseling significantly improves the
treatment success compared to treatment without nutritional counseling and <br/>
b) which subtypes of malnutrition exist
and assign appropriate treatment strategies and outcomes.


### Extraction   (- construction site -)

#### used files: nutriscope_extraction.R + config.json

The R-script reads the resources encounter, patient, condition, observation, procedure of the fhir server.

#### Please store the FHIR-server configuration information in the config.json file

It initially filters completed inpatient cases from 2013-2023, then searches for the patient data for these and calculates the age in order to limit the cohort to adults (18+).
The observation currently only looks for one code each for height, weight, albumin and phosphate. Height and weight are used to calculate the patient's bmi and then removed from the dataset.

#### To do: redesign the dataset according to the template from diz leipzig


### Data quality control

- Content: This script searches the required CSV files in folder_path for selected column names and performs a quality check of the data concerning distributions, frequencies, descriptive statistics, graphics, etc. Necessary functions have been defined and applied with the process_csv_file() function, which processes each CSV file, calculates the quality metrics, generates graphics and saves the results as csv or png.

- Required packages: commands to install and load included in the script
  
- Required input csv-files: "*.csv"

- Output: tables for quality check report; frequency tables for Hauptdiagnose, Nebendiagnosen, OPS-Kode, Geschlecht, OE; Histograms for Verweildauer, Zeit bis zum nächsten Aufenthalt, Alter, BMI; Boxplot for BMI

- PLEASE ADD YOUR FOLDER PATH AT THE END OF THE SCRIPT 
  
### Data analysis

[ADD FILENAMES, CONTENT, OUTPUT]
