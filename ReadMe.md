## Objective

The aim of this retrospective study is to analyze the prevalence and severity 
of disease-related malnutrition in hospitalized patients in university hospitals, as this is associated with considerable clinical and economic consequences. 
Medical documentation data recorded in various clinics will be used for this purpose.<br/> 
The goal is to clarify <br/>
a) whether the use of nutritional counseling significantly improves the
treatment success compared to treatment without nutritional counseling and <br/>
b) which subtypes of malnutrition exist
and assign appropriate treatment strategies and outcomes.

## jena_improvements branch
This branch contains scripts that have been adapted for efficiency and to work in the Jena setting. 

## Data Extraction 

**Used files: nutriscope_extraction.R + config.json**

The R-script downloads the resources encounter, patient, condition, observation, procedure from the fhir server and stores them in several csv-files in the folder *NutriScope_extracted_data*.

The script starts by downloading all finished inpatient Encounters of type "einrichtungskontakt" from 2018-2023. It then downloads for the referenced patient resources for these and calculates the age in order to limit the cohort to adults (18+). All Encounters belonging to underage Patients are then discarded. Using the remaining Encounter IDs, the script then extracts associated Conditions (diagnoses), Observations (restricted to body weight, height, albumin and phosphate by loinc code) and Procedures. 

The script saves a number of intermediate results as *Zwischenstandxx.RData* in the working directory to allow the user to restart the script at different stages if an error occurs at some point. These files can be deleted after the extraction has run sucessfully.

Currently, the fhir server in Jena has not the complete set of height and weight measurements as requested. Therefore, the script was extended to load some additional tabular data from disk, extracted directly from the primary information system.

The resources are preprocessed and tabularised, resulting in the following (intermediate) results. Please note that in contrast to the original scripts in the main branch, this branch does not create a single overall table with the information from all resource types as this would result in an extremly large and sparse table. Instead, variables are saved in several tables according to resource type.

### NutriScope_pat_enc.csv

Contains information from Patient and Encounter resources. Note: As the variable "Fachabteilungsschlüssel" is not well defined (e.g. one encounter may have several of those, which one is meant?) and is very difficult to extract, it has been ignored for now.

Variable name           | Description
------------------------|--------
Patientennummer         | FHIR ID of the Patient resource
Geschlecht              | Gender of the patient
Alter bei Aufnahme      | Calculated from birthdate and admission date
EID                     | FHIR ID of the Encounter resource  
Fallnummer              | Hospital identifier of the Encounter, this has been specified as the SAP ID in Jena, corresponds to identifier.system "http://www.uniklinikum-jena.de/fhir/sid/encounter"
Aufnahmedatum           | Admission date of the Encounter
Entlassdatum            | Discharge date of the encounter
Verweildauer            | Difference in days between admission and discharge
ZeitNaechsterAufenthalt | Difference in days between discharge of this encounter and admission of following encounter


### Nutriscope_con.csv

Contains information from Condition resources. Cave: At the moment all Conditions regardless of their source system are downloaded which means there are Conditions from the Hospital Information System (SAP) and from the so caled P21-data set (billing data). The categories "Haupt- und Nebendiagnose" are only defined for blling data Conditions.

Variable name           | Description
------------------------|--------
CID                     | FHIR ID of the Condition resource
C.category.coding.code  | The category of the Condition (e.g. CC/CM for Haupt-/Nebendiagnose) or other categories
C.code.coding.code      | The code of the resource, usually an ICD code
C.code.coding.system    | The code system belonging to the code, e.g. "http://fhir.de/CodeSystem/bfarm/icd-10-gm"
ICD-Version             | The `code.coding.version` FHIR element, usually the version of the ICD
Patientennummer         | FHIR ID of the referenced Patient resource
EID                     | FHIR ID of the referenced Encounter resource 
C.record.date           | Date when the diagnosis was recorded
Hauptdiagnose           | The value of `C.code.coding.code` when `C.category.coding.code=="CC"`, else `NA`
Nebendiagnose           | The value of `C.code.coding.code` when `C.category.coding.code=="CM"`, else `NA`

### Nutriscope_obs.csv
Contains information from the Obervation resources. The values in this table represent either an Albumin or Phosphate measurement, or the BMI which has been calculated from body height and weight observations. Note that no filtering for plausible values has been applied before generating this table.


Variable name           | Description
------------------------|------------------------------------
OID                     | FHIR ID of the Observation resource
O.code.coding.system    | The code system belonging to the code, has been restricted to "http://loinc.org"
O.code.coding.code      | The loinc code of the resource if its a lab value (either 14879-1 or 1751-7) of "BMI" if it's the BMI
Patientennummer         | FHIR ID of the referenced Patient resource
EID                     | FHIR ID of the referenced Encounter resource 
O.dateTime              | The `effectiveDateTime` element of the FHIR resource#
O.valueQuantity.value   | The value (e.g. weight, height or lab value) measured
O.valueQuantity.unit    | The measurement unit
O.referenceRange.low    | If available the lower limit of the reference range for the given quantity
O.referenceRange.high   | If available the upper limit of the reference range for the given quantity

### Nutriscope_proc.csv
Contains information from the Procedure resources.

Variable name           | Description
------------------------|------------------------------------
ProID                   | FHIR ID of the Procedure resource
Pro.code.coding.system  | The code system belonging to the code in column `OPS-Kode`, is not necessarily "http://fhir.de/CodeSystem/bfarm/ops"!
Pro.code.coding.version | The version of the Code System
OPS-Kode                | The code in `Procedure.code.coding.code`, doesn't have to be an OPS code!
Patientennummer         | FHIR ID of the referenced Patient resource
EID                     | FHIR ID of the referenced Encounter resource 
Prozeduren-Datum        | Date of the Procedure (`performedDateTime`)


### Data quality control
**Used files: data_quality_control_UKJ.R + csv-files in Nutriscope/extracted_data/**

This file has been adapted because it did not actually fit the data format that was coming out of *nutriscope_extraction.R* (neither the original version nor the one improved for Jena). The results created do however match the files that were supposed to be generated in the original script.

The output is stored in the folder qualitycheck_results, the output files are described by type in the following:

### Overall quality check results
Reads all four csv-files from the data extraction and creates an overall summary of the variables in those files.

Output-Files: Nutriscope_pat_enc_QC_results.csv, Nutriscope_con_QC_results.csv, Nutriscope_obs_QC_results.csv, Nutriscope_proc_QC_results.csv.

Structure of each file:

Column Name             | Description
------------------------|------------
column_name             | The name of the column (variable) in the analysed csv-file
data_type               | The R data type of the column
missing_values          | The number of missing values in the column
unique_values           | The number of unique values in the column
range_of_values         | A string giving the range of the values, if defined (i.e. variable is numeric or date type)
Column_with_duplicates  | Boolean value indicating whether there are any duplicate values in the column
mean                    | Mean of the variable if defined, else `NA`
sd                      | Standard deviation of the variable if defined, else `NA`
variance                | Variance of the variable if defined, else `NA`
skewness                | Skewness of the variable if defined, else `NA`
mode                    | Mode of the variable, may be redacted by DIC employee if this an ID
normality_p_value       | P-value of a test for normality, if applicable, else `NA`

### Frequency tables
For some of the variables frequency tables are generated

Output-Files:

File                                  | Description
--------------------------------------|---------------------------------------------
NutriScope_con_HauptDiagFREQ.csv      | Frequencies of all Hauptdiagnose ICD codes
NutriScope_con_NebenDiagFREQ.csv      | Frequencies of all Nebendiagnose ICD codes
NutriScope_pat_enc_agegroupsFREQ.csv  | Frequencies of age groups with bin size 10
NutriScope_pat_enc_genderFREQ.csv     | Frequencies of age groups with bin size 10
NutriScope_proc_ops_chaptersFREQ.csv  | Frequencies of OPS Chapters from Procedure data
NutriScope_proc_ops_ETFREQ.csv        | Frequencies of specific prefiltered OPS codes
NutriScope_proc_opsFREQ.csv           | Frequencies of all OPS codes

### Counts of implausible values
For some variable implausible values are counted

Output-Files:

File                                          | Description
----------------------------------------------|---------------------------------------------
NutriScope_pat_enc_Readm_kleiner0.csv         | Counts of negative readmission times
NutriScope_pat_enc_Verweildauer_kleiner0.csv  | Counts of negative length of stay times

### Histrograms
Output-Files:

File                                          | Description
----------------------------------------------|---------------------------------------------
NutriScope_obs_Histogramm_Albumin.png         | Histogram of Albumin
NutriScope_obs_Histogramm_BMI.png             | Histogram of BMI
NutriScope_obs_Histogramm_Phosphat.png        | Histogram of Phosphate
NutriScope_obs_Histogramm_admission.png       | Histogram of the length of stay
NutriScope_obs_Histogramm_Alter.png           | Histogram of age
NutriScope_obs_Histogramm_readmission.png     | Histogram of time until readmission

### Boxplots
Output-Files:

File                                          | Description
----------------------------------------------|---------------------------------------------
NutriScope_obs_Boxplot_BMI.png                | Boxplot of BMI



