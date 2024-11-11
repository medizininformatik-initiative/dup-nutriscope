# # # this scripts aims to extract the data for the NutriScope_project
# 
# please fill in the config.json first
#

################################################################################
# install and load necessary packages

package_names<-c("fhircrackr","dplyr","lubridate","jsonlite")      
# check if required packages are installed
for (package_name in package_names) {
  if (!requireNamespace(package_names, quietly = TRUE)) {
    install.packages(package_name)
  }
}
library('fhircrackr')
library('dplyr')
library('lubridate')
library('jsonlite')

#-------------------------------------------------------------------------------
# Create folder in your actual path
output_directory <- "NutriScope_extracted_data"
dir.create(output_directory, showWarnings = FALSE)
setwd(output_directory)

# Read in configuration file of fhir_server
config <- fromJSON("config.json")
FHIR_SERVER <- config$FHIR_SERVER_URL
username <- config$FHIR_USERNAME
password <- config$FHIR_PASSWORD

#-------------------------------------------------------------------------------
# Extract data from different resources

# # # Encounter
request <- fhir_url(url = FHIR_SERVER,
                    resource = "Encounter",
                    parameters = c(
                      "class"="http://terminology.hl7.org/CodeSystem/v3-ActCode|IMP", # inpatient class
                      "status"="finished",                                            # status 
                      #"type/coding/code"="https://www.medizininformatik-initiative.de/fhir/core/modul-fall/CodeSystem/Kontaktebene|einrichtungskontakt" # define type
                      "date"="ge2013-01-01",                                           # start 
                      "date"="le2023-09-30"                                            # end 2023
                    )                  
)
encounter_bundles <- fhir_search(request = request, username=username, password=password, verbose = 0)

enc_table <- fhir_table_description(
  resource = "Encounter",
  cols     = c(
    EID = "id",
    E.fallnummer = "identifier/value",
    E.status = "status",                # actually not needed, only kept for control
    E.class.code = "class/code",
    E.type.code ="type/coding/code",
    E.service.type = "serviceType/coding/code",
    PID ="subject/reference",
    E.period.start = "period/start",
    E.period.end = "period/end",
    #E.partOf.ref = "partOf/reference",  # do we need this?
    CID = "diagnosis/condition/reference"
  ),
  sep="|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

encounters <- fhir_crack(bundles=encounter_bundles, design=enc_table, verbose=0)
write.csv(encounters, file="NutriScope_encounters.csv", row.names=FALSE)

print("Encounter done.")

#-------------------------------------------------------------------------------
# extract and "clean" patient IDs
patient_ids <- unique(encounters$PID)  
patient_ids2 <- gsub("Patient/", "", patient_ids)
patient_id_string <- paste(patient_ids, collapse = ",")
patient_id_string2 <- paste(patient_ids2, collapse = ",")

# # # Patient
request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Patient",
                    parameters = c(paste0("_id=", patient_id_string2))     # extracted IDs only
)
patient_bundles <- fhir_search(request = request, username=username, password=password, verbose = 0)      

patient_table <- fhir_table_description(
  resource = "Patient",
  cols     = c(
    PID         = "id",
    gender      = "gender",
    birthDate    = "birthDate"
  ), 
  sep = "|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE)

patients <- fhir_crack(bundles = patient_bundles, design = patient_table, verbose = 0)

# calculate the age of patients

temp<-encounters %>%                      # look for date of first occurrence of PID
  group_by(PID) %>%
  arrange(E.period.start) %>%
  slice(1L) %>% select (PID, E.period.start)

temp$E_period.start<-gsub("T..*","",temp$E.period.start)
temp$PID<-gsub("\\Patient/","",temp$PID)
patients<-merge(patients, temp, by="PID")
patients$birthDate <- ifelse(nchar(patients$birthDate) == 4, paste0(patients$birthDate, "-07-01"), patients$birthDate)  # add mm-dd to birthDate if missing
patients$birthDate <- parse_date_time(patients$birthDate, c('%m-%Y', '%y', '%Y-%m-%d', '%m%%Y', '%m.%Y'))
patients$age<-trunc((patients$birthDate %--% patients$E.period.start) / years(1))

patients<-patients[, !names(patients) %in% c("birthDate","E.period.start")]     # delete date of birth and first encounter
patients<-patients[which(patients$age >=18),]                                   # only keep patients >= 18 years old
write.csv(patients, file="NutriScope_patients.csv", row.names=FALSE)

rm(temp)
print("Patients done.")

# # # Condition
request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Condition",
                    parameters = c(paste0("subject=", patient_id_string)))
condition_bundles <- fhir_search(request = request , username=username, password=password, verbose = 0) 

condition_table <- fhir_table_description(
  resource = "Condition",
  cols     = c(
    CID = "id",
    C.category.coding.code = "category/coding/code",        
    C.code.coding.code = "code/coding/code",
    C.code.coding.system = "code/coding/system",
    C.code.coding.version = "code/coding/version",
    
    PID ="subject/reference",
    EID = "encounter/reference",
    C.record.date = "recordedDate/extension/valueCode"
  ),
  sep = "|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

conditions <- fhir_crack(bundles=condition_bundles, design=condition_table, verbose=0)
write.csv(conditions, file="NutriScope_conditions.csv", row.names=FALSE)

print("Conditions done.")

# # # Observation
request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Observation",  
                    parameters = c(
                      "subject"= patient_id_string,
                      "code" ="http://loinc.org|8302-2", "http://loinc.org|29463-7", "http://loinc.org|1751-7", "http://loinc.org|14879-1")       # add loincs here
                    )

observation_bundles <- fhir_search(request = request, username=username, password=password, verbose = 0)

obs_table <- fhir_table_description(
  resource = "Observation",
  cols     = c(
    OID = "id",
    O.code.coding.system = "code/coding/system",
    O.code.coding.code = "code/coding/code",
    PID ="subject/reference",
    EID = "encounter/reference",
    O.effectiveDateTime = "effectiveDateTime",
    O.valueQuantity.value ="valueQuantity/value",
    O.valueQuantity.unit = "valueQuantity/unit",
    O.referenceRange.low = "referenceRange/low/value",
    O.referenceRange.high = "referenceRange/high/value"
  ),
  sep="|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

observations <- fhir_crack(bundles=observation_bundles, design=obs_table, verbose=0)
#write.csv(observations, file="NutriScope_observations.csv", row.names=FALSE)

#------------------------
# # # calculate BMI 
#------------------------

# filter for records with height and weight by `O.code.coding.code`
height_data <- observations[observations$O.code.coding.code == "8302-2", ]
weight_data <- observations[observations$O.code.coding.code == "29463-7", ]

# check height is in m; if in cm --> convert 
height_data$O.valueQuantity.value <- ifelse(
  height_data$O.valueQuantity.unit == "cm",
  height_data$O.valueQuantity.value / 100,  # convert cm to m
  height_data$O.valueQuantity.value         # or keep if already in m
)

# check weight is in kg too??? --> assuming all weight values are in kg

# merge data by EID
height_data <- height_data[, c("EID", "O.valueQuantity.value", "O.valueQuantity.unit")]
weight_data <- weight_data[, c("EID", "O.valueQuantity.value", "O.valueQuantity.unit")]
# rename columns temporarily (avoiding conflicts during the merge)
colnames(height_data)[2:3] <- c("height_value", "height_unit")
colnames(weight_data)[2:3] <- c("weight_value", "weight_unit")

merged_data <- merge(height_data, weight_data, by = "EID")
merged_data<-unique(merged_data)

# calculate BMI
merged_data$O.valueQuantity.value <- merged_data$weight_value / (merged_data$height_value^2)

# append to original dataframe
bmi_data <- merged_data[, c("EID", "BMI")]
bmi_data$O.code.coding.code <- "BMI"
bmi_data$O.valueQuantity.unit <- "kg/m^2"

# same column structure as in original observations
missing_cols <- setdiff(names(observations), names(bmi_data))
# add missing columns to `bmi_data` --> set values to NA
for (col in missing_cols) {
  bmi_data[[col]] <- NA
}

# Reorder `bmi_data` to match the column order in `observations`
bmi_data <- bmi_data[, names(observations)]

# remove original height and weight
observations_filtered <- observations[!(observations$O.code.coding.code %in% c("8302-2", "29463-7")), ]
# append BMI to filtered original data
observations_with_bmi <- rbind(observations_filtered, bmi_data)

rm(merged_data,bmi_data,observations_filtered, height_data, weight_data,observations)

write.csv(observations_with_bmi, file="NutriScope_observations.csv", row.names=FALSE)

print("Observations done.")


# # # Procedure

request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Procedure",
                    parameters = c(
                      "subject"= patient_id_string)
)
procedure_bundles <- fhir_search(request = request, username=username, password=password, verbose = 0)

proc_table <- fhir_table_description(
  resource = "Procedure",
  cols     = c(
    ProID = "id",
    #Pro.category.coding.code ="category/coding/code",
    #Pro.category.coding.display = "category/coding/display",
    Pro.code.coding.system = "code/coding/system",
    Pro.code.coding.version= "code/coding/version",
    Pro.code.coding.code = "code/coding/code",
    PID ="subject/reference",
    EID = "encounter/reference",
    Pro.performed.DateTime = "performedDateTime"
  ),
  sep="|",
  rm_empty_cols = FALSE,
  format = 'compact',
  keep_attr = FALSE
)

procedures <- fhir_crack(bundles=procedure_bundles, design=proc_table, verbose=0)
write.csv(procedures,file="NutriScope_procedures.csv", row.names=FALSE)

print("Procedures done.")

# # interim step --> get results of dimensions
datasets<-list(enc=encounters, pat=patients, obs=observations, proc=procedures)

summary_list<-lapply(names(datasets), function(name) {
# check dimension and unique patients
  data <- datasets[[name]]
  data_dim <- dim(data)
  unique_ids <- length(unique(data[["PID"]]))
  colnames <- colnames(data)

  list(
    dataset = name,
    nrows = data_dim[1],
    ncols = data_dim[2],
    unique_PID = unique_ids,
    cols = colnames
  )
})

# convert into a data frame and save
summary_df <- do.call(rbind, lapply(summary_list, as.data.frame))
write.csv(summary_df, file="NutriScope_overview-resources.csv")
