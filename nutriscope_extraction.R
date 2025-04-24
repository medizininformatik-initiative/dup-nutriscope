# # # this scripts aims to extract the data for the NutriScope_project
# 
# please fill in the config.json first
#

################################################################################
# install and load necessary packages

package_names<-c("fhircrackr","dplyr","lubridate","jsonlite", "data.table")      
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
library(data.table)

#-------------------------------------------------------------------------------
# Read in configuration file of fhir_server
config <- fromJSON("config.json")
FHIR_SERVER <- config$FHIR_SERVER
username <- config$FHIR_USERNAME
password <- config$FHIR_PASSWORD

# Create folder in your actual path
output_directory <- "NutriScope_extracted_data"
dir.create(output_directory, showWarnings = FALSE)

#-------------------------------------------------------------------------------
# Extract data from different resources

# # # # # # # # # # # # 
# # # Encounter   # # #
# # # # # # # # # # # #
request <- fhir_url(url = FHIR_SERVER,
                    resource = "Encounter",
                    parameters = c(
                      "class"="http://terminology.hl7.org/CodeSystem/v3-ActCode|IMP", # inpatient class
                      "status"="finished" ,                                           # status 
                      "type"="einrichtungskontakt", # define type
                      "date"="ge2018-01-01",                                           # start 
                      "date"="le2023-12-31"                                            # end 2023
                    )                  
)
encounter_bundles <- fhir_search(request = request, username=username, password=password)

# definition of table design
enc_table <- fhir_table_description(
  resource = "Encounter",
  cols     = c(
    EID = "id",
    E.fallnummer = "identifier[system[@value='http://www.uniklinikum-jena.de/fhir/sid/encounter']]/value", #only Jena: filter for SAP ID
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

encounters <- fhir_crack(bundles=encounter_bundles, design=enc_table, data.table = TRUE)

print("Encounter done.")

#remove bundles to free memory
rm(encounter_bundles)

#-------------------------------------------------------------------------------

# # # # # # # # # # # # 
# # # Patient     # # #
# # # # # # # # # # # #

#split and collapse patient IDs
patient_ids <- unique(encounters$PID)  
patient_ids <- gsub("Patient/", "", patient_ids)

patient_ids <- split(patient_ids, ceiling(seq_along(patient_ids) / 50000))
patient_ids <- lapply(patient_ids,  paste , collapse = ",")

#Download Patients
patient_request <- fhir_url(url = FHIR_SERVER, resource = "Patient")

patient_bundles <- lapply(patient_ids, function(x) {
  patient_body <- fhir_body(content = list("_id" = x))
  fhir_search(
    request = patient_request,
    body = patient_body,
    username = username,
    password = password
  )
})

#create one overall fhir_bundle_list that merges the patient_bundles fhir_bundle_lists
patient_bundles_merged <- methods::initialize(new("fhir_bundle_list"))  # construction of empty fhir_bundle_list object
slot(patient_bundles_merged, ".Data") <- unlist(patient_bundles, recursive = F)    # manual assignment of the fhir_bundle objects from all lists


# definition of table design
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

patients <- fhir_crack(bundles = patient_bundles_merged, design = patient_table, data.table = TRUE)

rm(patient_bundles)

save.image("Zwischenstand2.RData")
# calculate the age of patients
# look for date of first occurrence of PID
temp <- encounters[order(E.period.start), .SD[1], by = PID, .SDcols = c("E.period.start")]

temp[,PID := sub("Patient/", "", PID, fixed = TRUE)]
patients <- merge.data.table(patients, temp, by="PID")

#add mm-dd to birthdate if missing
patients[, birthDate := fifelse(nchar(birthDate) == 4, paste0(birthDate, "-07-01"), birthDate)]

#compute age
patients[, age := trunc((patients$birthDate %--% patients$E.period.start) / years(1))]

patients[,c("birthDate","E.period.start") := NULL] # delete date of birth and first encounter

underage <- patients[age < 18]$PID

patients <- patients[age >= 18,] # only keep patients >= 18 years old

print("Patients done.")


### Remove encounters belonging to underage patients before going on ###
encounters <- encounters[!(sub("Patient/", "", PID, fixed = TRUE) %in% underage)]

# # # # # # # # # # # # 
# # # Condition   # # # 
# # # # # # # # # # # # 

#request Conditions via POST because GET restricts parameter length
#only download conditions that belong to encounters
#in Jena, maximal number of Encounter IDs for one request is 65.535 ids
EIDs <- paste0("Encounter/",encounters$EID)

#split and collapse EIDs
EIDs <- split(EIDs, ceiling(seq_along(EIDs) / 50000))
EID_strings <- lapply(EIDs,  paste , collapse = ",")

#Download Conditions
condition_request <- fhir_url(url = FHIR_SERVER, resource = "Condition")

condition_bundles <- lapply(EID_strings, function(x) {
  condition_body <- fhir_body(content = list("encounter" = x))
  fhir_search(
    request = condition_request,
    body = condition_body,
    username = username,
    password = password
  )
})


#create one overall fhir_bundle_list that merges the condition_bundles fhir_bundle_lists
condition_bundles_merged <- methods::initialize(new("fhir_bundle_list"))  # construction of empty fhir_bundle_list object
slot(condition_bundles_merged, ".Data") <- unlist(condition_bundles, recursive = F)    # manual assignment of the fhir_bundle objects from all lists

# definition of table design
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

conditions <- fhir_crack(bundles=condition_bundles_merged, design=condition_table, data.table = TRUE)
rm(condition_bundles,condition_bundles_merged)

print("Conditions done.")

save.image("Zwischenstand3.RData")


# # # # # # # # # # # # 
# # # Observation     # 
# # # # # # # # # # # #

#Download Observations
observation_request <- fhir_url(url = FHIR_SERVER, resource = "Observation")

observation_bundles <- lapply(EID_strings, function(x) {
  observation_body <- fhir_body(content = list("encounter" = x,
                                               "code" ="http://loinc.org|8302-2, http://loinc.org|29463-7, http://loinc.org|1751-7, http://loinc.org|14879-1"
  ))
  
  fhir_search(
    request = observation_request,
    body = observation_body,
    username = username,
    password = password
  )
})

#create one overall fhir_bundle_list that merges the observation_bundles fhir_bundle_lists
observation_bundles_merged <- methods::initialize(new("fhir_bundle_list"))  # construction of empty fhir_bundle_list object
slot(observation_bundles_merged, ".Data") <- unlist(observation_bundles, recursive = F)    # manual assignment of the fhir_bundle objects from all lists


# definition of table design
obs_table <- fhir_table_description(
  resource = "Observation",
  cols     = c(
    OID = "id",
    O.code.coding.system = "code/coding[system[@value='http://loinc.org']]/system",
    O.code.coding.code = "code/coding[system[@value='http://loinc.org']]/code",
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

observations <- fhir_crack(bundles=observation_bundles_merged, design=obs_table, data.table = TRUE)

rm(observation_bundles, observation_bundles_merged)

save.image("Zwischenstand4.RData")

#------------------------
# # # calculate BMI 
#------------------------

# filter for records with height and weight by `O.code.coding.code`
height_data <- observations[O.code.coding.code == "8302-2", ]
weight_data <- observations[O.code.coding.code == "29463-7", ]

#------------ For Jena only !!! -------------#
# read local height and weight data table, because not all of it is available in FHIR
local_dat <- fread(paste(config$local_data_dir, "nutriscope_gewicht_grosse_2018_bis_2023.csv", sep = "/"), keepLeadingZeros = TRUE)

#add encounter IDs, only keep cases that appear in Encounter data
local_dat <- merge.data.table(local_dat, encounters[, .(EID, E.fallnummer)],
                              by.x = "SAPFallID", by.y = "E.fallnummer", 
                              all=FALSE)

#divide height and weight data
height_local <- local_dat[!is.na(Groesse), .(EID, O.valueQuantity.value = Groesse, O.effectiveDateTime = GroesseDatum)]
weight_local <- local_dat[!is.na(Gewicht), .(EID, O.valueQuantity.value = Gewicht, O.effectiveDateTime = GewichtDatum)]

#combine local and FHIR data
height_data <- rbindlist(list(height_data[, .(EID, O.valueQuantity.value, O.effectiveDateTime)], height_local))
weight_data <- rbindlist(list(weight_data[, .(EID, O.valueQuantity.value, O.effectiveDateTime)], weight_local))

height_data[, O.valueQuantity.value := as.numeric(O.valueQuantity.value)]
weight_data[, O.valueQuantity.value := as.numeric(O.valueQuantity.value)]


rm(height_local, weight_local, local_dat)

#--------------------------------------------#
# interim step: get latest height and weight (if multiple are available per EID)
height_data <- height_data[
  , .SD[as.Date(O.effectiveDateTime) == max(as.Date(O.effectiveDateTime))][1], 
  by = EID
]

weight_data <- weight_data[
  , .SD[as.Date(O.effectiveDateTime) == max(as.Date(O.effectiveDateTime))][1], 
  by = EID
]

#height is always in cm in Jena
height_data[, O.valueQuantity.value := O.valueQuantity.value/100]

# # check height is in m; if in cm --> convert 
# height_data$O.valueQuantity.value <- ifelse(
#   height_data$O.valueQuantity.unit == "cm",
#   height_data$O.valueQuantity.value / 100,  # convert cm to m
#   height_data$O.valueQuantity.value         # or keep if already in m
# )

# check weight is in kg too??? --> assuming all weight values are in kg

# merge data by EID

# rename columns temporarily (avoiding conflicts during the merge)
setnames(height_data, old = "O.valueQuantity.value", new = "height_value")
setnames(weight_data, old = "O.valueQuantity.value", new = "weight_value")

merged_data <- merge(x = height_data[, .(EID, height_value)], 
                     y = weight_data[, .(EID, weight_value)], 
                     by = "EID", all = FALSE)

# calculate BMI
merged_data$O.valueQuantity.value <- merged_data$weight_value / (merged_data$height_value^2)

# append to original dataframe
bmi_data <- copy(merged_data[, c("EID", "O.valueQuantity.value")])
bmi_data[, O.code.coding.code := "BMI"]
bmi_data[, O.valueQuantity.unit := "kg/m^2"]

# remove original height and weight
observations_filtered <- observations[!(O.code.coding.code %in% c("8302-2", "29463-7")), ]

# append BMI to filtered original data
observations <- rbindlist(list(observations_filtered, bmi_data), fill = TRUE)

rm(merged_data,bmi_data,observations_filtered, height_data, weight_data)

print("Observations done.")

# # # # # # # # # # # # 
# # # Procedure       # 
# # # # # # # # # # # # 


#Download procedures
procedure_request <- fhir_url(url = FHIR_SERVER, resource = "Procedure")

procedure_bundles <- lapply(EID_strings, function(x) {
  procedure_body <- fhir_body(content = list("encounter" = x))
  
  fhir_search(
    request = procedure_request,
    body = procedure_body,
    username = username,
    password = password
  )
})

#create one overall fhir_bundle_list that merges the procedure_bundles fhir_bundle_lists
procedure_bundles_merged <- methods::initialize(new("fhir_bundle_list"))  # construction of empty fhir_bundle_list object
slot(procedure_bundles_merged, ".Data") <- unlist(procedure_bundles, recursive = F)    # manual assignment of the fhir_bundle objects from all lists


# definition of table design
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

procedures <- fhir_crack(bundles=procedure_bundles_merged, design=proc_table, data.table = TRUE)

print("Procedures done.")

rm(procedure_bundles, procedure_bundles_merged)

save.image("Zwischenstand5.RData")

# -----------------------------------------------------------------------------------------------------------
# # # interim step --> get results of dimensions (leave commented out, no test run)
####################################################
#datasets<-list(enc=encounters, pat=patients, obs=observations, proc=procedures)

#summary_list<-lapply(names(datasets), function(name) {

# check dimension and unique patients
#  data <- datasets[[name]]
#  data_dim <- dim(data)
#  unique_ids <- length(unique(data[["PID"]]))
#  colnames <- colnames(data)

#  list(
#    dataset = name,
#    nrows = data_dim[1],
#    ncols = data_dim[2],
#    unique_PID = unique_ids,
#    cols = colnames
#  )
#})

# convert into a data frame and save
#summary_df <- do.call(rbind, lapply(summary_list, as.data.frame))
#write.csv(summary_df, file="NutriScope_overview-resources.csv")


#----------------------------------------------------------------------------------------------------
# create final dataset (following the example of DIZ leipzig)
#----------------------------------------------------------------------------------------------------
#This code currently doesn't make sense: putting all data in the same table results in a gigantic sparse table
#data from different resources should be kept in different tables.

# patients
df <- copy(patients)

# set columnnames as given 
setnames(df, old = names(df), new = c("Patientennummer","Geschlecht","Alter bei Aufnahme"))
#----------------------------------------------------------------------------------------------------

# add encounter
encounters[, Patientennummer := sub("Patient/","",PID, fixed = TRUE)]

#------------------------------------------------------------------------------------#
#This bit seems to be matched to the data from Leipzig only

# encounters$Fachabteilungsschluessel <- gsub("^[^_]*_([^_]*)_.*", "\\1", encounters$E.fallnummer)     # extract "Fachabteilung":this is only existing if patient was moved in hospital  
# encounters$Fallnummer <- gsub("_.*", "", encounters$E.fallnummer)                                    # extract case id
# # remove Fallnummern out of Fachabteilungsschluessel
# encounters[,"Fachabteilungsschluessel"] <- ifelse(
#   encounters[,"Fachabteilungsschluessel"] == encounters[,"Fallnummer"],
#   NA, 
#   encounters[,"Fachabteilungsschluessel"]
# )

encounters[, Fallnummer := E.fallnummer]
#----------------------------------------------------------------------------------------#

# calculate length of stay
encounters$Aufnahmedatum <- ymd_hms(encounters$E.period.start)
encounters$Entlassdatum <- ymd_hms(encounters$E.period.end)
encounters$Verweildauer<- as.numeric(difftime(encounters$Entlassdatum, encounters$Aufnahmedatum, units = "days"))

# # # calculate time gap to next stay 
#meaning: Time between discharge and and next admission

encounters[order(Patientennummer, Aufnahmedatum), 
           ZeitNaechsterAufenthalt := as.numeric(difftime(shift(Aufnahmedatum, type = "lead"), Entlassdatum, units = "days")) ,
           by = Patientennummer]

# merge patients + encounters
df <- merge.data.table(x = df, 
                       y = encounters[,c("Patientennummer","EID","Fallnummer","Aufnahmedatum","Entlassdatum","Verweildauer","ZeitNaechsterAufenthalt")],
                       by = "Patientennummer", all.x = TRUE)

#----------------------------------------------------------------------------------------------------

# rename some columns + removal prefix in "Patientennummer" + "EID"
setnames(conditions, old = c("PID", "C.code.coding.version"), new = c("Patientennummer", "ICD-Version"))

conditions[, Patientennummer := sub("Patient/", "", Patientennummer, fixed = TRUE)]
conditions[, EID := sub("Encounter/", "", EID, fixed = TRUE)]

# assign to main and secondary diagnosis
conditions[, Hauptdiagnose := fifelse(C.category.coding.code == "CC", C.code.coding.code, NA)] 
conditions[, Nebendiagnose := fifelse(C.category.coding.code == "CM", C.code.coding.code, NA)] 

# merge to previous df (patients+encounters)
# df <- merge.data.table(x = df, 
#                        y = conditions[,c("Patientennummer","EID","Hauptdiagnose","Nebendiagnose","ICD-Version")], 
#                        by = c("Patientennummer","EID"), all.x=TRUE)

#----------------------------------------------------------------------------------------------------

# add procedures (code+date), rename some columns
setnames(procedures, 
         old = c("PID", "Pro.code.coding.code", "Pro.performed.DateTime"), 
         new = c("Patientennummer", "OPS-Kode", "Prozeduren-Datum"))

procedures[, Patientennummer := sub("Patient/", "", Patientennummer, fixed = TRUE)]
procedures[, EID := sub("Encounter/", "", EID, fixed = TRUE)]

# merge to previous df (patients+encounters+conditions)
# df <- merge.data.table(x = df, 
#             y = procedures[,c("Patientennummer","EID","OPS-Kode","Prozeduren-Datum")], 
#             by=c("Patientennummer","EID"), all.x=TRUE)

#----------------------------------------------------------------------------------------------------

# add observations
setnames(observations, 
         old = c("PID", "O.effectiveDateTime"), 
         new = c("Patientennummer", "O.DateTime"))

observations[, Patientennummer := sub("Patient/", "", Patientennummer, fixed = TRUE)]
observations[, EID := sub("Encounter/", "", EID, fixed = TRUE)]

# merge to previous df (patients+encounters+conditions+procedures)
#df<-merge(df, observations[,c("Patientennummer","EID","BMI","Albumin","Phosphat","O.DateTime")], by=c("Patientennummer","EID"), all.x=TRUE)

# maybe not redesing dataframe, just keep O.code.coding.code
# df<-merge.data.table(x = df, 
#                      y = observations[,c("Patientennummer","EID","O.code.coding.code","O.DateTime")], 
#                      by=c("Patientennummer","EID"), all.x=TRUE)



#----------------------------------------------------------------------------------------------------
# # # save final datasets
fwrite(df,file="NutriScope_extracted_data/NutriScope_pat_enc.csv", row.names=FALSE,quote=FALSE)
fwrite(observations,file="NutriScope_extracted_data/NutriScope_obs.csv", row.names=FALSE,quote=FALSE)
fwrite(procedures,file="NutriScope_extracted_data/NutriScope_proc.csv", row.names=FALSE,quote=FALSE)
fwrite(conditions,file="NutriScope_extracted_data/NutriScope_con.csv", row.names=FALSE,quote=FALSE)


