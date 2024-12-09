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

# # # # # # # # # # # # 
# # # Encounter   # # #
# # # # # # # # # # # #
request <- fhir_url(url = FHIR_SERVER,
                    resource = "Encounter",
                    parameters = c(
                      "class"="http://terminology.hl7.org/CodeSystem/v3-ActCode|IMP", # inpatient class
                      "status"="finished",                                            # status 
                      #"type/coding/code"="https://www.medizininformatik-initiative.de/fhir/core/modul-fall/CodeSystem/Kontaktebene|einrichtungskontakt" # define type
                      "date"="ge2018-01-01",                                           # start 
                      "date"="le2023-12-31"                                            # end 2023
                    )                  
)
encounter_bundles <- fhir_search(request = request, username=username, password=password, verbose = 0)

# definition of table design
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
patient_id_string2 <- paste(patient_ids2, collapse = ",")      # used for patient resource only

# error-message: too long format for request, try to split into individual IDs again
patient_id_string <- strsplit(patient_id_string, ",")[[1]]
patient_id_string2 <- strsplit(patient_id_string2, ",")[[1]]


# need to split ids, otherwise it's too long for next requests
split_ids <- function(ids, chunk_size) {
  split(ids, ceiling(seq_along(ids) / chunk_size))
}
chunk_size <- 50
patient_id_chunks <- split_ids(patient_id_string, chunk_size)
patient_id_chunks2 <- split_ids(patient_id_string2, chunk_size)

# # # # # # # # # # # # 
# # # Patient     # # #
# # # # # # # # # # # #

patient_bundles <- list()
for (i in seq_along(patient_id_chunks2)) {
  chunk <- paste(patient_id_chunks2[[i]], collapse = ",")
  request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Patient",
                    parameters = c(paste0("_id=", chunk))     # in Ecounter extracted IDs only
)
patient_bundles[[i]] <- fhir_search(request = request, username=username, password=password, verbose = 0)      
}
patient_bundles <- do.call(c, patient_bundles)

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

patients <- fhir_crack(bundles = patient_bundles, design = patient_table, verbose = 0)

# calculate the age of patients

temp<-encounters %>%                      # look for date of first occurrence of PID
  group_by(PID) %>%
  arrange(E.period.start) %>%
  slice(1L) %>% select (PID, E.period.start)

temp$E.period.start<-gsub("T..*","",temp$E.period.start)
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

# # # # # # # # # # # # 
# # # Condition   # # # 
# # # # # # # # # # # # 

condition_bundles <- list()
for (i in seq_along(patient_id_chunks)) {
  chunk <- paste(patient_id_chunks[[i]], collapse = ",")
  request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Condition",
                    parameters = c("subject"= chunk)
                     )
  condition_bundles[[i]] <- fhir_search(request = request , username=username, password=password, verbose = 0) 
}

condition_bundles <- do.call(c, condition_bundles)

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

conditions <- fhir_crack(bundles=condition_bundles, design=condition_table, verbose=0)

# keep existing EIDs from encounters only
EIDs <- paste0("Encounter/",encounters$EID)
conditions <- conditions[which(conditions$EID %in% EIDs),]

write.csv(conditions, file="NutriScope_conditions.csv", row.names=FALSE)

print("Conditions done.")


# # # # # # # # # # # # 
# # # Observation     # 
# # # # # # # # # # # # 

observation_bundles <- list()
for (i in seq_along(patient_id_chunks)) {
  chunk <- paste(patient_id_chunks[[i]], collapse = ",")
  request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Observation",  
                    parameters = c("subject"= chunk,
                                  "code" ="http://loinc.org|8302-2, http://loinc.org|29463-7, http://loinc.org|1751-7, http://loinc.org|14879-1")       # add loincs here
                    )
observation_bundles[[i]] <- fhir_search(request = request, username=username, password=password, verbose = 0)
}
observation_bundles <- do.call(c, observation_bundles)

# definition of table design
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

observations <- fhir_crack(bundles=observation_bundles, design=obs_table, verbose=0)# keep existing EIDs from encounters only

# keep existing EIDs from encounters only
observations <- observations[which(observations$EID %in% EIDs),]

write.csv(observations, file="NutriScope_observations.csv", row.names=FALSE)

#------------------------
# # # calculate BMI 
#------------------------

# filter for records with height and weight by `O.code.coding.code`
height_data <- observations[observations$O.code.coding.code == "8302-2", ]
weight_data <- observations[observations$O.code.coding.code == "29463-7", ]

# interim step: get latest height and weight (if multiple are available per EID)
height_data <- height_data %>%
  group_by(EID) %>%
  filter(O.effectiveDateTime == max(as.Date(O.effectiveDateTime))) %>%
  slice(1)

weight_data <- weight_data %>%
  group_by(EID) %>%
  filter(O.effectiveDateTime == max(as.Date(O.effectiveDateTime))) %>%
  slice(1)

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

# reorder bmi_data to match the column order in observations
bmi_data <- bmi_data[, names(observations)]

# remove original height and weight
observations_filtered <- observations[!(observations$O.code.coding.code %in% c("8302-2", "29463-7")), ]
# append BMI to filtered original data
observations_with_bmi <- rbind(observations_filtered, bmi_data)

rm(merged_data,bmi_data,observations_filtered, height_data, weight_data,observations)

write.csv(observations_with_bmi, file="NutriScope_observations.csv", row.names=FALSE)

print("Observations done.")


# # # # # # # # # # # # 
# # # Procedure       # 
# # # # # # # # # # # # 
procedure_bundles <- list()
for (i in seq_along(patient_id_chunks)) {
  chunk <- paste(patient_id_chunks[[i]], collapse = ",")
  request <- fhir_url(url = FHIR_SERVER, 
                    resource = "Procedure",
                    parameters = c(
                      "subject"= chunk)
  )
  procedure_bundles[[i]] <- fhir_search(request = request, username=username, password=password, verbose = 0)
}
procedure_bundles <- do.call(c, procedure_bundles)

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

procedures <- fhir_crack(bundles=procedure_bundles, design=proc_table, verbose=0)

# keep existing EIDs from encounters only
procedures <- procedures[which(procedures$EID %in% EIDs),]

write.csv(procedures,file="NutriScope_procedures.csv", row.names=FALSE)

print("Procedures done.")


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

# patients

df<-patients
# set columnnames as given 
colnames(df)<-c("Patientennummer","Geschlecht","Alter bei Aufnahme")
#----------------------------------------------------------------------------------------------------

# add encounter
encounters$Patientennummer <- gsub("\\Patient/","",encounters$PID)
encounters$Fachabteilungsschluessel <- gsub("^[^_]*_([^_]*)_.*", "\\1", encounters$E.fallnummer)     # extract "Fachabteilung":this is only existing if patient was moved in hospital  
encounters$Fallnummer <- gsub("_.*", "", encounters$E.fallnummer)                                    # extract case id
# remove Fallnummern out of Fachabteilungsschluessel
encounters[,"Fachabteilungsschluessel"] <- ifelse(
  encounters[,"Fachabteilungsschluessel"] == encounters[,"Fallnummer"],
  NA, 
  encounters[,"Fachabteilungsschluessel"]
)
  
# calculate length of stay
encounters$Aufnahmedatum <- ymd_hms(encounters$E.period.start)
encounters$Entlassdatum <- ymd_hms(encounters$E.period.end)
encounters$Verweildauer<- as.numeric(difftime(encounters$Entlassdatum, encounters$Aufnahmedatum, units = "days"))

# # # calculate time gap to next stay 

encounters <- encounters[order(encounters$Patientennummer, encounters$Aufnahmedatum), ]
encounters$ZeitNaechsterAufenthalt <- NA

# first idea: only if Fallnummer is different in these rows
#for (unique_id in unique(encounters$Patientennummer)) {
 # id_rows <- which(encounters$Patientennummer == unique_id)

  # for (i in seq_along(id_rows)[-length(id_rows)]) {    # excluding last row, because there is no next "Aufnahmedatum"
   # current_row <- id_rows[i]
   # next_row <- id_rows[i + 1]
# time gap between the current end and the next start
    # if (encounters$Fallnummer[current_row] != encounters$Fallnummer[next_row]) {
     #     encounters$ZeitNaechsterAufenthalt[id_rows[-length(id_rows)]] <- as.numeric(
      #      difftime(
       #     encounters$Aufnahmedatum[id_rows[-1]],
        #    encounters$Entlassdatum[id_rows[-length(id_rows)]],
         #   units = "days"
       # )
     # )
   # }
 # }
#}

# second idea: use partOf.ref --> needs to be NA       # better choice if the partOf.ref exists in FHIR

encounters <- encounters[which(is.na(encounters$E.partOf.ref)),]

for (unique_id in unique(encounters$Patientennummer)) {
  id_rows <- which(encounters$Patientennummer == unique_id)

   for (i in seq_along(id_rows)[-length(id_rows)]) {    # excluding last row, because there is no next "Aufnahmedatum"
    current_row <- id_rows[i]
    next_row <- id_rows[i + 1]
# time gap between the current end and the next start
          encounters$ZeitNaechsterAufenthalt[id_rows[-length(id_rows)]] <- as.numeric(
            difftime(
            encounters$Aufnahmedatum[id_rows[-1]],
            encounters$Entlassdatum[id_rows[-length(id_rows)]],
            units = "days"
        )
      )
  }
}


# merge patients + encounters

df<-merge(df, encounters[,c("Patientennummer","EID","Fallnummer","Aufnahmedatum","Entlassdatum","Fachabteilungsschluessel","Verweildauer","ZeitNaechsterAufenthalt")], by="Patientennummer",all.x=TRUE)
#----------------------------------------------------------------------------------------------------

# rename some columns + removal prefix in "Patientennummer" + "EID"

names(conditions)[names(conditions) == "PID"] <- "Patientennummer"
names(conditions)[names(conditions) == "C.code.coding.version"] <- "ICD-Version"
conditions$Patientennummer <- gsub("\\Patient/","",conditions$Patientennummer)
conditions$EID <- gsub("\\Encounter/","",conditions$EID)

# assign to main and secondary diagnosis
conditions <- conditions %>%
  mutate(
    Hauptdiagnose = ifelse(C.category.coding.code == "CC", C.code.coding.code, NA),   
    Nebendiagnose = ifelse(C.category.coding.code == "CM", C.code.coding.code, NA) 
  )

# merge to previous df (patients+encounters)
df<-merge(df, conditions[,c("Patientennummer","EID","Hauptdiagnose","Nebendiagnose","ICD-Version")], by=c("Patientennummer","EID"),all.x=TRUE)

#----------------------------------------------------------------------------------------------------

# add procedures (code+date), rename some columns
names(procedures)[names(procedures) == "PID"] <- "Patientennummer"
names(procedures)[names(procedures) == "Pro.code.coding.code"] <- "OPS-Kode"
names(procedures)[names(procedures) == "Pro.performed.DateTime"] <- "Prozeduren-Datum"
procedures$Patientennummer <- gsub("\\Patient/","", procedures$Patientennummer)
procedures$EID <- gsub("\\Encounter/","", procedures$EID)

# merge to previous df (patients+encounters+conditions)
df<-merge(df, procedures[,c("Patientennummer","EID","OPS-Kode","Prozeduren-Datum")], by=c("Patientennummer","EID"),all.x=TRUE)

#----------------------------------------------------------------------------------------------------

# add observations
names(observations)[names(observations) == "PID"] <- "Patientennummer"
names(observations)[names(observations) == "O.effectiveDateTime"] <- "O.DateTime"
observations$Patientennummer <- gsub("\\Patient/","", observations$Patientennummer)
observations$EID <- gsub("\\Encounter/","", observations$EID)

observations <- observations %>%
  mutate(
    BMI = ifelse(O.code.coding.code == "BMI", O.valueQuantity.value, NA),   
    Albumin = ifelse(O.code.coding.code == "1751-7", O.valueQuantity.value, NA),    
    Phosphat = ifelse(O.code.coding.code == "14879-1", O.valueQuantity.value, NA)
  )

# merge to previous df (patients+encounters+conditions+procedures)
df<-merge(df, observations[,c("Patientennummer","EID","BMI","Albumin","Phosphat","O.DateTime")], by=c("Patientennummer","EID"), all.x=TRUE)

#----------------------------------------------------------------------------------------------------
# # # save final dataset
df<-unique(df)
write.csv(df,file="NutriScope_data.csv", row.names=FALSE,quote=FALSE)


#----------------------------------------------------------------------------------------------------
# # # remove the initially extracted FHIR-files
file.remove("NutriScope_encounters.csv","NutriScope_patients.csv","NutriScope_conditions.csv","NutriScope_observations.csv","NutriScope_procedures.csv")
