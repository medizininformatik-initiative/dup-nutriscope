# ######################################## #
# QUALITY CHECK                            #
# ######################################## #

# PACKAGES
package_names <- c("e1071", "dplyr", "tidyr", "stringr", "magrittr", "ggplot2")

# Check if required packages are installed
for (package_name in package_names) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

# FUNCTIONS

# Function to calculate the range of values
calculate_range <- function(column) {
  if (is.numeric(column) || is.integer(column)) {
    min_value <- min(column, na.rm = TRUE)
    max_value <- max(column, na.rm = TRUE)
    return(paste("Range:", min_value, "-", max_value))
  } else {
    return("NA")
  }
}

# Function to check if all values in a column have the same data type
check_same_datatype <- function(column) {
  if (length(unique(sapply(column, class))) == 1) {
    return("Yes")
  } else {
    return("No")
  }
}

# Function to calculate statistical metrics
calculate_mean <- function(column) {
  if (is.numeric(column) || is.integer(column)) {
    return(mean(column, na.rm = TRUE))
  } else {
    return(NA)
  }
}

calculate_sd <- function(column) {
  if (is.numeric(column) || is.integer(column)) {
    return(sd(column, na.rm = TRUE))
  } else {
    return(NA)
  }
}

calculate_variance <- function(column) {
  if (is.numeric(column) || is.integer(column)) {
    return(var(column, na.rm = TRUE))
  } else {
    return(NA)
  }
}

calculate_skewness <- function(column) {
  if (is.numeric(column) || is.integer(column)) {
    return(skewness(column, na.rm = TRUE))
  } else {
    return(NA)
  }
}

calculate_normality <- function(column) {
  if (is.numeric(column) || is.integer(column)) {
    non_na_values <- column[!is.na(column)]
    sample_values <- if (length(non_na_values) > 5000) sample(non_na_values, 5000) else non_na_values
    return(shapiro.test(sample_values)$p.value)
  } else {
    return(NA)
  }
}

calculate_mode <- function(column) {
  uniq_vals <- unique(column)
  uniq_vals[which.max(tabulate(match(column, uniq_vals)))]
}

# Function to create  histogram for  "Alter" 
create_age_histogram <- function(data, column_name, output_folder, age_file_name) {
  plot <- ggplot(data, aes(x = !!sym(column_name))) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Altersverteilung", x = "Alter", y = "Häufigkeit") +
    theme_minimal()
  
  # save plot as png
  ggsave(file.path(output_folder, paste0(age_file_name, "_Histogramm_Alter.png")), plot = plot, width = 8, height = 6)
}

# Function to create  histogram for  Verweildauer and Zeit bis zum nächsten Aufenthalt
create_adm_histogram <- function(data, column_name, output_folder, adm_file_name) {
  plot <- ggplot(data, aes(x = !!sym(column_name))) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Verteilung Verweildauer", x = "Verweildauerin Tagen", y = "Häufigkeit") +
    theme_minimal()
  
  # save plot as png
  ggsave(file.path(output_folder, paste0(adm_file_name, "_Histogramm_admission.png")), plot = plot, width = 8, height = 6)
}


create_readm_histogram <- function(data, column_name, output_folder, readm_file_name) {
  plot <- ggplot(data, aes(x = !!sym(column_name))) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Verteilung der Zeit bis zur Wiederaufnahme", x = "Zeit bis zum nächsten Aufenthalt in Tagen", y = "Häufigkeit") +
    theme_minimal()
  
  # save plot as png
  ggsave(file.path(output_folder, paste0(readm_file_name, "_Histogramm_readmission.png")), plot = plot, width = 8, height = 6)
}


# count values in Verweildauer columns based on  <0 and >0
count_adm_values <- function(data, column_name, output_folder, file_name) {
  # Count values that are <0 and >0
  adm_count <- data %>%
    dplyr::mutate(category = case_when(
      !!sym(column_name) < 0 ~ "<0",
      !!sym(column_name) > 0 ~ ">0",
      TRUE ~ "NA"
    )) %>%
    dplyr::filter(category != "NA") %>%
    dplyr::count(category, name = "n") %>%
    as.data.frame()
  
  # Save the results as CSV
  adm_file_name <- paste0(file_name, "_Verweildauer_kleiner0.csv")
  write.csv(adm_count, file.path(output_folder, adm_file_name), row.names = FALSE)
}


count_readm_values <- function(data, column_name, output_folder, file_name) {
  # Count values that are <0 and >0
  readm_count <- data %>%
    dplyr::mutate(category = case_when(
      !!sym(column_name) < 0 ~ "<0",
      !!sym(column_name) > 0 ~ ">0",
      TRUE ~ "NA"
    )) %>%
    dplyr::filter(category != "NA") %>%
    dplyr::count(category, name = "n") %>%
    as.data.frame()
  
  # Save the results as CSV
  readm_file_name <- paste0(file_name, "_Readm_kleiner0.csv")
  write.csv(readm_count, file.path(output_folder, readm_file_name), row.names = FALSE)
}

# Häufigkeit altersgruppen zählen
count_age_groups <- function(data, age_column, output_folder, file_name) {
  
  age_grouped <- data %>%
    dplyr::mutate(Altersgruppe = case_when(
      !!sym(age_column) < 18 ~ "<18",
      !!sym(age_column) >= 18 & !!sym(age_column) <= 19 ~ "18-19",
      !!sym(age_column) >= 20 & !!sym(age_column) <= 29 ~ "20-29",
      !!sym(age_column) >= 30 & !!sym(age_column) <= 39 ~ "30-39",
      !!sym(age_column) >= 40 & !!sym(age_column) <= 49 ~ "40-49",
      !!sym(age_column) >= 50 & !!sym(age_column) <= 59 ~ "50-59",
      !!sym(age_column) >= 60 & !!sym(age_column) <= 69 ~ "60-69",
      !!sym(age_column) >= 70 & !!sym(age_column) <= 79 ~ "70-79",
      !!sym(age_column) >= 80 & !!sym(age_column) <= 89 ~ "80-89",
      !!sym(age_column) >= 90 & !!sym(age_column) <= 99 ~ "90-99",
      !!sym(age_column) >= 100 & !!sym(age_column) <= 109 ~ "100-109",
      !!sym(age_column) >= 110 & !!sym(age_column) <= 119 ~ "110-119",
      TRUE ~ "120+"
    )) %>%
    dplyr::count(Altersgruppe, name = "frequency") %>%
    as.data.frame()
  
  # Save the results as CSV
  age_group_file_name <- paste0(file_name, "_agegroupsFREQ.csv")
  write.csv(age_grouped, file.path(output_folder, age_group_file_name), row.names = FALSE)
}


# Function to create OPS chapters and count frequencies
count_ops_chapters <- function(data, OPS_column, output_folder, file_name) {
  # Create OPS chapters
  ops_grouped <- data %>%
    dplyr::mutate(OPS_Kapitel = case_when(
      str_starts(!!sym(OPS_column), "0") ~ "Kapitel 0",
      str_starts(!!sym(OPS_column), "1") ~ "Diagnost. Maßnahmen",
      str_starts(!!sym(OPS_column), "3") ~ "Bildgeb. Diagnostik",
      str_starts(!!sym(OPS_column), "5") ~ "Operationen",
      str_starts(!!sym(OPS_column), "6") ~ "Medikamente",
      str_starts(!!sym(OPS_column), "8") ~ "Therapeut. Maßnahmen",
      str_starts(!!sym(OPS_column), "9") ~ "Ergänzende Maßnahmen",
      str_starts(OPS_column, "9") ~ "Ergänzende Maßnahmen", 
      !!sym(OPS_column) == "beatmung" ~ "beatmung",                  
      !!sym(OPS_column) == "its_24h" ~ "its_24h",  
      TRUE ~ "Unbekannt"           
    )) %>%
    # Gruppieren nach den Kategorien und Summe der Werte aus der zweiten Spalte
    dplyr::count(OPS_Kapitel, name = "Haeufigkeit") %>%
    as.data.frame()

  
  # Save the results as CSV
  ops_chapter_file_name <- paste0(file_name, "_ops_chaptersFREQ.csv")
  write.csv(ops_grouped, file.path(output_folder, ops_chapter_file_name), row.names = FALSE)
  
  # Count frequencies of specific OPS codes
  ops_kodes_ET <- c(
    "8-831", "5-399.5", "5-431.2", "5-450.3",
    "8-015", "8-016", "8-017", "8-018",
    "8-123", "8-124", "8-125", "795.81", "9-500", "8-89j"
  )
  
  ops_specific_counts <- data %>%
    dplyr::filter(!!sym(OPS_column) %in% ops_kodes_ET) %>%
    dplyr::count(OPS_Kode = !!sym(OPS_column), name = "freq") %>%
    as.data.frame()
  
  # Save the specific OPS code counts as CSV
  ops_specific_file_name <- paste0(file_name, "_ops_ETFREQ.csv")
  write.csv(ops_specific_counts, file.path(output_folder, ops_specific_file_name), row.names = FALSE)
}




# Function to create  histogram for BMI
create_bmi_histogram <- function(data, column_name, output_folder, bmi_file_name) {
  plot <- ggplot(data, aes(x = !!sym(column_name))) +
    geom_histogram(binwidth = 3, fill = "blue", color = "black", alpha = 0.8) +
    labs(title = "BMI-Verteilung", x = "BMI", y = "Häufigkeit") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5)  # Zentriert den Titel
    )
  
  # save plot as png
  ggsave(file.path(output_folder, paste0(bmi_file_name, "_Histogramm_BMI.png")), plot = plot, width = 8, height = 6)
}

# Function to create boxplot for BMI
create_bmi_boxplot <- function(data, column_name, output_folder, bmi_file_name) {
  plot <- ggplot(data, aes(y = !!sym(column_name))) +
    geom_boxplot(outlier.colour = "blue", outlier.shape = 8, outlier.size = 2) +
    labs(title = "BMI Boxplot", y = "BMI") +
    theme_minimal()
  
  # save plot as png
  ggsave(file.path(output_folder, paste0(bmi_file_name, "_Boxplot_BMI.png")), plot = plot, width = 8, height = 6)
}



create_movement_frequency <- function(data, output_folder) {
  # Liste der Spalten, nach denen gesucht werden soll
  movement_columns <- c("Bewegungsart", "Bewegungstyp")
  
  # Durchsuche die DataFrame-Spalten nach den gesuchten Spaltennamen
  for (col in movement_columns) {
    if (col %in% colnames(data)) {
      # Erstelle eine Frequenztabelle für die Spalte
      freq_data <- data %>%
        group_by(!!sym(col)) %>%
        summarise(Haeufigkeit = n()) %>%
        arrange(desc(Haeufigkeit)) %>%
        as.data.frame()
      
      # Speichere die Frequenztabelle als CSV-Datei
      output_file <- file.path(output_folder, paste0(col,"FREQ.csv"))
      write.csv(freq_data, output_file, row.names = FALSE)
    }
  }
}


######################################  
###### PROCESS CSV FILE ##############
######################################

process_csv_file <- function(file_path, output_folder) {
  data <- read.csv(file_path, sep = ";", fileEncoding = "UTF-8")
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # QC Results
  unique_counts <- sapply(data, function(x) length(unique(x)))
  range_values <- sapply(data, calculate_range)
  all_same_datatype <- sapply(data, check_same_datatype)
  duplicates <- sapply(data, function(column) {
    any(duplicated(column))
  })
  mean_values <- sapply(data, calculate_mean)
  sd_values <- sapply(data, calculate_sd )
  variance_values <- sapply(data, calculate_variance)
  skewness_values <- sapply(data, calculate_skewness)
  mode_values <- sapply(data, calculate_mode)
  normality_values <- sapply(data, calculate_normality)
  
  # Store results in a data frame
  results <- data.frame(
    "dimension" = paste("Rows:", nrow(data), "Columns:", ncol(data)),
    "column_name" = colnames(data),
    "data_type" = sapply(data, class),
    "missing_values" = colSums(is.na(data)),
    "unique_values" = unique_counts,
    "range_of_values" = range_values,
    "all_same_data_type" = all_same_datatype,
    "Column_with_duplicates" = duplicates,
    "mean" = mean_values,
    "sd" = sd_values,
    "variance" = variance_values,
    "skewness" = skewness_values,
    "mode" = mode_values,
    "normality_p_value" = normality_values
  )
  
  # Save results as CSV
  result_file_name <- paste0(file_name, "_QC_results.csv")
  write.csv(results, file.path(output_folder, result_file_name), row.names = FALSE)


  # Check if "Alter" is part of any column name
  age_columns <- grep("Alter", colnames(data), value = TRUE)
  
  if (length(age_columns) > 0) {
    for (age_column in age_columns) {
      if (is.numeric(data[[age_column]]) || is.integer(data[[age_column]])) {
        age_file_name <- tools::file_path_sans_ext(basename(file_path))
        create_age_histogram(data, age_column, output_folder, age_file_name)
      }
    }
  }
 
  # create freq table age gr.
  if (length(age_columns) > 0) {
    for (age_column in age_columns) {
      if (is.numeric(data[[age_column]]) || is.integer(data[[age_column]])) {
        count_age_groups(data, age_column, output_folder, file_name)
      }
    }
  }
  
   
  #search for Verweildauer und Zeit bis nächster Aufenthalt clolumns  
  adm_columns <- grep("Verweildauer", colnames(data), value = TRUE)
  
  if (length(adm_columns) > 0) {
    for (adm_column in adm_columns) {
      if (is.numeric(data[[adm_column]]) || is.integer(data[[adm_column]])) {
        adm_file_name <- tools::file_path_sans_ext(basename(file_path))
        create_adm_histogram(data, adm_column, output_folder, adm_file_name)
      }
    }
  }
 
  # create freq table for Verweildauer <0 
  if (length(adm_columns) > 0) {
    for (adm_column in adm_columns) {
      if (is.numeric(data[[adm_column]]) || is.integer(data[[adm_column]])) {
        count_adm_values(data, adm_column, output_folder, file_name)
      }
    }
  }
  
  readm_columns <- grep("Aufenthalt", colnames(data), value = TRUE)
  
  if (length(readm_columns) > 0) {
    for (readm_column in readm_columns) {
      if (is.numeric(data[[readm_column]]) || is.integer(data[[readm_column]])) {
        readm_file_name <- tools::file_path_sans_ext(basename(file_path))
        create_readm_histogram(data, readm_column, output_folder, readm_file_name)
      }
    }
  }
 
  readm_columns <- grep("Aufenthalt", colnames(data), value = TRUE)
  
  if (length(readm_columns) > 0) {
    for (readm_column in readm_columns) {
      if (is.numeric(data[[readm_column]]) || is.integer(data[[readm_column]])) {
        count_readm_values(data, readm_column, output_folder, file_name)
      }
    }
  }
   
  # Check if "Geschlecht" column exists
  if ("Geschlecht" %in% colnames(data)) {
    # Create frequency table for "Geschlecht"
    gender_freq <- data %>%
      dplyr::count(Geschlecht, name = "n") %>%
      as.data.frame()
    
    # Generate file name 
    gender_file_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_genderFREQ.csv")
    write.csv(gender_freq, file.path(output_folder, gender_file_name), row.names = FALSE)
  }
 
  
  # search for bmi columns
  bmi_columns <- grep("Body", colnames(data), value = TRUE)
  
  if (length(bmi_columns) > 0) {
    for (bmi_column in bmi_columns) {
      if (is.numeric(data[[bmi_column]]) || is.integer(data[[bmi_column]])) {
        
        # remove NAs
        filtered_bmi_data <- data[!is.na(data[[bmi_column]]), ]
        bmi_file_name <- tools::file_path_sans_ext(basename(file_path))
        create_bmi_histogram(filtered_bmi_data, bmi_column, output_folder, bmi_file_name)
        create_bmi_boxplot(filtered_bmi_data, bmi_column, output_folder, bmi_file_name)
      } 
    }
  }
  
  # Bewegungsart und typ
  create_movement_frequency(data, output_folder)
  
  
   
  OE_columns <- grep("OE", colnames(data), value = TRUE)
  
  if (length(OE_columns) > 0) {
    for (OE_column in OE_columns) {
      OE_freq <- data %>%
        dplyr::count(!!sym(OE_column), name = "n") %>%
        as.data.frame()
      
      OE_file_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_oeFREQ.csv")
      write.csv(OE_freq, file.path(output_folder, OE_file_name), row.names = FALSE)
    }
  }
  
 OPS_columns <- grep("OPS", colnames(data), value = TRUE)
  
  if (length(OPS_columns) > 0) {
    for (OPS_column in OPS_columns) {
      OPS_freq <- data %>%
        dplyr::count(!!sym(OPS_column), name = "n") %>%
        as.data.frame()
      
      OPS_file_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_opsFREQ.csv")
      write.csv(OPS_freq, file.path(output_folder, OPS_file_name), row.names = FALSE)
    }
  }
  
# ET freq und OPS kaiptel freq
  if (length(OPS_columns) > 0) {
    for (OPS_column in OPS_columns) {
      if (is.character(data[[OPS_column]])) {
        count_ops_chapters(data, OPS_column, output_folder, file_name)
      }
    }
  }
  
  
  # additional check for "condition" files
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Check for "Hauptdiagnose" columns
  main_diag_columns <- grep("Hauptdiagnose", colnames(data), value = TRUE)
  
  if (length(main_diag_columns) > 0) {
    for (main_diag_column in main_diag_columns) {
      main_diag_freq <- data %>%
        dplyr::count(!!sym(main_diag_column), name = "n") %>%
        as.data.frame()
      
      # Save as CSV
      freq_condition_file_name1 <- paste0(file_name, "_HauptDiagFREQ.csv")
      write.csv(main_diag_freq, file.path(output_folder, freq_condition_file_name1), row.names = FALSE)
    }
  }
  
  # Check for "Nebendiagnose" columns
  side_diag_columns <- grep("Nebendiagnose", colnames(data), value = TRUE)
  
  if (length(side_diag_columns) > 0) {
    for (side_diag_column in side_diag_columns) {
      side_diag_freq <- data %>%
        dplyr::count(!!sym(side_diag_column), name = "n") %>%
        as.data.frame()
      
      # Save as CSV
      freq_condition_file_name2 <- paste0(file_name, "_NebenDiagFREQ.csv")
      write.csv(side_diag_freq, file.path(output_folder, freq_condition_file_name2), row.names = FALSE)
    }
  }
}



# ######################################## #
# IMPORT DATA AND CREATE OUTPUT FOLDER     #
# ######################################## #

# Set input and output paths
input_path <- "path/to/your/folder"
output_folder <- file.path(input_path, "qualitycheck_results")

# Check if output folder exists, if not, create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# List all CSV files in the input folder
csv_files <- list.files(input_path, pattern = ".csv", full.names = TRUE)

# Process each CSV file and save results in the output folder
for (file in csv_files) {
  process_csv_file(file, output_folder)
}

# Clean up the environment
rm(list = ls())
