# Load the librarys
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Read the Excel file into a dataframe
df <- read_excel(file.choose())
head(df)

#Pivot all cycle-related columns into long format
df_long <- df %>%
  pivot_longer(
    cols = matches("(^Cyc_[A-Z]$)|(_Cyc_[A-Z]$)"),
    names_to = "FullName",       # give it a neutral name
    values_to = "Value"
  )

#Extract the cycle (e.g., Cyc_A) and variable type (e.g., E1_MPA)
df_tidy <- df_long %>%
  mutate(
    Cycle = str_extract(FullName, "Cyc_[A-Z]$"),
    VariableType = str_remove(FullName, "_Cyc_[A-Z]$"),
    VariableType = ifelse(VariableType == "Cyc", "Cycle_time", VariableType)
  ) %>%
  select(-FullName) %>%
  pivot_wider(
    names_from = VariableType,
    values_from = Value
  ) %>%
  arrange(SampleCode, Cycle)

head(df_tidy)

#Pivot the actual cycle time columns only
df_lt <- df_tidy %>%
  pivot_longer(
    cols = matches("^Cyc_[A-Z]$"),  # Cyc_A to Cyc_N
    names_to = "Cycle_column",      # Temporary name to avoid collision
    values_to = "Time"
  )

#Remove NA times
df_clean <- df_lt %>%
  filter(!is.na(Time)) %>%
  select(-Cycle) %>%                #Drop the existing "Cycle" column and rename
  rename(Cycle = Cycle_column)  

df_clean <- df_clean |>
  mutate(
    ChemIndex = as.numeric(str_split_fixed(SampleCode, "_", 4)[,2]),
    Porosity = as.numeric(str_split_fixed(SampleCode, "_", 4)[,3]))

df_clean <- df_clean %>%
  group_by(SampleCode) %>%
  mutate(
    Estatic_initial = first(Estatic_MPA),  # First Estatic value per SampleCode
    Estatic_rel = Estatic_MPA / Estatic_initial  # Relative to initial
  ) %>%
  ungroup()

df_clean_noresponse <- subset(df_clean, select = (-c(E1_MPA, tandelta, NVP)))

df_clean_noresponse <-
                                                  