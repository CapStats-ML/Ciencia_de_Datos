## ESTE ESCRIP DE R SIRVE DE APOYO PARA EL DESARROLLO DEL PROYECTO DE LA ASIGNATURA DE 
## CIENCIA DE DATOS, CON EL OBJETIVO DE LUEGRO CREAR UN CODIGO MÁS ORGANIZADO EN EL 
## CUADERNO DE JUPYTER 

## LIBRERIAS NECESARIAS ----
library(readr)
library(dplyr)
library(tidyr)


## ENTREGA 1 ----

## CARGA DE DATOS

test <- read_csv("test.csv")
train <- read_csv("train.csv")

## Unir ambos datasets para hacer limpieza de datos

data <- rbind(test, train)
#write.csv(data, "data.csv")

## LIMPIEZA DE DATOS

names <- names(data)

#Unificar variables de raza

data <- data %>%
  mutate(Raza = case_when(
    `race:AfricanAmerican` == 1 ~ "AfricanAmerican",
    `race:Asian` == 1 ~ "Asian",
    `race:Caucasian` == 1 ~ "Caucasian",
    `race:Hispanic` == 1 ~ "Hispanic",
    `race:Other` == 1 ~ "Other",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`race:AfricanAmerican`, -`race:Asian`, -`race:Caucasian`,
         -`race:Hispanic`, -`race:Other`)

table(data$Raza)

#Unificar variables de género

data <- data %>%
  mutate(Genero = case_when(
    `gender:Female` == 1 ~ "Mujer",
    `gender:Male` == 1 ~ "Hombre",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`gender:Female`, -`gender:Male`)

table(data$Genero)

# Uniformizar variables de edad

data <- data %>%
  mutate(Edad = case_when(
    `age:70+` == 1 ~ "70+",
    `age:[0-10)` == 1 ~ "[0-10)",
    `age:[10-20)` == 1 ~ "[10-20)",
    `age:[20-50)` == 1 ~ "[20-50)",
    `age:[50-70)` == 1 ~ "[50-70)",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`age:70+`, -`age:[0-10)`, -`age:[10-20)`, -`age:[20-50)`, -`age:[50-70)`)

table(data$Edad)

# Uniformizar variables de tipo de admisión

data <- data %>%
  mutate(TipoAdmision = case_when(
    `admission_type_id:Elective` == 1 ~ "Electiva",
    `admission_type_id:Emergency` == 1 ~ "Emergencia",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`admission_type_id:Elective`, -`admission_type_id:Emergency`)

table(data$TipoAdmision)

# Uniformizar variables de destino de alta

data <- data %>%
  mutate(DestinoAlta = case_when(
    `discharge_disposition_id:Discharged to Home` == 1 ~ "Dado de alta a casa",
    `discharge_disposition_id:Other` == 1 ~ "Otro",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`discharge_disposition_id:Discharged to Home`, -`discharge_disposition_id:Other`)

table(data$DestinoAlta)


# Uniformizar variables de fuente de admisión

data <- data %>%
  mutate(FuenteAdmision = case_when(
    `admission_source_id:Emergency` == 1 ~ "Emergencia",
    `admission_source_id:Other` == 1 ~ "Otro",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`admission_source_id:Emergency`, -`admission_source_id:Other`)

table(data$FuenteAdmision)

# Uniformizar variables de especialidad médica

data <- data %>%
  mutate(Especialidad = case_when(
    `medical_specialty:Cardiology` == 1 ~ "Cardiología",
    `medical_specialty:Emergency/Trauma` == 1 ~ "Emergencia/Trauma",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`medical_specialty:Cardiology`, -`medical_specialty:Emergency/Trauma`)

table(data$Especialidad)

# Uniformizar variables de diagnóstico

data <- data %>%
  mutate(Diagnostico = case_when(
    `diag_1:Circulatory` == 1 ~ "Circulatorio",
    `diag_1:Diabetes` == 1 ~ "Diabetes",
    `diag_1:Digestive` == 1 ~ "Digestivo",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`diag_1:Circulatory`, -`diag_1:Diabetes`, -`diag_1:Digestive`)

table(data$Diagnostico)

# Uniformizar variables de resultado de A1C

data <- data %>%
  mutate(ResultadoA1C = case_when(
    `A1Cresult:>7` == 1 ~ ">7",
    `A1Cresult:>8` == 1 ~ ">8",
    `A1Cresult:None` == 1 ~ "Ninguno",
    `A1Cresult:Norm` == 1 ~ "Normal",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`A1Cresult:>7`, -`A1Cresult:>8`, -`A1Cresult:None`, -`A1Cresult:Norm`)

table(data$ResultadoA1C)

# Uniformizar variables de resultado de max_glu_serum

data <- data %>%
  mutate(ResultadoGlu = case_when(
    `max_glu_serum:>200` == 1 ~ ">200",
    `max_glu_serum:>300` == 1 ~ ">300",
    `max_glu_serum:None` == 1 ~ "Ninguno",
    `max_glu_serum:Norm` == 1 ~ "Normal",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`max_glu_serum:>200`, -`max_glu_serum:>300`, -`max_glu_serum:None`,
         -`max_glu_serum:Norm`)

table(data$ResultadoGlu)

# Unificar variables de insulina

data <- data %>%
  mutate(Insulina = case_when(
    `insulin:Down` == 1 ~ "Bajada",
    `insulin:No` == 1 ~ "No",
    `insulin:Steady` == 1 ~ "Estable",
    `insulin:Up` == 1 ~ "Subida",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`insulin:Down`, -`insulin:No`, -`insulin:Steady`, -`insulin:Up`)

table(data$Insulina)

# Unificar variable de gliburida

data <- data %>%
  mutate(Gliburida = case_when(
    `glyburide:Down` == 1 ~ "Bajada",
    `glyburide:No` == 1 ~ "No",
    `glyburide:Steady` == 1 ~ "Estable",
    `glyburide:Up` == 1 ~ "Subida",
    TRUE ~ NA_character_  # Valor NA en caso de que ninguna de las indicadoras sea 1
  ))

data <- data %>%
  select(-`glyburide:Down`, -`glyburide:No`, -`glyburide:Steady`, -`glyburide:Up`)

table(data$Gliburida)

# SELECCION DE VARIABLES CATEGORICAS ----

categóricas <- c('Raza', 'Genero', 'Edad', 'TipoAdmision', 'DestinoAlta',
                 'ResultadoA1C', 'ResultadoGlu',"Gliburida" , "Insulina", "readmitted")

# Seleccionar solo las variables categóricas
data_categóricas <- data[, categóricas]

# SELECCION DE VARIABLES NUMERICAS ----

numéricas <- c(
  "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications",
  "number_outpatient", "number_emergency", "number_inpatient",
  "number_diagnoses", "change", "diabetesMed"
)

# Seleccionar solo las variables numéricas
data_numéricas <- data[, numéricas]

# Unir ambos datasets

data1 <- cbind(data_categóricas, data_numéricas)

# CONTAR VALORES NULOS ----

sapply(data1, function(x) sum(is.na(x)))

# ELIMINAR FILAS CON VALORES NULOS ----

data1 <- data1 %>%
  drop_na()

# GUARDAR DATOS LIMPIOS ----

write.csv(data1, "data1.csv")



