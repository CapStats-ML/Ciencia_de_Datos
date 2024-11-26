## ESTE ESCRIP DE R SIRVE DE APOYO PARA EL DESARROLLO DEL PROYECTO DE LA ASIGNATURA DE 
## CIENCIA DE DATOS, CON EL OBJETIVO DE LUEGRO CREAR UN CODIGO MÁS ORGANIZADO EN EL 
## CUADERNO DE JUPYTER 

## LIBRERIAS NECESARIAS ----
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
library(kableExtra)

## ENTREGA 1 ----

## CARGA DE DATOS
setwd("~/REPOS GIT/Ciencia_de_Datos/Datos")
test <- read_csv("test.csv")
train <- read_csv("train.csv")

## Unir ambos datasets para hacer limpieza de datos

data <- rbind(test, train)
#write.csv(data, "data.csv")

# LIMPIEZA DE DATOS ---- 

# VARIABLES CATEGORICAS ----

# Para diabetesMed ----

Prop_diabetesMed <- sapply(data["diabetesMed"], mean)

T_Proportion_diabetesMed <- data.frame(
  Variable = "diabetesMed",
  Proporcion = as.vector(Prop_diabetesMed)
)

T_Proportion_diabetesMed <- T_Proportion_diabetesMed[order(T_Proportion_diabetesMed$Proporcion, decreasing = TRUE), ]

kable(
  T_Proportion_diabetesMed, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en DiabetesMed"
)

T_Proportion_diabetesMed %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE PACIENTES QUE RECIBEN MEDICAMENTOS PARA LA DIABETES", 
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

# Para change ----

Prop_change <- sapply(data["change"], mean)

T_Prob_change <- data.frame(
  Variable = "change",
  Proporcion = as.vector(Prop_change)
)

T_Prop_change <- T_Prob_change[order(T_Prob_change$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_change, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Change"
)

T_Prop_change %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN CHANGE", 
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))


# Para Raza ----

Raza <- c("race:AfricanAmerican", "race:Asian", "race:Caucasian", "race:Hispanic", "race:Other")

Prop_Raza <- sapply(data[Raza], mean)

T_Prop_Raza <- data.frame(
  Variable = Raza,
  Proporcion = as.vector(Prop_Raza)
)

T_Prop_Raza <- T_Prop_Raza[order(T_Prop_Raza$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_Raza, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Raza"
)

T_Prop_Raza %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN RAZA", 
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Raza = case_when(
    `race:AfricanAmerican` == 1 ~ "Afroamericano",
    `race:Asian` == 1 ~ "Asiático",
    `race:Caucasian` == 1 ~ "Caucásico",
    `race:Hispanic` == 1 ~ "Hispano",
    `race:Other` == 1 ~ "Otro",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`race:AfricanAmerican`, -`race:Asian`, -`race:Caucasian`, -`race:Hispanic`, -`race:Other`)

# Para Genero ----

Genero <- c("gender:Female", "gender:Male")

Prop_Genero <- sapply(data[Genero], mean)

T_Prop_Genero <- data.frame(
  Variable = Genero,
  Proporcion = as.vector(Prop_Genero)
)

T_Porp_Genero <- T_Prop_Genero[order(T_Prop_Genero$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_Genero, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Genero"
)


T_Prop_Genero %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = c("steelblue", "red")) +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN GENERO", 
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Genero = case_when(
    `gender:Female` == 1 ~ "Femenino",  # Agregamos la coma aquí
    `gender:Male` == 1 ~ "Masculino",
    TRUE ~ "Desconocido"  # Agregar un caso por si no cumple con ninguno de los anteriores
  ))

data <- data %>%
  select(-`gender:Female`, -`gender:Male`)

# Para Edad ----

Edad <- c("age:[0-10)", "age:[10-20)", "age:[20-50)", "age:[50-70)", "age:70+")

Prop_Edad <- sapply(data[Edad], mean)

T_Prop_Edad <- data.frame(
  Variable = Edad,
  Proporcion = as.vector(Prop_Edad)
)

T_Prop_Edad <- T_Prop_Edad[order(T_Prop_Edad$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_Edad, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Edad"
)

T_Prop_Edad %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN EDAD", 
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Edad = case_when(
    `age:[0-10)` == 1 ~ "0-10",
    `age:[10-20)` == 1 ~ "10-20",
    `age:[20-50)` == 1 ~ "20-50",
    `age:[50-70)` == 1 ~ "50-70",
    `age:70+` == 1 ~ "70+",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`age:[0-10)`, -`age:[10-20)`, -`age:[20-50)`, -`age:[50-70)`, -`age:70+`)

# Para Tipo de Admision ----

Admision <- c("admission_type_id:Elective", "admission_type_id:Emergency",
              "admission_type_id:New Born", "admission_type_id:Trauma Center")

Prop_Admision <- sapply(data[Admision], mean)

T_Prop_Admision <- data.frame(
  Variable = Admision,
  Proporcion = as.vector(Prop_Admision)
)

T_Prop_Admision <- T_Prop_Admision[order(T_Prop_Admision$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_Admision, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Tipo de Admision"
)

T_Prop_Admision %>% 
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN TIPO DE ADMISION",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Admision = case_when(
    `admission_type_id:Elective` == 1 ~ "Electiva",
    `admission_type_id:Emergency` == 1 ~ "Emergencia",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`admission_type_id:Elective`, -`admission_type_id:Emergency`, -`admission_type_id:New Born`,
         -`admission_type_id:Trauma Center`)

# Para Discharge ----

Discharge <- c("discharge_disposition_id:Discharged to Home",
               "discharge_disposition_id:Other")

Prop_Discharge <- sapply(data[Discharge], mean)


T_Prop_Discharge <- data.frame(
  Variable = Discharge,
  Proporcion = as.vector(Prop_Discharge)
)

T_Prop_Discharge <- T_Prop_Discharge[order(T_Prop_Discharge$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_Discharge, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Discharge"
)

T_Prop_Discharge %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN DISCHARGE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Discharge = case_when(
    `discharge_disposition_id:Discharged to Home` == 1 ~ "Dado de alta a casa",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`discharge_disposition_id:Discharged to Home`, -`discharge_disposition_id:Other`)

# Eliminar variables Admission_source_id

data <- data %>%
  select(-`admission_source_id:Emergency`, - `admission_source_id:Other`,
         - `admission_source_id:Referral`, - `admission_source_id:Transfer`,)

# Para Especialidad Medica

Especialidad <- c("medical_specialty:Cardiology", "medical_specialty:Emergency/Trauma",
                  "medical_specialty:Family/GeneralPractice", "medical_specialty:Gastroenterology",
                  "medical_specialty:Hematology/Oncology", "medical_specialty:InternalMedicine",
                  "medical_specialty:Nephrology", "medical_specialty:ObstetricsandGynecology",
                  "medical_specialty:Orthopedics", 
                  "medical_specialty:Other", "medical_specialty:Psychiatry", 
                  "medical_specialty:Pulmonology", "medical_specialty:Radiology",
                  "medical_specialty:Surgery-Cardiovascular/Thoracic", "medical_specialty:Surgery-General",
                  "medical_specialty:Urology")

Prop_Especialidad <- sapply(data[Especialidad], mean)

T_Prop_Especialidad <- data.frame(
  Variable = Especialidad,
  Proporcion = as.vector(Prop_Especialidad)
)

T_Prop_Especialidad <- T_Prop_Especialidad[order(T_Prop_Especialidad$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_Especialidad, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Especialidad"
)

T_Prop_Especialidad %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 0.7) + # Línea horizontal
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN ESPECIALIDAD",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))


# Uniformizar variables de especialidad para las que tengan prop > 0.05

data <- data %>%
  mutate(Especialidad = case_when(
    `medical_specialty:Cardiology` == 1 ~ "Cardiología",
    `medical_specialty:Emergency/Trauma` == 1 ~ "Emergencia/Trauma",
    `medical_specialty:Family/GeneralPractice` == 1 ~ "Familia/Medicina General",
    `medical_specialty:Hematology/Oncology` == 1 ~ "Hematología/Oncología",
    `medical_specialty:InternalMedicine` == 1 ~ "Medicina Interna",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`medical_specialty:Cardiology`, -`medical_specialty:Emergency/Trauma`, 
         -`medical_specialty:Family/GeneralPractice`, -`medical_specialty:Hematology/Oncology`,
         -`medical_specialty:InternalMedicine`, -`medical_specialty:Nephrology`, 
         -`medical_specialty:ObstetricsandGynecology`, -`medical_specialty:Orthopedics`,
         -`medical_specialty:Other`, -`medical_specialty:Psychiatry`, 
         -`medical_specialty:Pulmonology`, -`medical_specialty:Radiology`,
         -`medical_specialty:Surgery-Cardiovascular/Thoracic`, -`medical_specialty:Surgery-General`,
         -`medical_specialty:Urology`, -`medical_specialty:Gastroenterology`)

# Para Diag 1 ----

# Explorar las proporciones de la variables diag_2

#Nombres en Diag_2
diag1 <- c("diag_1:Circulatory", "diag_1:Diabetes", "diag_1:Digestive", "diag_1:Genitourinary",
           "diag_1:Infectious", "diag_1:Injury", "diag_1:Mental", "diag_1:Musculoskeletal",
           "diag_1:Neoplasms", "diag_1:Non-diabetes endocrine/metabolic", "diag_1:Other",
           "diag_1:Respiratory", "diag_1:Skin")

# Encontrar las proporciones para cada uno de las variables en la base data que hacen referencia 
# a la variable diag_2

# Calcular las proporciones para las variables en diag2
Porp_diag1 <- sapply(data[diag1], mean)

# Crear un data frame con los resultados
T_Prop_diag1 <- data.frame(
  Variable = diag1,        # Usar directamente los nombres en diag2
  Proporcion = as.vector(Porp_diag1)  # Convertir a vector para asegurar el formato correcto
)

# Ordenar la tabla por proporción (opcional)
T_Prop_diag1 <- T_Prop_diag1[order(T_Prop_diag1$Proporcion, decreasing = TRUE), ]

# Generar la tabla con kable
kable(
  T_Prop_diag1, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Diag_2"
)

#graifco de barras ordenado 

T_Prop_diag1 %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", size = 0.7) + # Línea horizontal
  coord_flip() +
  ggtitle(label = "PROPORCION DELAS VARIABLES EN DIAG_1",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))


# Uniformizar variables de diag2 para las que tengan prop > 0.1
# Las variables con prop < 0.1 son agrupadas en la categoría "Other"
# las cuales son Injury, Skin, Neoplasms, Digestive, Genitourinary, Non-diabetes endocrine/metabolic,
# Mental, Musculoskeletal e Infectious

# Crear una nueva variable Diagnostico2

data <- data %>%
  mutate(Diagnostico1 = case_when(
    `diag_1:Circulatory` == 1 ~ "Circulatorio",
    `diag_1:Respiratory` == 1 ~ "Respiratorio",
    `diag_1:Diabetes` == 1 ~ "Diabetes",
    `diag_1:Digestive` == 1 ~ "Digestive",
    `diag_1:Other` == 1 ~ "Otro",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`diag_1:Circulatory`, -`diag_1:Diabetes`, -`diag_1:Digestive`, -`diag_1:Genitourinary`,
         -`diag_1:Infectious`, -`diag_1:Injury`, -`diag_1:Mental`, -`diag_1:Musculoskeletal`,
         -`diag_1:Neoplasms`, -`diag_1:Non-diabetes endocrine/metabolic`, -`diag_1:Other`,
         -`diag_1:Respiratory`, -`diag_1:Skin`)

# Para Diag 2 ----

#Nombres en Diag_2
diag2 <- c("diag_2:Circulatory", "diag_2:Diabetes", "diag_2:Digestive", "diag_2:Genitourinary",
           "diag_2:Infectious", "diag_2:Injury", "diag_2:Mental", "diag_2:Musculoskeletal",
           "diag_2:Neoplasms", "diag_2:Non-diabetes endocrine/metabolic", "diag_2:Other",
           "diag_2:Respiratory", "diag_2:Skin")

# Encontrar las proporciones para cada uno de las variables en la base data que hacen referencia 
# a la variable diag_2

# Calcular las proporciones para las variables en diag2
Porp_diag2 <- sapply(data[diag2], mean)

# Crear un data frame con los resultados
T_Prop_diag2 <- data.frame(
  Variable = diag2,        # Usar directamente los nombres en diag2
  Proporcion = as.vector(Porp_diag2)  # Convertir a vector para asegurar el formato correcto
)

# Ordenar la tabla por proporción (opcional)
T_Prop_diag2 <- T_Prop_diag2[order(T_Prop_diag2$Proporcion, decreasing = TRUE), ]

# Generar la tabla con kable
kable(
  T_Prop_diag2, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Diag_2"
)

#graifco de barras ordenado 

T_Prop_diag2 %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", size = 0.7) + # Línea horizontal
  coord_flip() +
  ggtitle(label = "PROPORCION DELAS VARIABLES EN DIAG_2",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))


# Uniformizar variables de diag2 para las que tengan prop > 0.1
# Las variables con prop < 0.1 son agrupadas en la categoría "Other"
# las cuales son  Genitourinary, Non-diabetes endocrine/metabolic, Digestive, Skin,
# Neoplasms, Mental, Injury, infectious y Mosculoskeletal

# Crear una nueva variable Diagnostico2

data <- data %>%
  mutate(Diagnostico2 = case_when(
    `diag_2:Circulatory` == 1 ~ "Circulatorio",
    `diag_2:Diabetes` == 1 ~ "Diabetes",
    `diag_2:Respiratory` == 1 ~ "Respiratorio",
    `diag_2:Other` == 1 ~ "Otro",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`diag_2:Circulatory`, -`diag_2:Diabetes`, -`diag_2:Digestive`, -`diag_2:Genitourinary`,
         -`diag_2:Infectious`, -`diag_2:Injury`, -`diag_2:Mental`, -`diag_2:Musculoskeletal`,
         -`diag_2:Neoplasms`, -`diag_2:Non-diabetes endocrine/metabolic`, -`diag_2:Other`,
         -`diag_2:Respiratory`, -`diag_2:Skin`)

# Para Diag 3 ----

#Nombres en Diag_3

diag3 <- c("diag_3:Circulatory", "diag_3:Diabetes", "diag_3:Digestive", "diag_3:Genitourinary",
           "diag_3:Infectious", "diag_3:Injury", "diag_3:Mental", "diag_3:Musculoskeletal",
           "diag_3:Neoplasms", "diag_3:Non-diabetes endocrine/metabolic", "diag_3:Other",
           "diag_3:Respiratory", "diag_3:Skin")

# Encontrar las proporciones para cada uno de las variables en la base data que hacen referencia

# Calcular las proporciones para las variables en diag3

Porp_diag3 <- sapply(data[diag3], mean)

# Crear un data frame con los resultados

T_Prop_diag3 <- data.frame(
  Variable = diag3,        # Usar directamente los nombres en diag3
  Proporcion = as.vector(Porp_diag3)  # Convertir a vector para asegurar el formato correcto
)

# Ordenar la tabla por proporción (opcional)

T_Prop_diag3 <- T_Prop_diag3[order(T_Prop_diag3$Proporcion, decreasing = TRUE), ]

# Generar la tabla con kable

kable(
  T_Prop_diag3, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Diag_3"
)

#graifco de barras ordenado 

T_Prop_diag3 %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", size = 0.7) + # Línea horizontal
  coord_flip() +
  ggtitle(label = "PROPORCION DELAS VARIABLES EN DIAG_3",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

# Crear una nueva variable Diagnostico3

data <- data %>%
  mutate(Diagnostico3 = case_when(
    `diag_3:Circulatory` == 1 ~ "Circulatorio",
    `diag_3:Diabetes` == 1 ~ "Diabetes",
    `diag_3:Other` == 1 ~ "Otro",
    TRUE ~ "Otro"
  ))

data <- data %>%
  select(-`diag_3:Circulatory`, -`diag_3:Diabetes`, -`diag_3:Digestive`, -`diag_3:Genitourinary`,
         -`diag_3:Infectious`, -`diag_3:Injury`, -`diag_3:Mental`, -`diag_3:Musculoskeletal`,
         -`diag_3:Neoplasms`, -`diag_3:Non-diabetes endocrine/metabolic`, -`diag_3:Other`,
         -`diag_3:Respiratory`, -`diag_3:Skin`)

# Para metformin ----

metmorfin <- c("metformin:Down", "metformin:No", "metformin:Steady", "metformin:Up")

Prop_metmorfin <- sapply(data[metmorfin], mean)

T_Prop_metmorfin <- data.frame(
  Variable = metmorfin,
  Proporcion = as.vector(Prop_metmorfin)
)

T_Prop_metmorfin <- T_Prop_metmorfin[order(T_Prop_metmorfin$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_metmorfin, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Metformin"
)

T_Prop_metmorfin %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN METFORMIN",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") + 
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Metformin = case_when(
    `metformin:Down` == 1 ~ "Disminuido",
    `metformin:No` == 1 ~ "No",
    `metformin:Steady` == 1 ~ "Estable",
    `metformin:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`metformin:Down`, -`metformin:No`, -`metformin:Steady`, -`metformin:Up`)

# Para repaglinide ----

repaglinide <- c("repaglinide:Down", "repaglinide:No", "repaglinide:Steady", "repaglinide:Up")

Prop_repaglinide <- sapply(data[repaglinide], mean)

T_Prop_repaglinide <- data.frame(
  Variable = repaglinide,
  Proporcion = as.vector(Prop_repaglinide)
)

T_Prop_repaglinide <- T_Prop_repaglinide[order(T_Prop_repaglinide$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_repaglinide, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Repaglinide"
)

T_Prop_repaglinide %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN REPAGLINIDE",
          subtitle= "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Repaglinide = case_when(
    `repaglinide:Down` == 1 ~ "Disminuido",
    `repaglinide:No` == 1 ~ "No",
    `repaglinide:Steady` == 1 ~ "Estable",
    `repaglinide:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`repaglinide:Down`, -`repaglinide:No`, -`repaglinide:Steady`, -`repaglinide:Up`)

# Para nateglinide ----

nateglinide <- c("nateglinide:Down", "nateglinide:No", "nateglinide:Steady", "nateglinide:Up")

Prop_nateglinide <- sapply(data[nateglinide], mean)

T_Prop_nateglinide <- data.frame(
  Variable = nateglinide,
  Proporcion = as.vector(Prop_nateglinide)
)

T_Prop_nateglinide <- T_Prop_nateglinide[order(T_Prop_nateglinide$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_nateglinide, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Nateglinide"
)

T_Prop_nateglinide %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN NATEGLINIDE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Nateglinide = case_when(
    `nateglinide:Down` == 1 ~ "Disminuido",
    `nateglinide:No` == 1 ~ "No",
    `nateglinide:Steady` == 1 ~ "Estable",
    `nateglinide:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`nateglinide:Down`, -`nateglinide:No`, -`nateglinide:Steady`, -`nateglinide:Up`)

# Para chlorpropamide ----

chlorpropamide <- c("chlorpropamide:Down", "chlorpropamide:No", "chlorpropamide:Steady", "chlorpropamide:Up")

Prop_chlorpropamide <- sapply(data[chlorpropamide], mean)

T_Prop_chlorpropamide <- data.frame(
  Variable = chlorpropamide,
  Proporcion = as.vector(Prop_chlorpropamide)
)

T_Prop_chlorpropamide <- T_Prop_chlorpropamide[order(T_Prop_chlorpropamide$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_chlorpropamide, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Chlorpropamide"
)

T_Prop_chlorpropamide %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN CHLORPROPAMIDE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Chlorpropamide = case_when(
    `chlorpropamide:Down` == 1 ~ "Disminuido",
    `chlorpropamide:No` == 1 ~ "No",
    `chlorpropamide:Steady` == 1 ~ "Estable",
    `chlorpropamide:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`chlorpropamide:Down`, -`chlorpropamide:No`, -`chlorpropamide:Steady`, -`chlorpropamide:Up`)

# Para glizide ----

glipizide <- c("glipizide:Down", "glipizide:No", "glipizide:Steady", "glipizide:Up")

Prop_glipizide <- sapply(data[glipizide], mean)

T_Prop_glipizide <- data.frame(
  Variable = glipizide,
  Proporcion = as.vector(Prop_glipizide)
)

T_Prop_glipizide <- T_Prop_glipizide[order(T_Prop_glipizide$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_glipizide, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Glimepiride"
)

T_Prop_glipizide %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN GLIPIZIDE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Glipizide = case_when(
    `glipizide:Down` == 1 ~ "Disminuido",
    `glipizide:No` == 1 ~ "No",
    `glipizide:Steady` == 1 ~ "Estable",
    `glipizide:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`glipizide:Down`, -`glipizide:No`, -`glipizide:Steady`, -`glipizide:Up`)

# Para glyburide ----

glyburide <- c("glyburide:Down", "glyburide:No", "glyburide:Steady", "glyburide:Up")

Prop_glyburide <- sapply(data[glyburide], mean)

T_Prop_glyburide <- data.frame(
  Variable = glyburide,
  Proporcion = as.vector(Prop_glyburide)
)

T_Prop_glyburide <- T_Prop_glyburide[order(T_Prop_glyburide$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_glyburide, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Gliplizide"
)

T_Prop_glyburide %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN GLYBURIDE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Glyburide = case_when(
    `glyburide:Down` == 1 ~ "Disminuido",
    `glyburide:No` == 1 ~ "No",
    `glyburide:Steady` == 1 ~ "Estable",
    `glyburide:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`glyburide:Down`, -`glyburide:No`, -`glyburide:Steady`, -`glyburide:Up`)

# Para Glimepiride ----

glimepiride <- c("glimepiride:Down", "glimepiride:No", "glimepiride:Steady", "glimepiride:Up")

Prop_glimepiride <- sapply(data[glimepiride], mean)

T_Prop_glimepiride <- data.frame(
  Variable = glimepiride,
  Proporcion = as.vector(Prop_glimepiride)
)

T_Prop_glimepiride <- T_Prop_glimepiride[order(T_Prop_glimepiride$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_glimepiride, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Glimepiride"
)

T_Prop_glimepiride %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN GLIMEPIRIDE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Glimepiride = case_when(
    `glimepiride:Down` == 1 ~ "Disminuido",
    `glimepiride:No` == 1 ~ "No",
    `glimepiride:Steady` == 1 ~ "Estable",
    `glimepiride:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`glimepiride:Down`, -`glimepiride:No`, -`glimepiride:Steady`, -`glimepiride:Up`)

# Para pioglitazone ----

pioglitazone <- c("pioglitazone:Down", "pioglitazone:No", "pioglitazone:Steady", "pioglitazone:Up")

Prop_pioglitazone <- sapply(data[pioglitazone], mean)

T_Prop_pioglitazone <- data.frame(
  Variable = pioglitazone,
  Proporcion = as.vector(Prop_pioglitazone)
)

T_Prop_pioglitazone <- T_Prop_pioglitazone[order(T_Prop_pioglitazone$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_pioglitazone, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Pioglitazone"
)

T_Prop_pioglitazone %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN PIOGLITAZONE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Pioglitazone = case_when(
    `pioglitazone:Down` == 1 ~ "Disminuido",
    `pioglitazone:No` == 1 ~ "No",
    `pioglitazone:Steady` == 1 ~ "Estable",
    `pioglitazone:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`pioglitazone:Down`, -`pioglitazone:No`, -`pioglitazone:Steady`, -`pioglitazone:Up`)

# Para rosiglitazone ----

rosiglitazone <- c("rosiglitazone:Down", "rosiglitazone:No", "rosiglitazone:Steady", "rosiglitazone:Up")

Prop_rosiglitazone <- sapply(data[rosiglitazone], mean)

T_Prop_rosiglitazone <- data.frame(
  Variable = rosiglitazone,
  Proporcion = as.vector(Prop_rosiglitazone)
)

T_Prop_rosiglitazone <- T_Prop_rosiglitazone[order(T_Prop_rosiglitazone$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_rosiglitazone, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Rosiglitazone"
)

T_Prop_rosiglitazone %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN ROSIGLITAZONE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Rosiglitazone = case_when(
    `rosiglitazone:Down` == 1 ~ "Disminuido",
    `rosiglitazone:No` == 1 ~ "No",
    `rosiglitazone:Steady` == 1 ~ "Estable",
    `rosiglitazone:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`rosiglitazone:Down`, -`rosiglitazone:No`, -`rosiglitazone:Steady`, -`rosiglitazone:Up`)

# Para acarbose ----

acarbose <- c("acarbose:Down", "acarbose:No", "acarbose:Steady", "acarbose:Up")

Prop_acarbose <- sapply(data[acarbose], mean)

T_Prop_acarbose <- data.frame(
  Variable = acarbose,
  Proporcion = as.vector(Prop_acarbose)
)

T_Prop_acarbose <- T_Prop_acarbose[order(T_Prop_acarbose$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_acarbose, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Acarbose"
)

T_Prop_acarbose %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN ACARBOSE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Acarbose = case_when(
    `acarbose:Down` == 1 ~ "Disminuido",
    `acarbose:No` == 1 ~ "No",
    `acarbose:Steady` == 1 ~ "Estable",
    `acarbose:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`acarbose:Down`, -`acarbose:No`, -`acarbose:Steady`, -`acarbose:Up`)

# Para miglitol ----

miglitol <- c("miglitol:Down", "miglitol:No", "miglitol:Steady", "miglitol:Up")

Prop_miglitol <- sapply(data[miglitol], mean)

T_Prop_miglitol <- data.frame(
  Variable = miglitol,
  Proporcion = as.vector(Prop_miglitol)
)

T_Prop_miglitol <- T_Prop_miglitol[order(T_Prop_miglitol$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_miglitol, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Miglitol"
)

T_Prop_miglitol %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN MIGLITOL",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Miglitol = case_when(
    `miglitol:Down` == 1 ~ "Disminuido",
    `miglitol:No` == 1 ~ "No",
    `miglitol:Steady` == 1 ~ "Estable",
    `miglitol:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`miglitol:Down`, -`miglitol:No`, -`miglitol:Steady`, -`miglitol:Up`)

# Para tolazamide ----

tolazamide <- c("tolazamide:No", "tolazamide:Steady", "tolazamide:Up")

Prop_tolazamide <- sapply(data[tolazamide], mean)

T_Prop_tolazamide <- data.frame(
  Variable = tolazamide,
  Proporcion = as.vector(Prop_tolazamide)
)

T_Prop_tolazamide <- T_Prop_tolazamide[order(T_Prop_tolazamide$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_tolazamide, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Tolazamide"
)

T_Prop_tolazamide %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN TOLAZAMIDE",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Tolazamide = case_when(
    `tolazamide:No` == 1 ~ "No",
    `tolazamide:Steady` == 1 ~ "Estable",
    `tolazamide:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select( -`tolazamide:No`, -`tolazamide:Steady`, -`tolazamide:Up`)

# Para insulin ----

insulin <- c("insulin:Down", "insulin:No", "insulin:Steady", "insulin:Up")

Prop_insulin <- sapply(data[insulin], mean)

T_Prop_insulin <- data.frame(
  Variable = insulin,
  Proporcion = as.vector(Prop_insulin)
)

T_Prop_insulin <- T_Prop_insulin[order(T_Prop_insulin$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_insulin, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Insulin"
)

T_Prop_insulin %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN INSULIN",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Insulin = case_when(
    `insulin:Down` == 1 ~ "Disminuido",
    `insulin:No` == 1 ~ "No",
    `insulin:Steady` == 1 ~ "Estable",
    `insulin:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`insulin:Down`, -`insulin:No`, -`insulin:Steady`, -`insulin:Up`)

# Para glyburide-metformin ----

glyburide_metformin <- c("glyburide-metformin:Down", "glyburide-metformin:No", "glyburide-metformin:Steady", "glyburide-metformin:Up")

Prop_glyburide_metformin <- sapply(data[glyburide_metformin], mean)

T_Prop_glyburide_metformin <- data.frame(
  Variable = glyburide_metformin,
  Proporcion = as.vector(Prop_glyburide_metformin)
)

T_Prop_glyburide_metformin <- T_Prop_glyburide_metformin[order(T_Prop_glyburide_metformin$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_glyburide_metformin, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Glyburide-Metformin"
)

T_Prop_glyburide_metformin %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN GLYBURIDE-METFORMIN",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Glyburide_Metformin = case_when(
    `glyburide-metformin:Down` == 1 ~ "Disminuido",
    `glyburide-metformin:No` == 1 ~ "No",
    `glyburide-metformin:Steady` == 1 ~ "Estable",
    `glyburide-metformin:Up` == 1 ~ "Aumentado",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`glyburide-metformin:Down`, -`glyburide-metformin:No`, -`glyburide-metformin:Steady`, -`glyburide-metformin:Up`)

# Para A1Cresult ----

A1Cresult <- c("A1Cresult:Norm", "A1Cresult:None", "A1Cresult:>7", "A1Cresult:>8")

Prop_A1Cresult <- sapply(data[A1Cresult], mean)

T_Prop_A1Cresult <- data.frame(
  Variable = A1Cresult,
  Proporcion = as.vector(Prop_A1Cresult)
)

T_Prop_A1Cresult <- T_Prop_A1Cresult[order(T_Prop_A1Cresult$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_A1Cresult, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en A1Cresult"
)

T_Prop_A1Cresult %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN A1CRESULT",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(A1Cresult = case_when(
    `A1Cresult:Norm` == 1 ~ "Normal",
    `A1Cresult:None` == 1 ~ "No",
    `A1Cresult:>7` == 1 ~ "Mayor a 7",
    `A1Cresult:>8` == 1 ~ "Mayor a 8",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`A1Cresult:Norm`, -`A1Cresult:None`, -`A1Cresult:>7`, -`A1Cresult:>8`)

# Para max_glu_serum ----

max_glu_serum <- c("max_glu_serum:Norm", "max_glu_serum:None", "max_glu_serum:>200", "max_glu_serum:>300")

Prop_max_glu_serum <- sapply(data[max_glu_serum], mean)

T_Prop_max_glu_serum <- data.frame(
  Variable = max_glu_serum,
  Proporcion = as.vector(Prop_max_glu_serum)
)

T_Prop_max_glu_serum <- T_Prop_max_glu_serum[order(T_Prop_max_glu_serum$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_max_glu_serum, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Max Glu Serum"
)

T_Prop_max_glu_serum %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN MAX GLU SERUM",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))

data <- data %>%
  mutate(Max_Glu_Serum = case_when(
    `max_glu_serum:Norm` == 1 ~ "Normal",
    `max_glu_serum:None` == 1 ~ "No",
    `max_glu_serum:>200` == 1 ~ "Mayor a 200",
    `max_glu_serum:>300` == 1 ~ "Mayor a 300",
    TRUE ~ "Desconocido"
  ))

data <- data %>%
  select(-`max_glu_serum:Norm`, -`max_glu_serum:None`, -`max_glu_serum:>200`, -`max_glu_serum:>300`)

# Para readmitted ----

Prop_readmitted <- sapply(data["readmitted"], mean)

T_Prop_readmitted <- data.frame(
  Variable = "readmitted",
  Proporcion = as.vector(Prop_readmitted)
)

T_Prop_readmitted <- T_Prop_readmitted[order(T_Prop_readmitted$Proporcion, decreasing = TRUE), ]

kable(
  T_Prop_readmitted, 
  format = "markdown", 
  col.names = c("Variable", "Proporción"),
  caption = "Proporciones de las variables en Readmitted"
)

T_Prop_readmitted %>%
  ggplot(aes(x = reorder(Variable, Proporcion), y = Proporcion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle(label = "PROPORCION DE LAS VARIABLES EN READMITTED",
          subtitle = "PERFILES DE PACIENTES CON DIABETES") +
  labs(x = "Variable", y = "Proporción") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        legend.position = "none", text = element_text(size = 10),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"))



# VARIABLES NUMERICAS ---- 

# Las variables numerica en la base son : time_in_hospital, num_lab_procedures, num_procedures
# num_medications, number_outpatient, number_emergency, number_inpatient, number_diagnoses,


## Para time_in_hospital ---- 

# Summary en formato kable de la variable time_in_hospital

# Crear el resumen como un vector nombrado
summary_time_in_hospital <- summary(data$time_in_hospital)

# Convertir el vector a un data.frame
summary_time_in_hospital_df <- data.frame(
  Variable = names(summary_time_in_hospital),
  Valor = as.numeric(summary_time_in_hospital)
)

# Mostrar con kable
kable(
  summary_time_in_hospital_df, 
  format = "markdown", 
  caption = "Resumen de la variable Time in Hospital"
)

# Grafica 

data %>%
  ggplot(aes(x = time_in_hospital)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DEL TIEMPO EN EL HOSPITAL",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Días en el hospital", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

## Para num_lab_procedures ----

# Summary en formato kable de la variable num_lab_procedures

# Crear el resumen como un vector nombrado

summary_num_lab_procedures <- summary(data$num_lab_procedures)

# Convertir el vector a un data.frame

summary_num_lab_procedures_df <- data.frame(
  Variable = names(summary_num_lab_procedures),
  Valor = as.numeric(summary_num_lab_procedures)
)

# Mostrar con kable

kable(
  summary_num_lab_procedures_df, 
  format = "markdown", 
  caption = "Resumen de la variable Num Lab Procedures"
)

# Grafica

data %>%
  ggplot(aes(x = num_lab_procedures)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE PROCEDIMIENTOS DE LABORATORIO",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de procedimientos", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

## Para num_procedures ----

# Summary en formato kable de la variable num_procedures

# Crear el resumen como un vector nombrado

summary_num_procedures <- summary(data$num_procedures)

# Convertir el vector a un data.frame

summary_num_procedures_df <- data.frame(
  Variable = names(summary_num_procedures),
  Valor = as.numeric(summary_num_procedures)
)

# Mostrar con kable

kable(
  summary_num_procedures_df, 
  format = "markdown", 
  caption = "Resumen de la variable Num Procedures"
)

# Grafica

data %>%
  ggplot(aes(x = num_procedures)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE PROCEDIMIENTOS",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de procedimientos", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

## Para num_medications ----

# Summary en formato kable de la variable num_medications

# Crear el resumen como un vector nombrado

summary_num_medications <- summary(data$num_medications)

# Convertir el vector a un data.frame

summary_num_medications_df <- data.frame(
  Variable = names(summary_num_medications),
  Valor = as.numeric(summary_num_medications)
)

# Mostrar con kable

kable(
  summary_num_medications_df, 
  format = "markdown", 
  caption = "Resumen de la variable Num Medications"
)

# Grafica

data %>%
  ggplot(aes(x = num_medications)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE MEDICAMENTOS",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de medicamentos", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

## Para number_outpatient ----

# Summary en formato kable de la variable number_outpatient

# Crear el resumen como un vector nombrado

summary_number_outpatient <- summary(data$number_outpatient)

# Convertir el vector a un data.frame

summary_number_outpatient_df <- data.frame(
  Variable = names(summary_number_outpatient),
  Valor = as.numeric(summary_number_outpatient)
)

# Mostrar con kable

kable(
  summary_number_outpatient_df, 
  format = "markdown", 
  caption = "Resumen de la variable Number Outpatient"
)

# Grafica

data %>%
  ggplot(aes(x = number_outpatient)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE VISITAS AMBULATORIAS",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de visitas", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

# Para number_emergency ----

# Summary en formato kable de la variable number_emergency

# Crear el resumen como un vector nombrado

summary_number_emergency <- summary(data$number_emergency)

# Convertir el vector a un data.frame

summary_number_emergency_df <- data.frame(
  Variable = names(summary_number_emergency),
  Valor = as.numeric(summary_number_emergency)
)

# Mostrar con kable

kable(
  summary_number_emergency_df, 
  format = "markdown", 
  caption = "Resumen de la variable Number Emergency"
)

# Grafica

data %>%
  ggplot(aes(x = number_emergency)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE VISITAS DE EMERGENCIA",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de visitas", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

# Para number_inpatient ----

# Summary en formato kable de la variable number_inpatient

# Crear el resumen como un vector nombrado

summary_number_inpatient <- summary(data$number_inpatient)

# Convertir el vector a un data.frame

summary_number_inpatient_df <- data.frame(
  Variable = names(summary_number_inpatient),
  Valor = as.numeric(summary_number_inpatient)
)

# Mostrar con kable

kable(
  summary_number_inpatient_df, 
  format = "markdown", 
  caption = "Resumen de la variable Number Inpatient"
)

# Grafica

data %>%
  ggplot(aes(x = number_inpatient)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE VISITAS HOSPITALARIAS",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de visitas", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

# Para number_diagnoses ----

# Summary en formato kable de la variable number_diagnoses

# Crear el resumen como un vector nombrado

summary_number_diagnoses <- summary(data$number_diagnoses)

# Convertir el vector a un data.frame

summary_number_diagnoses_df <- data.frame(
  Variable = names(summary_number_diagnoses),
  Valor = as.numeric(summary_number_diagnoses)
)

# Mostrar con kable

kable(
  summary_number_diagnoses_df, 
  format = "markdown", 
  caption = "Resumen de la variable Number Diagnoses"
)

# Grafica

data %>%
  ggplot(aes(x = number_diagnoses)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  ggtitle(label = "DISTRIBUCIÓN DE DIAGNÓSTICOS",
          subtitle = "PACIENTES CON DIABETES") +
  labs(x = "Número de diagnósticos", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey50"),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 10))

