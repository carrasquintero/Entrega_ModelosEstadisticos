library(dplyr)
library(tidyr)
library(stringr)
library(GGally)

# Cargar los datos
ruta_archivo <- "C:/Users/gusta/Documents/Entrega_ModelosEstadisticos-git/Train bank.csv"
datos <- read.csv(ruta_archivo)

# 1. Verificar valores faltantes
print("Valores faltantes por columna:")
sapply(datos, function(x) sum(is.na(x)))


# 2. Verificar duplicados
print("Número de filas duplicadas:")
print(sum(duplicated(datos)))


# 3. Verificar tipos de datos
print("Tipos de datos por columna:")
str(datos)


# 5. Verificar inconsistencias de texto en variables categóricas
# Convertir a minúsculas y eliminar espacios en blanco
datos <- datos %>%
  mutate_if(is.character, ~ str_to_lower(str_trim(.)))


# 9. Verificar valores categóricos inconsistentes (Ejemplo: "sí" y "Sí")
print("Valores únicos en columnas categóricas:")
sapply(datos %>% select_if(is.character), unique)

columnas_si_no <- c("Credit", "Housing.Loan","Personal.Loan") # Reemplaza con los nombres reales

# 7. Conversión de "sí"/"no" a valores binarios solo en columnas específicas
datos <- datos %>%
  mutate(across(all_of(columnas_si_no), 
                ~ ifelse(. %in% c("yes"), 1, 
                          ifelse(. == "no", 0, NA))))

# Vista de los datos limpios
print("Datos limpios:")
print(head(datos))

numericos <- c("Credit","Age" ,"Housing.Loan", "Personal.Loan", 
               "Balance..euros.", "Last.Contact.Day", 
               "Last.Contact.Duration", "Campaign", 
               "Pdays", "Previous", "Subscription")

# Calcular la matriz de correlación
correlacion <- datos %>%
  select(all_of(numericos)) %>%       # Seleccionar solo las columnas numéricas
  cor(method = "pearson")              # Calcular la correlación

# Redondear y mostrar la matriz de correlación
correlacion_redondeada <- round(correlacion, digits = 3)
print(correlacion_redondeada)


# Convertir la variable de suscripción en un factor para que sea más fácil de visualizar
datos$Subscription <- as.factor(datos$Subscription)

# Box plot de edad según suscripción
ggplot(datos, aes(x = Subscription, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribución de Edad por Suscripción", x = "Suscripción (0=No, 1=Sí)", y = "Edad") +
  theme_minimal()

# Box plot de balance en euros según suscripción
ggplot(datos, aes(x = Subscription, y = Contact)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribución de Balance en Euros por Suscripción", x = "Suscripción (0=No, 1=Sí)", y = "Balance en Euros") +
  theme_minimal()

# Box plot de duración del último contacto según suscripción
ggplot(datos, aes(x = Subscription, y = Last.Contact.Duration)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Distribución de Duración del Último Contacto por Suscripción", x = "Suscripción (0=No, 1=Sí)", y = "Duración del Último Contacto (segundos)") +
  theme_minimal()

# Box plot de campañas previas según suscripción
ggplot(datos, aes(x = Subscription, y = Campaign)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Distribución de Número de Campañas por Suscripción", x = "Suscripción (0=No, 1=Sí)", y = "Número de Campañas") +
  theme_minimal()

# Box plot de balance de crédito según suscripción
ggplot(datos, aes(x = Subscription, y = Credit)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Distribución de Crédito por Suscripción", x = "Suscripción (0=No, 1=Sí)", y = "Crédito") +
  theme_minimal()


datos_numericos <- datos %>% 
  select(all_of(numericos)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Calcular medidas de tendencia central y dispersión, ignorando NAs
medidas <- datos_numericos %>%
  summarise(across(everything(), list(
    Media = ~ mean(., na.rm = TRUE),
    Mediana = ~ median(., na.rm = TRUE),
    Varianza = ~ var(., na.rm = TRUE),
    Desviacion = ~ sd(., na.rm = TRUE)
  )))

print(medidas)

# Variables categóricas para analizar con tablas de contingencia
categoricas <- c("Job", "Marital.Status", "Education", "Subscription")

# Crear tablas de contingencia para cada par de variables categóricas
for (var1 in categoricas) {
  for (var2 in categoricas) {
    if (var1 != var2) {
      cat("\nTabla de contingencia entre", var1, "y", var2, ":\n")
      print(table(datos[[var1]], datos[[var2]]))
    }
  }
}


for (var1 in categoricas) {
  for (var2 in categoricas) {
    if (var1 != var2) {

porcentajes_marital_status <- prop.table(tabla, margin = 2) * 100

