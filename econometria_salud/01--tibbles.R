### Instalar y/o cargar paquetes

# install.packages("tidyverse")
# install.packages("Hmisc")
library(tidyverse)
library(Hmisc)

### Leer y limpiar (primer) base de base de datos

# Leer y explorar base de datos
db1 <- read_csv("econometria_salud--datos/CancerData_P1.csv")
describe(db1)
plot(describe(db1))

# Limpieza de base datos
db1$age[db1$age<18 | db1$age>120] <-NA
db1$sex[db1$sex=="12.2"]<-NA
db1[db1==-98]<-NA
db1[db1==-99]<-NA
db1[db1=="not assesed"]<-NA
plot(describe(db1))

### Leer (segunda) base de base de datos y unir
db2 <- read_csv("econometria_salud--datos/CancerData_P2.csv")
data <- bind_rows(db1, db2, .id="source")

### Leer (tercer) base de base de datos y combinar
db3 <- read_csv("econometria_salud--datos/CancerData_D1.csv")
data_full <- full_join(data, db3)

### Verbos (utilizando el operador "pipe" %>%)

# Múltiples pasos
data_old <- data_full %>% filter(age>=65)
data_old_arranged <- data_old %>% arrange(age)
data_old_arranged_short <- data_old_arranged %>% select(sex, age, test1, test2)
data_old_newvariable <- data_old_arranged_short %>% mutate(test=(test1+test2)/2) 

# Un solo paso
data_final <- data_full %>% filter(age>=65) %>%
  arrange(age) %>% select(sex, age, test1, test2) %>%
  mutate(test=(test1+test2)/2)

# Crear variable dummy

#...