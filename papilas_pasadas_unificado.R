
install.packages("tidyverse")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("glmmTMB")
install.packages("multcomp")
library(multcomp)
library(dplyr)                  
library(rstatix)
library(ggplot2)
library(ggpubr)
library(readxl)
library(lme4)
library(DHARMa)
library(emmeans)
library(glmmTMB)

ruta_atrox <- "Matriz_comp_papilas_osc.xlsx"

hojas_atrox_ <- excel_sheets(ruta_atrox)

datos_atrox <- vector("list", length(hojas_atrox_))
names(datos_atrox) <- hojas_atrox_

for (i in seq_along(hojas_atrox_)) {
  datos_atrox[[i]] <- read_excel(
    ruta_atrox,
    sheet = hojas_atrox_[i]
  )
}

datos_atrox<- lapply(datos_atrox, function(df) {
  
  df[, -1] <- lapply(df[, -1], function(x) {
    suppressWarnings(as.numeric(x))
  })
  
  df
})
datos_atrox<- lapply(datos_atrox, function(df) {
  
  df[, -1][is.na(df[, -1])] <- 0
  
  df
})


matriz_atrox<- lapply(datos_atrox, function(df) {
  
  mat <- as.matrix(df[, -1])
  
  # usar la primera columna como nombres de fila
  rownames(mat) <- df[[1]]
  
  mat
})


total_comp_ <- lapply(datos_atrox, function(df) {
  
  # seleccionar SOLO momentos (columnas 2 a n)
  datos_num <- df[, -1, drop = FALSE]
  
  # sumar por columna
  suma <- colSums(datos_num, na.rm = TRUE)
  
  # devolver como data frame de 1 fila
  as.data.frame(t(suma))
})

data_pasadas_atrox=data.frame(ID=numeric(50),Papilas=character(50),Fosetas=character(50), P1=numeric(50),P2=numeric(50), P3=numeric(50), P4=numeric(50), P5=numeric(50), P6=numeric(50), P7=numeric(50), P8=numeric(50), P9=numeric(50), P10=numeric(50))
data_pasadas_atrox$ID=c(rep("B3",5),rep("B5",5),rep("B6",5),rep("B7",5), rep("B8",5), rep("B9",5), rep("B10",5), rep("B11",5), rep("B15",5),rep("B2",5))
data_pasadas_atrox$Papilas=rep(c("PD", "PT", "PD", "PT", "PD_SE"),10)
data_pasadas_atrox$Fosetas=rep(c(rep("FD",2),rep("FT",3)),10)

data_pasadas_atrox <- as.data.frame(data_pasadas_atrox)

P_vals <- do.call(rbind, lapply(total_comp_, function(x) {
  as.numeric(x[1:10])
}))

data_pasadas_atrox[, paste0("P", 1:10)] <- P_vals


data_pasadas_atrox_f=data.frame(ID=character(), Papilas=character(),Fosetas=character(),Pasada=numeric(),Reacciones=numeric())
#Ordenar base de datos
data_pasadas_atrox_f <- do.call(rbind, lapply(seq_len(nrow(data_pasadas_atrox)), function(i) {
  
  a <- data_pasadas_atrox[i, ]
  
  # repetir ID, Papilas y Fosetas 10 veces
  d <- a[rep(1, 10), c("ID", "Papilas", "Fosetas")]
  
  # crear variable Pasada
  d$Pasada <- 1:10
  
  # asignar valores P1-P10
  d$Reacciones <- as.numeric(a[1, paste0("P", 1:10)])
  
  d
}))

data_pasadas_atrox_f$Rx=log(data_pasadas_atrox_f$Reacciones+1)
data_pasadas_atrox_f$Trat=as.factor(paste(data_pasadas_atrox_f$Fosetas,data_pasadas_atrox_f$Papilas,sep=""))
data_pasadas_atrox_f$Estimulo <- "CE"
data_pasadas_atrox_f$Estimulo[data_pasadas_atrox_f$Trat == "FTPD_SE"] <- "SE"
data_pasadas_atrox_f$Pasada <- factor(data_pasadas_atrox_f$Pasada)


#####DATA FRAME ASPER

ruta_asper <- "Matriz_papilas_CR.xlsx"

hojas_asper_<- excel_sheets(ruta_asper)

datos_asper <- vector("list", length(hojas_asper_))
names(datos_asper) <- hojas_asper_

for (i in seq_along(hojas_asper_)) {
  datos_asper[[i]] <- read_excel(
    ruta_asper,
    sheet = hojas_asper_[i]
  )
}

datos_asper<- lapply(datos_asper, function(df) {
  
  df[, -1] <- lapply(df[, -1], function(x) {
    suppressWarnings(as.numeric(x))
  })
  
  df
})
datos_asper<- lapply(datos_asper, function(df) {
  
  df[, -1][is.na(df[, -1])] <- 0
  
  df
})


matriz_asper<- lapply(datos_asper, function(df) {
  
  mat_a <- as.matrix(df[, -1])
  
  # usar la primera columna como nombres de fila
  rownames(mat_a) <- df[[1]]
  
  mat_a
})


total_comp_a <- lapply(datos_asper, function(df) {
  
  # seleccionar SOLO momentos (columnas 2 a n)
  datos_num_a <- df[, -1, drop = FALSE]
  
  # sumar por columna
  suma_a <- colSums(datos_num_a, na.rm = TRUE)
  
  # devolver como data frame de 1 fila
  as.data.frame(t(suma_a))
})

data_pasadas_asper=data.frame(ID=numeric(60),Papilas=character(60),Fosetas=character(60), P1=numeric(60),P2=numeric(60), P3=numeric(60), P4=numeric(60), P5=numeric(60), P6=numeric(60), P7=numeric(60), P8=numeric(60), P9=numeric(60), P10=numeric(60))
data_pasadas_asper$ID=c(rep("LIAP1104",5),rep("LIAP1112",5),rep("LIAP1125",5),rep("LIAP1122",5), rep("LIAP1129",5), rep("LIAP1105",5), rep("LIAP1133",5), rep("LIAP933",5),rep("LIAP908",5), rep("LIAP981",5),rep("LIAP1128",5),rep("LIAP1137",5))
data_pasadas_asper$Papilas=rep(c("PD", "PT", "PD", "PT", "PD_SE"),12)
data_pasadas_asper$Fosetas=rep(c(rep("FD",2),rep("FT",3)),12)


data_pasadas_asper <- as.data.frame(data_pasadas_asper)

P_vals_a <- do.call(rbind, lapply(total_comp_a, function(x) {
  as.numeric(x[1:10])
}))

data_pasadas_asper[, paste0("P", 1:10)] <- P_vals_a


data_pasadas_asper_f=data.frame(ID=character(), Papilas=character(),Fosetas=character(),Pasada=numeric(),Reacciones=numeric())
#Ordenar base de datos
data_pasadas_asper_f <- do.call(rbind, lapply(seq_len(nrow(data_pasadas_asper)), function(i) {
  
  a <- data_pasadas_asper[i, ]
  
  # repetir ID, Papilas y Fosetas 10 veces
  d <- a[rep(1, 10), c("ID", "Papilas", "Fosetas")]
  
  # crear variable Pasada
  d$Pasada <- 1:10
  
  # asignar valores P1-P10
  d$Reacciones <- as.numeric(a[1, paste0("P", 1:10)])
  
  d
}))

data_pasadas_asper_f$Rx=log(data_pasadas_asper_f$Reacciones+1)
data_pasadas_asper_f$Trat=as.factor(paste(data_pasadas_asper_f$Fosetas,data_pasadas_asper_f$Papilas,sep=""))
data_pasadas_asper_f$Estimulo <- "CE"
data_pasadas_asper_f$Estimulo[data_pasadas_asper_f$Trat == "FTPD_SE"] <- "SE"
data_pasadas_asper_f$Pasada <- factor(data_pasadas_asper_f$Pasada)


#### UNIR
data_pasadas_atrox_f <- data_pasadas_atrox_f %>%
  mutate(Especie = "B. atrox")

data_pasadas_asper_f <- data_pasadas_asper_f %>%
  mutate(Especie = "B. asper")

data_pasadas_conjunto <- bind_rows(
  data_pasadas_atrox_f,
  data_pasadas_asper_f
)


data_pasadas_conjunto$ID=as.factor(data_pasadas_conjunto$ID)
data_pasadas_conjunto$Papilas=as.factor(data_pasadas_conjunto$Papilas)
data_pasadas_conjunto$Fosetas=as.factor(data_pasadas_conjunto$Fosetas)
data_pasadas_conjunto$Trat=as.factor(data_pasadas_conjunto$Trat)
data_pasadas_conjunto$Pasada=as.numeric(data_pasadas_conjunto$Pasada)

##ESTADISTICOS 
tabla_varianza <- data_pasadas_conjunto %>%
  group_by(Trat) %>%
  summarise(
    Total_Observaciones = n(),
    Suma_Total = sum(Reacciones, na.rm = TRUE),
    Media = mean(Reacciones, na.rm = TRUE),
    Varianza = var(Reacciones, na.rm = TRUE),
    Desviacion_Estandar = sd(Reacciones, na.rm = TRUE)
  )

print(tabla_varianza)

data_pasadas_conjunto%>%
  group_by(Papilas,Fosetas, Estimulo, Pasada) %>%
  get_summary_stats(Reacciones, type = "mean_sd")

#POISSON con inflacion porque tiene muchos 0 

m_pois3<- glmmTMB(Reacciones ~ Trat + Pasada +(1 | ID),
                  ziformula = ~ 1,
                  family = poisson,
                  data = data_pasadas_conjunto)
summary(m_pois3)

res_simulados3 <- simulateResiduals(m_pois3)
plot(res_simulados3)

#DIAGNOSTICO
diagnose(m_pois3)
# Sobredispersión no hay p>0.05
testDispersion(res_simulados3)

# Outliers 
testOutliers(res_simulados3)

# Uniformidad rozando
testUniformity(res_simulados3)

# uniformidad entre tratamientos ME SALE UN ERROR QUE NO ENTIENDO 
testCategorical(
  res_simulados3,
  catPred = data_pasadas_conjunto$Trat
)

# Uniformidad chiqui entre especies asper pero el resto sale bien entonces yo lo dejaria asi
testCategorical(
  res_simulados,
  catPred = data_pasadas_conjunto$Especie
)


## COMPARACIÓN DE MEDIAS
emm <- emmeans(m_pois3,~ Trat,
               type = "response")

pairs(emm, adjust = "tukey")


##GRAFICA
significancias <- cld(
  emm,
  adjust = "tukey",
  Letters = letters
)


significancias <- as.data.frame(significancias)
emm<- as.data.frame(emm)

ggplot(emm, aes(x = Trat, y = rate)) +
  
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.12,
    linewidth = 0.7
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_text(
    data = significancias,
    aes(
      x = Trat,
      y = asymp.UCL,
      label = .group
    ),
    vjust = -0.6,
    size = 5
  ) +
  
  labs(
    x = "Tratamiento",
    y = "Media estimada de reacciones"
  ) +
  
  theme_classic(base_size = 13)
