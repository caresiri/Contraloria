library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(gridExtra)

temp <- tempfile(fileext = ".xlsx")
download.file("https://drive.google.com/uc?authuser=0&id=1Kl6--5GnckLUWQzUwGOZ96fHFxS0cyGB&export=download",temp)
Instituciones <- read_excel(temp, sheet = "Inst. Publicas")
download.file("https://drive.google.com/uc?authuser=0&id=1dCiQv_uAsTmoHkPlMgvwTXDtXlbG6SwY&export=download",temp)
Proveedores <- read_excel(temp)

AdjI <- Adj_Total %>% group_by(INSTITUCION, CED_INSTITUCION) %>%
  summarise(Monto = sum(MONTO_ADJUDICADO), n = n()) %>%
  arrange(desc(Monto)) %>%
  left_join(Instituciones, by = "CED_INSTITUCION") %>%
  select(INSTITUCION, CED_INSTITUCION, Monto,  Alias)

AdjI$Monto_Acumulado <- cumsum(AdjI$Monto)
AdjI$Alias <- factor(AdjI$Alias, levels=AdjI$Alias)

AIM <- ggplot(AdjI, aes(x=AdjI$Alias)) + 
  geom_bar(aes(y=AdjI$Monto), fill='blue', stat="identity") +
  geom_point(aes(y=AdjI$Monto_Acumulado), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(y=AdjI$Monto_Acumulado, group=1), colour="slateblue1", lty=3, size=0.9) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6), axis.text=element_text(size=2)) +
  labs(title = "Pareto", subtitle = "Instituciones y Monto Adjudicado", x = 'Instituciones', y = 'Monto Adjudicado') 
#  theme(axis.text.x = element_blank())

AdjI_filt <- AdjI %>%
  filter(Monto_Acumulado< 4.8e+10)
           
AIM_Filt <- ggplot(AdjI_filt, aes(x=AdjI_filt$Alias)) + 
  geom_bar(aes(y=AdjI_filt$Monto), fill='blue', stat="identity") +
  theme(axis.text.x = element_text(angle=90), axis.text=element_text(size=6)) +
  labs(title = "80% del Pareto", subtitle = "Instituciones y Monto Adjudicado", x = 'Instituciones', y = 'Monto Adjudicado') 
grid.arrange(AIM, AIM_Filt, nrow = 2)


# Análisis Proveedores
AdjP <- Adj_Total %>% group_by(NOMBRE_PROVEEDOR, CEDULA_PROVEEDOR) %>%
  summarise(Monto = sum(MONTO_ADJUDICADO), n = n()) %>%
  arrange(desc(Monto)) %>%
  left_join(Proveedores, by = "CEDULA_PROVEEDOR") %>%
  select(NOMBRE_PROVEEDOR, CEDULA_PROVEEDOR, Monto, `Nombre Comercial`)
AdjP$Nombre <- ifelse(is.na(AdjP$`Nombre Comercial`), AdjP$NOMBRE_PROVEEDOR, AdjP$`Nombre Comercial`)


AdjP$Monto_Acumulado <- cumsum(AdjP$Monto)
AdjP$Nombre[521] <- "SONDA2"
AdjP$Nombre <- factor(AdjP$Nombre, levels=AdjP$Nombre)

APM <- ggplot(AdjP, aes(x=AdjP$Nombre)) + 
  geom_bar(aes(y=AdjP$Monto), fill='blue', stat="identity") +
  geom_point(aes(y=AdjP$Monto_Acumulado), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(y=AdjP$Monto_Acumulado, group=1), colour="slateblue1", lty=3, size=0.9) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title = "Pareto", subtitle = "Instituciones y Monto Adjudicado", x = 'Proveedores', y = 'Monto Adjudicado') +
  theme(axis.text.x = element_blank())
APM

AdjP_filt <- AdjP %>%
  filter(Monto_Acumulado<AdjP$Monto_Acumulado[957]*.7) %>%
  select(NOMBRE_PROVEEDOR, Monto, Monto_Acumulado)

AdjP_filt

Adj <- Adj_Total %>% group_by(INSTITUCION) %>%
  summarise(Monto = sum(MONTO_ADJUDICADO), n = n()) %>%
  arrange(desc(Monto)) %>%
  mutate(Monto_Acumulado = cumsum(Monto))

Adj$INSTITUCION <- factor(Adj$INSTITUCION, levels=Adj$INSTITUCION)

AIM <- ggplot(Adj, aes(x=Adj$INSTITUCION)) + 
  geom_bar(aes(y=Adj$Monto), fill='blue', stat="identity") +
  geom_point(aes(y=Adj$Monto_Acumulado), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(y=Adj$Monto_Acumulado, group=1), colour="slateblue1", lty=3, size=0.9) +
 # theme(axis.text.x = element_text(angle=90, vjust=0.6)) 
  labs(title = "Pareto", subtitle = "Instituciones y Monto Adjudicado", x = 'Instituciones', y = 'Monto Adjudicado') +
  theme(axis.text.x = element_blank())

Adj_filt <- Adj %>%
  filter(Monto_Acumulado< 4.8e+10)
           
AIM_Filt <- ggplot(Adj_filt, aes(x=Adj_filt$INSTITUCION)) + 
  geom_bar(aes(y=Adj_filt$Monto), fill='blue', stat="identity") +
  theme(axis.text.x = element_text(angle=90), axis.text=element_text(size=4)) +
  labs(title = "Pareto", subtitle = "Instituciones y Monto Adjudicado", x = 'Instituciones', y = 'Monto Adjudicado') 
AIM_Filt

