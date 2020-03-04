
df <- SIACSICOP1519
df$tech = "No_Se"
lower <- tolower(df$DESC_BIEN_SERVICIO)
df$tech[grepl(paste(filter(palabras_unicas, eliminate == "Si")$word, collapse="|"), lower, perl = TRUE)] = "No"
df$tech[grepl(paste(filter(palabras_unicas, eliminate == "No")$word, collapse="|"), lower, perl = TRUE)] = "Si"

## Especiales
df$tech[grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', lower, perl = TRUE)] = "No"#MARCADOR SIN RELOJ

corNO <- filter(correlaciones, Eliminar == "Si")
for(i in 1:nrow(corNO))
{
  df$tech[grepl(paste("(^(?=.*\\b",as.String(correlaciones[i,1]), "\\b)(?=.*\\b", as.String(correlaciones[i,2]), "\\b))", sep = "")
                           , lower, perl = TRUE)] = "No"
}

#### Categorización de Bienes y Servicios segun SICOP ####
df$CAT_BIEN_SERVICIO_MODIFIED <- lower
for(i in 1:nrow(palabras_unicas))
{
  df$CAT_BIEN_SERVICIO_MODIFIED[grepl(paste("(^(?=.*\\b",as.String(palabras_unicas[i,1]),"\\b))"), lower, perl = TRUE)] = as.String(palabras_unicas[i,9])
}
sum(df$tech == 'No_Se')
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bportatil\\b))', lower, perl = TRUE)] = "Equipo informático y accesorios"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\blicencia\\b))', lower, perl = TRUE)] = "Software"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\blicencias\\b))', lower, perl = TRUE)] = "Software"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bsoftware\\b))', lower, perl = TRUE)] = "Software"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bmantenimiento\\b))', lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\bmantenimiento y reparacion de equipo de computo y sistemas\\b))', 
                                               lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\servicio\\b))', 
                                               lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\servicios\\b))', 
                                               lower, perl = TRUE)] = "Servicios informáticos, de computación, audio y video"
df$CAT_BIEN_SERVICIO_MODIFIED[grepl('(^(?=.*\\impresora multifuncional(fax, copiadora,escaner)\\b))', 
                                               lower, perl = TRUE)] = "Equipo informático y accesorios"
save(df, file = "df.Rda")
