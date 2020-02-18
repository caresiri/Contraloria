
df <- SIACSICOP1519
df$tech = "No_Se"
lower <- tolower(df$DESC_BIEN_SERVICIO)
df$tech[grepl(paste(filter(palabras_unicas, eliminate == "Si")$word, collapse="|"), lower, perl = TRUE)] = "No"
df$tech[grepl(paste(filter(palabras_unicas, eliminate == "No")$word, collapse="|"), lower, perl = TRUE)] = "Si"

## Especiales
df$tech[grepl('(^(?=.*\\bmarcador\\b)(?!.*\\breloj\\b))', lower, perl = TRUE)] = "No"#MARCADOR SIN RELOJ

for(i in 1:nrow(correlaciones))
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

