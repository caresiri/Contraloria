library(ggplot2)
library(dplyr)

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

