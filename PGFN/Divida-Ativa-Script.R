#################
### BIBLIOTECA ##
#################

library(dplyr)

#################
# BASE DE DADOS #
#################


base <- read.table("20180516-base-pfgn.txt", 
                   header = T, sep = ";", quote = "\"",
                   dec = ",", fill = TRUE, 
                   stringsAsFactors=F)
head(base)

#################
## MANIPULACAO ##
#################

options(digits = 2, scipen = 14)

colnames(base) <- c("CNPJ","Nome","Municpio","UF","Valor")

temp_uf <- base$UF
temp_uf <- gsub(" ","-",temp_uf)
head(temp_uf)

valor <- gsub("[.]","",base$Valor)
valor <- gsub("[,]",".",valor)
valor <- as.numeric(valor)
# head(valor)


cnpjLimpo <- base$CNPJ
cnpjLimpo <- gsub("[.]","",cnpjLimpo)
cnpjLimpo <- gsub("[-]", "", cnpjLimpo)
cnpjLimpo <- gsub("[/]", "", cnpjLimpo)
cnpjLimpo <- as.numeric(cnpjLimpo)
# head(cnpjLimpo)

base$UF <- temp_uf
base$CNPJ <- cnpjLimpo
base$Valor <- valor

# rm(cnpjLimpo, valor, temp_uf)

#################
### SPLIT - UF ##
#################

estados <- unique(base$UF)
# head(estados)

lista_por_UF <- split(base, base$UF)

for(i in 1:length(estados)){
  uf <- estados[i]
  base_uf <- lista_por_UF[[uf]]
  write.csv2(base_uf,paste0('refis_pgfn_',uf,'.csv'),row.names = FALSE)
}
# rm(estados, i, lista_por_UF, uf, base_uf)

#################
### STATISTIC ###
#################

valor_por_uf <-
  base %>%
  group_by(UF) %>%
  summarise(qtde = n(), soma_total = sum(Valor))

valor_por_uf$divida_media <- valor_por_uf$soma_total / valor_por_uf$qtde

valor_por_uf
View(valor_por_uf)
