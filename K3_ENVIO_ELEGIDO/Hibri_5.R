rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection


#setwd('~/buckets/b1')
setwd("C:/Users/totor/Documents/CS/DMEF2023/03COMPETENCIA")
require("data.table")

BO1first_nb03_fq2_5seeds_9m <- fread("MEJORES_KAGGLE/2911/2911_OB1/fisrtBO1_nb03-fq2-5seeds_9m/BO1first_nb03_fq2_5seeds.csv")
BO1second_nb03_fq2_5seeds_9m<- fread("MEJORES_KAGGLE/2911/2911_OB1/secondBO1_nb3_fq2_5seeds_9m/BO1second_nb03_fq2_5seeds_9m.csv")

BOJRfirst_nb03_fq2_5seeds_20m <- fread("MEJORES_KAGGLE/2911/2911_OBJR/firtsBOJR_nb03fq2_5seeds_20m/ftBOJR_nb03_fq2_5seeds_20m.csv")
BOJRsecond_nb03_fq2_5seeds_20m <- fread("MEJORES_KAGGLE/2911/2911_OBJR/sdBOJR_nb03fq2_5seeds_20m/sdBOJR_nb03_fq2_5seeds_20m.csv")


df <- rbindlist(list(BO1first_nb03_fq2_5seeds_9m,
                     BO1second_nb03_fq2_5seeds_9m,
                     BOJRfirst_nb03_fq2_5seeds_20m,
                     BOJRsecond_nb03_fq2_5seeds_20m
), fill=TRUE)
df$predicciones_acumuladas
df <- df[,sum(predicciones_acumuladas),by=numero_de_cliente]
df <- setorder(df,-V1)

#dir.create("C:/Users/totor/Documents/CS/DMEF2023/03COMPETENCIA/exp")
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0("hibri_05", "_", envios, ".csv"),
         sep = ","
  )
}