# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA8240_PARAMz823_CA_FE_FEHIST"

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202012, 202101, 202102, 202103, 202104, 202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 729191

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 315
PARAM$finalmodel$optim$learning_rate <- 0.0695621551328802
PARAM$finalmodel$optim$feature_fraction <- 0.479016074240119
PARAM$finalmodel$optim$min_data_in_leaf <- 115
PARAM$finalmodel$optim$num_leaves <- 678


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]


dataset[ foto_mes==202006,  active_quarter  := NA ]
dataset[ foto_mes==202006,  internet  := NA ] 
dataset[ foto_mes==202006,  mcuentas_saldo  := NA ]
dataset[ foto_mes==202006,  ctarjeta_debito_transacciones  := NA ]
dataset[ foto_mes==202006,  mautoservicio := NA ]
dataset[ foto_mes==202006,  ctarjeta_visa_transacciones  := NA ]
dataset[ foto_mes==202006,  mtarjeta_visa_consumo  := NA ]
dataset[ foto_mes==202006,  ctarjeta_master_transacciones  := NA ]
dataset[ foto_mes==202006,  mtarjeta_master_consumo  := NA ]
dataset[ foto_mes==202006,  cextraccion_autoservicio  := NA ]
dataset[ foto_mes==202006,  mextraccion_autoservicio  := NA ]
dataset[ foto_mes==202006,  ccheques_depositados  := NA ]
dataset[ foto_mes==202006,  mcheques_depositados  := NA ]
dataset[ foto_mes==202006,  ccheques_emitidos  := NA ]
dataset[ foto_mes==202006,  mcheques_emitidos  := NA ]
dataset[ foto_mes==202006,  ccheques_depositados_rechazados  := NA ]
dataset[ foto_mes==202006,  mcheques_depositados_rechazados  := NA ]
dataset[ foto_mes==202006,  ccheques_emitidos_rechazados  := NA ]
dataset[ foto_mes==202006,  mcheques_emitidos_rechazados  := NA ]
dataset[ foto_mes==202006,  tcallcenter  := NA ]
dataset[ foto_mes==202006,  ccallcenter_transacciones  := NA ]
dataset[ foto_mes==202006,  thomebanking  := NA ]
dataset[ foto_mes==202006,  catm_trx  := NA ]
dataset[ foto_mes==202006,  matm  := NA ]
dataset[ foto_mes==202006,  catm_trx_other  := NA ]
dataset[ foto_mes==202006,  matm_other  := NA ]
dataset[ foto_mes==202006,  tmobile_app  := NA ]
dataset[ foto_mes==202006,  cmobile_app_trx  := NA ]

dataset[ foto_mes==201910 | foto_mes==202006,  chomebanking_transacciones  := NA ]


dataset[ foto_mes==201905 | foto_mes==201910 | foto_mes==202006,  mrentabilidad  := NA ]  
dataset[ foto_mes==201905 | foto_mes==201910 | foto_mes==202006,  mrentabilidad_annual  := NA ]
dataset[ foto_mes==201905 | foto_mes==201910 | foto_mes==202006,  mcomisiones  := NA ]
dataset[ foto_mes==201905 | foto_mes==201910 | foto_mes==202006,  mactivos_margen  := NA ]
dataset[ foto_mes==201905 | foto_mes==201910 | foto_mes==202006,  ccomisiones_otras  := NA ]
dataset[ foto_mes==201905 | foto_mes==201910 | foto_mes==202006,  mcomisiones_otras  := NA ]

dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201904,  mttarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  := NA ]
dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  := NA ]


dataset[ foto_mes==201901 | foto_mes==201902 | foto_mes==201903 | foto_mes==201904 | foto_mes==201905 ,  ctransferencias_recibidas  := NA ]
dataset[ foto_mes==201901 | foto_mes==201902 | foto_mes==201903 | foto_mes==201904 | foto_mes==201905 ,  mtransferencias_recibidas  := NA ]


# Data Drifting
# por ahora, no hago nada


#FE
dataset[  , cliente_edad_sobre_cliente_antiguedad  := cliente_edad*12 / cliente_antiguedad ]

dataset[  , mrentabilidad_annual_norm  := ifelse(cliente_antiguedad <12,mrentabilidad_annual/cliente_antiguedad, mrentabilidad_annual)]


dataset[  , rent_anual_norm_sobre_antig  := mrentabilidad_annual_norm / cliente_antiguedad ]

dataset[ , gastos_consumo_total := rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo, mcuenta_debitos_automaticos, mpagodeservicios, mpagomiscuentas) , na.rm=TRUE ) ]

dataset[ , gastos_deb_automatico := rowSums( cbind( mttarjeta_visa_debitos_automaticos,  mttarjeta_master_debitos_automaticos, mcuenta_debitos_automaticos) , na.rm=TRUE ) ]

dataset[ , gastos_no_deb_automatico := rowSums( cbind( mpagodeservicios, mpagomiscuentas) , na.rm=TRUE ) ]

dataset[ , payroll_total := rowSums( cbind( mpayroll, mpayroll2) , na.rm=TRUE ) ]

dataset[  , gastos_consumo_sobre_transf_recibidas  := gastos_consumo_total / mtransferencias_recibidas ]

dataset[  , gastos_consumo_sobre_payroll  := gastos_consumo_total / payroll_total ]

dataset[  , gastos_deb_autom_sobre_payroll  := gastos_deb_automatico / payroll_total ]

dataset[ , comisiones_total := rowSums( cbind( mcomisiones_mantenimiento,  mcomisiones_otras) , na.rm=TRUE ) ]

dataset[ , descuentos_total := rowSums( cbind( mcajeros_propios_descuentos,  mtarjeta_visa_descuentos, mtarjeta_master_descuentos) , na.rm=TRUE ) ]

dataset[  , descuentos_sobre_comisiones  := descuentos_total / comisiones_total ]

dataset[ , saldototal_visa_master := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]

dataset[  , saldo_visa_master_sobre_payroll  := saldototal_visa_master / payroll_total ]

#CORRECCiON INF y NAN GENERADOS POR FE
dataset[mapply(is.infinite, dataset)] <- NA # o DT[is.infinite(DT)] <- NA
dataset[mapply(is.nan, dataset)] <- NA # o 0?? o  DT[is.nan(DT)] <- NA

# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso !  corta la bocha

setorder( dataset, numero_de_cliente, foto_mes ) #por las dudas vuelvo a ordenar


col_para_lags <- setdiff(colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria")) 

dataset[ , paste0(col_para_lags, "_lag1") := shift(.SD, 1, type='lag'),  by= numero_de_cliente, .SDcols= col_para_lags] #https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group
dataset[ , paste0(col_para_lags, "_lag2") := shift(.SD, 2, type='lag'),  by= numero_de_cliente, .SDcols= col_para_lags]      
dataset[ , paste0(col_para_lags, "_lag3") := shift(.SD, 3, type='lag'),  by= numero_de_cliente, .SDcols= col_para_lags] 
dataset[ , paste0(col_para_lags, "_lag4") := shift(.SD, 4, type='lag'),  by= numero_de_cliente, .SDcols= col_para_lags] #https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group
dataset[ , paste0(col_para_lags, "_lag5") := shift(.SD, 5, type='lag'),  by= numero_de_cliente, .SDcols= col_para_lags]      


for( nm_col in col_para_lags )
{dataset[ , paste0(nm_col, "_delta1") := get(nm_col)  - get(paste0( nm_col, "_lag1"))]}


for( nm_col in col_para_lags )
{dataset[ , paste0(nm_col, "_delta2") := get(nm_col)  - get(paste0( nm_col, "_lag2"))]}


for( nm_col in col_para_lags )
{dataset[ , paste0(nm_col, "_delta3") := get(nm_col)  - get(paste0( nm_col, "_lag3"))]}

for( nm_col in col_para_lags )
{dataset[ , paste0(nm_col, "media_6") := (get(nm_col)  + get(paste0( nm_col, "_lag1")) + 
                                            get(paste0( nm_col, "_lag2")) + get(paste0( nm_col, "_lag3")) +
                                            get(paste0( nm_col, "_lag4")) + get(paste0( nm_col, "_lag5")))/6 ]}  

for( nm_col in col_para_lags )
{dataset[ , paste0(nm_col, "_lag4") := NULL]}

for( nm_col in col_para_lags )
{dataset[ , paste0(nm_col, "_lag5") := NULL]}

#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


# genero el modelo
param_completo <- c(PARAM$finalmodel$lgb_basicos,
  PARAM$finalmodel$optim)

modelo <- lgb.train(
  data = dtrain,
  param = param_completo,
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
)

# genero la tabla de entrega
tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_entrega,
  file = "prediccion.txt",
  sep = "\t"
)

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
