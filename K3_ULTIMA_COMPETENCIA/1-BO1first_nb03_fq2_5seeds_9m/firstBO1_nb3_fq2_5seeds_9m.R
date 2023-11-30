# Acá evaluar y escribir recomendación de recursos

# Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")

#-----------------------------------CONFIGURAR PARÁMETROS-------------------------------------------#
# Defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()

# Nombre del experimento
PARAM$experimento <- "BO1second_nb03_fq2_5seeds_9m" 

# Path donde se aloja el dataset (puede cargar su dataset preprocesado o puede hacerlo en el apartado de preprocesamiento de abajo)
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# Meses donde se entrena el modelo
PARAM$input$training <- c( 202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107)

# Mes donde aplico el modelo
PARAM$input$future <- c(202109)

# Defino parámetros:

# Parámetro variable (esto genera semillas con valor entre 15k y 80k, puede ajustar a preferencia)
#cantidad_semillas = 1 # Cuántas semillas desea ensamblar?
#semillas <- as.integer(seq(15000, 80000, length.out = cantidad_semillas))
semillas <- c(729199, 356327, 125915, 998544, 27563) 
#c(729199, 356327, 125915, 998544, 27563 ) 
#77915, 10312, 89541, 1556, 1986


# Parámetros fijos obtenidos en la Optimización Bayesiana. mayor ganancia
PARAM$finalmodel$num_iterations <- 1284
PARAM$finalmodel$learning_rate <-0.0825522872668654
PARAM$finalmodel$feature_fraction <- 0.541945693700052
PARAM$finalmodel$min_data_in_leaf <- 11197
PARAM$finalmodel$num_leaves <- 861
PARAM$finalmodel$max_bin <- 31

#---------------------------------CARGAR DATOS---------------------------------------------#
# Aqui empieza el programa que voy a ejecutar para cada semilla
# Directorio de origen
setwd("~/buckets/b1/")

# Cargo el conjunto de datos
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

#---------------------------------PREPROCESAMIENTO DE DATOS---------------------------------------------#
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

dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]

dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]

dataset[ , prest_cantidad          := cprestamos_personales + cprestamos_hipotecarios + cprestamos_prendarios ]
dataset[ , prest_monto             := mprestamos_personales + mprestamos_hipotecarios + mprestamos_prendarios ]

dataset[ , inv_cant_total          := cplazo_fijo + cinversion1 + cinversion2 ]
dataset[ , inv_monto_total         := mplazo_fijo_pesos + mplazo_fijo_dolares  +minversion1_dolares + minversion2 ]
dataset[ , inv_ratio_saldo         := inv_monto_total / mcuentas_saldo ]

dataset[  , ingreso_edad  := payroll_total / cliente_edad ]



#CORRECCiON INF y NAN GENERADOS POR FE
dataset[mapply(is.infinite, dataset)] <- NA # o DT[is.infinite(DT)] <- NA
dataset[mapply(is.nan, dataset)] <- NA # o 0?? o  DT[is.nan(DT)] <- NA

# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#CS: hago lags y deltas 1,2,3, 6
#   Sin lags no hay paraiso !  corta la bocha

cols_lagueables <- copy(setdiff( colnames(dataset),
                                 c("numero_de_cliente", "foto_mes", "clase_ternaria") ))

# FUNDAMENTAL  ordenar el dataset anes de los lags
setorder( dataset, numero_de_cliente, foto_mes )

# lags de orden 1
dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables]

# agrego los delta lags de orden 1
for (vcol in cols_lagueables) 
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]


# lags de orden 2
dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables]

# agrego los delta lags de orden 2
for (vcol in cols_lagueables) 
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]


# lags de orden 6
dataset[, paste0(cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables]

# agrego los delta lags de orden 6
for (vcol in cols_lagueables) 
  dataset[, paste0(vcol, "_delta6") := get(vcol) - get(paste0(vcol, "_lag6"))]

# reordeno original
setorder( dataset, foto_mes, numero_de_cliente )


##----------------------------------------------------------------------------##
#calcula  el ratio entre el valor actual y el promedio de los ultimos nhistoria meses

#RatioMean  <- function( dataset, cols, nhistoria )
#{
#  sufijo  <- paste0( "_rmean", nhistoria )

#  dataset[ , paste0( cols, sufijo) := .SD/frollapply(x=.SD, FUN="mean", n=nhistoria, na.rm=TRUE, align="right"), 
#           by= numero_de_cliente, 
#           .SDcols= cols]

#  ReportarCampos( dataset )
#}

#calcula el promedio de los ultimos  nhistoria meses

Promedios  <- function( dataset, cols, nhistoria )
{
  
  sufijo  <- paste0( "_avg", nhistoria )
  
  dataset[ , paste0( cols, sufijo) := frollmean(x=.SD, n=nhistoria, na.rm=TRUE, algo="fast", align="right"), 
           by= numero_de_cliente, 
           .SDcols= cols]
  
  #  ReportarCampos( dataset )
}
##----------------------------------------------------------------------------##
#RatioMean( dataset, cols_lagueables, 3)
Promedios( dataset, cols_lagueables, 3 )
##------------------------------------------------------------------------------------------------##
#----------------------------------------------------------------------------------------------#

# Configuro la variable target como binaria
# El criterio: POS = { BAJA+1, BAJA+2 }, NEG {CONTINUA}
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#-----------------------------------SELECCIONAR DATOS-------------------------------------------#
# Campos a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# Establezco qué datos usaré para entrenar
# Creo columna train con valor cero en todas sus filas
dataset[, train := 0L]

# Asigno un 1 a todas las filas correspondiente al foto_mes configurado en los parámetros de entrada
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#---------------------------------CREAR DIRECTORIOS---------------------------------------------#
# Creo carpeta donde guardar los experimentos en caso de que no exista
dir.create("./exp/", showWarnings = FALSE)

# Creo carpeta donde guardar este experimento en caso de que no exista
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory de este experimento
setwd(paste0("./exp/", PARAM$experimento, "/"))


#----------------------------------CONFIGURAR DATOS DE ENTRADA MODELO----------------------------------#
# Dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#----------------------------------ITERACIÓN----------------------------------#

# Obtengo los datos a predecir
dataset[, predicciones_acumuladas := 0]
dapply <- dataset[foto_mes == PARAM$input$future]

# Selecciono columna con numero de cliente y foto mes en df para guardar las predicciones
predicciones <- dapply[, list(numero_de_cliente, foto_mes, predicciones_acumuladas)]

cat("\n\nEmpieza la iteración, hora:", Sys.time(), "\n")

for (semilla in semillas) {
  #----------------------------------CONFIGURAR MODELO--------------------------------------------#
  # Utilizo los parámetros configurados al inicio para el modelo
  
  modelo <- lgb.train(
    data = dtrain,
    param = list(
      objective = "binary",
      max_bin = PARAM$finalmodel$max_bin,
      learning_rate = PARAM$finalmodel$learning_rate,
      num_iterations = PARAM$finalmodel$num_iterations,
      num_leaves = PARAM$finalmodel$num_leaves,
      min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
      feature_fraction = PARAM$finalmodel$feature_fraction,
      
      
      boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
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
      
      #bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
      pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
      neg_bagging_fraction = 0.3, # 0.0 < neg_bagging_fraction <= 1.0
      bagging_freq = 2,
      is_unbalance = FALSE, #
      scale_pos_weight = 1.0, # scale_pos_weight > 0.0
      seed = semilla 
    )
  )
  
  #----------------------------------PERSISTIR IMPORTANCIA DE VARIABLES---------------------------------#
  # Este paso guarda la importancia de variables de cada modelo generado, puede descomentar si desea guardarlas)
  # Calculo la importancia de variables del modelo
  # tb_importancia <- as.data.table(lgb.importance(modelo))
  
  # Configuro nombre del archivo
  # archivo_importancia <- paste0("impo_", semilla, ".txt")
  
  # Guardo en el archivo 
  # fwrite(tb_importancia,
  # file = archivo_importancia,
  # sep = "\t"
  #)
  
  #----------------------------------PREDECIR SOBRE MES DE INTERÉS---------------------------------#
  # Aplico el modelo a los nuevos datos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # Agrego columna con las predicciones de cada semilla
  col_name <- paste0("semilla_", semilla)
  predicciones[, (col_name) := prediccion] 
  predicciones[  , predicciones_acumuladas :=  predicciones_acumuladas +  prediccion ]  #acumulo las predicciones
  
  
}

#-------------------------------PERSISTO SALIDA CON LAS PREDICCIONES DE CADA SEMILLA------------------------------#
# Guardo el archivo (con probas)
archivo_salida <- paste0(PARAM$experimento, "_predicciones_semillas.csv")
fwrite(predicciones, file = archivo_salida, sep = ",")

#-----------------------------------------------GENERO ENSEMBLE---------------------------------------------------#

# Calcular el promedio de las predicciones (probas) de los 100 modelos ejecutados (excluyo cols "numero_de_cliente" y "foto_mes")
predicciones$proba_ensemble <- rowMeans(predicciones[, .SD, .SDcols = -(1:2)])

cat("\n\nEnsemble generado, hora:", Sys.time(), "\n")

#------------------------------------------GENERO ENTREGA A KAGGLE------------------------------------------------#
# Ordeno por probabilidad descendente
setorder(predicciones, -proba_ensemble)


# Genero archivos variando la cantidad de estímulos
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  predicciones[, Predicted := 0L]
  predicciones[1:envios, Predicted := 1L]
  
  fwrite(predicciones[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado, hora:", Sys.time(),"\n")