
library("mice")
library("FactoMineR") #MCA
library("factoextra") #fviz_screeplot
library("fpc") #fpc
library(cluster) #clusplot
library(spatstat) #crossdist


setwd("C:/Users/hecto/Documents/Finanzas clase/ChileTextMining/products/csv")
getwd()
enut<-read_csv("enut.csv")

bd <- enut

#Conoce las clases de las variables 
lapply(bd, class)

#Detectar datos faltantes
if (sum(is.na(bd))>1){print("si hay NA")}

#Trabajamos con una subset de la bd original (para prueba)
bd<-bd[sample(1:nrow(bd),1000,F),]

#Si hay datos faltantes, realizamos imputación 
bd<-mice::mice(bd)
bd<-mice::complete(bd)
if (sum(is.na(bd))>1){print("si hay NA")}



fn_mca <- function(bd, par_cex=1)
{
  
  ##@ pars
  # n_words=50
  # par_cex=.8
  # bd=fn_limpieza_comments$palabras_base_df
  # bd=fn_limpieza_noticias$palabras_base_df
  # colnames(bd)
  
  # head(bd)
  # # # #@ parametros
  # bd <- bd[1:n_words,]
  
  results_ls <- list()
  
  ### SE RENOMBRAN LAS COLUMNAS
  
  # id_col_na <- agrep("Var.",colnames(bd))
  # if(length(id_col_na)){
  #   bd <- bd[,-id_col_na]
  # }
  
  # id_ <- match(colnames(bd),cat_candidatos[,tipo_df])
  ## se sustituyen los nombres
  # colnames(bd) <- cat_candidatos[id_,"nombre_corto"]
  
  ### se transpone la matrix
  # bd <- t(bd)
  # head(bd)
  
  
  # ### SE LES DA FORMATO DE CLASE A LAS VARIABLES
  # i=1
  # for(i in 1:ncol(bd))
  # {
  #   print(c(i, ncol(bd)))
  #   id_match <- match(colnames(bd)[i], cat_vars_mca_val[,"nombre_corto"])
  #   tipo_var <- cat_vars_mca_val[id_match,"tipo"] 
  #   
  #   if(tipo_var=="factor")
  #   {
  #     bd[,i] <- as.factor(bd[,i])
  #   }
  #   if(tipo_var=="numeric")
  #   {
  #     bd[,i] <- as.numeric(as.matrix(bd[,i]))
  #   }
  #   if(tipo_var=="character")
  #   {
  #     bd[,i] <- as.character(bd[,i])
  #   }
  # }
  # # lapply(bd, class)
  
  
  # ### SE CREAN NUEVAS VARIABLES DE: VELOCIDAD PROMEDIO, RANGO DE VELOCIDAD
  # # head(bd)
  # dif_AA_B_min <- bd[,"AA_B_time_sup"]-bd[,"AA_B_time_inf"]
  # mean_AA_B_min <- (bd[,"AA_B_time_sup"]+bd[,"AA_B_time_inf"])/2
  # dif_A_B_min <- bd[,"A_B_time_sup"]-bd[,"A_B_time_inf"]
  # mean_A_B_min <- (bd[,"A_B_time_sup"]+bd[,"A_B_time_inf"])/2
  # vel_AA_B_min <- (bd[,grep("AA_B_km",colnames(bd))]/bd[,"AA_B_time_inf"])
  # vel_AA_B_sup <- (bd[,grep("AA_B_km",colnames(bd))]/bd[,"AA_B_time_sup"])
  # vel_AA_B_mean <- (vel_AA_B_sup+vel_AA_B_min)/2
  # vel_A_B_min <- bd[,"A_B_km"]/bd[,"AA_B_time_inf"]
  # vel_A_B_sup <- bd[,"A_B_km"]/bd[,"AA_B_time_sup"]
  # vel_A_B_mean <- (vel_A_B_min+vel_A_B_sup)/2
  # 
  # bd <- cbind(bd, dif_AA_B_min,
  #                                         dif_A_B_min, mean_AA_B_min, mean_A_B_min,
  #                                         vel_AA_B_min, vel_AA_B_sup, vel_AA_B_mean, vel_A_B_min,
  #                                         vel_A_B_sup, vel_A_B_mean)
  # View(bd)
  # cat_vars_bd_orig <- colnames(bd)
  # write.csv(cat_vars_bd_orig, "cat_vars_bd_ventas_formato_orig2.csv")
  
  
  
  # ### SE RENOMBRAN LOS NOMBRES DE LOS RENGLONES (ADEMAS SE ELIMINAN LOS REGISTROS DUPLICADOS)
  # # View(bd)
  # # sort(bd[,"cod_entorno"])[37:38]
  # # diff(sort(bd[,"cod_entorno"]))
  # id_elim_duplicado <- which(bd[,"cod_entorno"]==7058)[-1]
  # bd <- bd[-id_elim_duplicado,]
  # rownames(bd) <- bd[,"cod_entorno"]
  # # View(bd)
  # write.csv(bd, "bd.csv")
  
  # ### SE ELIGEN LAS VARIABLES PARA EL ANALISIS MCA
  # vars_mca <- cat_vars_mca_val[which(cat_vars_mca_val[,par_escenario]==1),]
  # id_match2 <- match(vars_mca[,"nombre_corto"], colnames(bd))
  # bd_mca <- bd[,id_match2]
  # # View(bd_mca)
  # # colnames(bd_mca)
  
  ### SE TRANSFORMAN LAS VARIABLES NUMERICAS A VARIABLES CATEGORICAS POR CUANTILES
  # bd_mca <- bd
  id_vars_numericas <- which(id_vars_numericas <- unlist(lapply(bd, class))=="numeric")
  bd_mca_numeric <- bd[,id_vars_numericas]
  bd_mca_numeric <- data.frame(bd_mca_numeric)
  # colnames(bd_mca_numeric) <- names(id_vars_numericas)
  # View(bd_mca_numeric)
  
  # bd_mca <- bd_mca[,-id_vars_numericas]
  
  vars_numericas_categoricas_ls <- list()
  i=1
  for(i in 1:ncol(bd_mca_numeric))
  {
    
    vector_numerico <- bd_mca_numeric[,i]
    vector_numerico_categorias <- rep(NA, length(vector_numerico))
    
    id_validos <- which(!is.na(vector_numerico))
    quantiles <- quantile(as.numeric(vector_numerico[id_validos]), probs=seq(0,1, .2))[-c(1)]
    
    j=1
    for(j in 1:length(quantiles))
    {
      print(c(i, ncol(bd_mca_numeric), j, length(quantiles)))
      if(j==1)
      {
        id_val_quant <- which(vector_numerico<=quantiles[j])
      }
      
      if(j==length(quantiles))
      {
        id_val_quant <- which(vector_numerico<=quantiles[j] & vector_numerico>quantiles[j-1])
      } 
      
      if(j!=1 & j!=length(quantiles)) 
      {
        id_val_quant <- which(vector_numerico>quantiles[j-1] & vector_numerico<=quantiles[j])
      }
      
      # vector_numerico_val <- vector_numerico[id_val_quant]
      
      vector_numerico_categorias[id_val_quant] <- names(quantiles)[j]
      
      vars_numericas_categoricas_ls[[i]] <- vector_numerico_categorias
    }
    
  }
  
  if(length(vars_numericas_categoricas_ls)==1)
  {
    vars_numericas_categoricas_df <- data.frame(vars_numericas_categoricas_ls[[1]])
  }else
  {
    vars_numericas_categoricas_df <- do.call("cbind", vars_numericas_categoricas_ls)
    vars_numericas_categoricas_df <- data.frame(vars_numericas_categoricas_df)
  }
  
  colnames(vars_numericas_categoricas_df) <- colnames(bd_mca_numeric)
  # dim(vars_numericas_categoricas_df)
  
  # ### SE SUSTITUYEN LAS VARIABLES CATEGORIZADAS
  # par_ventas <- "ventas_natural"
  # par_ventas_ls <- c("ventas_comercial", "ventas_natural", "ventas_total")
  # id_elim_ventas <- which(par_ventas!=par_ventas_ls)
  # vars_numericas_categoricas_df_val <- vars_numericas_categoricas_df[,-id_elim_ventas]
  
  ### SE ANEXA LA(S) COLUMNA(S) DE LA(S) VARIABLE(S) NUMERICA(S) VALIDA(S) SELECCIONADA(S)
  # bd_mca <- cbind(bd_mca, vars_numericas_categoricas_df)
  bd_mca <- vars_numericas_categoricas_df
  
  ## se agregan los nombres de los renglones
  # rownames(bd_mca) <- rownames(bd)
  
  ### SE RELLENAN LOS DATOS FALTANTES CON LA FUNCION "MICE"
  # lapply(bd_mca, class)
  # View(bd_mca)
  # bd_mca_mice <- mice(bd_mca, remove.collinear=F)
  # ?mice
  # bd_mca_mice <- complete(bd_mca_mice, 1)
  # rownames(bd_mca_mice) <- rownames(bd_mca)
  # dim(bd_mca_mice)
  
  # lapply(bd_mca_mice, class)
  
  bd_mca_mice <- bd_mca
  rownames(bd_mca_mice)
  
  par_text <- "MCA"
  dim(bd_mca_mice)
  res.mca <- MCA(bd_mca_mice, graph = F, na.method="NA")
  # write.infile(res.mca, "mca.csv", sep = ",")
  
  ### OBSERVACIONES
  obs_crds <- res.mca$ind$coord
  obs_crds <- cbind(tipo="observaciones", id=rownames(obs_crds),obs_crds)
  
  var_crds <- (res.mca$var$coord)
  var_crds <- cbind(tipo="variables", id=rownames(var_crds),var_crds)
  rownames(var_crds) <- NULL
  # colnames(var_crds) <- NULL
  
  obs_var_crds <- data.frame(rbind(obs_crds, var_crds))
  
  obs_var_crds[,3:ncol(obs_var_crds)] <- lapply(obs_var_crds[,3:ncol(obs_var_crds)], FUN=function(x)as.numeric(as.character(x)))
  # lapply(obs_var_crds, class)
  # View(obs_var_crds)
  
  plot(obs_var_crds[,3], obs_var_crds[,4])
  # write.csv(obs_var_crds, "obs_var_crds.csv")
  
  # ??fviz_screeplot
  fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 30))
  a <- fviz_mca_biplot(res.mca, 
                       repel = TRUE, # Avoid text overlapping (slow if many point)
                       ggtheme = theme_minimal(), addEllipses=T, title=par_text)
  # str(a)
  # a$data
  print(a)
  ###
  ### GRAFICA
  ###
  
  # graphics.off()
  # dev.new()
  
  # png("_mca.png", width=1000, height=750)
  # print(a)
  # dev.off()
  
  # ###
  # ## CLUSTERING
  # ###
  
  coords <- obs_var_crds[,c(3,4)]
  
  # coords <- cbind((a$data$x), (a$data$y))
  # 
  X.mds <- apply(coords, 2, function(x) (x-mean(x))/sd(x))
  # plot(coords)
  d <- dist(coords, method = "euclidean") # distance matrix
  maxk <- 6  # arbitrary here, you can set this to whatever you like
  k.best <- pamk(d, krange=1:maxk)$nc
  # X.means <- kmeans(X.mds, k.best)
  # k.best2 <- 3
  X.means <- kmeans(X.mds, k.best)
  
  rownames(coords) <- obs_var_crds[,2]
  
  coordenadas <- cbind(par_text, rownames(coords), coords[,1], coords[,2])
  coordenadas <- data.frame(coordenadas)
  colnames(coordenadas) <- c("escenario", "id", "x", "y")
  # View(coordenadas)
  ###
  ### GRAFICA
  ###
  
  # png("_clasificacion.png", width=1000, height=750)
  plot(coords,  col="grey", pch=19, main=par_text)
  # text(coords[,1], coords[,2], labels=rownames(coords), col=X.means$cluster, cex= 0.7)
  # dev.off()
  
  coords <- as.data.frame(coords)
  coords$clasificacion <- X.means$cluster
  coords <- data.frame(coords)
  
  ###
  ### GRAFICA
  ###
  # ??clusplot
  # png("_clust.png", width=1000, height=750)
  b <- clusplot(coords, X.means$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main=par_text)
  # dev.off()
  
  ###
  ### CLUSTERING JERARQUICO
  ###
  
  # Ward Hierarchical Clustering
  d <- dist(coords, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward.D")
  groups <- cutree(fit, k=k.best) # cut tree into 5 clusters
  # groups <- cutree(fit, k=k.best2) # cut tree into 5 clusters
  # draw dendogram with red borders around the 5 clusters
  
  ###
  ### GRAFICA
  ###
  
  # png("_hclust.png", width=1000, height=750)
  par_cex = 1
  plot(fit, main=par_text, cex=par_cex) # display dendogram
  # # rect.hclust(fit, k=k.best, border=1:k.best)
  rect.hclust(fit, k=k.best, border=1:k.best)
  # dev.off()
  
  ### SE GUARDAN EN UN DATA.FRAME LAS CLASIFICACIONES
  clasificaciones <- cbind(kmeans=coords$clasificacion, hclust=groups)
  colnames(clasificaciones) <- paste(par_text, colnames(clasificaciones), sep="_")
  # dim(clasificaciones)
  # dim(bd_mca_mice)
  
  ### SE CONCATENAN LAS CLASIFICACIONES A LA BD ORIGINAL
  # bd_mca_mice <- cbind(bd_mca_mice, clasificaciones) ## por el MOMENTO NO SE USA PORQUE SE USA UNA BD EXTENDIDA CON VARIABLES Y OBSERVACIONES, ESTA TIENE LA DIMENSION DE OBSERVACIONES UNICAMENTE
  
  
  obs_var_crds <- cbind(obs_var_crds, clasificaciones)
  # head(obs_var_crds)
  
  ###
  ### MATRIZ DE DISTANCIAS
  ###
  # ??crossdist
  # View(coords)
  coords <- round(coords, digits=4)
  crds_distm <- crossdist(coords[,1], coords[,2], coords[,1], coords[,2])
  # dim(crds_distm)
  rownames(crds_distm) <- rownames(coords)
  colnames(crds_distm) <- rownames(coords)
  tipo_var <- c(rep("obs", nrow(obs_crds)), rep("vars", nrow(var_crds)))
  crds_distm <- cbind(tipo=tipo_var, crds_distm)
  # head(crds_distm)
  # View(crds_distm)
  # write.csv(crds_distm, "crds_distm.csv")
  
  crds_distm <- data.frame(crds_distm)
  # head(crds_distm[,1:5])
  
  res_ls <- list()
  i=2
  for(i in 2:ncol(crds_distm))
  {
    res_ls[[i]] <- data.frame(colnames(crds_distm)[i],as.numeric(as.character(crds_distm[,i])))
  }
  res_df <- do.call("rbind", res_ls)
  # dim(res_df)
  # dim(crds_distm)
  crds_distm_df_tableau <- data.frame(cod_entorno=rownames(crds_distm),tipo=crds_distm[,c(1)], res_df)
  # View(crds_distm_df_tableau)
  
  ### se identifican las observaciones con "NA" presentes
  obs_var_crds <- cbind(obs_var_crds,id_na=NA)
  id_NA <- grep("NA",obs_var_crds$id)
  obs_var_crds$id_na[id_NA] <- 1
  
  
  results_ls$distance_matrix <- crds_distm_df_tableau
  results_ls$crds <- obs_var_crds
  
  return(results_ls)
}

mca_ejercicio <- fn_mca(bd=bd_mca)
