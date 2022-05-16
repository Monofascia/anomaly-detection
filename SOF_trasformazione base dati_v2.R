rm(list = ls())
library(readr)
library(ggplot2)
library(plyr)
library(data.table)
library(base)
library(datasets)
library(methods)
library(stats)
library(tibble)
library(utils)
library(factoextra)
library(broom)
library(car)
library(devtools)
library(githubinstall)
# install_github("vqv/ggbiplot")
library(ggbiplot)
library(xlsx)
library(cluster)
library(dplyr)
library(solitude)
library(ranger)
library(tidyverse)
library(mlbench)
library(caret)
library(uwot) #?
library(umap)

setwd("C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection")
df <- fread("Dataset aggiornati 18.1.21/SOF_Widiba_2021_servNoZA_new.csv", dec = ",", sep = ";", stringsAsFactors = T, 
            drop = c('DT_PROSPETTO',
                     'CD_AZIENDA',
                    # 'CD_FILIALE',
                    # 'CD_RAPPORTO_MOD',
                    # 'CD_SERVIZIO_RAPP',
                    # 'ID_CONDIZIONE',
                    # 'PG_CONDIZIONE',
                    # 'CD_SERVIZIO_ALIM',
                     'PC_TASSO_INTERESSE',
                     #'QT_SPESA_TOT',
                     'IM_SPESA_UNIT',
                     'QT_SPESA_ADD',
                     #'IM_SPESA_ADD',
                     'DT_DECOR_COND',
                     'CD_CATEG_CC_DEPO',
                    # 'SALDMEDCONC',
                    # 'CD_NDC_MOD',
                    # 'CD_NGR_MOD',
                    # 'TY_CLIENTE',
                    # 'CD_SAE',
                    # 'CD_SEDE_LEGALE',
                    # 'DS_SEDE_LEGALE',
                     'TX_SIGLA_PRV',
                    # 'DT_NASC',
                    # 'TY_SESSO',
                    # 'CL_COND_SPEC ',
                     'CD_MODELLO_SERV',
                     'DS_MODELLO_SERV'
            ))

##Costruisco un DB la cui chiave è il CD_NGR_MOD. Ad ogni step successivo creo una colonna aggregata e la inserisco nel DB finale chiave
chiave <- distinct (df, CD_NGR_MOD)

##1-Costruisco la variabile COUNT_FILIALE

df1 <- distinct(df, CD_NGR_MOD, CD_FILIALE) %>%
  count(CD_NGR_MOD, wt = NULL, sort = FALSE, name = 'COUNT_FILIALE')
chiave1 <- merge(chiave, df1, by='CD_NGR_MOD', sort = FALSE)
rm(df1)

##2-Costruisco la variabile COUNT_RAPPORTO

df2 <- distinct (df, CD_NGR_MOD, CD_RAPPORTO_MOD) %>% count(CD_NGR_MOD, wt = NULL, sort = FALSE, name = 'COUNT_RAPPORTO')
chiave2 <- merge(chiave1, df2, by='CD_NGR_MOD', sort = FALSE)
rm(df2)

##3-Costruisco la variabile COUNT_RIGHE e COUNT_RIGHE_X_RAPPORTO

df3 <- df %>% count(CD_NGR_MOD, wt = NULL, sort = FALSE, name = 'COUNT_RIGHE')
chiave3 <- merge(chiave2, df3, by='CD_NGR_MOD', sort = FALSE)
chiave3 <- mutate(chiave3,COUNT_RIGHE_X_RAPPORTO = COUNT_RIGHE/COUNT_RAPPORTO )
rm(df3)

##4-Costruisco la variabile SALDO_MEDIO

df4 <- df %>% count(CD_NGR_MOD, wt = SALDMEDCONC, sort = FALSE, name = 'SALDO_TOT')
chiave4 <- merge(chiave3, df4, by='CD_NGR_MOD', sort = FALSE)
chiave4 <- mutate(chiave4,SALDO_MEDIO = SALDO_TOT/COUNT_RIGHE )
chiave4 <- select(chiave4, -SALDO_TOT)
rm(df4)


##5/6-Costruisco la variabile SALDO_MIN e SALDO_MAX

require(dplyr)
df5 <- df %>% group_by(CD_NGR_MOD) %>% summarise(SALDO_MIN = min(SALDMEDCONC))
df6 <- df %>% group_by(CD_NGR_MOD) %>% summarise(SALDO_MAX = max(SALDMEDCONC))

chiave5 <- merge(chiave4, df5, by='CD_NGR_MOD', sort = FALSE)
chiave6 <- merge(chiave5, df6, by='CD_NGR_MOD', sort = FALSE)
rm(df5)
rm(df6)

## 6.5 - costruisco variabile im_spesa_add_MEDIO e qt_spesa_TOT_medio
df6.5 <- df %>% count(CD_NGR_MOD, wt = IM_SPESA_ADD, name = 'im_sp_tot' )
chiave6.5 <- merge(chiave6, df6.5, by='CD_NGR_MOD', sort = FALSE)
chiave6.5 <- mutate(chiave6.5,IM_SP_ADD_MEDIO = im_sp_tot/COUNT_RIGHE )
chiave6.5 <- select(chiave6.5,- im_sp_tot)

df6.75 <- df %>% count(CD_NGR_MOD, wt = QT_SPESA_TOT, name = 'tot')
chiave6.75 <- merge(chiave6.5, df6.75, by = 'CD_NGR_MOD')
chiave6.75 <- mutate(chiave6.75, QT_SP_TOT_MEDIO = tot / COUNT_RIGHE)
chiave6.75 <- select(chiave6.75, - tot)
rm(df6.75, df6.5)
##Costruisco la variabile TIPO_CLIENTE

df7 <- df %>% distinct(CD_NGR_MOD, TY_CLIENTE, TY_SESSO)
df7<- df7 %>% mutate(TIPO_CLIENTE = case_when((TY_CLIENTE == "SPF" & TY_SESSO == "M") ~ "M",
                                             (TY_CLIENTE == "SPF" & TY_SESSO == "F") ~ "F",
                                             (TY_CLIENTE== "COI") ~ "COI",
                                              TRUE ~ "ALTRO")) 
df7<- df7 %>% mutate(TIPO_CLIENTE_NUM = case_when((TY_CLIENTE == "SPF" & TY_SESSO == "M") ~ 1,
                                              (TY_CLIENTE == "SPF" & TY_SESSO == "F") ~ 2,
                                              (TY_CLIENTE== "COI") ~ 3,
                                              TRUE ~ 0)) 
df7 <- select(df7, -TY_CLIENTE,-TY_SESSO)
chiave7 <- merge(chiave6.75, df7, by='CD_NGR_MOD', sort = FALSE)
rm(df7)

##Costruisco la variabile CD_SAE

df8 <- df %>% distinct(CD_NGR_MOD, CD_SAE)
df8<- df8 %>% mutate(TIPO_SAE = case_when((CD_SAE >= 700 & CD_SAE < 800) ~ "MONDO",
                                             (CD_SAE < 700) ~ "ITALIA",
                                             TRUE ~ "ALTRO")) 
df8<- df8 %>% mutate(TIPO_SAE_NUM = case_when((CD_SAE >= 700 & CD_SAE < 800) ~ 1,
                                          (CD_SAE < 700) ~ 2,
                                          TRUE ~ 0)) 
df8 <- select(df8, -CD_SAE)
chiave8 <- merge(chiave7, df8, by='CD_NGR_MOD', sort = FALSE)
rm(df8)

##Costruisco la variabile FASCIA_ETA

df9 <- df %>% distinct(CD_NGR_MOD, DT_NASC)
df9 <- df9 %>% mutate(DT_NASC = DT_NASC %>% as.character() %>% substr(1,4))
df9 <- df9 %>% mutate(DT_NASC = DT_NASC %>% as.numeric())
df9 <- df9 %>% mutate(ETA = coalesce (2019 - DT_NASC, 0))
df9 <- df9 %>% mutate(FASCIA_ETA = case_when(ETA <= 18 ~ "UNDER18",
                                             ETA <= 30 &  ETA > 18 ~ "19-30",
                                             ETA <= 45 &  ETA > 30 ~ "31-45",
                                             ETA <= 65 &  ETA > 45 ~ "46-65",
                                             ETA >= 65 ~ "OVER65",
                                             TRUE ~ "ALTRO"))
df9 <- select(df9, -DT_NASC)
chiave9 <- merge(chiave8, df9, by='CD_NGR_MOD', sort = FALSE)
rm(df9)

##Aggiungo la variabile CL_COND_SPEC

df10 <- df %>% distinct(CD_NGR_MOD, CL_COND_SPEC)
SOF_x_NGR <- merge(chiave9, df10, by='CD_NGR_MOD', sort = FALSE)
rm(df10)

##Export nuovo dataset

#write.csv(SOF_x_NGR, "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/NGR_widiba.csv", row.names=FALSE)


rm(chiave, chiave1, chiave2, chiave3, chiave4, chiave5, chiave6, chiave6.5, chiave6.75, chiave7, chiave8, chiave9)


##Elimino le colonne non numeriche per poter applicare un modello di clustering

asd <- select(SOF_x_NGR, -TIPO_CLIENTE, -TIPO_SAE, -FASCIA_ETA)
asd[is.na(asd)] = 0
#asd <- na.omit(asd)
rownames(asd) <- asd$CD_NGR_MOD
head(asd)
asd_1 <- asd %>% mutate(ID = row_number())
asd_1 <- asd_1 %>% relocate(ID, .before = CD_NGR_MOD)
##Estraggo un campione di 10mila righe
set.seed(1)
train <- asd[sample(nrow(asd), 10000), ]
#train <- asd[1:10000,]
train_1<- train %>% mutate(ID = row_number())
train_1 <- train_1 %>% relocate(ID, .before = CD_NGR_MOD)

##### PCA ######
cor(train) # controllo correlazione
# count_righe e count_righe_x_rapp sono (ovviamente) correlati!
# stessa cosa tra i saldi
# tipo_cliente_num ed età hanno uno strano 60% di corr
train <- na.omit(train) # pulisco dt da NA
matrix0 <- data.matrix(train) # creo nuova matrice
matrix1 <- matrix0[,2:ncol(matrix0)] # creo nuova matrice senza la colonna 1
rownames(matrix1) <- train$CD_NGR_MOD # indico il nome delle righe richiamando
# la variabile (che non è) cd_ngr_mod

PCA <- prcomp(matrix1[,], center = T, scale. = T)
options(scipen = 99)
PCA # mostro i loadings
# si osserva che il pc1 pone più peso sui tipi di saldo (correlati)
# il pc2 pone più peso sui conteggi di righe e, righe x rapporto
# il pc3 su età e tipo cliente num
# il pc4 su count_filiae, rapporto
# il pc5 solo su tipo sae num
# il pc6 solo su CL_cond_spec
summary(PCA)
# ponendo un PVE (proportion of variance explained) pari a .8, si dovranno
# considerare i primi 6 gruppi che comprendono:

##### Grafici PCA #####
screeplot(PCA, type = c("lines")) #elbow a 5
graphics.off()
biplot(PCA, col = c("black", "blue"), arrow.len=0, var.axes=F) # grafico di 
# confronto PC1 e PC2

# provo nuovo plot per usare ellisse
library(devtools)
library(githubinstall)
# install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(PCA, 
         choices = c(1,2),
         #obs.scale = 1, var.scale = 1, #stessa scala per osservazioni e variabili
         labels = row.names(train),
         varname.size = 4,
         varname.abbrev = F,
         var.axes = T,
         circle = T,
         ellipse = T,
         groups = ifelse(train$COUNT_RIGHE > 30 | train$SALDO_MEDIO > 70000, "1", "0")) +
  #labs(title = "PCA", colour = "Nrow > 30 o avg_sld > 70000") +
  theme_bw() # poco chiaro
# non mi crea le frecce ma con esse potrei notare la correlazione tra le var.
# la variabilità tra i gruppi è spiegata dalle variabili che ha al suo interno/vicino
# ad es i tre tipi di saldo e il numero di righe per cliente descrviono bene la
# differenza, ma ciò è ovvio perchè sono proprio la condizione di raggruppamento 

rm(matrix0, matrix1)
##### PCA2 senza correlazioni logiche #####
train_NO_CORR <- select(train, -SALDO_MIN, 
                        -SALDO_MAX, 
                        -COUNT_RIGHE)

matrix0.1 <- data.matrix(train_NO_CORR)
matrix1.1 <- matrix0.1[,2:ncol(matrix0.1)] 
rownames(matrix1.1) <- train_NO_CORR$CD_NGR_MOD 

PCA2 <- prcomp(matrix1.1[,], center = T, scale. = T)
options(scipen = 99)
PCA2 
summary(PCA2)
screeplot(PCA2, type = c("lines")) # no elbow
graphics.off()
biplot(PCA2, col = c("black", "blue"), arrow.len=0, var.axes=F) # poco chiaro
rm(matrix0.1, matrix1.1)
##### PCA3 senza nessun tipo di correlazione ######
# uso questa!
train_HARD <- select(train, -SALDO_MIN, 
                     -SALDO_MAX, 
                     -COUNT_RIGHE, 
                     - ETA)
matrix0.2 <- data.matrix(train_HARD)
matrix1.2 <- matrix0.2[,2:ncol(matrix0.2)] 
rownames(matrix1.2) <- train_HARD$CD_NGR_MOD 

PCA3 <- prcomp(matrix1.2[,], center = T, scale. = T)
PCA3 
summary(PCA3) # FINO AL SETTIMO
screeplot(PCA3, type = c("lines")) # elbow a pc3
# var più importanti:
# pC1: count_righe_x_rapp, qt_sp_tot_medio
# PC2: count_filiale, count_rapp
# PC4: CL_COND_SPEC, TIPO_SAE
# pc5: TIPO_CLIENTE_NUM
# PC6: IM_SP_ADD_MEDIO
# PC7: SALDO_MEDIO
biplot(PCA3, col = c("black", "blue"), arrow.len=0, var.axes=F)

ggbiplot(PCA3, 
         choices = c(1,2),
         #obs.scale = 1, var.scale = 1, #stessa scala per osservazioni e variabili
         labels = row.names(train_HARD),
         varname.size = 4,
         varname.abbrev = F,
         var.axes = T,
         circle = T,
         ellipse = T,
         groups = ifelse(train$COUNT_FILIALE == 1, "1", "2")) +
  labs(title = "PCA", colour = "N. filiale") +
  theme_bw()

# non è servita, solo con le correlazioni ho ridotto le var, le PCA sono
# state inutili
# 455 7790
rm(matrix0.2, matrix1.2)
##### cluster k-means all -----
train %>% fviz_nbclust(kmeans)

km.res <- kmeans(train, 2)
fviz_cluster(km.res, train)
#clusplot(train, km.res$cluster, shade = T, lines = 0)

##### cluster k-means PCA3 ######


train_HARD %>% fviz_nbclust(kmeans) #2
km.res3 <- kmeans(train_HARD, 2)
fviz_cluster(km.res3, train_HARD)
km.res3.1 <- kmeans(train_HARD, 3)
fviz_cluster(km.res3.1, train_HARD) 
km.res3.1$centers
km.res3.2 <- kmeans(train_HARD, 4)
fviz_cluster(km.res3.2, train_HARD)
km.res3.3 <- kmeans(train_HARD, 5)
fviz_cluster(km.res3.3, train_HARD) 
km.res3.4 <- kmeans(train_HARD, 6)
fviz_cluster(km.res3.4, train_HARD)
km.res3.5 <- kmeans(train_HARD, 7)
fviz_cluster(km.res3.5, train_HARD)
km.res3.6 <- kmeans(train_HARD, 8)
fviz_cluster(km.res3.6, train_HARD) 
km.res3.7 <- kmeans(train_HARD, 9)
fviz_cluster(km.res3.7, train_HARD) 
km.res3.8 <- kmeans(train_HARD, 10)
fviz_cluster(km.res3.8, train_HARD) 

rm(km.res, km.res3, km.res3.1, km.res3.2, km.res3.3, km.res3.4,
   km.res3.5, km.res3.6, km.res3.7, km.res3.8)
##### cluster K-medoids con var selezionate da PCA3#####
library(cluster)
#gap_stat_PCA3 <- clusGap(train_HARD,
                    #FUN = pam,
                    #K.max = 10, #max clusters to consider
                    #B = 50) #total bootstrapped iterations
# gira all'infinito!!!!!!!!

#plot number of clusters vs. gap statistic
#fviz_gap_stat(gap_stat_PCA3)
#library(fpc)
#pamk.best <- pamk(train_semplificato) # ETERNO!!!!!!!!
#cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
#plot(pam(d, pamk.best$nc))

train_HARD %>% fviz_nbclust(clara)
#fviz_nbclust(train_HARD, clara, method = "wss") #non ce la fa
kmed <- clara(train_HARD, k =3) 
kmed$medoids
fviz_cluster(kmed, train_HARD) # non li becca bene
kmed4 <- clara(train_HARD, k =4) 
fviz_cluster(kmed4, train_HARD)
kmed5 <- clara(train_HARD, k =5) 
fviz_cluster(kmed5, train_HARD)
kmed6 <- clara(train_HARD, k =6) 
fviz_cluster(kmed6, train_HARD)
kmed10 <- clara(train_HARD, k =10) 
fviz_cluster(kmed10, train_HARD)
#kmed$medoids
rm(kmed, kmed4, kmed5, kmed10, kmed6)
##### Prova kmed con sole variabili significative di PCA3 ######
# solo i primi due gruppi
# IGNORARE! ! ! !  
train_PC1_e_2 <- train %>% select(COUNT_RIGHE_X_RAPPORTO, 
                                  QT_SP_TOT_MEDIO, 
                                  COUNT_FILIALE, 
                                  COUNT_RAPPORTO)
train_PC1_e_2 %>% fviz_nbclust(clara) # silhouette

kmed_e_2 <- clara(train_PC1_e_2, k =2) 
fviz_cluster(kmed_e_2, train_PC1_e_2)
kmed_e_3 <- clara(train_PC1_e_2, k =3) 
fviz_cluster(kmed_e_3, train_PC1_e_2)
kmed_e_4 <- clara(train_PC1_e_2, k =4) 
fviz_cluster(kmed_e_4, train_PC1_e_2)
kmed_e_5 <- clara(train_PC1_e_2, k=5)
fviz_cluster(kmed_e_5, train_PC1_e_2)
kmed_e_6 <- clara(train_PC1_e_2, k=6)
fviz_cluster(kmed_e_6, train_PC1_e_2)
##### lm saldo TOT e medio ----------


## da qui in poi ho cambiato data da train a ASD
# gli export su excel sono per train

# !!!!!!!!!!!!!!!!!!!!!

lm_saldo <- lm(SALDO_MEDIO ~. - SALDO_MIN - SALDO_MAX, data=asd)
options(scipen = 10)
summary(lm_saldo) 
# elimino le non significative e ripeto lm per confronto
lm_saldo_clean <- lm(SALDO_MEDIO ~ COUNT_RIGHE_X_RAPPORTO +
                       TIPO_CLIENTE_NUM + 
                       ETA +
                       IM_SP_ADD_MEDIO +
                       QT_SP_TOT_MEDIO +
                       TIPO_CLIENTE_NUM,
                     data = asd )
summary(lm_saldo_clean)
par(mfrow = c(2,2))
plot(lm_saldo_clean)
# L-H la variabilità degli errori non è costante
# H-H il q-q plot mostra una normalità degli errori
# la distribuzione degli errori non è costante e si osservano valori anomali
# nel grafico H-L si mostrano i residui standardizzati
# rispetto ai punti di leva. Si evidenziano 3 outliers ad alta leva
graphics.off()
qqPlot(lm_saldo_clean,labels=row.names(asd), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") # stampa la riga a cui si trova
# 7790 8149
influencePlot(lm_saldo_clean, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's distance") 

head(sort(abs(lm_saldo_clean$residuals), decreasing = T))
wsx <- outlierTest(lm_saldo_clean, n.max = 99999)
wsx <- names(wsx[[1]])
wsx <- data.frame(wsx)
wsx <- rename(wsx, CD_NGR_MOD = wsx)
wsx$CD_NGR_MOD <- as.integer(as.character(wsx$CD_NGR_MOD)) 

outliers_saldo <- wsx %>% inner_join(asd, by = "CD_NGR_MOD")
write.xlsx(outliers_saldo, file = "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/SOF_anomalie_ML_20220215_v2.xlsx", 
           sheetName = "lm saldo", append = T, row.names = F)


remove(wsx, lm_saldo, lm_saldo_clean)

##### lm saldo medio ~ saldo min e max -------
# SALTARE
lm_saldoMMM <- lm(SALDO_MEDIO ~ SALDO_MIN + SALDO_MAX,
                  data = train)
summary(lm_saldoMMM)
qqPlot(lm_saldoMMM,labels=row.names(train), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
# 4604 7790
influencePlot(lm_saldoMMM, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's distance")
# 4604 2374 7790 8149
outlierTest(lm_saldoMMM)

otl_sMMM <- outlierTest(lm_saldoMMM)
otl_sMMM <- names(otl_sMMM[[1]])
otl_sMMM <- data.frame(otl_sMMM)
otl_sMMM <- rename(otl_sMMM, ID = otl_sMMM)
otl_sMMM$ID <- as.integer(as.character(otl_sMMM$ID))
outliers_sMMM <- otl_sMMM %>% inner_join(train_1, by = "ID")
write.xlsx(outliers_sMMM, file = "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/SOF_anomalie_ML_20220215_v1.xlsx", 
           sheetName = "lm intervalli saldo", append = T, row.names = F)
rm(otl_sMMM, lm_saldoMMM)
##### lm qt spesa media -----------
lm_qt_sp_media <- lm(QT_SP_TOT_MEDIO ~., data=asd[, 2:ncol(asd)])
summary(lm_qt_sp_media)
cor(train, y=asd$QT_SP_TOT_MEDIO)
par(mfrow = c(2,2))
plot(lm_qt_sp_media)
head(sort(abs(lm_qt_sp_media$residuals), decreasing = T))
library(broom)
library(car)
graphics.off()

qqPlot(lm_qt_sp_media,labels=row.names(asd), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") # stampa nella console il codice NGR
# e sotto la riga a cui si trova
# 3835     3989  
influencePlot(lm_qt_sp_media, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's distance") 

otl_qt <- outlierTest(lm_qt_sp_media, n.max = 99999)
otl_qt <- names(otl_qt[[1]])
otl_qt <- data.frame(otl_qt)
otl_qt <- rename(otl_qt, ID = otl_qt)
otl_qt$ID <- as.integer(as.character(otl_qt$ID))
outliers_qt_sp <- otl_qt %>% inner_join(asd_1, by = "ID")
write.xlsx(outliers_qt_sp, file = "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/SOF_anomalie_ML_20220215_v2.xlsx", 
           sheetName = "lm qt", append = T, row.names = F)
rm(otl_qt, lm_qt_sp_media, rsd)
##### lm con count_righe  -----------
lm_count_rows_x_rapp <- lm(COUNT_RIGHE_X_RAPPORTO ~., data=asd[, 2:ncol(asd)])
summary(lm_count_rows_x_rapp)
#@
cor(asd, y= asd$COUNT_RIGHE_X_RAPPORTO)
lm_count_rows_x_rapp1 <- lm(COUNT_RIGHE_X_RAPPORTO ~. - COUNT_RIGHE - SALDO_MIN
                            - SALDO_MAX ,
                            data=asd[, 2:ncol(asd)])
summary(lm_count_rows_x_rapp1)
par(mfrow = c(2,2))
plot(lm_count_rows_x_rapp1)
head(sort(abs(lm_count_rows_x_rapp1$residuals), decreasing = T))
rsd2 <- augment(lm_count_rows_x_rapp1, asd) # ignorare
rsd2 <- mutate(rsd, abs = abs(.resid))

graphics.off()
qqPlot(lm_count_rows_x_rapp1,labels=row.names(asd), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") # stampa la riga a cui si trova
# 3989 8149
influencePlot(lm_count_rows_x_rapp1, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's distance")

otl_n_rows <- outlierTest(lm_count_rows_x_rapp1, n.max = 99999)
otl_n_rows <- names(otl_n_rows[[1]])
otl_n_rows <- data.frame(otl_n_rows)
otl_n_rows <- rename(otl_n_rows, ID = otl_n_rows)
otl_n_rows$ID <- as.integer(as.character(otl_n_rows$ID))
outliers_n_rows <- otl_n_rows %>% inner_join(asd_1, by = "ID")
write.xlsx(outliers_n_rows, file = "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/SOF_anomalie_ML_20220215_v2.xlsx", 
           sheetName = "lm rows", append = T, row.names = F)



rm(otl_n_rows, lm_count_rows_x_rapp, lm_count_rows_x_rapp1, rsd2)

##### GLM #######
# per usare anche variabili non numeriche
attach(SOF_x_NGR)
glmm <- glm(SALDO_MEDIO ~. - SALDO_MIN - SALDO_MAX - CD_NGR_MOD 
            - COUNT_RIGHE - COUNT_RAPPORTO
            - TIPO_CLIENTE_NUM - TIPO_SAE_NUM - FASCIA_ETA, 
    data = SOF_x_NGR[sample(nrow(SOF_x_NGR), 10000), ])
summary(glmm)
1-(deviance(glmm)/glmm$null.deviance) # bad
library(broom)
rsd_glm_saldo <- augment(glmm, glmm$data) # ignorare
rsd_glm_saldo <- mutate(rsd_glm_saldo, abs_rsd = abs(.resid))
rsd_glm_saldo <- rsd_glm_saldo[order(rsd_glm_saldo$abs_rsd, decreasing = T),]
outliers_glm <- outlierTest(glmm, n.max = 99999) 
outliers_glm <- names(outliers_glm[[1]])
outliers_glm <- data.frame(outliers_glm)
outliers_glm <- rename(outliers_glm, ID = outliers_glm)
outliers_glm$ID <- as.integer(as.character(outliers_glm$ID))
outliers_glm <- outliers_glm %>% inner_join(train_1, by = "ID")
# write.xlsx()

par(mfrow = c(2,2))
plot(glmm)
detach(SOF_x_NGR)

rm(glmm, rsd_glm_saldo)

##### SVM #####
library(caret)
library(e1071)
mean(SOF_x_NGR$SALDO_MEDIO)
training.sample <- createDataPartition(SOF_x_NGR$SALDO_MEDIO, p = .8, list = F)
train_SVM <- SOF_x_NGR[training.sample, ]
test_SVM <- SOF_x_NGR[-training.sample, ]
train_SVM <- train_SVM %>% mutate(SALDO_MEDIO_LOG = ifelse(SALDO_MEDIO >= mean(SOF_x_NGR$SALDO_MEDIO),
                                         "1", "0"))
train_SVM$SALDO_MEDIO_LOG <- as.numeric(as.character(train_SVM$SALDO_MEDIO_LOG))
test_SVM <- test_SVM %>% mutate(SALDO_MEDIO_LOG = ifelse(SALDO_MEDIO >= mean(SOF_x_NGR$SALDO_MEDIO),
                                                           "1", "0"))
test_SVM$SALDO_MEDIO_LOG <- as.numeric(as.character(test_SVM$SALDO_MEDIO_LOG))
svmfit_L = svm(SALDO_MEDIO_LOG ~ ., data = train_SVM, kernel = "linear", cost = 10, scale = FALSE)
# gira forever
print(svmfit_L)
svmfit_R = svm(SALDO_MEDIO_LOG ~ ., data = train_SVM, kernel = "radial", cost = 10, scale = FALSE)
print(svmfit_R)
##### iForest: anomaly detection -----
library(solitude)
library(ranger)
library(tidyverse)
library(mlbench)
library(caret)
library(uwot) #?
library(umap) #?
iso = isolationForest$new()

###### |- train e test ########
# splitting ---------- ignorare
set.seed(1)
training.sample <- createDataPartition(asd$SALDO_MEDIO, p = .8, list = F)
train_iF <- asd[training.sample, ]
test_iF <- asd[-training.sample, ]
train_iF[is.na(train_iF)] = 0
test_iF[is.na(test_iF)] = 0

iso$fit(train_iF)
scores_train = train_iF %>% 
  iso$predict() %>%
  arrange(desc(anomaly_score))
scores_train

umap_train = train_iF %>%
  scale() %>%
  uwot::umap() %>%
  setNames(c("V1", "V2")) %>%
  as_tibble() %>%
  rowid_to_column() %>%
  left_join(scores_train, by = c("rowid" = "id")) # unisco per id
umap_train <- umap_train %>% 
  mutate(outliers = as.factor(ifelse(umap_train$anomaly_score >= 0.6, "outliers", "normal")))
ggplot(umap_train, aes(x = V1, y = V2, color = outliers)) + 
  geom_point(shape = 8, size = 5) + 
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend") # non chiaro
# riprovo usando intervallo più elevato >= 0.7 (altamente probabile)
umap_train <- umap_train %>% 
  mutate(outliers = as.factor(ifelse(umap_train$anomaly_score >= 0.7, "outliers", "normal")))
ggplot(umap_train, aes(x = V1, y = V2, color = outliers)) + 
  geom_point(shape = 8, size = 5) + 
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")

# test ---------- ignorare
scores_test = test_iF %>%
  iso$predict() %>%
  arrange(desc(anomaly_score))

umap_test = test_iF %>%
  scale() %>%
  uwot::umap() %>%
  setNames(c("V1", "V2")) %>%
  as_tibble() %>%
  rowid_to_column() %>%
  left_join(scores_test, by = c("rowid" = "id"))

umap_test <- umap_test %>% 
  mutate(outliers = as.factor(ifelse(umap_test$anomaly_score >= 0.6, "outliers", "normal")))
ggplot(umap_test, aes(x = V1, y = V2, color = outliers)) + 
  geom_point(shape = 8, size = 5) + 
  labs(x = "x", y = "y") +
  labs(alpha = "", colour="Legend")

rm(umap_test, umap_train, scores_train, scores_test, test_iF, train_iF, training.sample,
   scores_train)
##### ALL ######
# lo provo su tutto, senza suddivisione in train e test
# usato questo su ppt
iso$fit(asd)
scores = asd %>% 
  iso$predict() %>%
  arrange(desc(anomaly_score))

scores95 <- quantile(scores$anomaly_score, probs = 0.95) # 95esimo percentile
umap_tot = asd %>%
  scale() %>%
  uwot::umap() %>%
  setNames(c("V1", "V2")) %>%
  as_tibble() %>%
  rowid_to_column() %>%
  left_join(scores, by = c("rowid" = "id")) # unisco per id
umap_tot <- umap_tot %>% 
  mutate(outliers = as.factor(ifelse(umap_tot$anomaly_score >= scores95, "outliers", "normal")))
ggplot(umap_tot, aes(x = V1, y = V2, color = outliers)) + 
  geom_point(shape = 1, size = 2) + 
  labs(x = "V1", y = "V2") +
  labs(alpha = "", colour="Legend")

# stampo outliers >= 95%
asd_id <- asd %>% mutate(id = row_number())
asd_id <- asd_id %>% relocate(id, .before = CD_NGR_MOD)
outliers_iF <- scores %>% 
  inner_join(asd_id, by = "id") %>%
  subset(scores$anomaly_score >= scores95) %>%
  arrange(desc(anomaly_score))
# i valori appartenenti al 95esimo e 100esimo percentile sono tanti



##### iForest senza nessuna correlazione #####
# provo dataset senza correlazioni 
asd_HARD <- select(asd, -SALDO_MIN, 
                   -SALDO_MAX, 
                   -COUNT_RIGHE, 
                   - ETA)
asd_HARD[is.na(asd_HARD)] = 0 # pongo NA uguali a zero
iso$fit(asd_HARD)
scores_HARD = asd_HARD %>% 
  iso$predict() %>%
  arrange(desc(anomaly_score))
scores_HARD95 <- quantile(scores_HARD$anomaly_score, probs = 0.95) # 95esimo percentile
umap_totH = asd_HARD %>%
  scale() %>%
  uwot::umap() %>%
  setNames(c("V1", "V2")) %>%
  as_tibble() %>%
  rowid_to_column() %>%
  left_join(scores_HARD, by = c("rowid" = "id")) # unisco per id, lento
umap_totH <- umap_totH %>% 
  mutate(outliers = as.factor(ifelse(umap_totH$anomaly_score >= scores_HARD95, "outliers", "normal")))
ggplot(umap_totH, aes(x = V1, y = V2, color = outliers)) + 
  geom_point(shape = 1, size = 2) + 
  labs(x = "V1", y = "V2") +
  labs(alpha = "", colour="Legend")

asd_HARD <- asd_HARD %>% mutate(id = row_number())
asd_HARD <- asd_HARD %>% relocate(id, .before = CD_NGR_MOD)
outliers_iFH <- scores_HARD %>% 
  inner_join(asd_HARD, by = "id") %>%
  subset(scores_HARD$anomaly_score >= scores_HARD95) %>%
  arrange(desc(anomaly_score))
# i valori appartenenti al 95esimo e 100esimo percentile sono troppi
# scrivo i primi 25 per probabilità
outliers_iF <- list(outliers_iF, outliers_iFH) %>%
  reduce(inner_join, by = "CD_NGR_MOD") # solo quelle in comune tra i due
outliers_iF <- outliers_iF[, -c(18:29) ] # colonne non necessarie droppate
write.xlsx(head(outliers_iF,25), file = "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/SOF_anomalie_ML_20220215_v2.xlsx", 
           sheetName = "Isolation Forset", append = T, row.names = F) 

rm(scores, umap_tot, scores95, asd_id, asd_HARD, umap_totH, scores_HARD, scores_HARD95)


##### Valori comuni tra outliers modelli #####
out_com <- list(#outliers_glm,
                outliers_qt_sp,
                outliers_saldo,
                outliers_n_rows,
                outliers_iFH,
                outliers_iF)%>%
  reduce(inner_join, by = "CD_NGR_MOD")
out_com <- out_com[,2:15]
write.xlsx(out_com, file = "C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection/Marzia requests/SOF_anomalie_ML_20220215_v2.xlsx", 
           sheetName = "OUTLIERS IN COMUNE", append = T, row.names = F) 

##### NN ######
library(keras)
#library(mlbench)
#library(magrittr)
library(neuralnet)
nn=neuralnet(SALDO_MEDIO ~ COUNT_RIGHE_X_RAPPORTO +
               TIPO_CLIENTE_NUM + 
               ETA +
               IM_SP_ADD_MEDIO +
               QT_SP_TOT_MEDIO +
               TIPO_CLIENTE_NUM,
             data = train, 
             hidden= c(6,3),
             lifesign = "full",
             linear.output = T,
             rep=1)

plot(nn,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

data <- as.matrix(train)
dimnames(data) <- NULL
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
training <- data[ind==1,1:6]
test <- data[ind==2, 1:13]
trainingtarget <- data[ind==1, 14]
testtarget <- data[ind==2, 14]
str(trainingtarget)
str(testtarget)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 6, activation = 'relu', input_shape = c(6)) %>%
  layer_dense(units = 6, activation = 'relu') %>%
  layer_dense(units = 1)

model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop', 
                  metrics = list('accuracy')) 
mymodel <- model %>%          
  fit(training,trainingtarget,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)
# automatizzare estrazione outliers modello da pca 
#(non provato perchè gruppi non risoutivi)

# creare excel OK
# creare slide come nextbit su qnt fatto!!



df %>% subset(df$CD_NGR_MOD %in% 296721960) %>% view()
df4 %>% subset(df4$CD_NGR_MOD %in% 296721960) %>% view()

