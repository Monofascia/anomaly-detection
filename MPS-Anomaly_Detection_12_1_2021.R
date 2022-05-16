library(readr)
library(ggplot2)
library(dplyr)
#library(plyr)
library(data.table)
library(base)
library(knitr)
library(datasets)
library(methods)
library(stats)
library(tibble)
library(utils)
library(readxl)
library(xlsx)
library(cluster)
setwd("C:/Users/Patrizio.Iezzi/Desktop/TUTTO/MPS-Anomaly Detection")
#df <- fread("SOF_Widiba_2021_new.csv", dec = ",", sep = ";", stringsAsFactors = T, drop = c("CD_SERVIZIO_RAPP", "CD_AZIENDA"))
df <- fread("Dataset aggiornati 18.1.21/SOF_Widiba_2021_servNoZA_new.csv", 
            stringsAsFactors = T, drop = c("DT_PROSPETTO", "CD_AZIENDA", 
                                           "CD_MODELLO_SERV", "DS_MODELLO_SERV"),
            dec = ",",
            sep=";",
            na.strings = "")
options(scipen = 10)
df<- df %>% mutate(ID = row_number()) # aggiungo index riga
df <- df %>% relocate(ID, .before = CD_FILIALE) # sposto index riga alla prima colonna
#df = df[, c(ncol(df), seq(1,ncol(df)-1))]
#df = subset(df, df$DT_PROSPETTO >= "2021-12-31") # solo anno 2021
View(df)
# write.csv(df,"C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\SOF_Widiba_2021_new.csv", row.names=FALSE)
 

##### Analisi esplorativa base #####

# Data di riferimento del Prospetto SOF
df%>% count(DT_PROSPETTO) # DROPPED: tutto 2021

##### CD_AZIENDA #####
# Codice Azienda (1030=MPS; 3442=Widiba)
df %>% count(CD_AZIENDA) # DROPPED: solo 3442 

##### CD_FILIALE #####
# Codice Filiale del rapporto CC
df %>% count(CD_FILIALE) 
ggplot(df, aes(x=CD_FILIALE))+
    geom_histogram(fill ="violet")


##### CD_RAPPORTO_MOD #####
# Codice Rapporto CC (dato anonimizzato)
df %>% count(CD_RAPPORTO_MOD, sort = T)


##### CD_SERVIZIO_RAPP #####
# Codice Servizio (CC=Conto Corrente, CA= carte di credito)
df %>% count(CD_SERVIZIO_RAPP)
ggplot(df, aes(x=CD_SERVIZIO_RAPP))+
  geom_histogram(fill = "orange", stat = "count") 


##### ID_CONDIZIONE #####
# Condizione di spesa applicata alla singola operazione
id_cond_count <- df %>% count(ID_CONDIZIONE, sort = T) # esistono valori ripetuti


##### PG_CONDIZIONE #####
# Progressivo della condizione di spesa applicata
pg_cond_count <- df %>% count(PG_CONDIZIONE)
ggplot(df, aes(x=PG_CONDIZIONE))+
  geom_histogram(binwidth = 1, fill = "red", color = "black") + 
  coord_cartesian(xlim = c(0,73), ylim = c(0,100)) # zoom su primi 73
 
ggplot(df, aes(x=PG_CONDIZIONE))+
  geom_histogram(binwidth = 1, fill = "red", color = "black") + 
  coord_cartesian(xlim = c(999, 1005), ylim = c(0,100)) # zoom da 1000 in poi


##### CD-SERVIZIO_ALIM #####
# Sigla Tecnica Servizio Alimentante, servizio di riferimento delle condizione di spesa applicata
df %>% count(CD_SERVIZIO_ALIM) # su  DescrizioneCampi_Sample_SOF descrizione voci

##### PC_TASSO_INTERESSE #####
# Percentuale Tasso di Interesse
df %>% count(PC_TASSO_INTERESSE, sort = T) %>% view()  
ggplot(df, aes(x=PC_TASSO_INTERESSE))+
  geom_bar(fill = "red") +
  coord_cartesian(ylim = c(0,1000)) 


##### QT_SPESA_TOT #####
# Numero Operazioni Spesa Totali
df %>% count(QT_SPESA_TOT) # no negativi

##### IM_SPESA_UNIT #####
# Importo Spesa /Commissione Unitaria
df %>% count(IM_SPESA_UNIT) # no negativi

##### QT_SPESA_ADD #####
# Numero Operazioni Spesa Addebitate
df %>% count(QT_SPESA_ADD) # no negativi

##### IM_SPESA_ADD #####
# Importo Spesa /Commissione Addebitate
df %>% count(IM_SPESA_ADD) # ci sono negativi!

##### DT_DECOR_COND #####
# Data decorrenza tasso di interesse
df %>% count(DT_DECOR_COND) # moda a 0001-01-01, normale?

##### CD_CATEG_CC_DEPO #####
# Codice Categoria del Conto Corrente
df %>% count(CD_CATEG_CC_DEPO, sort = T) # a cosa serve?
# table(df$CD_CATEG_CC_DEPO)

##### SALDMEDCONC #####
# Saldo Medio Mensile del Conto Corrente
saldomed_count <- df %>% count(SALDMEDCONC , sort = T) # ci sono negativi!


##### CD_NDC_MOD #####
# Numero di Cliente a livello di Azienda (dato anonimizzato)
df %>% count(CD_NDC_MOD , sort = T)

##### CD_NGR_MOD #####
# Numero di Cliente a livello di Gruppo (dato anonimizzato)
df %>% count(CD_NGR_MOD , sort = T) # è la stessa cosa di sopra

##### TY_CLIENTE #####
# Tipo Cliente (SPF=Singole Persone Fisiche; COI=Cointestazioni)
df %>% count(TY_CLIENTE , sort = T) # ci sono 55 vuoti

##### CD_SAE #####
# Codice SAE
df %>% count(CD_SAE , sort = T) # 6 tipi
CD_SAE_factor <- as.factor(df$CD_SAE)
SAE <- ggplot(df, aes(x=CD_SAE_factor)) + 
  geom_bar(aes(fill = CD_SAE_factor)) + 
  coord_cartesian(ylim = c(0,2000)) +
  labs(x = "Codici SAE",
       fill = "CD_SAE",
       y = "Frequenze con zoom sotto 2000")
SAE # ci sono NA: 55


##### CD_SEDE_LEGALE #####
# Codice Sede Legale codifica Bankit
df %>% count(CD_SEDE_LEGALE) # 55 NA
# numerosità = 6369 

##### DS_SEDE_LEGALE #####
# Descrizione Sede Legale
df %>% count(DS_SEDE_LEGALE) # 55 NA
# numerosità = 6365

##### TX_SIGLA_PRV #####
# Sigla Provincia Sede Legale
df %>% count(TX_SIGLA_PRV, sort = T) # ci sono molti NA

##### DT_NASC #####
# Data di Nascita (solo per SPF)
df %>% count(DT_NASC, sort = T) # ci sono molti na

##### TY_SESSO #####
# Genere (solo per SPF)
df %>% count(TY_SESSO, sort = T) # num NA uguale a DT_NASC ok

##### CL_COND_SPEC #####
df %>% count(CL_COND_SPEC) # 1 e 2 presentano condizioni speciali

##### CD_MODELLO_SERV #####
df %>% count(CD_MODELLO_SERV) # DROPPED: solo ND

##### DS_MODELLO_SERV #####
df %>% count(DS_MODELLO_SERV) # DROPPED: solo ND

##### (fatto) Analisi condizione C1 #####
# C1 = IM_SPESA_UNIT * QT_SPESA_ADD = IM_SPESA_ADD
C1_all<- df %>% mutate(C1 = ifelse(IM_SPESA_UNIT*QT_SPESA_ADD == IM_SPESA_ADD, TRUE, FALSE))
C1_all %>% count(C1)

ID_not2consider = c(seq(272,287), 394,397) # non sono da escludere
C1_all$C1[which(C1_all$ID_CONDIZIONE %in% ID_not2consider)] = NA

C1_all %>% count(C1)
sum(C1_all$C1 == FALSE, na.rm = T)
sum(C1_all$C1 == FALSE, na.rm = T) / nrow(C1_all) # tasso di errore
sum(C1_all$C1 == T, na.rm = T) / nrow(C1_all) # corrette
C1_all_FALSE <- subset(C1_all, C1_all$C1 == F )
options(scipen = 999)
C1_all_FALSE <- C1_all_FALSE %>% 
  mutate(scarto = IM_SPESA_ADD - IM_SPESA_UNIT * QT_SPESA_ADD)
C1_all_FALSE$scarto = abs(C1_all_FALSE$scarto) # valore assoluto#
C1_all_FALSE <- subset(C1_all_FALSE, C1_all_FALSE$scarto > 0.001) # epsilon
table(C1_all_FALSE$C1)
sum(C1_all_FALSE$C1 == F, na.rm = T) / nrow(C1_all)
#C1_all_FALSE_200 <- sample_n(C1_all_FALSE, size = 300 )
write.csv(C1_all_FALSE, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_12.1.21\\anomalia C1\\anomalia_C1.csv", row.names=FALSE)

#C1_lower <- df %>% mutate(C1 =ifelse(IM_SPESA_UNIT*QT_SPESA_ADD < IM_SPESA_ADD, TRUE, FALSE))
#C1_lower %>% count(C1)
#C1_upper <- df %>% mutate(C1 =ifelse(IM_SPESA_UNIT*QT_SPESA_ADD > IM_SPESA_ADD, TRUE, FALSE))
#C1_upper %>% count(C1)
#sum(C1_all$C1 == FALSE) == 
#  (sum(C1_lower$C1 == TRUE)) + (sum(C1_upper$C1 == T)) # verifica

##### Valori vuoti di sesso (TY_SESSO) numerosi come i valori vuoti di nascita (DT_NASC) #####
df %>% count(TY_SESSO)
ty_sesso_na <- sum(is.na(df$TY_SESSO)) # salvo somma di valori vuoti
table(ty_sesso_na == sum(is.na(df$DT_NASC))) # la confronto con DT_NASC=""
# ok

##### Valori vuoti di nascita e sesso confrontati con tipo cliente (TY_CLIENTE) #####
df %>% count(TY_CLIENTE)
length(which(df$TY_CLIENTE == "COI"))
length(which(df$TY_CLIENTE %in% c("COI", "")))
ty_cliente_NA <- length(which(is.na(df$TY_CLIENTE)))


table(ty_sesso_na == length(which(df$TY_CLIENTE == "COI"))) # falso perchè TY_CLIENTE ha valori vuoti
table(ty_sesso_na == length(which(df$TY_CLIENTE == "COI")) + ty_cliente_NA) # ok
table(sum(length(which(df$TY_CLIENTE == "COI")) + ty_cliente_NA) == sum(is.na(df$DT_NASC))) # ok 

##### (fatto) Analisi righe DS SEDE LEGALE e CD SEDE LEGALE #####
cd_e_ds <- fread("Dataset aggiornati 18.1.21/SOF_Widiba_2021_servNoZA_new.csv", 
            stringsAsFactors = T, 
            select = c("CD_SEDE_LEGALE", "DS_SEDE_LEGALE"),
            sep=";",
            na.strings = "")

CD_DS_sede <- distinct(cd_e_ds)
CD_DS_sede <- CD_DS_sede %>% 
  group_by(DS_SEDE_LEGALE) %>% 
  summarize(count=n())
errati_ds_sede_leg <- subset(df, df$DS_SEDE_LEGALE %in% c("CALLIANO","SAN TEODORO","LIVO","CASTRO"))
errati_ds_sede_leg_distinti <- distinct(errati_ds_sede_leg, errati_ds_sede_leg$CD_SEDE_LEGALE, errati_ds_sede_leg$DS_SEDE_LEGALE)
# la stessa sede presenta due codici differenti, dovrebbero essere univoci
write.csv(errati_ds_sede_leg_distinti, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_12.1.21\\anomalia DS SEDE LEGALE e CD SEDE LEGALE\\anomalia_DS_e_CD_sede_legale.csv", row.names=FALSE)


##### Analisi under 30, tdi atteso pari a zero  #####
# Df per analisi età under 30 
df$Y_NASC<-format(as.Date(df$DT_NASC), "%Y") # creo colonna anni
df %>% count(Y_NASC>1991) # under 30
under30<-subset(df, df$Y_NASC>1991) # creo dataset solo con under 30
under30 %>% count(TY_CLIENTE) # solo SPF
max(under30$PC_TASSO_INTERESSE) # valore anomalo
ggplot(under30, aes(PC_TASSO_INTERESSE, Y_NASC)) + 
  geom_jitter(width = .5, size=1)
 
ggplot(under30, aes(IM_SPESA_ADD, Y_NASC)) + 
  geom_jitter(width = .5, size=1) # ci sono outliers 

under30 %>% count(IM_SPESA_ADD)                    
under30 %>% count(PC_TASSO_INTERESSE, sort = T)
under30 %>% count(PC_TASSO_INTERESSE>0) # correntisti con tdi >0
under30_tdi_NO_ZERO <- subset(under30, under30$PC_TASSO_INTERESSE>0)


sum(under30$PC_TASSO_INTERESSE>0) / nrow(df)*100 # incidenza percentuale sul totale
sum(under30$PC_TASSO_INTERESSE>0) / nrow(under30)*100 # incidenza percentuale su under30
write.csv(under30_tdi_NO_ZERO, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_12.1.21\\anomalia_under30\\anomalia_under30.csv", row.names=FALSE)

##### (fatto) Analisi correntisti con IM_SPESA_ADD sopra i 1000 #####
df %>% count(IM_SPESA_ADD>1000) # sono 156
im_spesa_add_1000piu <- subset(df, df$IM_SPESA_ADD>1000) 
im_spesa_add_1000piu %>% count(CL_COND_SPEC) # c'è un numero 1, eccezione
im_spesa_add_1000piu %>% count(CD_SERVIZIO_RAPP) # solo CC
im_spesa_add_1000piu %>% count(CD_SERVIZIO_ALIM) 
im_spesa_add_1000piu %>% count(IM_SPESA_UNIT)
im_spesa_add_1000piu %>% count(SALDMEDCONC)
ggplot(im_spesa_add_1000piu, aes(x=IM_SPESA_ADD,y=SALDMEDCONC)) + 
  geom_jitter(width = .5, size=1)

##### Analisi under 30 e ID_condizione #####
summary(under30)
under30 %>% count(ID_CONDIZIONE, sort = T)
hist(under30$ID_CONDIZIONE, breaks = 1000) # non si può dire nulla

##### Analisi under30 e SAE #####
under30 %>% count(CD_SAE, sort = T)
under30$CD_SAE <- as.factor(as.character(under30$CD_SAE))
ggplot(under30, aes(CD_SAE)) +
  geom_bar(fill="gold2") + 
  coord_cartesian(ylim = c(0,50))
# CD_SAE: 
# 600 FAMIGLIE CONSUMATRICI
# 615 ALTRE FAMIGLIE PRODUTTRICI
# 773 FAMIGLIE CONSUMATRICI UE/UM
# 774 FAMIGLIE CONSUMATRICI UE/NO UM 
# 775 FAMIGLIE CONSUMATRICI PAESI NON UE (no ???)
under30 %>% count(CD_SERVIZIO_ALIM)
ggplot(under30, aes(CD_SERVIZIO_ALIM)) +
  geom_bar(fill="gold3") +
  coord_cartesian(ylim = c(0,500))
# tutto nella norma

##### Analisi over 65, tdi atteso diverso da zero ########
# Df per analisi età over 65 
df %>% count(Y_NASC<1956) # over 65
over65<-subset(df, df$Y_NASC<1956) # creo dataset solo con over 65
max(over65$PC_TASSO_INTERESSE)
b<-ggplot(over65, aes(PC_TASSO_INTERESSE, Y_NASC)) 
b + geom_jitter(width = .5, size=1)

##### Analisi conti correnti SAE: esteri #####
RdM <-subset(df, df$CD_SAE>700) # resto del mondo
table(RdM$CD_SAE) # da 772 a 775
table(RdM$TX_SIGLA_PRV == "")
ggplot(RdM, aes(CD_SAE,TX_SIGLA_PRV)) +
  geom_jitter(width=.5, size=1) # sono tutti 775 gli errati

RdM_not_empty <- subset(RdM, RdM$TX_SIGLA_PRV != "") # db con errori
RdM_not_empty %>% count(TX_SIGLA_PRV) # solo MI e MO
nrow(RdM_not_empty) # incidenza tot
nrow(RdM_not_empty) / nrow(df) # incidenza tot in percentuale
nrow(RdM_not_empty) / nrow(RdM) # incidenza su SAE esteri in percentuale
RdM_not_empty %>% count(CD_NDC_MOD) # numero di cliente aziendae con codici SAE esteri e province italiane
RdM_not_empty %>% count(CD_NGR_MOD) # soggetti gruppo con codici SAE esteri e province italiane


write.csv(RdM_not_empty, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_12.1.21\\anomalia SAE\\SAE_esteri.csv", row.names=FALSE)




##### Saldo medio mensile pari a zero confrontato con spese #####
saldo_medio_mensile <- subset(df, df$SALDMEDCONC==0)
table(cut(saldo_medio_mensile$IM_SPESA_ADD, breaks = c(-7,-0.1,0,5,10,100,500,2000, Inf))) # Importo Spesa /Commissione Addebitate
table(cut(saldo_medio_mensile$IM_SPESA_UNIT, breaks = c(0,1,5,10,50, Inf))) # Importo Spesa /Commissione Unitaria
table(cut(saldo_medio_mensile$QT_SPESA_ADD, breaks = c(0,1,5,10,50, Inf)))# Numero Operazioni Spesa Addebitate
table(cut(saldo_medio_mensile$QT_SPESA_ADD, breaks = c(0,1,5,10,50, Inf)))# Numero Operazioni Spesa Addebitate

##### Analisi distribuzione TdI #####
prop_tdi <- prop.table(table(df$PC_TASSO_INTERESSE))
prop_tdi <- as.data.frame(prop_tdi)
prop_tdi$Var1 <- as.numeric(as.character(prop_tdi$Var1))
prop_tdi <- prop_tdi[order(prop_tdi$Freq, decreasing = T),]
prop_tdi %>% head() %>% mutate(Freq = Freq*100) # trasformato in % ma non me lo salva
prop_tdi$groups <- cut(prop_tdi$Var1, 
                       breaks = c(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))
table(prop_tdi$groups) # freq per raggruppamenti
ggplot(prop_tdi, aes(x=groups, y=Freq)) +
  geom_jitter() + 
  coord_cartesian(ylim = c(0,0.001))
ggplot(prop_tdi, aes(x=groups, y=Freq)) +
  geom_jitter() + 
  coord_cartesian(ylim = c(0.000125,0.0005), xlim = c(12,20))


# decido di porre come anomali tutti quei tdi presenti meno di 0.0003 (0.03%)
# quindi quelli presenti con una numerosità inferiore agli 800
 
pc_tdi_anomali <- subset(df, df$PC_TASSO_INTERESSE != 0 & 
                           df$PC_TASSO_INTERESSE != 8.8 &
                           df$PC_TASSO_INTERESSE != 13.6) 

pc_tdi_anomali <- sample_n(pc_tdi_anomali, size = 1000)
write.csv(pc_tdi_anomali, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA\\anomalia tdi\\PC_TASSO_INTERESSE_anomali.csv", row.names=FALSE)


##### (fatto) Analisi IM_SPESA_ADD negativo #####
table(df, df$IM_SPESA_ADD)
IM_SPESA_ADD_NEG <- subset(df, df$IM_SPESA_ADD<0)
write.csv(IM_SPESA_ADD_NEG, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA\\anomalie IM_SPESA_ADD_NEGATIVO\\IM_SPESA_ADD_NEG.csv", row.names=FALSE)

##### (fatto) Analisi IM_SPESA_UNIT negativo #####
df %>% count(IM_SPESA_UNIT)
df %>% count(IM_SPESA_UNIT<0) # nessun negativo

##### Analisi SALDOMEDCONC negativo #####
saldomed_negativi <- subset(df, df$SALDMEDCONC<0)
saldomed_negativi %>% count(IM_SPESA_ADD)

##### Analisi conteggio distinto di tdi e spese addebitate per condizione #####
id_cond_groupby_id_tdi <- df %>%  
  group_by(ID_CONDIZIONE, PC_TASSO_INTERESSE) %>%
  summarize(count=n()) %>%
  ungroup()

id_cond_groupby_id_tdi2 <- id_cond_groupby_id_tdi %>%  
  group_by(ID_CONDIZIONE) %>%
  summarize(count=n()) %>%
  ungroup()

anomalie_id_tdi <- subset(df, df$ID_CONDIZIONE == c(275,276,277))
write.csv(anomalie_id_tdi, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA\\Anomalie ID_COND con differenti tdi\\anomalie_id_tdi.csv", row.names=FALSE)
View(id_cond_groupby_id_tdi)
##
id_cond_groupby_id_spesa <- df %>%  
  group_by(ID_CONDIZIONE, IM_SPESA_UNIT) %>%
  summarize(count=n()) %>%
  ungroup()

id_cond_groupby_id_spesa2 <- id_cond_groupby_id_spesa %>%  
  group_by(ID_CONDIZIONE) %>%
  summarize(count=n()) %>%
  ungroup()

View(id_cond_groupby_id_spesa2)



##### Analisi anomalia su saldo a parità di rapporto #####
saldomed_groupby <- df %>% 
  group_by(CD_FILIALE, CD_RAPPORTO_MOD, SALDMEDCONC) %>% 
  summarize(count=n())
saldomed_groupby2 <- saldomed_groupby %>%  
  group_by(CD_FILIALE, CD_RAPPORTO_MOD) %>%
  summarize(count=n()) %>%
  ungroup()

View(saldomed_groupby2) # todo bien

##### (fatto) Analisi ID_CONDIZIONE =! CD_SERVIZIO_ALIM #####

# su widiba condizioni intorno al 400 con servizio alimentato diverso da quello
# presente sul dataset ID_condizione =! Cd_servizio_alim 
# (stando alla tab SOF_domini)
ID_cond_VS_CD_SERV_ALIM <- fread("SOF_Widiba_2021_new.csv", stringsAsFactors = T, 
                                 select = c("ID_CONDIZIONE", "CD_SERVIZIO_ALIM"))
str(ID_cond_VS_CD_SERV_ALIM)
class(ID_cond_VS_CD_SERV_ALIM)

check_id_serv_and_id <- distinct(ID_cond_VS_CD_SERV_ALIM) # DISTINCT
write.csv(check_id_serv_and_id, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA\\Check servizio alimentante e ID condizione\\check.csv", row.names=FALSE)
# VERIFICATO CON EXCEL, OK

##### Analisi servizio alimentante con possibili cluster di spesa #####
# esempio per il bonifico è EU (tab 154) ci sono commissioni di vario tipo,
# se raggruppo per questa condizione EU gli importi restano costanti
# HC lo faccio per ultimo
# raggruppo per servizio alim e mostro la spesa
# aggiungere anche SALDOMED
table(df$CD_SERVIZIO_ALIM) # 10 livelli

ID_not2consider = c(seq(272,287), 394,397) # ID_COND dei tassi che hanno
# im_spesa_unit vuoto 
df$IM_SPESA_UNIT[which(df$ID_CONDIZIONE %in% ID_not2consider)] = NA

# nelle altre righe che trovero esistono zeri che sono spese ma non sono valorizzate
# eliminare ID con tasso dare e avere con 

# DI: debito diretto
CDA_DI <- subset(df, df$CD_SERVIZIO_ALIM == "DI")
ggplot(CDA_DI, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "red", color = "red", size = 1) + 
  coord_cartesian(ylim = c(0, 50))
CDA_DI %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_DI$groups <- cut(CDA_DI$IM_SPESA_UNIT, 
                       breaks = c(-1,0,1,2,3,Inf)) # creo classi
kable(prop.table(table(CDA_DI$groups))*100) # in %
# il 3 è un probabile outliers
# cercarlo
CDA_DI_outliers <- subset(df, df$IM_SPESA_UNIT > 2 & df$CD_SERVIZIO_ALIM == "DI")
view(CDA_DI_outliers)

# EU: bonifici
CDA_EU <- subset(df, df$CD_SERVIZIO_ALIM == "EU")
ggplot(CDA_EU, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "blue", color = "blue", size = 1) + 
  coord_cartesian(ylim = c(0, 500))
CDA_EU %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_EU %>% count(IM_SPESA_UNIT, sort = T) 
CDA_EU$groups <- cut(CDA_EU$IM_SPESA_UNIT, 
                     breaks = c(-1,0,1,2,3,4,5,10,Inf)) # creo classi
kable(prop.table(table(CDA_EU$groups))*100) # in %

# HC: conti correnti
CDA_HC <- subset(df, df$CD_SERVIZIO_ALIM == "HC")
ggplot(CDA_HC, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "gold2", color = "gold2", size = 1) + 
  coord_cartesian(ylim = c(0, 50))
CDA_HC %>% count(is.na(IM_SPESA_UNIT)) # ci sono NA 
CDA_HC %>% count(IM_SPESA_UNIT, sort = T) 
CDA_HC$groups <- cut(CDA_HC$IM_SPESA_UNIT, 
                     breaks = c(-1,0,1,2,3,4,5,6,7,8,9,10,20,40,Inf)) # creo classi
kable(prop.table(table(CDA_HC$groups))*100) # in %

# I1: INCASSI VARI
CDA_I1 <- subset(df, df$CD_SERVIZIO_ALIM == "I1")
ggplot(CDA_I1, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "brown", color = "brown", size = 1) + 
  coord_cartesian(ylim = c(0, 100))
CDA_I1 %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_I1 %>% count(IM_SPESA_UNIT, sort = T) 
CDA_I1$groups <- cut(CDA_I1$IM_SPESA_UNIT, 
                     breaks = c(-1,0,1,2,Inf)) # creo classi
kable(prop.table(table(CDA_I1$groups))*100) # in %

# NG: GESTIONE CARTE
CDA_NG <- subset(df, df$CD_SERVIZIO_ALIM == "NG")
ggplot(CDA_NG, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "green", color = "green", size = 1) + 
  coord_cartesian(ylim = c(0, 500))
CDA_NG %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_NG %>% count(IM_SPESA_UNIT, sort = T) 
CDA_NG$groups <- cut(CDA_NG$IM_SPESA_UNIT, 
                     breaks = c(-1,0,1,2,3,4,5,6,7,8,9,10,15,17,Inf)) # creo classi
kable(prop.table(table(CDA_NG$groups))*100) # in %
# NI: assegni
CDA_NI <- subset(df, df$CD_SERVIZIO_ALIM == "NI")
ggplot(CDA_NI, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "brown", size = 1) + 
  coord_cartesian(ylim = c(0, 100))
CDA_NI %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_NI %>% count(IM_SPESA_UNIT, sort = T) 
CDA_NI$groups <- cut(CDA_NI$IM_SPESA_UNIT, 
                     breaks = c(0,4,5,14)) # creo classi
kable(prop.table(table(CDA_NI$groups))*100) # in %

# PJ: portafoglio
CDA_PJ <- subset(df, df$CD_SERVIZIO_ALIM == "PJ")
ggplot(CDA_PJ, aes(IM_SPESA_UNIT)) + 
  geom_bar(fill = "pink", size = 1) + 
  coord_cartesian(ylim = c(0, 100), xlim = c(-5,5))
CDA_PJ %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_PJ %>% count(IM_SPESA_UNIT, sort = T) 
kable(prop.table(table(CDA_PJ$IM_SPESA_UNIT))*100) # in %

# SH: estero
CDA_SH <- subset(df, df$CD_SERVIZIO_ALIM == "SH")
ggplot(CDA_SH, aes(IM_SPESA_UNIT)) + 
  geom_bar(color = "yellow4", fill = "yellow4", size = 1) + 
  coord_cartesian(ylim = c(0, 100))
# da 0 a 20
ggplot(CDA_SH, aes(IM_SPESA_UNIT)) + 
  geom_bar(color = "yellow4", fill = "yellow4", size = 1) + 
  coord_cartesian(ylim = c(0, 50), xlim = c(0,20))
# da 20 a 100
ggplot(CDA_SH, aes(IM_SPESA_UNIT)) + 
  geom_bar(color = "yellow4", fill = "yellow4", size = 1) + 
  coord_cartesian(ylim = c(0, 50), xlim = c(20,100))
# da 100 a 500
ggplot(CDA_SH, aes(IM_SPESA_UNIT)) + 
  geom_bar(color = "yellow4", fill = "yellow4", size = 1) + 
  coord_cartesian(ylim = c(0, 50), xlim = c(100, 500))

CDA_SH %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
kable(table(IM_SPESA_UNIT))
CDA_SH %>% count(CDA_SH$IM_SPESA_UNIT, sort = T)
CDA_SH$groups <- cut(CDA_SH$IM_SPESA_UNIT, 
                     breaks = c(0,5,10,15,20,30,40,50,60,100,500,Inf)) # creo classi
kable(prop.table(table(CDA_SH$groups))*100) # in %

# WI: pacchetti WIDIBA
CDA_WI <- subset(df, df$CD_SERVIZIO_ALIM == "WI")
ggplot(CDA_WI, aes(IM_SPESA_UNIT)) + 
  geom_bar(color = "orange", fill = "orange", size = 1) + 
  coord_cartesian(ylim = c(0, 100))

CDA_WI %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_WI %>% count(IM_SPESA_UNIT, sort = T)
CDA_WI$groups <- cut(CDA_WI$IM_SPESA_UNIT, 
                     breaks = c(0,1,2,3,4,5,6,7,8,9,10,13,15,20,30)) # creo classi
kable(prop.table(table(CDA_WI$groups))*100) # in %

# XY: CARTE CRED PREPAGATE SPIDER/MAXI WI
CDA_XY <- subset(df, df$CD_SERVIZIO_ALIM == "XY")
ggplot(CDA_XY, aes(IM_SPESA_UNIT)) + 
  geom_bar(color = "purple", fill = "purple", size = 1) + 
  coord_cartesian(ylim = c(0, 100))

CDA_XY %>% count(is.na(IM_SPESA_UNIT)) # non ci sono NA 
CDA_XY %>% count(IM_SPESA_UNIT, sort = T)
CDA_XY$groups <- cut(CDA_XY$IM_SPESA_UNIT, 
                     breaks = c(0,1,2,3,4,5,6,7,8,9,10)) # creo classi
kable(prop.table(table(CDA_XY$groups))*100) # in %

##### (fatto) Anomalie CD_SAE non esteri con sede legale estera #####
sedelegale_prov <- fread("Dataset aggiornati 12.1.21/Widiba/SOF_Widiba_2021_servNoZA.csv", 
            stringsAsFactors = T,
            select = c("DS_SEDE_LEGALE", "TX_SIGLA_PRV", "CD_FILIALE", "CD_SAE", "CD_NGR_MOD"),
            dec = ",",
            sep=";",
            na.strings = "")

sedelegale_prov %>% count(CD_SAE)
sedelegale_prov <- subset(sedelegale_prov, sedelegale_prov$CD_SAE<700)
sedelegale_prov %>% count(DS_SEDE_LEGALE) 
sedelegale_prov %>% count(TX_SIGLA_PRV) 
sedelegale_prov %>% count(CD_NGR_MOD) 

sedelegale_prov <- sedelegale_prov %>% 
  mutate(check = ifelse(sedelegale_prov$TX_SIGLA_PRV != "" & sedelegale_prov$DS_SEDE_LEGALE != "",TRUE, FALSE))
# se uno dei due campi è vuoto check = NA
sedelegale_prov %>% count(check)
sedelegale_prov_NA <- subset(sedelegale_prov, is.na(sedelegale_prov$check))
# sedelegale_prov_NA$CD_NGR_MOD <- as.factor(sedelegale_prov_NA$CD_NGR_MOD)
sedelegale_prov_NA <- sedelegale_prov_NA %>%
  distinct(CD_NGR_MOD, TX_SIGLA_PRV, DS_SEDE_LEGALE, CD_SAE)
write.csv(sedelegale_prov_NA, 
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_12.1.21\\Anomalie CD_SAE non esteri con sede legale estera\\SAE_non_esteri.csv", row.names=FALSE)

###### (fatto) Analisi TASSI avere e dare con ID_CONDIZIONE #####
Tasso_avere_e_dare <- read_excel("Tasso avere e dare.xlsx")
df_merged <- merge(x=df, y= Tasso_avere_e_dare, by = "ID_CONDIZIONE",
            all.x = T)
df_merged %>% count(`TIPO TASSO`)
tasso_avere_spese_VALORIZZATE <- subset(df_merged, 
                      df_merged$`TIPO TASSO` %in% 
                        c("TASSO AVERE", "TASSO DARE") & (df_merged$IM_SPESA_UNIT != 0 |
                        df_merged$QT_SPESA_ADD != 0))

tasso_avere_spese_VALORIZZATE %>% count(ID_CONDIZIONE)

tasso_avere_spese_VALORIZZATE_1 <- subset(df_merged, 
                                        df_merged$ID_CONDIZIONE %in% 
                                          c(445,447,452,453) & (df_merged$IM_SPESA_UNIT == 0 &
                                                                              df_merged$QT_SPESA_ADD == 0))

# gli id_cond nella tab tasso_avere_spese_VALORIZZATE presentano righe
# dove rispettano le condizioni (tasso_avere_spese_VALORIZZATE_1)
# e righe dove non le rispettano
# QUINDI, SONO INQUADRABILI COME TASSI E RELATIVA CONDIZIONE?

# da 272 a 287, 394 e 397 sono tassi, quindi
# im_spesa_unit e qt_spesa_add NON SONO MAI VALORIZZATI

##### Analisi outliers su id_condizione e spese negative #####
# cluster per id condizione
ggplot(df, aes(x=ID_CONDIZIONE)) + 
  geom_bar(fill = "lightblue")
# cluster per importi negativi (im_spesa_add)
# già verificato in Analisi IM_SPESA_ADD negativo (subset)
IM_SPESA_ADD_NEG %>% count(ID_CONDIZIONE) # n73
# ID_not2consider = c(seq(272,287), 394,397) # sono tassi, quindi
# im_spesa_unit e qt_spesa_add NON SONO MAI VALORIZZATI
# cd_sae solo 600
# cd_Serv_rapp tutti CC
# serv_alim = Hc
# CL_COND_SPEC tutti zero
IM_SPESA_ADD_NEG %>% count(IM_SPESA_UNIT) # tutti zero
IM_SPESA_ADD_NEG %>% count(QT_SPESA_ADD) # tutti zero
ggplot(IM_SPESA_ADD_NEG, aes(x=IM_SPESA_ADD, y=SALDMEDCONC)) + 
  geom_jitter(width = .5, size=1, fill = "gold2", color = "gold2")
IM_SPESA_ADD_NEG %>% count(SALDMEDCONC) # 11 zero
# non ci sono apparenti storni



##### Analisi giroconti ####
giroconti <- subset(df, df$ID_CONDIZIONE %in% c(116,
                                              119,
                                              122,
                                              124,
                                              126,
                                              128,
                                              131,
                                              134,
                                              137,
                                              140,
                                              143,
                                              145,
                                              147,
                                              149,
                                              152,
                                              155,
                                              164,
                                              167,
                                              170,
                                              465,
                                              466,
                                              467))
id_cond_count <- df %>% count(ID_CONDIZIONE, sort = T) # empty

##### Analisi commissione servizi vari #####
# si presuppone siano pari a zero
COMMISSIONE_SERVIZI_VARI <- subset(df, df$ID_CONDIZIONE %in% c(112,
                                                               113,
                                                               114,
                                                               343,
                                                               346,
                                                               347,
                                                               348,
                                                               349,
                                                               352,
                                                               355,
                                                               356,
                                                               362,
                                                               367,
                                                               368,
                                                               376,
                                                               379,
                                                               384,
                                                               385,
                                                               386,
                                                               387,
                                                               388,
                                                               389,
                                                               390,
                                                               391,
                                                               392,
                                                               393,
                                                               485,
                                                               486,
                                                               525,
                                                               526,
                                                               527,
                                                               528,
                                                               529,
                                                               530,
                                                               531,
                                                               556,
                                                               557,
                                                               558,
                                                               559,
                                                               560))
COMMISSIONE_SERVIZI_VARI %>% count(ID_CONDIZIONE)
COMMISSIONE_SERVIZI_VARI %>% count(IM_SPESA_ADD, sort = T) # non sono tutti zero
COMMISSIONE_SERVIZI_VARI %>% count(IM_SPESA_UNIT, sort = T)
COMMISSIONE_SERVIZI_VARI_con_imp_unit_ZERO <- subset(COMMISSIONE_SERVIZI_VARI, 
                                                    COMMISSIONE_SERVIZI_VARI$IM_SPESA_UNIT == 0)
COMMISSIONE_SERVIZI_VARI_con_imp_unit_ZERO %>% count(ID_CONDIZIONE) # questi sono gli ID che non prevedono spese
# evidenziati marrone su excel
COMMISSIONE_SERVIZI_VARI_con_imp_unit_NON_ZERO <-subset(COMMISSIONE_SERVIZI_VARI, 
                                                        COMMISSIONE_SERVIZI_VARI$IM_SPESA_UNIT > 0)

ggplot(COMMISSIONE_SERVIZI_VARI_con_imp_unit_NON_ZERO, aes(x=IM_SPESA_UNIT)) + 
  geom_bar(fill = "black")
ggplot(COMMISSIONE_SERVIZI_VARI_con_imp_unit_NON_ZERO, aes(x=ID_CONDIZIONE)) + 
  geom_bar(fill = "red")
COMMISSIONE_SERVIZI_VARI_con_imp_unit_NON_ZERO %>% count(ID_CONDIZIONE)

df_556 <- subset(df, df$ID_CONDIZIONE == 556) # ANOMALIA 
df_556 %>% count(IM_SPESA_UNIT) 
df_556 %>% count(TY_CLIENTE) 

df_112 <- subset(df, df$ID_CONDIZIONE == 112) # ANOMALIA 
df_112 %>% count(IM_SPESA_UNIT) 
df_112 %>% count(TY_CLIENTE) 

###### DA SEGNALRE ######
df_114 <- subset(df, df$ID_CONDIZIONE == 114) # ANOMALIA 
df_114 %>% count(IM_SPESA_UNIT) 
df_114 %>% count(TY_CLIENTE) 
# non ci sono isole

df_384 <- subset(df, df$ID_CONDIZIONE == 384) # ok
df_384 %>% count(IM_SPESA_UNIT) 
df_384 %>% count(TY_CLIENTE) 

df_529 <- subset(df, df$ID_CONDIZIONE == 529) # diversità accettabile
df_529 %>% count(IM_SPESA_UNIT) 
df_529 %>% count(TY_CLIENTE) 
# ANOMALIA 

COMMISSIONE_SERVIZI_VARI_con_imp_unit_NON_ZERO %>% count(IM_SPESA_UNIT)
ggplot(COMMISSIONE_SERVIZI_VARI_con_imp_unit_NON_ZERO, aes(x=IM_SPESA_UNIT)) + 
  geom_bar(fill = "black") + 
  coord_cartesian(xlim = c(1.09,3.01), ylim = c(0,50))

##### Analisi storno #####
storno <- subset(df, df$ID_CONDIZIONE %in% c(208,209,222,223))

storno %>% count(ID_CONDIZIONE)
storno %>% count(IM_SPESA_UNIT) # hanno costo unitario di 5
storno %>% count(IM_SPESA_ADD)
storno %>% count(QT_SPESA_ADD) # tutto ok

##### Analisi commissioni POS #####
commissioni_POS <- subset(df, df$ID_CONDIZIONE %in% c(361,
                                                      371,
                                                      372,
                                                      375,
                                                      383))

##### Analisi cbill: addebito bollette #####
cbill <- subset(df, df$ID_CONDIZIONE %in% c(196,
                                            197,
                                            198,
                                            582))

cbill %>% count(IM_SPESA_UNIT)
cbill %>% count(SALDMEDCONC)

df_196 <- subset(df, df$ID_CONDIZIONE == 196) # non è costante
df_196 %>% count(IM_SPESA_UNIT) 
df_196 %>% count(TY_CLIENTE) 
df_196_cl_cond_spec_NON_zero <- subset(df_196, df_196$CL_COND_SPEC != 0)
df_196_cl_cond_spec_NON_zero %>% count(IM_SPESA_UNIT)
df_196_cl_cond_spec_zero <- subset(df_196, df_196$CL_COND_SPEC == 0)
df_196_cl_cond_spec_zero %>% count(IM_SPESA_UNIT)

df_197 <- subset(df, df$ID_CONDIZIONE == 197) # ok
df_197 %>% count(IM_SPESA_UNIT) 
df_197 %>% count(TY_CLIENTE) 

df_198 <- subset(df, df$ID_CONDIZIONE == 198) # vuoto

df_582 <- subset(df, df$ID_CONDIZIONE == 582) # vuoto

##### Analisi ID_condizione che abbiano im_spesa_unit costante #####

##### (fatto) Analisi con qt_Spesa_ADD e QT_Spesa_tot diversi diversi #####
# il servizio alimentante PK è da escludere xk a pacchetto
# es. ho spesa tot 1000 e addebitata 0


##### Analisi vuoti CD_SEDE_LEGALE #####
# TY_CLIENTE: vuoti 
# TX_SIGLA_PRV: NA 
# DT_NASC: NA 



##### (fatto) condizioni spese riferite a cointestazione nel caso di ty_cliente = a SPF #####
# 228,229,230,231,241,242,243,244,245,246,254,288,289, 600(?)
# cerco "coin" su ds_descrizione di tab_sof_dominio
# copio id_cond
id_cond_COI <- subset(df, df$ID_CONDIZIONE %in% c(228,229,230,231,241,242,243,244,245,246,254,288,289))
table(df$ID_CONDIZIONE)
# non ci sono su widiba, verifica non effettuata. 
# sono tutti PK e su widiba non ci sono
# buttare via tutte le colonne con zero
# prendo poi, tutte le ID e faccio il confronto di anno



##### Analisi YoY per imp_spesa_unit #####
# Controllo spese addebitate al 12/21 e il 12/20 
# coppia cd_rapp_mod e filiale
# poi li confronto per spese unitarie
# primary_key : cd_rapp + filiale + id_condizione
YoY <- read_excel("SOF_Anomaly Detection_20220118_v2_integrato.xlsx", 
                  sheet = "T.154 WIDIBA")
colnames(YoY)[20] <- "FLG_YoY"
YoY <- select(YoY, -c("CD_CONDIZIONE",
                               "CD_QUALIFICATORE",
                               "CD_SEZIONE_SOF",
                               "CD_MACROFAMIGLIA",
                               "PG_ORD_STP",
                               "CD_SEZIONE_DDSISCO",
                               "CD_DATO_DD_DS_COND")) # tolgo un po di colonne
df_and_YoY <- merge(x=df, y= YoY, by = "ID_CONDIZIONE", all.x = T)
df_and_YoY %>% count(FLG_YoY)
df_and_YoY %>% count(TASSO)
df_and_YoY <- subset(df_and_YoY, df_and_YoY$CL_COND_SPEC == 0) # solo condizioni 0
df_and_YoY %>% count(CL_COND_SPEC)

df_2020 <- fread("SOF_Widiba_2020_servNoZA_new.csv", 
                 stringsAsFactors = T, drop = c("DT_PROSPETTO", "CD_AZIENDA", 
                                                "CD_MODELLO_SERV", "DS_MODELLO_SERV"),
                 dec = ",",
                 sep=";",
                 na.strings = "") # carico il 2020
df_2020 <- subset(df_2020, df_2020$CL_COND_SPEC == 0) # solo condizioni speciali 0
df_2020 %>% count(CL_COND_SPEC)
df_2020$key <- paste(df_2020$CD_FILIALE, 
                     df_2020$CD_RAPPORTO_MOD,
                     df_2020$ID_CONDIZIONE,
                     df_2020$PG_CONDIZIONE) #chiave primaria

df_and_YoY$key <- paste(df_and_YoY$CD_FILIALE,
                df_and_YoY$CD_RAPPORTO_MOD,
                df_and_YoY$ID_CONDIZIONE,
                df_and_YoY$PG_CONDIZIONE) #chiave primaria

YoY_in_comune = df_and_YoY %>% inner_join(df_2020, by = "key") # voci in comune nei due anni
YoY_in_comune <- YoY_in_comune %>% mutate(check = ifelse(YoY_in_comune$IM_SPESA_UNIT.x == YoY_in_comune$IM_SPESA_UNIT.y, T, F)) 
YoY_in_comune %>% count(check) # anomalie
YoY_in_comune %>% count(TASSO)
YoY_in_comune %>% count(CL_COND_SPEC.x)
YoY_in_comune_FALSE <- subset(YoY_in_comune, YoY_in_comune$check == F)
# output riga sotto
YoY_in_comune_FALSE <- subset(YoY_in_comune_FALSE, YoY_in_comune_FALSE$TASSO != c("TASSO ATTIVO", "TASSO PASSIVO")) # tolgo tassi; useless
YoY_in_comune_FALSE %>% count(TASSO)
YoY_in_comune_FALSE %>% count(ID_CONDIZIONE.x)
YoY_in_comune_FALSE %>% 
  select(ID_CONDIZIONE.x, ID_CONDIZIONE.y, IM_SPESA_UNIT.x, IM_SPESA_UNIT.y) %>% 
  view()
# YoY_in_comune_FALSE_sample <- sample_n(YoY_in_comune_FALSE, size = 20000)
# YoY_in_comune_FALSE_s <- YoY_in_comune_FALSE %>% select()
write.csv(YoY_in_comune_FALSE,
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_18.1.21\\conf_20_21_spese_unitarie\\conf_20_21_spese_unitarie.csv", row.names=FALSE)
##### Analisi sulla spesa unit tra anni (NEW) #####
df_2020$key <- paste(df_2020$CD_FILIALE, 
                     df_2020$CD_RAPPORTO_MOD) #chiave primaria

df_and_YoY$key <- paste(df_and_YoY$CD_FILIALE,
                        df_and_YoY$CD_RAPPORTO_MOD) #chiave primaria
df_2020_1 <- df_2020 %>% group_by(key) %>% summarise(IM_SPESA_ADD = sum(IM_SPESA_ADD))

df_and_YoY_1 <- df_and_YoY %>% group_by(key) %>% summarise(IM_SPESA_ADD = sum(IM_SPESA_ADD))

join <- df_and_YoY_1 %>% inner_join(df_2020_1, by = "key") 

join <- select(join, c("ID_CONDIZIONE.x", "ID_CONDIZIONE.y", "IM_SPESA_ADD.x", "IM_SPESA_ADD.y","CD_RAPPORTO_MOD.x", "CD_RAPPORTO_MOD.y"))
join_1 <- join %>% group_by(CD_RAPPORTO_MOD.x, ID_CONDIZIONE.x) %>% summarise(IM_SPESA_ADD = sum(IM_SPESA_ADD))

##### Analisi variazioni tdi tra anni #####
in_join_pc <- YoY_in_comune %>% mutate(check_tdi = ifelse(YoY_in_comune$PC_TASSO_INTERESSE.x == YoY_in_comune$PC_TASSO_INTERESSE.y, T, F))
in_join_pc %>% count(check_tdi) # mutazioni del tdi
pc_tdi_differenti <- subset(in_join_pc, in_join_pc$check_tdi == F)
pc_tdi_differenti %>% count(TASSO)
pc_tdi_differenti <- subset(pc_tdi_differenti, pc_tdi_differenti$TASSO != "-") # useless
# output sopra
pc_tdi_differenti %>% count(TASSO)
pc_tdi_differenti %>% 
  select(ID_CONDIZIONE.x, ID_CONDIZIONE.y, PC_TASSO_INTERESSE.x, PC_TASSO_INTERESSE.y) %>%
  view()
write.csv(pc_tdi_differenti,
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_18.1.21\\conf_20_21_tdi\\conf_20_21_tdi.csv", row.names=FALSE)

# analisi tdi 2021
df_2020 %>% count(PC_TASSO_INTERESSE, sort = T) 
ggplot(df_2020, aes(x=PC_TASSO_INTERESSE))+
  geom_bar() +
  coord_cartesian(ylim = c(0,3000)) 


##### Analisi ID_COND mutati tra gli anni #####
df_2020$key_no_ID <- paste(df_2020$CD_FILIALE, 
                     df_2020$CD_RAPPORTO_MOD) #chiave primaria

df_and_YoY$key_no_ID <- paste(df_and_YoY$CD_FILIALE,
                        df_and_YoY$CD_RAPPORTO_MOD)
df_and_YoY %>% count(FLG_YoY)
df_and_YoY_FLG1 <- subset(df_and_YoY, df_and_YoY$FLG_YoY == 1) # solo FLAG 1 !!!
outer_in_2021 = df_and_YoY_FLG1 %>% anti_join(df_2020, by="key_no_ID") 
# df_2021 - df_2020
# seleziono le voci che nel 2021 non erano presenti nel 2020
# sono tutti ID_condizione nuovi
outer_in_2021 %>% count(ID_CONDIZIONE)
outer_in_2021 %>% count(CL_COND_SPEC)
outer_in_2021_sample <- sample_n(outer_in_2021, size = 20000)
write.csv(outer_in_2021_sample,
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_18.1.21\\conf_20_21_ID_COND\\ID_COND_nuovi.csv", row.names=FALSE)


outer_in_2020 = df_2020 %>% anti_join(df_and_YoY_FLG1, by = "key_no_ID") 
# seleziono le voci che nel 2020 sono scomparse nel 2021
# df_2020 - df_2021
outer_in_2020 %>% count(ID_CONDIZIONE)
outer_in_2020_sample <- sample_n(outer_in_2020, size = 20000)
write.csv(outer_in_2021_sample,
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_18.1.21\\conf_20_21_ID_COND\\ID_COND_scomparsi.csv", row.names=FALSE)
##### Analisi spesa_add addebitata negli anni #####
# creo check colonna spese addebitate
in_join_spese <- YoY_in_comune %>%
  mutate(check_spese_add = ifelse(YoY_in_comune$IM_SPESA_ADD.x == YoY_in_comune$IM_SPESA_ADD.y, T, F)) 
in_join_spese_F <-subset(in_join_spese, in_join_spese$check_spese_add == F)
in_join_spese_F %>% select(ID_CONDIZIONE.x, ID_CONDIZIONE.y, IM_SPESA_ADD.x, IM_SPESA_ADD.y) %>%
  view()
in_join_spese_F %>% count(ID_CONDIZIONE.x)


##### Analisi spesa tra anni (NEW) ######
df_2021 <- fread("Dataset aggiornati 18.1.21/SOF_Widiba_2021_servNoZA_new.csv", 
            stringsAsFactors = T, drop = c("DT_PROSPETTO", "CD_AZIENDA", 
                                           "CD_MODELLO_SERV", "DS_MODELLO_SERV"),
            dec = ",",
            sep=";",
            na.strings = "")

df_2020 <- fread("SOF_Widiba_2020_servNoZA_new.csv", 
                 stringsAsFactors = T, drop = c("DT_PROSPETTO", "CD_AZIENDA", 
                                                "CD_MODELLO_SERV", "DS_MODELLO_SERV"),
                 dec = ",",
                 sep=";",
                 na.strings = "") # carico il 2020
df_2021$key <- paste(df_2021$CD_FILIALE, df_2021$CD_RAPPORTO_MOD)
df_2020$key <- paste(df_2020$CD_FILIALE, df_2020$CD_RAPPORTO_MOD)

df_2020_gr <- df_2020 %>% group_by(key) %>% summarise(IM_SPESA_ADD = sum(IM_SPESA_ADD))
df_2021_gr <- df_2021 %>% group_by(key) %>% summarise(IM_SPESA_ADD = sum(IM_SPESA_ADD))

join_20_21 <- df_2020_gr %>% inner_join(df_2021_gr, by = "key")

join_20_21 <- join_20_21 %>% mutate(delta = join_20_21$IM_SPESA_ADD.x - join_20_21$IM_SPESA_ADD.y)

join_20_21_noZero <- subset(join_20_21, join_20_21$delta != 0)

sample <- sample_n(join_20_21_noZero, size = 10)

sample_20_dett <- sample %>% inner_join(df_2020, by = "key")
write.csv(sample_20_dett,
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_18.1.21\\conf_20_21_spese_add\\spese_add_2020.csv", row.names=FALSE)
sample_21_dett <- sample %>% inner_join(df_2021, by = "key")
write.csv(sample_21_dett,
          "C:\\Users\\Patrizio.Iezzi\\Desktop\\TUTTO\\MPS-Anomaly Detection\\estrazioni WIDIBA_18.1.21\\conf_20_21_spese_add\\spese_add_2021.csv", row.names=FALSE)
library(xlsx)
require(openxlsx)
xl_list <- list("2020" = sample_20_dett, "2021" = sample_21_dett)
write.xlsx(xl_list, file = "estrazioni WIDIBA_18.1.21\\conf_20_21_spese_add\\spese_add.xlsx", 
           rowNames=FALSE)

##### TEST da saltare #######
# calcolare le differenze tra le spese addebitate e sommarle per id_cond comune
ID = unique(in_join_spese_F$ID_CONDIZIONE.x)
sp_add1 = in_join_spese_F$IM_SPESA_ADD.x
sp_add2 = in_join_spese_F$IM_SPESA_ADD.y
in_join_spese_F$diff <- array()

for (i in 1:nrow(in_join_spese_F)) {
  in_join_spese_F$diff[i] <- abs(sp_add1[i] - sp_add2[i])
         print(i) } # ?????

in_join_spese_F %>% count(diff, sort = T)


##### CLUSTER tdi #####
sdf <- df[c(1:200000),]
remove(df)
clus_tdi <- kmeans(sdf$PC_TASSO_INTERESSE, 5)
table(clus_tdi$cluster)
plot(sdf$PC_TASSO_INTERESSE, 
     cex=1,pch=19,
     col=clus_tdi$cluster+1,
     ylab = "TdI" )
sdf %>% 
  cbind(sdf, clus_tdi$cluster) %>% 
  subset(clus_tdi$cluster == 1) %>% 
  view()

##### https://www.r-bloggers.com/2016/06/clustering-mixed-data-types-in-r/ #####
sdf <- sdf[,c(-12,-22)]
sdf <- sdf[,c(-19,-20)] # non ripeterli
sdf <- sdf[,c(-15)]
sdf <- sdf[,c(-1)]
sdf <- sdf[,c(-18)]
grower_dist <- daisy(sdf, 
                     metric = "gower") # too big
summary(gower_dist)

#### solo num####
library(factoextra)
sdf1 <- df[c(1:200000),]
sdf1 <- sdf1[,c(2:11,13:15,16:18)]
sdf1 <- na.omit(sdf1)
sdf500 <-sdf1[c(1:500)]
fviz_nbclust(sdf500, FUNcluster = clara, method = "gap_stat")
clara.res <- clara(sdf1, 5, samples = 100, pamLike = TRUE, correct.d = T)
summary(clara.res)
clara.res$medoids
fviz_cluster(clara.res, data = sdf1,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07", "red"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot")

##### ----------------- THE END --------------------


# logistico
#df$y <- TRUE # preparo colonna
#df$y[df$CD_SAE == "NA"] <- 'FALSE'
#df$y[df$CD_SAE == 280 & df$TY_CLIENTE == "SPF"] <- 'FALSE'
#df$y[df$CD_SAE == 284 & df$TY_CLIENTE == "SPF"] <- 'FALSE'
#df$y[df$CD_SAE == 772|773|774|775 & df$TX_SIGLA_PRV != "NA"] <- 'FALSE'
check <- df %>% mutate(check = 
                ifelse(df$CD_SAE == "NA", FALSE,
                       ifelse(df$CD_SAE == 280 & df$TY_CLIENTE == "SPF", FALSE,
                              ifelse(df$CD_SAE == 284 & df$TY_CLIENTE == "SPF", FALSE,
                                     ifelse(df$CD_SAE %in% c(772,773,774,775) & df$TX_SIGLA_PRV != "NA", FALSE,
                                            ifelse(df$Y_NASC>1991 & df$PC_TASSO_INTERESSE>0, FALSE, TRUE ))))))




##### CONDIZIONI #####


# ID_CONDIZIONE 115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,
# 130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,
# 149,150,151,152,153,154,155,156,157,163,164,165,166,167,168,169,170,171,172,
# 173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
# 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,210,216,221,339,
# 340,344,345,346,347,362,367,368,380,381,382,383,384,387,388,389,390,391,392,
# 393,411,412,413,414,416,464,488,489,490,493,494,495,506,507,508,509,510,511,
# 512,513,514,515,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,
# 550,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,
# 579,580,582,583
# devono avere carattere spese non vuoto (sono commissioni). 
# Relativi servizi alimentati sono NI, SH, EZ
# nelle colonne 


# id condizione per pensionati con under30 (solo spf ovviamente)
# (sono tutti ZA i giovani e i pensionati)
# ignorare



##### Appunti #####

# ZA si può ignorare
