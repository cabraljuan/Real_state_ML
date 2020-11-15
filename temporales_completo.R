


###############################################################################
###############################################################################
###                                                                         ###
###  CALCULATING MEDIAN PRICES FOR <<TEMPORALES>> FROM <<COSTA ATLÁNTICA>>  ###
###                                                                         ###
###############################################################################
###############################################################################

# Libraries
library(beepr)
library(plyr)
library(dplyr)
library(beepr)
library(bannerCommenter)
library(ggplot2)
library(gridExtra)
library(grid)
library(readxl)
library(expss)
library("writexl")
library(scales)
library(sf)
library(raster)
library(rgdal)
library(shape)
library(data.table)
library(lubridate)
library(DescTools)
library(zoo) # for rollsum
library(stargazer)


# Path:
path <-"G:/MELI"
setwd(path)


##################################################################
##                         Loading DATA                         ##
##################################################################

# AMBA
data <- read.csv("output/REALESTATE_MLA_AMBA_2017_v1.csv",stringsAsFactors = FALSE)
data1 <- read.csv("output/REALESTATE_MLA_AMBA_2018_v1.csv",stringsAsFactors = FALSE)
data2 <- read.csv("output/REALESTATE_MLA_AMBA_2019_v1.csv",stringsAsFactors = FALSE)
data3 <- read.csv("output/REALESTATE_MLA_AMBA_2020_10.csv",stringsAsFactors = FALSE)


# Merge all dataframes in one big dataframe called DT
DT<-rbind(data,data1,data2,data3)


# INTERIOR
data1int<-read.csv("output/RE_MLA_INTERIOR_2017_v1.csv",stringsAsFactors = FALSE)
data2int<-read.csv("output/RE_MLA_INTERIOR_2018_v1.csv",stringsAsFactors = FALSE)
data3int<-read.csv("output/RE_MLA_INTERIOR_2019_v1.csv",stringsAsFactors = FALSE)
data4int<-read.csv("output/RE_MLA_INTERIOR_2020_10.csv",stringsAsFactors = FALSE)
DTint<-rbind(data1int,data2int,data3int,data4int)


# Merge datasets
DT<-rbind(DT,DTint)


# Backup
DT<-DTbackup


# CHECKING
sort(unique(DT$ITE_ADD_STATE_NAME))
sort(unique(DT$ITE_ADD_CITY_NAME))


# Only interested in BA cities
BA <- c("Bs.As. Cap. Federal","Bs.As. Costa Atlantica","Bs.As. Costa Atlántica",
        "Bs.As. G.B.A. Norte","Bs.As. G.B.A. Oeste","Bs.As. G.B.A. Sur",
        "Buenos Aires","Buenos Aires Interior","Gran Buenos Aires","Gran Buenos Aires Oeste")
DT<-DT[DT$ITE_ADD_STATE_NAME %in% BA,]



# Only interested in "Casas" and "temporales"
DT<-DT[DT$OPERACION=="Alquiler temporal"|DT$OPERACION=="Alquiler Temporario"&
         DT$TIPOPROPIEDADNORM=="Casa",]


##################################################################
##                           CLEANING                           ##
##################################################################

# Drop observations for which price is 0:
DT <- DT[DT$ITE_BASE_CURRENT_PRICE!=0,]
DT <- DT[DT$ITE_SITE_CURRENT_PRICE!=0,]



# Converting all cities strings to lower case
DT$ITE_ADD_CITY_NAME<-tolower(DT$ITE_ADD_CITY_NAME)



# Missclasified places
# Castelar is not a "partido", it is a city inside moron, we need to fix it
DT[DT$ITE_ADD_CITY_NAME=="castelar",]$ITE_ADD_CITY_NAME<-"moron"
DT[DT$ITE_ADD_CITY_NAME=="caseros",]$ITE_ADD_CITY_NAME<-"tres de febrero"


# REMOVING ACCENTS
DT[DT$ITE_ADD_CITY_NAME=="vicente lópez",]$ITE_ADD_CITY_NAME <-"vicente lopez"
DT[DT$ITE_ADD_CITY_NAME=="cariló",]$ITE_ADD_CITY_NAME<-"carilo"
DT[DT$ITE_ADD_CITY_NAME=="mar del tuyú",]$ITE_ADD_CITY_NAME<-"mar del tuyu"
DT[DT$ITE_ADD_CITY_NAME=="mar de ajó",]$ITE_ADD_CITY_NAME<-"mar de ajo"
DT[DT$ITE_ADD_CITY_NAME=="morón",]$ITE_ADD_CITY_NAME<-"moron"
DT[DT$ITE_ADD_CITY_NAME=="esteban echeverría",]$ITE_ADD_CITY_NAME<-"esteban echeverria"
DT[DT$ITE_ADD_CITY_NAME=="general rodríguez",]$ITE_ADD_CITY_NAME<-"general rodriguez"
DT[DT$ITE_ADD_CITY_NAME=="presidente perón",]$ITE_ADD_CITY_NAME<-"presidente peron"
DT[DT$ITE_ADD_CITY_NAME=="josé c. paz",]$ITE_ADD_CITY_NAME<-"jose c. paz"
DT[DT$ITE_ADD_CITY_NAME=="ituzaingó",]$ITE_ADD_CITY_NAME<-"ituzaingo"
DT[DT$ITE_ADD_CITY_NAME=="general san martín",]$ITE_ADD_CITY_NAME<-"general san martin"
DT[DT$ITE_ADD_CITY_NAME=="lanús",]$ITE_ADD_CITY_NAME<-"lanus"
DT[DT$ITE_ADD_CITY_NAME=="zárate",]$ITE_ADD_CITY_NAME<-"zarate"
DT[DT$ITE_ADD_CITY_NAME=="punta médanos",]$ITE_ADD_CITY_NAME<-"punta medanos"


# STotalM2 should be larger than SConstrM2:
nrow(DT[DT$STotalM2<DT$SConstrM2,]) 

# I define METROS as the max from SConstrM2 and equivalent area:
DT$METROS<- DT$SConstrM2 + pmax((DT$STotalM2-DT$SConstrM2)/2, 0, na.rm=TRUE)

# I add the size of a parking lot:
DT[DT$Estacionamiento=="Sí"&DT$TIPOPROPIEDADNORM=="Departamento",]$METROS<-
  DT[DT$Estacionamiento=="Sí"&DT$TIPOPROPIEDADNORM=="Departamento",]$METROS+12


# Drop observations for which 0 squared meters:
DT <- DT[DT$METROS!=0,]


# Price/Squared meters:
DT$PM2 <- DT$ITE_BASE_CURRENT_PRICE/DT$METROS


#PROPERTIES:
DT$INMUEBLE <- ""
DT$INMUEBLE<-ifelse(DT$TIPOPROPIEDADNORM=="Casa","Casa",
                    ifelse(DT$TIPOPROPIEDADNORM=="Departamento","Departamento",
                           ifelse(DT$TIPOPROPIEDADNORM=="Oficina"|DT$TIPOPROPIEDADNORM=="Local","Oficina",NA)))
DT$INMUEBLE<-as.factor(DT$INMUEBLE)
DT <- DT[!is.na(DT$INMUEBLE),]
ambientes <- data.frame(table(DT[DT$INMUEBLE=="Departamento",]$Ambientes,DT[DT$INMUEBLE=="Departamento",]$Dormitorios))
ambientes <- ambientes[ambientes$Freq>0,]

DT$INMUEBLE1 <- ifelse(DT$INMUEBLE=="Departamento" & DT$Ambientes==1,"Monoambiente",
                       ifelse(DT$INMUEBLE=="Departamento"&(DT$Ambientes==2|DT$Ambientes==3),"Dos o tres ambientes",
                              ifelse(DT$INMUEBLE=="Departamento"&DT$Ambientes>=4,"Cuatro o mas ambientes",0)))
DT$INMUEBLE1<-as.factor(DT$INMUEBLE1)

# Formatting time variables
DT$TIM_DAY <- as.Date(DT$TIM_DAY, "%Y-%m-%d")
DT$ITE_AUCTION_START <- as.Date(DT$ITE_AUCTION_START, "%Y-%m-%d")
DT$ITE_AUCTION_STOP <- as.Date(DT$ITE_AUCTION_STOP, "%Y-%m-%d")
DT$MONTH <- format(as.Date(DT$TIM_DAY), "%Y-%m")
DT$MONTH_START <- format(as.Date(DT$ITE_AUCTION_START), "%Y-%m")
DT$MONTH_END <- format(as.Date(DT$ITE_AUCTION_STOP), "%Y-%m")



# Remove accents from costa atlantica
DT[DT$ITE_ADD_STATE_NAME=="Bs.As. Costa Atlántica",]$ITE_ADD_STATE_NAME<- "Bs.As. Costa Atlantica"



### Costa dummy

# Costacities
DT_costa<-DT[DT$ITE_ADD_STATE_NAME=="Bs.As. Costa Atlantica"|
               DT$ITE_ADD_STATE_NAME=="Bs.As. Costa Atlántica",]
costacities<-as.character(unique(DT_costa$ITE_ADD_CITY_NAME))

# Correcting missclasified places
DT[DT$ITE_ADD_CITY_NAME %in% costacities,]$ITE_ADD_STATE_NAME<-"Bs.As. Costa Atlantica"

# Dummy
DT$dcosta<-ifelse(DT$ITE_ADD_STATE_NAME=="Bs.As. Costa Atlantica",1,0)

alquiler<-DT


# Subset, only interested in costa
alquiler<-alquiler[alquiler$ITE_ADD_CITY_NAME %in% costacities,]

# Checking
summary(alquiler)
stargazer(alquiler)
nrow(alquiler)
#alquiler$METROS<-as.numeric(alquiler$METROS)

# # Checking
# nrow(alquiler[alquiler$ITE_ADD_CITY_NAME=="Almirante Brown"&
#                 alquiler$MONTH=="2017-01",]$METROS)
# sort(unique(alquiler$MONTH))


##################################################################
##                           Analysis                           ##
##################################################################


# Truncating
baseflujoalquilerpartinterior<- data.frame()
for (month in sort(unique(alquiler$MONTH)) ) {
  # Active properties in t=month:
  activo1 <- alquiler[alquiler$MONTH==month,]
  
  
  # CITIES/STATES. flujo:
  activo <- activo1 %>% group_by(ITE_ADD_CITY_NAME)%>% 
    mutate(p05=quantile(ITE_SITE_CURRENT_PRICE,probs=0.05,na.rm=TRUE))%>%
    mutate(p95=quantile(ITE_SITE_CURRENT_PRICE,probs=0.95,na.rm=TRUE))%>%
    mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
    mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))
  activo <- activo[activo$ITE_SITE_CURRENT_PRICE<=activo$p95& 
                     activo$ITE_SITE_CURRENT_PRICE>=activo$p05&
                     activo$METROS<=activo$q95&activo$METROS>=activo$q05,] 
  base.1<-ddply(activo,
                .(ITE_ADD_CITY_NAME), summarize, 
                # mediana.m2=median(PM2_P, na.rm=TRUE),
                # media.m2=mean(PM2_P,na.rm=TRUE),
                # desv.m2=sd(PM2_P,na.rm=TRUE),
                mediana=median(ITE_SITE_CURRENT_PRICE, na.rm=TRUE),
               # mediana_real=median(precio_real,na.rm=TRUE),
                media=mean(ITE_SITE_CURRENT_PRICE,na.rm=TRUE),
                n=sum(!is.na(ITE_SITE_CURRENT_PRICE)),
                desv=sd(ITE_SITE_CURRENT_PRICE,na.rm=TRUE))
  activo <- NA
  base.1$MONTH  <- NA
  base.1$MONTH <- paste0(month)
  basex <-  rbind(as.data.frame(base.1))
  baseflujoalquilerpartinterior<- rbind(baseflujoalquilerpartinterior,basex)
  activo <- NA
  print(month)
  
}




#rename
testn<-baseflujoalquilerpartinterior

# Time format
testn$MONTH<-as.Date(paste(testn$MONTH,"-01",sep=""))
testn$MONTH <- format(as.Date(testn$MONTH), "%Y-%m")


# Inflation
bcra <- read_excel("input/Inflación BCRA.xlsx")
bcra$Fecha <-format(as.Date(bcra$Fecha),"%Y-%m")


# Merging inflation with datasets and getting real values
baseflujoalquilercosta<-testn
baseflujoalquilercosta <-
  merge(baseflujoalquilercosta, bcra, by.x="MONTH",by.y="Fecha",all=TRUE)
baseflujoalquilercosta$mediana_real <-
  baseflujoalquilercosta$mediana/baseflujoalquilercosta$Pondera*100

#renaming
test<-baseflujoalquilercosta

# Formatting date
test$MONTH<-as.Date(paste(test$MONTH,"-01",sep=""))

# Omit NAs
sum(is.na(test))
test<-na.omit(test)
sort(unique(test$ITE_ADD_CITY_NAME))

# Inmueble de interés
tigre<-test[
  test$ITE_ADD_CITY_NAME=="tigre",]
necochea<-test[
  test$ITE_ADD_CITY_NAME=="necochea",]
costazul<-test[
  test$ITE_ADD_CITY_NAME=="costa azul",]
nrow(costazul)

mardelsur<-test[
  test$ITE_ADD_CITY_NAME=="mar del sur",]
tuyu<-test[
  test$ITE_ADD_CITY_NAME=="mar del tuyu",]
esmeralda<-test[
  test$ITE_ADD_CITY_NAME=="costa esmeralda",]
pinamar<-test[
  test$ITE_ADD_CITY_NAME=="pinamar",]
carilo<-test[
  test$ITE_ADD_CITY_NAME=="carilo",]
nrow(carilo)
mardeajo<-test[
  test$ITE_ADD_CITY_NAME=="mar de ajo",]
#sanmi<-test[
#  test$ITE_ADD_CITY_NAME=="san miguel",]
#escobar<-test[
# test$ITE_ADD_CITY_NAME=="escobar",]
pilar<-test[
  test$ITE_ADD_CITY_NAME=="pilar",]
#laplata<-test[
# test$ITE_ADD_CITY_NAME=="la plata",]
sanisidro<-test[
  test$ITE_ADD_CITY_NAME=="san isidro",]
vicent<-test[
  test$ITE_ADD_CITY_NAME=="vicente lopez",]
nrow(vicent)
ostende<-test[
  test$ITE_ADD_CITY_NAME=="ostende",]
nrow(ostende)
mardel<-test[
  test$ITE_ADD_CITY_NAME=="mar del plata",]
nrow(mardel)
sanber<-test[
  test$ITE_ADD_CITY_NAME=="san bernardo",]
nrow(sanber)



# Graphs
graph <- ggplot(test, aes(x = MONTH)) + 
  #geom_line(data=mardel,aes(y = mediana_real, colour = "Mar del Plata"))+
  #geom_line(data=mardelsur,aes(y = mediana_real, colour = "Mar del sur"))+
  #geom_line(data=costazul,aes(y = mediana_real, colour = "Costa Azul"))+
 # geom_line(data=necochea,aes(y = mediana_real, colour = "Necochea"))+
  geom_line(data=ostende,aes(y = mediana_real, colour = "Ostende"))+
  # geom_line(data=pinamar,aes(y = mediana_real, colour = "Pinamar"))+
 #geom_line(data=esmeralda,aes(y = mediana_real, colour = "esmeralda"))+
  # geom_line(data=tuyu,aes(y = mediana_real, colour = "Mar del Tuyú"))+
  # geom_line(data=sanber,aes(y = mediana_real, colour = "San Bernardo"))+
  
  #geom_line(data=pilar,aes(y = mediana_real, colour = "Pilar"))+
  #geom_line(data=sanmi,aes(y = mediana_real, colour = "San Miguel"))+
  #geom_line(data=escobar,aes(y = mediana_real, colour = "Escobar"))+
  #geom_line(data=laplata,aes(y = mediana_real, colour = "La Plata"))+
  #geom_line(data=sanisidro,aes(y = mediana_real, colour = "San Isidro"))+
  #geom_line(data=vicent,aes(y = mediana_real, colour = "Vicente López"))+
  #geom_line(data=carilo,aes(y = mediana_real, colour = "carilo"))+
   # geom_line(data=mardeajo,aes(y = mediana_real, colour = "Mar de ajó"))+
  
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Mediana real Pinamar")) +
  labs(y = "Mediana real",
       x = "Fecha")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  scale_shape_discrete(name  ="Región")+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  scale_x_date(date_labels = "%y-%m-%d")+ 
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-08-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-09-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-10-01"),linetype=1,color="gray",size=1)+
  
  geom_vline(xintercept = as.Date("2020-03-01"),linetype=1,color="brown",size=1,
             show.legend=TRUE) 


graph


# Save graph
ggsave("G:/MELI/MELI SINCRONIZADO/Mercado Libre/MELI_RE/Estudios especificos/ 202011_Temporarios/output/carilpymardelplatacasastemporales.png",
       dpi=320,units="in",width=10,height=5)



#################################################################
##                   graphics for the report                   ##
#################################################################

# Necochea and Costa azul

# Prices as proportions
costazul$mediana_realp<-(costazul$mediana_real/(costazul[1,]$mediana_real))
necochea$mediana_realp<-(necochea$mediana_real/(necochea[1,]$mediana_real))

# Graphs
graph <- ggplot(necochea, aes(x = MONTH)) + 
  #geom_line(data=mardel,aes(y = mediana_real, colour = "Mar del Plata"))+
  #geom_line(data=mardelsur,aes(y = mediana_real, colour = "Mar del sur"))+
  geom_line(data=costazul,aes(y = mediana_realp, colour = "Costa Azul"))+
  geom_line(data=necochea,aes(y = mediana_realp, colour = "Necochea"))+
  labs(y = "Mediana real como proporción de 2017",
     x = "Fecha")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  scale_shape_discrete(name  ="Región")+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  
  scale_x_date(date_labels = "%y-%m-%d" )+ 
  
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-08-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-09-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-10-01"),linetype=1,color="gray",size=1)+
  
  geom_vline(xintercept = as.Date("2020-03-01"),linetype=1,color="brown",size=1,
             show.legend=TRUE) +
  geom_text(aes(x=as.Date("2020-03-01"), label="Cuarentena", y=1.35),
          colour="brown", angle=90, vjust = -0.5)+
  geom_text(aes(x=as.Date("2020-08-01"), label="Agosto", y=1.35),
          colour="grey", angle=90, vjust = -0.5)+
  geom_text(aes(x=as.Date("2020-09-01"), label="Septiembre", y=1.35),
          colour="grey", angle=90, vjust = -0.5)+
  geom_text(aes(x=as.Date("2020-10-01"), label="Octubre", y=1.35),
          colour="grey", angle=90, vjust = -0.5)

graph


# Save
ggsave("G:/MELI/MELI SINCRONIZADO/Mercado Libre/MELI_RE/Estudios especificos/ 202011_Temporarios/output/informe/necocheacostaazul.png",
       dpi=320,units="in",width=10,height=5)




# Prices as proportions: Mar del plata and cariló
mardel$mediana_realp<-(mardel$mediana_real/(mardel[1,]$mediana_real))
carilo$mediana_realp<-(carilo$mediana_real/(carilo[1,]$mediana_real))

p <- ggplot(test, aes(x = MONTH)) + 
  geom_line(data=mardel,aes(y = mediana_realp, colour = "Mar del Plata"))+
  geom_line(data=carilo,aes(y = mediana_realp, colour = "Cariló"))+
  labs(y = "Mediana real como proporción de 2017",
       x = "Fecha")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  scale_shape_discrete(name  ="Región")+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  #ylim(NA,1.5)+
  scale_x_date(date_labels = "%y-%m-%d" )+ 
  
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=1,color="black",size=1)+
  # geom_vline(xintercept = as.Date("2020-08-01"),linetype=1,color="gray",size=1)+
  # geom_vline(xintercept = as.Date("2020-09-01"),linetype=1,color="gray",size=1)+
  # geom_vline(xintercept = as.Date("2020-10-01"),linetype=1,color="gray",size=1)+
  
  geom_vline(xintercept = as.Date("2020-03-01"),linetype=1,color="brown",size=1,
             show.legend=TRUE)+
  geom_text(aes(x=as.Date("2020-03-01"), label="Cuarentena", y=1.35),
            colour="brown", angle=90, vjust = -0.5)

p



ggsave("G:/MELI/MELI SINCRONIZADO/Mercado Libre/MELI_RE/Estudios especificos/ 202011_Temporarios/output/informe/mardelycariló.png",
       dpi=320,units="in",width=10,height=5)

# Interannual variation
(mardel[46,]$mediana_real-mardel[34,]$mediana_real)/mardel[34,]$mediana_real *100
(carilo[46,]$mediana_real-carilo[34,]$mediana_real)/carilo[34,]$mediana_real *100



# More graphs

p <- ggplot(necochea, aes(x = MONTH)) + 
  #geom_line(data=mardel,aes(y = mediana_real, colour = "Mar del Plata"))+
  #geom_line(data=mardelsur,aes(y = mediana_real, colour = "Mar del sur"))+
  # geom_line(data=costazul,aes(y = mediana_realp, colour = "Costa Azul"))+
  # geom_line(data=necochea,aes(y = mediana_realp, colour = "Necochea"))+
   geom_smooth(data=ostende, aes(y=mediana_real), method="loess", se=FALSE)+
  geom_smooth(data=sanber, aes(y=mediana_real), method="loess",se=FALSE)+
  geom_smooth(data=costazul, aes(y=mediana_real), method="loess",se=FALSE)+
  #geom_smooth(data=pinamar, aes(y=mediana_real), method="loess")+
  geom_smooth(data=mardeajo, aes(y=mediana_real), method="loess")+
  
  
  labs(y = "Mediana real como proporción de 2017",
       x = "Fecha")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  scale_shape_discrete(name  ="Región")+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  
  scale_x_date(date_labels = "%y-%m-%d" )+ 
  
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-08-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-09-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-10-01"),linetype=1,color="gray",size=1)+
  
  geom_vline(xintercept = as.Date("2020-03-01"),linetype=1,color="brown",size=1,
             show.legend=TRUE) 
  # geom_text(aes(x=as.Date("2020-03-01"), label="Cuarentena", y=1.35),
  #           colour="brown", angle=90, vjust = -0.5)+
  # geom_text(aes(x=as.Date("2020-08-01"), label="Agosto", y=1.35),
  #           colour="grey", angle=90, vjust = -0.5)+
  # geom_text(aes(x=as.Date("2020-09-01"), label="Septiembre", y=1.35),
  #           colour="grey", angle=90, vjust = -0.5)+
  # geom_text(aes(x=as.Date("2020-10-01"), label="Octubre", y=1.35),
  #           colour="grey", angle=90, vjust = -0.5)

p




#################################################################
##                         More graphs                         ##
#################################################################
tigre<-test[
  test$ITE_ADD_CITY_NAME=="tigre",]
necochea<-test[
  test$ITE_ADD_CITY_NAME=="necochea",]
costazul<-test[
  test$ITE_ADD_CITY_NAME=="costa azul",]
nrow(costazul)

mardelsur<-test[
  test$ITE_ADD_CITY_NAME=="mar del sur",]
tuyu<-test[
  test$ITE_ADD_CITY_NAME=="mar del tuyu",]
esmeralda<-test[
  test$ITE_ADD_CITY_NAME=="costa esmeralda",]
pinamar<-test[
  test$ITE_ADD_CITY_NAME=="pinamar",]
carilo<-test[
  test$ITE_ADD_CITY_NAME=="carilo",]
nrow(carilo)
mardeajo<-test[
  test$ITE_ADD_CITY_NAME=="mar de ajo",]
#sanmi<-test[
#  test$ITE_ADD_CITY_NAME=="san miguel",]
#escobar<-test[
# test$ITE_ADD_CITY_NAME=="escobar",]
pilar<-test[
  test$ITE_ADD_CITY_NAME=="pilar",]
#laplata<-test[
# test$ITE_ADD_CITY_NAME=="la plata",]
sanisidro<-test[
  test$ITE_ADD_CITY_NAME=="san isidro",]
vicent<-test[
  test$ITE_ADD_CITY_NAME=="vicente lopez",]
nrow(vicent)
ostende<-test[
  test$ITE_ADD_CITY_NAME=="ostende",]
nrow(ostende)
mardel<-test[
  test$ITE_ADD_CITY_NAME=="mar del plata",]
nrow(mardel)
sanber<-test[
  test$ITE_ADD_CITY_NAME=="san bernardo",]
nrow(sanber)









# Current prices

p <- ggplot(test, aes(x = MONTH)) + 
 # geom_line(data=mardel,aes(y = mediana, colour = "Mar del Plata"))+
  #geom_line(data=pinamar,aes(y = mediana, colour = "Pinamar"))+
  geom_line(data=esmeralda,aes(y = mediana, colour = "esmeralda"))+
  #geom_line(data=tuyu,aes(y = mediana, colour = "Pinamar"))+
  #geom_line(data=sanber,aes(y = mediana, colour = "San Bernardo"))+
  
  #geom_line(data=pilar,aes(y = mediana, colour = "Pilar"))+
  #geom_line(data=sanmi,aes(y = mediana, colour = "San Miguel"))+
  #geom_line(data=escobar,aes(y = mediana, colour = "Escobar"))+
  #geom_line(data=laplata,aes(y = mediana, colour = "La Plata"))+
  #geom_line(data=sanisidro,aes(y = mediana, colour = "San Isidro"))+
  #geom_line(data=vicent,aes(y = mediana, colour = "Vicente López"))+
 # geom_line(data=carilo,aes(y = mediana, colour = "carilo"))+
  #geom_line(data=mardeajo,aes(y = mediana, colour = "Mar de ajó"))+
  
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Mediana real Pinamar")) +
  labs(y = "Mediana corriente",
       x = "Fecha")  +
  # scale_colour_manual(values = c("blue", "red","green","orange","cyan","yellow","black")) +
  scale_shape_discrete(name  ="Región")+
  theme(legend.position = c(0.8, 0.9)) +
  theme_bw(base_size = 13) +
  theme(legend.title=element_blank())+
  scale_x_date(date_labels = "%y-%m-%d")+ 
  geom_vline(xintercept = as.Date("2017-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2018-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2019-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-01-01"),linetype=1,color="black",size=1)+
  geom_vline(xintercept = as.Date("2020-08-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-09-01"),linetype=1,color="gray",size=1)+
  geom_vline(xintercept = as.Date("2020-10-01"),linetype=1,color="gray",size=1)+
  
  geom_vline(xintercept = as.Date("2020-03-01"),linetype=1,color="brown",size=1,
             show.legend=TRUE) 


p





#################################################################
##                         Regressions                         ##
#################################################################


alquiler<-DT


# Subsets
alquiler<-alquiler[alquiler$ITE_ADD_CITY_NAME %in% costacities,]

alquiler<-alquiler[alquiler$INMUEBLE=="Casa",]

nrow(alquiler)

# TRUNCAR
count=1
#alquiler<-alquiler[,c("MONTH","ITE_ADD_CITY_NAME","precio_real","METROS")]
for (month in sort(unique(alquiler$MONTH)) ) {
  activo1 <- alquiler[alquiler$MONTH==month,]
  
  # COTAS
  activo2 <- activo1 %>% group_by(ITE_ADD_CITY_NAME)%>% 
    mutate(p05=quantile(ITE_SITE_CURRENT_PRICE,probs=0.05,na.rm=TRUE))%>%
    mutate(p95=quantile(ITE_SITE_CURRENT_PRICE,probs=0.95,na.rm=TRUE))%>%
    mutate(q05=quantile(METROS,probs=0.05,na.rm=TRUE))%>%
    mutate(q95=quantile(METROS,probs=0.95,na.rm=TRUE))
  
  if (count>1){
    activo3<-rbind(activo3,activo2)
  } else{
    activo3<-activo2
  }
  print(month)
  count=count+1
}

# tests
sum(is.na(activo3) )==sum(is.na(baseflujoalquilercosta) )
nrow((activo3) )==nrow(baseflujoalquilercosta)

#test
# ciudad<-"pilar"
# activo3[activo3$TIM_DAY==paste0("2020-05-",round(runif(1,10,29)))&
#           activo3$ITE_ADD_CITY_NAME==ciudad,]$p05==
#   activo3[activo3$TIM_DAY==paste0("2020-05-",round(runif(1,10,29)))&
#             activo3$ITE_ADD_CITY_NAME==ciudad,]$p05

# ya truncado

baseflujoalquilercostatruncado<-activo3[activo3$ITE_SITE_CURRENT_PRICE<=activo3$p95& 
                                          activo3$ITE_SITE_CURRENT_PRICE>=activo3$p05&
                                          activo3$METROS<=activo3$q95&activo3$METROS>=activo3$q05,] 


# Avoid scientific notation
options(scipen=999)

# costacities<-tolower(c("Miramar"  ,           "Mar del Plata"  ,     "Pinamar"     ,        "Villa Gesell" ,
#                "Valeria del Mar"   ,  "Chapadmalal"      ,   "La Lucila del Mar" ,           
#                "San Bernardo"    ,    "Costa Esmeralda"  ,   "Mar Azul"     ,       "Santa Clara del Mar",
#                "Mar de Ajo"   ,       "Santa Teresita"     , "Mar de las Pampas" ,  "San Clemente"    ,   
#                "Costa del Este"    ,  "Ostende"   ,          "Mar del Tuyu"    ,    "Costa Azul" ,        
#                "Aguas Verdes"     ,   "Las Toninas"  ,              "Necochea"    ,       
#                "Nueva Atlantis"  ,    "Mar De Cobo"  ,       "Carilo"    ,          "Mar del Sur" ,       
#                "Bahia Blanca"   ,     "Costa Chica"      ,  "Punta Medanos" ))
# # Missclassified places
# baseflujoalquilercosta[baseflujoalquilercosta$ITE_ADD_CITY_NAME %in% costacities,]$AGRUPACION<-"costa"
# 
# GBA<-unique(baseflujoalquilercostatruncado[baseflujoalquilercostatruncado$ITE_ADD_STATE_NAME %in% 
#                                              c("Bs.As. G.B.A. Norte","Bs.As. G.B.A. Sur","Bs.As. G.B.A. Oeste" ),]$ITE_ADD_CITY_NAME)
# 
# #GBA<-unique(baseflujoalquilercosta[baseflujoalquilercosta$ITE_ADD_STATE_NAME %in% 
# #                                     c("Bs.As. G.B.A. Norte"),]$ITE_ADD_CITY_NAME)
# # CHECK
# baseflujoalquilercostatruncado[baseflujoalquilercostatruncado$MONTH=="2020-06"&
#                                  baseflujoalquilercostatruncado$ITE_ADD_CITY_NAME=="presidente peron",]
# 
# baseflujoalquilercosta[baseflujoalquilercosta$MONTH=="2020-06"&
#                          baseflujoalquilercosta$ITE_ADD_CITY_NAME=="presidente peron",]

# Renaming
testn<-baseflujoalquilercostatruncado

# Inflation
bcra <- read_excel("input/Inflación BCRA.xlsx")
bcra$Fecha <-format(as.Date(bcra$Fecha),"%Y-%m")


# Merging inflation with datasets and getting real values
baseflujoalquilercosta<-testn
baseflujoalquilercosta <-
  merge(baseflujoalquilercosta, bcra, by.x="MONTH",by.y="Fecha",all=TRUE)
baseflujoalquilercosta$precioreal <-
  baseflujoalquilercosta$ITE_SITE_CURRENT_PRICE/baseflujoalquilercosta$Pondera*100

#renaming
baseflujoalquilercostatruncado<-baseflujoalquilercosta







# Getting coefficients for many regressions

count=1
for (ciudad in costacities){
  # renaming
  test<-baseflujoalquilercostatruncado[baseflujoalquilercostatruncado$INMUEBLE=="Casa",]
  test<-subset(test,ITE_ADD_CITY_NAME==ciudad)
  if (nrow(test)==0){next}
  else { 
    # formatting date
    print(sum(is.na(test$MONTH)))
    #test<-data.table::na.omit(test, cols="MONTH")
    test$MONTH<-as.Date(paste(test$MONTH,"-01",sep=""))
    #Adding month dummy
    test$month <- format(as.Date(test$MONTH), "%Y-%m")
    
    
    # adding time trend
    test$trend<-NA
    test$trend<-ifelse(endsWith(as.character(test$month),"2017-01"),1,
                       ifelse(endsWith(test$month,"2017-02"),2,
                              ifelse(endsWith(test$month,"2017-03"),3,
                                     ifelse(endsWith(test$month,"2017-04"),4,
                                            ifelse(endsWith(test$month,"2017-05"),5,
                                                   ifelse(endsWith(test$month,"2017-06"),6,
                                                          ifelse(endsWith(test$month,"2017-07"),7,
                                                                 ifelse(endsWith(test$month,"2017-08"),8,
                                                                        ifelse(endsWith(test$month,"2017-09"),9,
                                                                               ifelse(endsWith(test$month,"2017-10"),10,
                                                                                      ifelse(endsWith(test$month,"2017-11"),11,
                                                                                             ifelse(endsWith(test$month,"2017-12"),12,
                                                                                                    ifelse(endsWith(as.character(test$month),"2018-01"),13,
                                                                                                           ifelse(endsWith(test$month,"2018-02"),14,
                                                                                                                  ifelse(endsWith(test$month,"2018-03"),15,
                                                                                                                         ifelse(endsWith(test$month,"2018-04"),16,
                                                                                                                                ifelse(endsWith(test$month,"2018-05"),17,
                                                                                                                                       ifelse(endsWith(test$month,"2018-06"),18,
                                                                                                                                              ifelse(endsWith(test$month,"2018-07"),19,
                                                                                                                                                     ifelse(endsWith(test$month,"2018-08"),20,
                                                                                                                                                            ifelse(endsWith(test$month,"2018-09"),21,
                                                                                                                                                                   ifelse(endsWith(test$month,"2018-10"),22,
                                                                                                                                                                          ifelse(endsWith(test$month,"2018-11"),23,
                                                                                                                                                                                 ifelse(endsWith(test$month,"2018-12"),24,NA))))))))))))  ))))))))))))
    test$trend<-ifelse(endsWith(as.character(test$month),"2019-01"),25,
                       ifelse(endsWith(test$month,"2019-02"),26,
                              ifelse(endsWith(test$month,"2019-03"),27,
                                     ifelse(endsWith(test$month,"2019-04"),28,
                                            ifelse(endsWith(test$month,"2019-05"),29,
                                                   ifelse(endsWith(test$month,"2019-06"),30,
                                                          ifelse(endsWith(test$month,"2019-07"),31,
                                                                 ifelse(endsWith(test$month,"2019-08"),32,
                                                                        ifelse(endsWith(test$month,"2019-09"),33,
                                                                               ifelse(endsWith(test$month,"2019-10"),34,
                                                                                      ifelse(endsWith(test$month,"2019-11"),35,
                                                                                             ifelse(endsWith(test$month,"2019-12"),36,
                                                                                                    ifelse(endsWith(as.character(test$month),"2020-01"),37,
                                                                                                           ifelse(endsWith(test$month,"2020-02"),38,
                                                                                                                  ifelse(endsWith(test$month,"2020-03"),39,
                                                                                                                         ifelse(endsWith(test$month,"2020-04"),40,
                                                                                                                                ifelse(endsWith(test$month,"2020-05"),41,
                                                                                                                                       ifelse(endsWith(test$month,"2020-06"),42,
                                                                                                                                              ifelse(endsWith(test$month,"2020-07"),43,
                                                                                                                                                     ifelse(endsWith(test$month,"2020-08"),44,
                                                                                                                                                            ifelse(endsWith(test$month,"2020-09"),45,
                                                                                                                                                                   ifelse(endsWith(test$month,"2020-10"),46,
                                                                                                                                                                          ifelse(endsWith(test$month,"2020-11"),47,
                                                                                                                                                                                 ifelse(endsWith(test$month,"2020-12"),48,test$trend))))))))))))
                                                                                             ))))))))))))
    
    
    test$dMONTH<-ifelse(endsWith(as.character(test$month),"01"),1,
                        ifelse(endsWith(test$month,"02"),2,
                               ifelse(endsWith(test$month,"03"),3,
                                      ifelse(endsWith(test$month,"04"),4,
                                             ifelse(endsWith(test$month,"05"),5,
                                                    ifelse(endsWith(test$month,"06"),6,
                                                           ifelse(endsWith(test$month,"07"),7,
                                                                  ifelse(endsWith(test$month,"08"),8,
                                                                         ifelse(endsWith(test$month,"09"),9,
                                                                                ifelse(endsWith(test$month,"10"),10,
                                                                                       ifelse(endsWith(test$month,"11"),11,
                                                                                              ifelse(endsWith(test$month,"12"),12,NA))))))))))))
    # MONTH Dummies again
    test$ene <- ifelse(test$dMONTH==1,1,0)
    test$feb<- ifelse(test$dMONTH==2,1,0)
    test$mar<- ifelse(test$dMONTH==3,1,0)
    test$apr <- ifelse(test$dMONTH==4,1,0)
    test$may<- ifelse(test$dMONTH==5,1,0)
    test$jun<- ifelse(test$dMONTH==6,1,0)
    test$jul<- ifelse(test$dMONTH==7,1,0)
    test$aug<- ifelse(test$dMONTH==8,1,0)
    test$sept<- ifelse(test$dMONTH==9,1,0)
    test$oct<- ifelse(test$dMONTH==10,1,0)
    test$nov<- ifelse(test$dMONTH==11,1,0)
    test$dec<- ifelse(test$dMONTH==12,1,0)
    # Month dummies for 2020
    test$aug2020 <- ifelse(test$MONTH=="2020-08-01",1,0)
    test$sept2020 <- ifelse(test$MONTH=="2020-09-01",1,0)
    test$oct2020 <- ifelse(test$MONTH=="2020-10-01",1,0)
    # log price
    test$logprice<-log(test$precioreal)
    
    # dummy anual
    test$year17<-ifelse(startsWith(test$month,"2017"),1,0)
    test$year18 <-ifelse(startsWith(test$month,"2018"),1,0) 
    test$year19 <-ifelse(startsWith(test$month,"2019"),1,0) 
    test$year20  <-ifelse(startsWith(test$month,"2020"),1,0)
    
    # model 1
    model1<-lm(logprice~trend+aug2020+sept2020+oct2020+ene+feb+mar+apr+may+jun+jul+aug+sept+aug+oct+nov,data=test)
    
    summary<-summary(model1)
    # First loop create dataframe then it adds more rows
    if (count==1){
      resultados<-as.data.frame(model1$coefficients[3:5])
      resultados$pvalor<-summary(model1)$coefficients[,4][3:5]
      resultados$R2<-summary(model1)$r.squared
      resultados$n<-length(model1$residuals)
      resultados$Ciudad<-paste(ciudad)
    } else { 
      resultados1<-as.data.frame(model1$coefficients[3:5])
      resultados1$pvalor<-summary(model1)$coefficients[,4][3:5]
      resultados1$R2<-summary(model1)$r.squared
      resultados1$n<-length(model1$residuals)
      resultados1$Ciudad<-paste(ciudad)
      resultados<-rbind(resultados,resultados1)
    }
    count<-count+1
  }
}
#Names
names(resultados)[names(resultados) == "model1$coefficients[3:5]"] <- "Coeficiente"

# Stars: ***
resultados$significatividad<-ifelse(resultados$pvalor<0.001,"***",ifelse(
  resultados$pvalor<0.01,"**",ifelse(
    resultados$pvalor<0.05,"*",ifelse(
      resultados$pvalor<0.1,".","NA"
    )
  )
))

# Drop NAS
citieswithNA<-unique(resultados[is.na(resultados$Coeficiente),]$Ciudad)
resultados<-resultados[!(resultados$Ciudad %in% citieswithNA),]  

# Split dataframe for latex

split1 <- resultados[1:(nrow(resultados)/2),]
split2<-resultados[(nrow(resultados)/2):(nrow(resultados)),]
stargazer(split1, summary = FALSE, notes="*** si p-valor $<$ 0.001, ** si p-valor $<$ 0.01, * si p-valor $<$ 0.05, . si p-valor $<$ 0.1",
          title="Resultado de regresiones, controlando por mes, tendencia lineal, variable dependiente logaritmo del precio en pesos, Casas")
stargazer(split2, summary = FALSE, notes="*** si p-valor $<$ 0.001, ** si p-valor $<$ 0.01, * si p-valor $<$ 0.05, . si p-valor $<$ 0.1", 
          label="tabla2", title="Resultado de regresiones, controlando por mes, tendencia lineal, variable dependiente logaritmo del precio en pesos, Casas")
#stargazer(resultados, summary = FALSE, notes="*** si p-valor $<$ 0.001, ** si p-valor $<$ 0.01, * si p-valor $<$ 0.05, . si p-valor $<$ 0.1",
#title="Resultado de regresiones, controlando por mes, tendencia lineal, variable dependiente logaritmo del precio en pesos, Casas")



##################################################################
##                      Tables for report                       ##
##################################################################

# Signif coeffs
citieswithsignif<-unique(resultados[!(resultados$pvalor>0.1),]$Ciudad)

tablainforme<-resultados[(resultados$Ciudad %in% citieswithsignif),]  

# Non negative coeffs
tablainforme<-tablainforme[!(tablainforme$Coeficiente<0),]  

stargazer(tablainforme, summary = FALSE, notes="*** si p-valor $<$ 0.001, ** si p-valor $<$ 0.01, * si p-valor $<$ 0.05, . si p-valor $<$ 0.1",
          title="Resultado de regresiones, controlando por mes, tendencia lineal, variable dependiente logaritmo del precio en pesos, alquiler temporario de casas",
          label ="tablareg1")





# CHECKING
nrow(DT[DT$ITE_ADD_CITY_NAME=="costa esmeralda",])
median(DT[DT$ITE_ADD_CITY_NAME=="costa esmeralda"&
            DT$MONTH=="2020-10",]$ITE_SITE_CURRENT_PRICE)

median(DT[DT$ITE_ADD_CITY_NAME=="costa esmeralda"&
            DT$MONTH=="2019-03",]$ITE_SITE_CURRENT_PRICE)




