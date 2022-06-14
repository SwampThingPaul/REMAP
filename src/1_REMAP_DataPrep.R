## R-EMAP Data prep
## Created by: Paul Julian (pjulian@sccf.org/pauljulianphd@gmail.com)
## Created on: 06/14/2022

## BAD ## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)
library(openxlsx)

#GIS Libraries
library(rgdal)
library(rgeos)
library(tmap)
library(raster)

#Paths
wd="C:/Julian_LaCie/_GitHub/REMAP"
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/","/_documents/"))

plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

gen.gis="C:/Julian_LaCie/_GISData/"

# GIS ---------------------------------------------------------------------
wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")

#ogrListLayers(paste0(gen.gis,"SFER_GIS_Geodatabase.gdb"))
canal=spTransform(readOGR(paste0(gen.gis,"SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)
wcas=spTransform(readOGR(paste0(gen.gis,"SFER_GIS_Geodatabase.gdb"),"WCAs"),utm17)
enp=spTransform(readOGR(paste0(gen.gis,"SFER_GIS_Geodatabase.gdb"),"ENP"),utm17)

# REMAP Data --------------------------------------------------------------
# FROM REMAP data file
# PF 2014 = PF 2005 = PM 1995, 1996, 1999; periphyton floating mat
# PB 2014 = PB 2005 = PS 1995, 1996, 1999; periphyton benthic
# PE 2014 = PE 2005 = PU 1995, 1996, 1999; periphyton epiphytic
# PC 2014 =  PF 2014 + PE 2014;            periphyton composite

idvars.REMAP=c("CYCLE","STA_ID",'SUBAREA',"DECLONG","DECLAT","DATE")
chem.var.names=c(paste0(c("TP","AFDW","BD","MC"),".sed.",c("mgkg","per","gcc","per")),"sed.z.ft",
                 paste0(c("TP","AFDW","BD","MC"),".peri_epi.",c("mgkg","per","gcc","per")),
                 paste0(c("TP","AFDW","BD","MC"),".peri_mat.",c("mgkg","per","gcc","per")),
                 paste0(c("TP","AFDW","BD","MC"),".peri_ben.",c("mgkg","per","gcc","per")),
                 paste0(c("TP","AFDW","BD","MC"),".peri_comp.",c("mgkg","per","gcc","per")))
## Phase 1 & 2 ------------------------------------------------------------
p12=readxl::read_xls(paste0(data.path,"P12/P12join7FINAL.xls"),sheet=2)
colnames(p12)=sapply(strsplit(names(p12),"\\$"),"[",1);#removes "$" in column name
p12=data.frame(p12)
p12$DATE=date.fun(convertToDate(p12$DATE))
p12=subset(p12,is.na(DECLONG)==F|is.na(DECLAT)==F)
p12[p12==-9999]<-NA
p12[p12==-3047.6952]<-NA

ddply(p12,"CYCLE",summarise,min.date=min(DATE,na.rm=T),max.date=max(DATE,na.rm=T))

p12.vars=c(paste0(c("TP","AFDW","BD","MC"),"SDF"),"SOIL_TKNSFT",
       paste0(c("TP","AFDW","BD","MC"),"PUF"),
       paste0(c("TP","AFDW","BD","MC"),"PMF"),
       paste0(c("TP","AFDW","BD","MC"),"PSF"))
tmp=names(p12)[names(p12)%in%c(idvars.REMAP,p12.vars)]

fill=c(paste0("TP",c("PUF","PMF","PSF")),
       paste0(c("TP","AFDW","BD","MC"),"PCFS"))
p12[,fill]=NA

p12_v2=p12[,c(idvars.REMAP,p12.vars,paste0(c("TP","AFDW","BD","MC"),"PCFS"))]
p12_v2[,7:27]=apply(p12_v2[,7:27],2,as.numeric)

names(p12_v2)
colnames(p12_v2)=c(idvars.REMAP,
                   paste0(c("TP","AFDW","BD","MC"),".sed"),"SOIL_TKNSFT",
                   paste0(c("TP","AFDW","BD","MC"),".PE"),
                   paste0(c("TP","AFDW","BD","MC"),".PF"),
                   paste0(c("TP","AFDW","BD","MC"),".PB"),
                   paste0(c("TP","AFDW","BD","MC"),".PC"))

# ddply(p12_v2,"CYCLE",summarise,min.val=min(BDPSF,na.rm=T),max.val=max(BDPSF,na.rm=T))
## Phase 3 ----------------------------------------------------------------
p3=readxl::read_xls(paste0(data.path,"P3/epa904r07001.xls"),sheet=1)
p3=subset(p3,is.na(STATION)==F)
p3=data.frame(p3)
p3$DATE=with(p3,ifelse(DATE>50000,
                        paste(paste0("20",substring(DATE,4,5)),paste0("0",substring(DATE,1,1)),substring(DATE,2,3),sep="-"),
                        as.character(convertToDate(p3$DATE))))
p3$DATE=date.fun(p3$DATE)

p3.colname=c("STA_ID", "CYCLE", "SUBAREA", "SUBAREA7", "DATE", "TIME", 
             "LAT", "LONG", "DECLAT", "DECLONG", "X", "CREW", "CHOPPER", 
             "WEATHER", "FLOW", "CAMERA", "CHLAVOL", "YSIDEPTH", "TEMP", "DO", 
             "COND", "pH", "TURB", "WATDEPT1", "WATDEPT2", "WATDEPT3", "WATDEPAV", 
             "DEPBEDR1", "DEPBEDR2", "DEPBEDR3", "SOILTHI1", "SOILTHI2", "SOILTHI3", 
             "SOILTHAV", "PWEAgCl1", "PWEAgCl2", "PWEAgCl3", "PWEh1", "PWEh2", 
             "PWEh3", "PWEhAvg", "PERIPE", "PERIPB", "PERIPF", "PERIDOM", 
             "SOILTYPE", "SOILCTH1", "SOILCTH2", "SOILCTH3", "SOILCTHA", "DRYSOCD1", 
             "DRYSOCD2", "DRYSOCD3", "FLOCTH1", "FLOCTH2", "FLOCTH3", "FLOCTHA", 
             "VEGTYPE", "FISHNO1", "FISHNO2", "FLOCCONT", "FLOCCOL.", "COMMENTS", 
             "x1", "TPRSWFB", "TPRSWFB.ugL", "TPRSWFBQ", "SRPRSWFB", 
             "SRPRSWFB.ugL", "SRPRSWFQ", "TNRSWFB", "TNRSWFBQ", "FNH4RSWF", 
             "FNH4RSWQ", "FNNRSWFB", "FNNRSWFQ", "FNO3RSWF", "FNO3RSWQ", "FNO2RSWF", 
             "FNO2RSWQ", "TONSWFB", "TONSWFBQ", "TINSWFB", "TINSWFBQ", "CHLASWFB", 
             "CHLASWFQ", "APASWEE", "APASWEEQ", "DOCSWD", "DOCSWDQ", "BRSWEA", 
             "BRSWEAQ", "CLSWEA", "CLSWEAQ", "FSWEA", "FSWEAQ", "SO4SWEA", 
             "SO4SWEQ", "H2SSWEE", "H2SSWEEQ", "MEHGSWB", "MEHGSWBQ", "THGSWD", 
             "THGSWDQ", "x2", "FNH4RPWF", "FNH4RPWQ", "FNNRPWFB", "FNNRPWFQ", 
             "FNO3RPWF", "FNO3RPWQ", "FNO2RPWF", "FNO2RPWQ", "TINAPWFB", "TINAPWFQ", 
             "SRPRPWFB", "SRPRPWFQ", "FDOCPWD", "FDOCPWDQ", "BRPWEA", "BRPWEAQ", 
             "CLPWEA", "CLPWEAQ", "FPWEA", "FPWEAQ", "SO4PWEA", "SO4PWEAQ", 
             "H2SPWEE", "H2SPWEEQ", "x3", "pHSDEE", "TPRSDFB", "TPRSDFBQ", 
             "AVSSDA", "AVSSDAQ", "TCSDFB", "TCSDFBQ", "TNSDFB", "TNSDFBQ", 
             "MCSDFS", "MCSDFSQ", "BDSDFS", "BDSDFCQ","AFDWSDFS.m", "AFDWSDFS", "AFDWSDFSQ",
             "WCSDFS", "WCSDFSQ", "THGSDFC", "THGSDFCQ", "MEHGSDFC", 
             "MEHGSDFQ", "x4", "THGFCFC", "THGFCFCQ", "MEHGFCFC", "MEHGFCFQ", 
             "CHLAFCFS", "CHLAFCFQ", "TCFCFB", "TCFCFBQ", "TNFCFB", "TNFCFBQ", 
             "TPRFCFB", "TPRFCFBQ", "MCFCFS", "MCFCFSQ", "AFDWFCFS", "AFDWFCFQ", 
             "BDFCFS", "BDFCFSQ", "WCFCFS", "WCFCFSQ", "x5", "THGPFFC", 
             "THGPFFCQ", "THGPBFC", "THGPBFCQ", "THGPEFC", "THGPEFCQ", "MEHGPFFC", 
             "MEHGPFFQ", "MEHGPBFC", "MEHGPBFQ", "MEHGPEFC", "MEHGPEFQ", "x6", 
             "THGFSFC", "THGFSFCQ", "THGFSSD")
colnames(p3)=p3.colname
names(p3)

p3.colname[substring(p3.colname,1,4)%in%c('AFDW')]
p3.colname[substring(p3.colname,1,2)%in%c('MC')]
p3.colname[substring(p3.colname,1,2)%in%c('BD')]
p3.colname[substring(p3.colname,1,2)%in%c('TP')]

p3.vars=c("TPRSDFB",paste0(c("AFDW","BD","MC"),"SDFS"),"SOILTHAV",
           paste0(c("TP","AFDW","BD","MC"),"PUFC"),
           paste0(c("TP","AFDW","BD","MC"),"PFFC"),
           paste0(c("TP","AFDW","BD","MC"),"PBFC"))
names(p3)[names(p3)%in%c(idvars.REMAP,p3.vars)]

fill=c(paste0(c("TP","AFDW","BD","MC"),"PUFC"),
       paste0(c("TP","AFDW","BD","MC"),"PFFC"),
       paste0(c("TP","AFDW","BD","MC"),"PBFC"),
       paste0(c("TP","AFDW","BD","MC"),"PCFS"))

p3[,fill]=NA

p3_v2=p3[,c(idvars.REMAP,p3.vars,paste0(c("TP","AFDW","BD","MC"),"PCFS"))]

p3_v2[,c(4:5,7:27)]=apply(p3_v2[,c(4:5,7:27)],2,as.numeric)

colnames(p3_v2)=c(idvars.REMAP,
                   paste0(c("TP","AFDW","BD","MC"),".sed"),"SOIL_TKNSFT",
                   paste0(c("TP","AFDW","BD","MC"),".PE"),
                   paste0(c("TP","AFDW","BD","MC"),".PF"),
                   paste0(c("TP","AFDW","BD","MC"),".PB"),
                   paste0(c("TP","AFDW","BD","MC"),".PC"))

## P4 ---------------------------------------------------------------------
p4_1=read.xlsx(paste0(data.path,"P4/epa_everglades_emap_2013_data.xlsx"),sheet=1,startRow=2)
p4_1$STA_ID=p4_1$STATION
p4_1$DATE=date.fun(convertToDate(p4_1$DATE))
p4_1$SUBAREA=p4_1$SUBAREA4
p4_1$DECLONG=p4_1$TRIMBLE.LONG
p4_1$DECLAT=p4_1$TRIMBLE.LAT

names(p4_1)[names(p4_1)%in%idvars.REMAP]
p4_1[,c(idvars.REMAP,"SOILTHAV")]

p4_1.lab=read.xlsx(paste0(data.path,"P4/epa_everglades_emap_2013_data.xlsx"),sheet=2,startRow=2)
p4_1.lab$DATE=date.fun(convertToDate(p4_1.lab$DATE))
p4_1.lab$STA_ID=p4_1.lab$STATION
names(p4_1.lab)[substring(names(p4_1.lab),1,3)%in%c('ASH')]
names(p4_1.lab)[substring(names(p4_1.lab),1,2)%in%c('BD')]
names(p4_1.lab)[substring(names(p4_1.lab),1,2)%in%c('MC')]
names(p4_1.lab)[substring(names(p4_1.lab),1,2)%in%c('TP')]

# no MC data
p4.vars=c("TPSDFB",paste0(c("ASH","BD"),"SDFS"),
          "TPPBFB",paste0(c("ASH","BD"),"PBFS"),
          "TPPCFB",paste0(c("ASH","BD"),"PCFS"))
names(p4_1.lab)[names(p4_1.lab)%in%c(idvars.REMAP,p4.vars)]

fill=c(paste0(c("MC"),"SDFS"),
       paste0(c("TP","ASH","BD","MC"),"PUFC"),
       paste0(c("TP","ASH","BD","MC"),"PFFC"),
       paste0(c("MC"),"PBFS"),
       paste0(c("MC"),"PCFS"))
p4_1.lab[,fill]=NA

p4_1.chemvar1=c("TPSDFB",paste0(c("ASH","BD","MC"),"SDFS"),"SOILTHAV",
                paste0(c("TP","ASH","BD","MC"),"PUFC"),
                paste0(c("TP","ASH","BD","MC"),"PFFC"),
                "TPPBFB",paste0(c("ASH","BD","MC"),"PBFS"),
                "TPPCFB",paste0(c("ASH","BD","MC"),"PCFS"))
p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"]

length(c("STA_ID","DATE",p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"]))
tmp=names(p4_1.lab)[names(p4_1.lab)%in%c("STA_ID","DATE",p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"])]
tmp
length(tmp)


p4_1=merge(p4_1[,c(idvars.REMAP,"SOILTHAV")],
           p4_1.lab[,c("STA_ID","DATE",p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"])],
           c("STA_ID","DATE"))

p4_1=p4_1[,c(idvars.REMAP,p4_1.chemvar1)]

colnames(p4_1)=c(idvars.REMAP,
                  paste0(c("TP","AFDW","BD","MC"),".sed"),"SOIL_TKNSFT",
                  paste0(c("TP","AFDW","BD","MC"),".PE"),
                  paste0(c("TP","AFDW","BD","MC"),".PF"),
                  paste0(c("TP","AFDW","BD","MC"),".PB"),
                  paste0(c("TP","AFDW","BD","MC"),".PC"))
p4_1[,c(4:5,7:27)]=apply(p4_1[,c(4:5,7:27)],2,as.numeric)

colnames(p4_1)=c(idvars.REMAP,
                  paste0(c("TP","AFDW","BD","MC"),".sed"),"SOIL_TKNSFT",
                  paste0(c("TP","AFDW","BD","MC"),".PE"),
                  paste0(c("TP","AFDW","BD","MC"),".PF"),
                  paste0(c("TP","AFDW","BD","MC"),".PB"),
                  paste0(c("TP","AFDW","BD","MC"),".PC"))

###
p4_2=read.xlsx(paste0(data.path,"P4/epa_everglades_emap_2014_data.xlsx"),sheet=1,startRow=1)
p4_2$STA_ID=p4_2$STATION
p4_2$DATE=date.fun(convertToDate(p4_2$DATE))
p4_2$SUBAREA=p4_2$SUBAREA4
p4_2$DECLONG=p4_2$TRIMBLE.LONG
p4_2$DECLAT=p4_2$TRIMBLE.LAT

names(p4_2)[names(p4_2)%in%idvars.REMAP]
p4_2[,c(idvars.REMAP,"SOILTHAV")]

p4_2.lab=read.xlsx(paste0(data.path,"P4/epa_everglades_emap_2014_data.xlsx"),sheet=2,startRow=3)

p4_2.lab$STA_ID=p4_2.lab$Station
names(p4_2.lab)[substring(names(p4_2.lab),1,3)%in%c('ASH')]
names(p4_2.lab)[substring(names(p4_2.lab),1,2)%in%c('BD')]
names(p4_2.lab)[substring(names(p4_2.lab),1,2)%in%c('MC')]
names(p4_2.lab)[substring(names(p4_2.lab),1,2)%in%c('TP')]

p4_2.lab[,fill]=NA

length(c("STA_ID",p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"]))
tmp=names(p4_2.lab)[names(p4_2.lab)%in%c("STA_ID",p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"])]
tmp
length(tmp)

p4_2=merge(p4_2[,c(idvars.REMAP,"SOILTHAV")],
           p4_2.lab[,c("STA_ID",p4_1.chemvar1[p4_1.chemvar1!="SOILTHAV"])],
           c("STA_ID"))

p4_2=p4_2[,c(idvars.REMAP,p4_1.chemvar1)]
p4_2[,c(4:5,7:27)]=apply(p4_2[,c(4:5,7:27)],2,as.numeric)

colnames(p4_2)=c(idvars.REMAP,
                 paste0(c("TP","AFDW","BD","MC"),".sed"),"SOIL_TKNSFT",
                 paste0(c("TP","AFDW","BD","MC"),".PE"),
                 paste0(c("TP","AFDW","BD","MC"),".PF"),
                 paste0(c("TP","AFDW","BD","MC"),".PB"),
                 paste0(c("TP","AFDW","BD","MC"),".PC"))

remap.dat=rbind(p12_v2,
                p3_v2,
                p4_1,
                p4_2)
summary(remap.dat)
remap.dat=subset(remap.dat,is.na(DATE)==F)

colnames(remap.dat)=c(idvars.REMAP,chem.var.names)
write.csv(remap.dat,paste0(export.path,"REMAP_SED_PERI.csv"),row.names = F)
