
##########################################################################
#                                                                        #
# Una aproximacion metodologica al uso de datos de encuestas en hogares  #
#                                                                        #
##########################################################################


# Todas las bases de datos de la ENOE pueden ser obtenidas en la pagina de
# internet del INEGI: www.inegi.org.mx

# Para llevar a cabo los ejercicios se utilizaran los datos del primer
# trimestre de 2016 y las tabla: SDEMT116.dbf


#0. Consideraciones iniciales

    #0.1  Instalar los paquete necesarios
          install.packages(c("foreign","data.table","questionr","survey"))

    #0.2  Cargar las librerias que se van a utilizar
          library(data.table)
          library(foreign)
          library(questionr)
          library(survey)

    #0.5  Una vez cargada la base de datos, se recodifican las variables

            # Factor
            SDEMT116$SEX <-as.factor(SDEMT116$SEX)
            SDEMT116$NIV_INS <-as.factor(SDEMT116$NIV_INS)
            SDEMT116$E_CON <-as.factor(SDEMT116$E_CON)
            SDEMT116$POS_OCU <-as.factor(SDEMT116$POS_OCU)
            # Numéricas
            SDEMT116$HRSOCUP <-as.numeric(as.character(SDEMT116$HRSOCUP))
            SDEMT116$EDA <-as.numeric(as.character(SDEMT116$EDA))
            SDEMT116$R_DEF <-as.numeric(as.character(SDEMT116$R_DEF))
            SDEMT116$C_RES <-as.numeric(as.character(SDEMT116$C_RES))
            # Filtro
            SDEMT116$filtro<- 0
            SDEMT116$filtro[SDEMT116$CLASE2 == 1 & SDEMT116$EDA>=15 & SDEMT116$EDA<=98 & SDEMT116$R_DEF==0 & (SDEMT116$C_RES==1 | SDEMT116$C_RES==3)] <- 1
            SDEMT116$filtro <-as.numeric(SDEMT116$filtro)
            SD<-SDEMT116[which(SDEMT116$filtro==1),]


#1. Razon por series de Taylor

    #1.1. Muestreo aleatorio simple
              ds_enoe1<-svydesign(id=~1,weight=~FAC,data=SD)
              svy1<-round(svymean(~HRSOCUP, ds_enoe1, deff=TRUE),2)
              cv1<-round(data.frame(cv(svy1)*100),digits=2)
              x1<-data.frame(svy1,cv1);colnames(x1)[4] <- "CV"
              rm(ds_enoe1,svy1,cv1)
    #1.2. Muestreo por conglomerados
              ds_enoe2<-svydesign(id=~UPM, weight=~FAC, data=SD, nest=TRUE)
              svy2<-round(svymean(~HRSOCUP, ds_enoe2, deff=TRUE),2)
              cv2<-round(data.frame(cv(svy2)*100),digits=2)
              x2<-data.frame(svy2,cv2);colnames(x2)[4] <- "CV"
              rm(ds_enoe2,svy2,cv2)
    #1.3. Muestreo estratificado
              ds_enoe3<-svydesign(id=~1, strata=~EST_D, weight=~FAC, data=SD, nest=TRUE)
              svy3<-round(svymean(~HRSOCUP, ds_enoe3, deff=TRUE),2)
              cv3<-round(data.frame(cv(svy3)*100),digits=2)
              x3<-data.frame(svy3,cv3);colnames(x3)[4] <- "CV"
              rm(ds_enoe3,svy3,cv3)
    #1.4. Muestreo estratificado y por conglomerados
              ds_enoe4<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=SD, nest=TRUE)
              svy4<-round(svymean(~HRSOCUP, ds_enoe4, deff=TRUE),2)
              cv4<-round(data.frame(cv(svy4)*100),digits=2)
              x4<-data.frame(svy4,cv4);colnames(x4)[4] <- "CV"
              rm(ds_enoe4,svy4,cv4)
    #1.5. Resultado
              Taylor<-rbind(x1, x2,x3,x4)
              row.names(Taylor)<-c("MAS","Conglomerados","Estratificado","Est/Cong")
              Taylor


#2. Razon por Bootstrap

    #2.1. Muestreo aleatorio simple
              enoe1<-svydesign(id=~1,weight=~FAC, data=SD, nest=TRUE)
              boot_design1<-as.svrepdesign (enoe1,type="bootstrap",replicates=80)
              svy21<-round(svymean(~HRSOCUP, boot_design1, deff=TRUE),2)
              cv21<-round(data.frame(cv(svy21)*100),digits=2)
              x21<-data.frame(svy21,cv21);colnames(x21)[4] <- "CV"
              rm(enoe1,svy21,cv21,boot_design1)
    #2.2. Muestreo por conglomerados
              enoe2<-svydesign(id=~UPM,weight=~FAC, data=SD, nest=TRUE)
              boot_design2<-as.svrepdesign (enoe2,type="bootstrap",replicates=80)
              svy22<-round(svymean(~HRSOCUP, boot_design2, deff=TRUE),2)
              cv22<-round(data.frame(cv(svy22)*100),digits=2)
              x22<-data.frame(svy22,cv22);colnames(x22)[4] <- "CV"
              rm(enoe2,svy22,cv22,boot_design2)
    #2.3. Muestreo estratificado
              enoe3<-svydesign(id=~1,strata=~EST_D,weight=~FAC, data=SD, nest=TRUE)
              boot_design3<-as.svrepdesign (enoe3,type="bootstrap", replicates=80)
              svy23<-round(svymean(~HRSOCUP, boot_design3, deff=TRUE),2)
              cv23<-round(data.frame(cv(svy23)*100),digits=2)
              x23<-data.frame(svy23,cv23);colnames(x23)[4] <- "CV"
              rm(enoe3,svy23,cv23,boot_design3)
    #2.4. Muestreo estratificado y por conglomerados
              enoe4<-svydesign(id=~UPM,strata=~EST_D,weight=~FAC, data=SD, nest=TRUE)
              boot_design4<-as.svrepdesign (enoe4,type="bootstrap", replicates=80)
              svy24<-round(svymean(~HRSOCUP, boot_design4, deff=TRUE),2)
              cv24<-round(data.frame(cv(svy24)*100),digits=2)
              x24<-data.frame(svy24,cv24);colnames(x24)[4] <- "CV"
              rm(enoe4,svy24,cv24,boot_design4)
    #2.5. Resultado
              Bot<-rbind(x21, x22,x23,x24)
              row.names(Bot)<-c("MAS","Conglomerados","Estratificado","Est/Cong")
              Bot
              rm(x1,x2,x3,x4,x21,x22,x23,x24)

#3. Tabulados
              
    #3.1. Tabulado considerando el muestreo estratificado y por conglomerados
              SD2<-SD[which(SD$SEX == 2),]
              ds_enoe8<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=SD2, nest=TRUE)
              options(survey.lonely.psu="adjust")
              svy8<-round(svytotal(~factor(CS_P13_1), ds_enoe8, deff=TRUE), digits = 2)
              cv8<-round(data.frame(cv(svy8)*100),digits=2)
              x8<-data.frame(svy8,cv8);colnames(x8)[4] <- "CV"
              x8

#4. Regresion Lineal
              
          #4.1 Modelo con series de Taylor
                ds_enoe<-svydesign(id=~UPM, strata=~EST_D, weight=~FAC, data=SD, nest=TRUE)
                options(survey.lonely.psu="adjust")
                glmsvy1 <- svyglm(HRSOCUP~SEX+NIV_INS+E_CON+POS_OCU+EDA, design=ds_enoe)
                summary(glmsvy1, df.resid = degf(ds_enoe))
                glmsvy2 <- glm(HRSOCUP~SEX+NIV_INS+E_CON+POS_OCU+EDA, data=SD)
                summary(glmsvy2)

          #4.2 Modelo con Bootstrap
                boot_design<-as.svrepdesign (ds_enoe, type="bootstrap",replicates=100)
                options(survey.lonely.psu="adjust")
                glmsvy3 <- svyglm(HRSOCUP~SEX+NIV_INS+E_CON+POS_OCU+EDA, design=boot_design)
                summary(glmsvy3, df.resid = degf(boot_design))
                confint(glmsvy3, level = 0.95, method = c("Wald"))





