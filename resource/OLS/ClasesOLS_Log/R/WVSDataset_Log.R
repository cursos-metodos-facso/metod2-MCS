#Remove any object
rm(list = ls())

# 1. Install packages ----------------------------------------------

library(pacman)
pacman::p_load(tidyr,
               dplyr,
               sjPlot,
               sjmisc,
               psych,
               texreg,
               haven,
               ggplot2,
               lmtest,
               DescTools)

# 2. Cargar datos  ---------------------------------------------------------

## 2.1 WVS (2005-2022): N = 1.457

WVS_2005_2022_Chl<- readRDS(file = "Data/WVS_2005_2022_Chl.rds")

#3. Descriptivos------
summary(WVS_2005_2022_Chl) #con comando de paquete haven
describe(WVS_2005_2022_Chl) #con comando de paquete psych


#4. Regresiones logisticas-------

#Tabla de contingencia bivariado: sindicalizacion / part marchas
WVS_2005_2022_Chl <- WVS_2005_2022_Chl %>%
  mutate(Unionized = labelled(.$Unionized,c("No"=0,"Sí"=1)),
         demonstr_dummy = labelled(.$demonstr_dummy,c("No"=0,"Sí"=1)))
  
WVS_2005_2022_Chl <- as.data.frame(WVS_2005_2022_Chl) #para que la base quede como data frame (necesario para las figuras)
 
frq(WVS_2005_2022_Chl$Unionized)
frq(WVS_2005_2022_Chl$demonstr_dummy)


sjPlot::tab_xtab(var.col = WVS_2005_2022_Chl$Unionized, 
                 var.row = WVS_2005_2022_Chl$demonstr_dummy, 
                 title = "Participación en marchas según afiliación sindical", 
                 show.col.prc = TRUE,
                 value.labels = TRUE)

#Variable dependiente: participacion en marchas-
#Modelo de Probabilidad lineal
m1mpl <- lm(demonstr_dummy ~ Unionized + Wave,
            data = WVS_2005_2022_Chl)
screenreg(m1mpl,
          custom.model.names = "Modelo de Prob Lineal",
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")



#Modelos de regresión logística

m0log <- glm(demonstr_dummy~ Unionized,
             data = WVS_2005_2022_Chl,family = "binomial"(link = "logit"))

m1log <- glm(demonstr_dummy~ Unionized + Wave,
                   data = WVS_2005_2022_Chl,family = "binomial"(link = "logit"))


#nota: "logit" viene por defecto en la opción "binomial", por eso no es necesario 
#incluirla explícitamente en el código (tal como lo hago en los modelos sgtes)

m2log <- glm(demonstr_dummy~ Unionized + Female + X003 + Educ + private_sector 
             + Wave,
             data = WVS_2005_2022_Chl,family = "binomial")
m3log <- glm(demonstr_dummy~ Unionized + Female + X003 + Educ + private_sector 
             + politicization + Wave,
             data = WVS_2005_2022_Chl,family = "binomial")

screenreg(list(m1mpl,m1log,m2log,m3log),
          custom.model.names = c("M1 (m prob lineal)","M1 (log odds)","M2 (log odds)","M3 (log odds)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")



# 5.1 Estimacion de Odds rations----
exp(coef(m1log)) #comando básico

### Cálculo de OR para cada modelo
m0log_OR <- exp(coef(m0log))
m1log_OR <- exp(coef(m1log))
m2log_OR <- exp(coef(m2log))
m3log_OR <- exp(coef(m3log))

##Odds ratios en tabla de texreg
screenreg(list(m1log,m2log,m3log),
          override.coef = list(m1log_OR,m2log_OR,m3log_OR),
          custom.model.names = c("m1 (OR)","m2 (OR)","m3 (OR)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")
#Nota: errores estándares en esta tabla NO tienen  sentido (no están calculados a partir de OR, sino de log odds)
#Es mejor no reportarlos si solo se van a presentar odds ratios

#Misma tabla, en log odds
screenreg(list(m1log,m2log,m3log),
          custom.model.names = c("m1 (log odds)","m2 (log odds)","m3 (log odds)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")


#Tabla básica: sólo sindicalizacion como vble independ
screenreg(m0log,
          custom.model.names = c("m0 (log odds)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")


screenreg(m0log,
          override.coef = m0log_OR,
          custom.model.names = c("m0 (OR)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")


# 5.2. Bondad de ajuste (comando de paquete lmtest)-----

#Razon de verosimilitudes 
anova(m1log, m2log, test = "Chisq") 
anova(m2log, m3log, test = "Chisq") 
lrtest(m1log, m2log) #likelihood ratio test / Prueba de razón de verosimilitud (comparación m1-m2)

lrtest(m2log, m3log) #likelihood ratio test / Prueba de razón de verosimilitud (comparación m2-m3)

# Pseudo R2 (McFadden)

m1log_R2<-DescTools::PseudoR2(m1log)
m2log_R2<-DescTools::PseudoR2(m2log)
m3log_R2<-DescTools::PseudoR2(m3log)


#Misma tabla, en log odds, con Pseudo R2
screenreg(list(m1log,m2log,m3log),
          custom.model.names = c("m1 (log odds)","m2 (log odds)","m3 (log odds)"),
          custom.gof.rows=list("Pseudo R2" = c(m1log_R2, m2log_R2,m3log_R2)),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")





# Cálculo de probabilidades predichas----

FigSind_1_Prob<-ggeffects::ggpredict(m3log, terms = c("Unionized")) %>% 
  ggplot(aes(x=x, y=predicted))+
  geom_bar(stat="identity",color="grey", fill="grey")+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=.1) +
  labs(title="Sindicalización", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(size = 12), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
  scale_x_continuous(name = "",breaks = c(0,1),labels = c("Non-union members", "Union members"))+
  scale_y_continuous(limits = c(0,0.35), breaks=seq(0,0.35, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L))


#Politizacion (0 a 6)
FigPolit_1_Prob<-ggeffects::ggpredict(m3log, terms="politicization")%>%  
  ggplot(mapping=aes(x = x, y=predicted))+
  labs(title="Politización", x = "", y = "")+
  theme_bw() +
  geom_smooth()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill = "black") +
  theme(plot.title = element_text(size = 12), 
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10),
        axis.text.y = element_text(vjust = 0.5, size = 10))+
  scale_x_continuous(breaks=seq(0,6, by = 1))+
  scale_y_continuous(limits = c(0,0.6), breaks=seq(0,0.6, by = 0.1),
                     labels = scales::percent_format(accuracy = 1L))

### 5.2. Efectos de interacción ------

#Efectos de interacción: sindicalizacion - sector privado

m3.1log <- glm(demonstr_dummy~ Unionized + Female + X003 + Educ + private_sector
               + politicization + Wave + Unionized*private_sector,
             data = WVS_2005_2022_Chl,family = "binomial")
screenreg(list(m1log,m2log,m3log,m3.1log),
          custom.model.names = c("M1 (log odds)","M2 (log odds)","M3 (log odds)",
                                 "M3.1 (log odds)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")

#Figura 
FigSindSector_int<-ggeffects::ggpredict(m3.1log, terms = c("Unionized", "private_sector")) %>%
  ggplot(aes(x=x, y=predicted, shape = group, color = group)) +
  geom_line(aes(group=group,linetype = group),position = position_dodge(.1)) + 
  geom_point(size = 2.5,position = position_dodge(.1))+
  scale_x_continuous(name = "", breaks=c(0,1), labels = c("No sindicalizados/as", "Sindicalizados/as")) + 
  scale_shape_discrete(name = "Sector de empleo",
                       limits = c("0", "1"),
                       labels = c("Público", "Privado")) +
  scale_color_manual(name = "Sector de empleo",
                     limits = c("0", "1"),
                     labels = c("Público", "Privado"),
                     values = c("black", "black")) +
  scale_linetype_manual(name = "Sector de empleo",
                        limits = c("0", "1"),
                        labels = c("Público", "Privado"),
                        values = c("solid", "dashed")) +
  scale_y_continuous(limits = c(0,0.40), breaks=seq(0,0.40, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  labs(title="", y = "") + 
  theme(plot.title = element_text(size = 11),
        axis.text=element_text(size=11))





# Sector de empleo - politización (no está en PPT)
m3.2log <- glm(demonstr_dummy~ Unionized + Female + age + Educ + private_sector 
               + politicization + Wave + private_sector*politicization,
               data = WVS_2005_2022_Chl,family = "binomial")
screenreg(list(m1log,m2log,m3log,m3.1log,m3.2log),
          custom.model.names = c("M1 (log odds)","M2 (log odds)","M3 (log odds)",
                                 "M3.1 (log odds)","M3.2 (log odds)"),
          digits = 3, 
          stars = c(0.001, 0.01, 0.05, 0.1),symbol = "†")

#Figura
FigPolitSector_int<-ggeffects::ggpredict(m3.2log, terms = c("politicization", "private_sector")) %>%
  ggplot(aes(x=x, y=predicted, shape = group, color = group)) +
  geom_line(aes(group=group,linetype = group),position = position_dodge(.1)) + 
  geom_point(size = 2.5,position = position_dodge(.1))+
  scale_x_continuous(breaks=seq(0,6, by = 1), name = "") + 
  scale_shape_discrete(name = "Sector de empleo",
                       limits = c("0", "1"),
                       labels = c("Público", "Privado")) +
  scale_color_manual(name = "Sector de empleo",
                     limits = c("0", "1"),
                     labels = c("Público", "Privado"),
                     values = c("black", "black")) +
  scale_linetype_manual(name = "Sector de empleo",
                        limits = c("0", "1"),
                        labels = c("Público", "Privado"),
                        values = c("solid", "dashed")) +
  scale_y_continuous(limits = c(0,0.8), breaks=seq(0,0.8, by = 0.1),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  labs(title="", y = "") + 
  theme(plot.title = element_text(size = 11),
        axis.text=element_text(size=11))





