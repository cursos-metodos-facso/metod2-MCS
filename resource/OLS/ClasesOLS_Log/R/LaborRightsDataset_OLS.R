#Remove
rm(list = ls())

# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse,sjPlot,dplyr,texreg,
               ggeffects,psych)


#Read Dataset 2017------
LaborRights_Data2017 <- readRDS(file = "Data/LaborRights_Data2017.rds")



#Estadisticos descriptivos -------
describe(LaborRights_Data2017) #


#Regresiones OLS----


#Model 1-3 (VD: derechos laborales; VI: desgualdad de poder)------
#M1: 1 variable indep
m1 <- lm(LR_Overall_Rev ~ v2pepwrses_osp_Rev,
            data = LaborRights_Data2017)
summary(m1)
screenreg(m1,
          custom.model.names = "m1",
          digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          symbol = "†")

#M2: m1 + controles económicos (PIB per capita, Inv extranjera)
m2 <- lm(LR_Overall_Rev ~ v2pepwrses_osp_Rev+
           GDPpp_log+FDI_inflow,
         data = LaborRights_Data2017)

#M3: m2 + controles políticos (democracia liberal + gob izquierda)
m3 <- lm(LR_Overall_Rev ~ v2pepwrses_osp_Rev+
           GDPpp_log+FDI_inflow+v2x_libdem_InPerc+LeftGvt,
         data = LaborRights_Data2017)

#M3.0: pre-interacción (poder + left gvt)
m1.0 <- lm(LR_Overall_Rev ~ v2pepwrses_osp_Rev+
             LeftGvt,
           data = LaborRights_Data2017)
#M3.1: interaccion (simple)
m1.1 <- lm(LR_Overall_Rev ~ v2pepwrses_osp_Rev+
           LeftGvt+LeftGvt*v2pepwrses_osp_Rev,
         data = LaborRights_Data2017)



#M3.1: interaccion  con controles
m3.1 <- lm(LR_Overall_Rev ~ v2pepwrses_osp_Rev+
             GDPpp_log+FDI_inflow+v2x_libdem_InPerc+LeftGvt+LeftGvt*v2pepwrses_osp_Rev,
           data = LaborRights_Data2017)

#Modelos sin interacciones 
screenreg(list(m1,m2,m3),
          custom.model.names = c("m1","m2: econ","m3: pol"),
                                 digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          symbol = "†")

#Interacciones
screenreg(list(m1,m1.0,m1.1),
          custom.model.names = c("m1","m1.0","m1.1: inter (simple)"),
          digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          symbol = "†")

screenreg(list(m1,m1.1,m3,m3.1),
          custom.model.names = c("m1","m1.1: inter (simple)",
                                 "m3: pol","m3.1: inter (controles)"),
          digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          symbol = "†")


#Figura: impacto desigualdad de poder (m1): solo linea recta: modelo simple
DesPoder_m1<-ggeffects::ggpredict(m1, terms = c("v2pepwrses_osp_Rev")) %>% 
  ggplot(aes(x=x, y=predicted))+
  geom_line(stat="identity")+
  labs(title="Modelo 1: relación bivariada", x = "Desigualdad de poder entre clases", y = "Derechos sindicales")+
  theme_bw()+
  theme(plot.title = element_text(size = 12), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2,2.5,3,3.5), limits = c(0,3.5)) +
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10))

#Figura: impacto desigualdad de poder (m3) + intervalo de confianza
DesPoder_m3.1<-ggeffects::ggpredict(m3.1, terms = c("v2pepwrses_osp_Rev")) %>% 
  ggplot(aes(x=x, y=predicted))+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=0.2)+
  labs(title="Modelo 3: controles", x = "Desigualdad de poder entre clases", y = "Derechos sindicales")+
  theme_bw()+
  theme(plot.title = element_text(size = 12), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  scale_x_continuous(breaks=c(0,0.5,1,1.5,2,2.5,3,3.5), limits = c(0,3.5)) +
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits = c(1,10))


#Figura interaccion gob de izquierda * desigualdad de poder: m1.1 (simple)
Poder_GobIzq_Int<-ggeffects::ggpredict(m1.1, terms = c("v2pepwrses_osp_Rev[0,3.5]","LeftGvt")) %>%
  ggplot(aes(x, predicted, shape = group, color = group)) +
  geom_line(aes(group=group,linetype = group),position = position_dodge(.1)) + 
  geom_point(size = 2.5,position = position_dodge(.1))+
  scale_x_continuous(name = "Desigualdad de poder", 
                     breaks=seq(0,3.5, by = 0.25)) + 
  scale_shape_discrete(name = "Gobierno de izquierda",
                       limits = c("0", "1"),
                       labels = c("No", "Sí")) +
  scale_color_manual(name = "Gobierno de izquierda",
                     limits = c("0", "1"),
                     labels = c("No", "Sí"),
                     values = c("black", "black")) +
  scale_linetype_manual(name = "Gobierno de izquierda",
                        limits = c("0", "1"),
                        labels = c("No", "Sí"),
                        values = c("solid", "dashed")) +
  scale_y_continuous(limits = c(1,10),breaks=seq(0,10, by = 1)) +
  theme_bw() +
  labs(title="Interacción gob. izquierda - desigualdad de poder", 
       y = "Derechos de sindicalización") + 
  theme(plot.title = element_text(size = 11),
        axis.text=element_text(size=11))






#Modelos 4-6 (VD: Top 1% ingreso, VI: densidad sindical)-------
#M4: 2 variables
m4 <- lm(OnePercentShare_InPerc ~ Union_Density,
         data = LaborRights_Data2017)

#M5: m4 + controles económicos (PIB per capita, Inv extranjera)
m5 <- lm(OnePercentShare_InPerc ~ Union_Density+
           GDPpp_log+FDI_inflow,
         data = LaborRights_Data2017)

#M6: m5 + controles políticos (democracia liberal + gob izquierda)
m6 <- lm(OnePercentShare_InPerc ~ Union_Density+
           GDPpp_log+FDI_inflow+v2x_libdem_InPerc+LeftGvt,
         data = LaborRights_Data2017)

#m5 + controles políticos (democracia liberal + gob izquierda) + interacción
m6.1 <- lm(OnePercentShare_InPerc ~ Union_Density+
           GDPpp_log+FDI_inflow+v2x_libdem_InPerc+LeftGvt+LeftGvt*Union_Density,
         data = LaborRights_Data2017)

screenreg(list(m4,m5,m6,m6.1),
          custom.model.names = c("m4","m4+econ","m6+pol","m6+inter"),
          digits = 3,
          stars = c(0.001, 0.01, 0.05, 0.1),
          symbol = "†")

#Figura: impacto densidad sindical (m6)
DensidadSind_m6<-ggeffects::ggpredict(m6, terms = c("Union_Density")) %>% 
  ggplot(aes(x=x, y=predicted))+
  geom_line(stat="identity")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=0.2)+
  labs(title="Efecto de poder sindical (efecto marginal)", x = "Densidad sindical", y = "1% riqueza")+
  theme_bw()+
  theme(plot.title = element_text(size = 12), 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  scale_x_continuous(breaks = seq(0,100, by = 10))+
  scale_y_continuous(limits = c(0,20), breaks=seq(0,20, by = 2))





