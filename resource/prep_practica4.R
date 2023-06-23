pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#cargamos la base de datos desde internet
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))

proc_data <- latinobarometro2020 %>% select(p22stm_b, # apoyo autoritarismo
                                            P13STGBS_A,  #Fuerzas armadas
                                            P13STGBS_B, # Policias/carabineros
                                            p13st_c, # Iglesia
                                            p13st_d, # Confianza en el congreso
                                            p13st_e, # Confianza en el Gobierno
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g, # Confianza en los partidos políticos
                                            p13st_i, # presidente
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais

proc_data <- proc_data %>% dplyr::filter(idenpa==152) # Seleccionar solo Chile
proc_data <- proc_data %>% set_na(., na = c(-2, -1)) # Eliminar NA -2 y -1

proc_data <- proc_data %>% rename("conf_fa"=P13STGBS_A,
                                  "conf_pol"=P13STGBS_B,
                                  "conf_iglesia"=p13st_c,
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g, # Confianza en los partidos políticos
                                  "conf_presi"=p13st_i, # Confianza presidente
                                  "apoyo_autoritarismo"=p22stm_b)

proc_data <- proc_data %>% mutate_at(vars(starts_with("conf")), ~(5-.))
proc_data <- proc_data %>% mutate_at(vars(starts_with("apoyo")), ~(5-.))

proc_data$conf_fa <- set_label(x = proc_data$conf_fa,label = "Confianza: Fuerzas armadas")
proc_data$conf_pol <- set_label(x = proc_data$conf_pol,label = "Confianza: Policías")
proc_data$conf_iglesia <- set_label(x = proc_data$conf_iglesia,label = "Confianza: Iglesia")
proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
proc_data$conf_presi  <- set_label(x = proc_data$conf_presi, label = "Confianza: Presidente")

proc_data$conf_fa <- set_labels(proc_data$conf_fa,
                                  labels=c( "Ninguna"=1,
                                            "Poca"=2,
                                            "Algo"=3,
                                            "Mucha"=4))

proc_data$conf_pol <- set_labels(proc_data$conf_pol,
                                labels=c( "Ninguna"=1,
                                          "Poca"=2,
                                          "Algo"=3,
                                          "Mucha"=4))

proc_data$conf_iglesia <- set_labels(proc_data$conf_iglesia,
                                labels=c( "Ninguna"=1,
                                          "Poca"=2,
                                          "Algo"=3,
                                          "Mucha"=4))

proc_data$conf_cong <- set_labels(proc_data$conf_cong,
                                labels=c( "Ninguna"=1,
                                          "Poca"=2,
                                          "Algo"=3,
                                          "Mucha"=4))

proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                labels=c( "Ninguna"=1,
                                          "Poca"=2,
                                          "Algo"=3,
                                          "Mucha"=4))

proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                labels=c( "Ninguna"=1,
                                          "Poca"=2,
                                          "Algo"=3,
                                          "Mucha"=4))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                labels=c( "Ninguna"=1,
                                          "Poca"=2,
                                          "Algo"=3,
                                          "Mucha"=4))

proc_data$conf_presi <- set_labels(proc_data$conf_presi,
                                     labels=c( "Ninguna"=1,
                                               "Poca"=2,
                                               "Algo"=3,
                                               "Mucha"=4))

proc_data <- proc_data %>% rowwise() %>% mutate(educacion = case_when(reeduc_1==1~"Educacion basica",
                                                            reeduc_1==2~"Educacion basica",
                                                            reeduc_1==3~"Educacion basica",
                                                            reeduc_1==4~"Educacion media",
                                                            reeduc_1==5~"Educacion media",
                                                            reeduc_1==6~"Educacion superior",
                                                            reeduc_1==7~"Educacion superior"
                                                            ))

proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")

proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

proc_data$sexo <- set_labels(proc_data$sexo,
                             labels=c( "Hombre"=0,
                                       "Mujer"=1))
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")

proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data,file = "files/data/latinobarometro_pract4.RData")
