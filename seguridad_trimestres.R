library(tidyverse)
library(readxl)
library(treemapify)
library(bbplot)
library(sf)

options(scipen = 999)

#Limpia ambiente y establece el directorio de trabajo.
rm(list = ls())
setwd('~/Documents/R/analisis_seguridad')
dir <- getwd()

#Define la variable "Estado" para efectuar el análisis. 
clave_ent <- 14
estado <- 'Jalisco'
anio_lab <- '2021'
anio_filter <- 2021
mes <- '03'
anio_mes <- 'enero-marzo 2021'
fecha <- Sys.Date()

##ESTADOS Y POBLACIÓN
entidades <- read_csv('~/Documents/R/poblacion/2021_poblacion_estatal.csv') %>% 
  transmute(clave_entidad = as.numeric(clave_ent),
            entidad = entidad,
            poblacion = as.numeric(poblacion))

poblacion_nacional <- sum(entidades$poblacion)

#POBLACION POR MUNICIPIO
municipios <- read_csv('~/Documents/R/poblacion/2021_poblacion_municipal.csv')

municipios_estado <- municipios %>% 
  filter(entidad == estado) %>% 
  mutate(clave_municipio = clave)


#SHAPEFILES PARA MAPAS
shapes <- st_read('~/Documents/R/shapefiles/01_32_mun.shp') %>% #shapefiles para mapas
  mutate(clave_municipio = as.numeric(CVEGEO),
         clave_entidad = as.numeric(CVE_ENT))

shapes_estado <- shapes %>% 
  filter(clave_entidad == clave_ent)

####CARPETAS DE INVESTIGACIÓN
#Cargar los datos de la incidencia delictiva. Se excluyen los nombres de las columnas porque generar un error.
id_municipal <- read_csv('~/Documents/R/analisis_seguridad/datasets_seguridad/202103_Marzo/Municipal-Delitos-2015-2021_mar2021.csv',
                         col_names = FALSE)

#Crear vector con los nombres de las columnas.
nombres_columnas <- c('anio', 'clave_entidad', 'entidad', 'clave_municipio',
                      'municipio', 'categoria', 'tipo', 'subtipo', 'modalidad',
                      'enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio',
                      'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')

#Cambiar el nombre de las columnas.
colnames(id_municipal) <- nombres_columnas

#Ajustes necesarios previo al análisis.
id_municipal <- id_municipal %>%
  filter(entidad != 'Entidad') %>%
  transmute(anio = anio,
            clave_entidad = as.numeric(clave_entidad),
            entidad = entidad,
            clave_municipio = as.numeric(clave_municipio),
            municipio = municipio,
            categoria = categoria,
            tipo = tipo,
            subtipo = subtipo,
            modalidad = modalidad,
            enero = as.numeric(enero),
            febrero = as.numeric(febrero),
            marzo = as.numeric(marzo),
            abril = as.numeric(abril),
            mayo = as.numeric(mayo),
            junio = as.numeric(junio),
            julio = as.numeric(julio),
            agosto = as.numeric(agosto),
            septiembre = as.numeric(septiembre),
            octubre = as.numeric(octubre),
            noviembre = as.numeric(noviembre),
            diciembre = as.numeric(diciembre),
            q1 = enero+febrero+marzo,
            q2 = abril+mayo+junio,
            q3 = julio+agosto+septiembre,
            q4 = octubre+noviembre+diciembre)


###VÍCTIMAS
victimas <- read_csv('~/Documents/R/analisis_seguridad/datasets_seguridad/202103_Marzo/IDVFC_NM_mar2021.csv',
                     col_names = FALSE)

nombres_columnas <- c('anio', 'clave_entidad', 'entidad', 
                      'categoria', 'tipo', 'subtipo', 'modalidad','sexo', 'rango_edad',
                      'enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio',
                      'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')

#Cambiar el nombre de las columnas.
colnames(victimas) <- nombres_columnas

#Ajustes necesarios previo al análisis.
victimas <- victimas %>%
  filter(entidad != 'Entidad') %>%
  transmute(anio = anio,
            clave_entidad = as.numeric(clave_entidad),
            entidad = entidad,
            categoria = categoria,
            tipo = tipo,
            subtipo = subtipo,
            modalidad = modalidad,
            sexo = sexo,
            rango_edad = rango_edad,
            enero = as.numeric(enero),
            febrero = as.numeric(febrero),
            marzo = as.numeric(marzo),
            abril = as.numeric(abril),
            mayo = as.numeric(mayo),
            junio = as.numeric(junio),
            julio = as.numeric(julio),
            agosto = as.numeric(agosto),
            septiembre = as.numeric(septiembre),
            octubre = as.numeric(octubre),
            noviembre = as.numeric(noviembre),
            diciembre = as.numeric(diciembre),
            q1 = enero+febrero+marzo,
            q2 = abril+mayo+junio,
            q3 = julio+agosto+septiembre,
            q4 = octubre+noviembre+diciembre)

#VALORES QUE SE AJUSTAN MES CON MES PREVIO A REALIZAR EL ANÁLISIS: "mes" y "anio"
#Crea el dataset del mes a evaluar para comparativas nacionales
incidencia_mes_estatal <- id_municipal %>% 
  filter(clave_entidad == clave_ent) %>%
  mutate(mes = q1) %>% 
  select(c(1:9), -c(10:21), mes)

#Crea el dataset del mes y estado a evaluar
incidencia_mes <- id_municipal %>% 
  filter(anio == anio_filter) %>%
  mutate(mes = q1) %>% 
  select(-1, c(2:9), -c(10:21), mes)

#Crea directorio por estado para guardar las gráficas
dir.create(paste0(dir,'/',anio_lab, '_', mes, '_',estado))
setwd(paste0(dir,'/',anio_lab, '_', mes, '_',estado))

#INCIDENCIA GENERAL
#Crea el data set del mes en curso y colpasa por estados.
incidencia_total_mes_estatal <- incidencia_mes %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_delitos_nacional <- sum(incidencia_total_mes_estatal$delitos)
media_nacional_incidencia_general <- round(mean(incidencia_total_mes_estatal$delitos),2)
tasa_nacional_incidencia_general <-  round((total_delitos_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
incidencia_total_mes_estatal <-  left_join(incidencia_total_mes_estatal, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
incidencia_total_mes_estatal <- incidencia_total_mes_estatal %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_delitos_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_total <- c(0,1,5,10,15,20,
                max((incidencia_total_mes_estatal$porcentaje+1), na.rm = TRUE))

g_arbol_total <- incidencia_total_mes_estatal %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_total, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_general_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_total)
dev.off()

#INCIDENCIA GENERAL
#Crea el data set del mes en curso y colpasa por estados.
incidencia_total_mes_estatal <- incidencia_mes %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_delitos_nacional <- sum(incidencia_total_mes_estatal$delitos)
media_nacional_incidencia_general <- round(mean(incidencia_total_mes_estatal$delitos),2)
tasa_nacional_incidencia_general <-  round((total_delitos_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
incidencia_total_mes_estatal <-  left_join(incidencia_total_mes_estatal, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
incidencia_total_mes_estatal <- incidencia_total_mes_estatal %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_delitos_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_total <- c(0,1,5,10,15,20,
                max((incidencia_total_mes_estatal$porcentaje+1), na.rm = TRUE))

g_arbol_total <- incidencia_total_mes_estatal %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_total, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_general_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_total)
dev.off()

#Árbol de la distribución porcentual de la incidencia estatal por bien jurídco afectado
bien_afectado <- incidencia_mes %>% 
  filter(entidad == estado) %>% 
  group_by(categoria) %>% 
  summarise(delitos = sum(mes)) %>% 
  mutate(porcentaje = round(delitos/sum(bien_afectado$delitos)*100,2))

brks_bien_afectado <- c(0,1,10,15,20,
                        max((bien_afectado$porcentaje+1), na.rm = TRUE))

g_arbol_bien_afectado <- bien_afectado %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_bien_afectado, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(categoria,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva por bien jurídico afectado como porcentaje del total general",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_bien_afectado_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_bien_afectado)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_general <- incidencia_total_mes_estatal %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Incidencia delictivia general por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=incidencia_total_mes_estatal,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_incidencia_general, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_incidencia_general, label = paste('Media nacional:',tasa_nacional_incidencia_general), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_general_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_general)
dev.off()


#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(incidencia_total_mes_estatal$delitos), 2)

g_total <- incidencia_total_mes_estatal %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Incidencia delictivia general por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=incidencia_total_mes_estatal,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_general_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()


#VEHÍCULOS
#Crea el data set del mes en curso y colpasa por estados.
vehiculos <- incidencia_mes %>%
  filter(entidad != 'Entidad',
         modalidad == 'Robo de coche de 4 ruedas Con violencia' |
           modalidad == 'Robo de coche de 4 ruedas Sin violencia') %>%
  select(clave_entidad, mes) %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_vehiculos_nacional <- sum(vehiculos$delitos)
media_nacional_vehiculos <- round(mean(vehiculos$delitos),2)
tasa_nacional_vehiculos <-  round((total_vehiculos_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
vehiculos <-  left_join(vehiculos, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
vehiculos <- vehiculos %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_vehiculos_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_vehiculos <- c(0,1,5,10,15,20,
                    max((vehiculos$porcentaje+1), na.rm = TRUE))

g_arbol_vehiculos <- vehiculos %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_vehiculos, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución del robo de vehículos por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_vehiculos_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_vehiculos)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_vehiculos <- vehiculos %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robo de vehículos por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=vehiculos,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_vehiculos, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_vehiculos,
                label = paste('Media nacional:',tasa_nacional_vehiculos), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_vehiculos_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_vehiculos)
dev.off()

#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(vehiculos$delitos), 2)

g_total <- vehiculos %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robos de vehículos por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=vehiculos,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_vehiculos_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()


#CASA HABITACIÓN
#Crea el data set del mes en curso y colpasa por estados.
casahabitacion <- incidencia_mes %>%
  filter(entidad != 'Entidad',
         grepl('casa', subtipo)) %>%
  select(clave_entidad, mes) %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_casahabitacion_nacional <- sum(casahabitacion$delitos)
media_nacional_casahabitacion <- round(mean(casahabitacion$delitos),2)
tasa_nacional_casahabitacion <-  round((total_casahabitacion_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
casahabitacion <-  left_join(casahabitacion, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
casahabitacion <- casahabitacion %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_casahabitacion_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_casahabitacion <- c(0,1,5,10,15,20,
                         max((casahabitacion$porcentaje+1), na.rm = TRUE))

g_arbol_casahabitacion <- casahabitacion %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_casahabitacion, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución del robo a casa habitación por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_casahabitacion_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_casahabitacion)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_casahabitacion <- casahabitacion %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robo a casa habitación por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=casahabitacion,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_casahabitacion, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_casahabitacion,
                label = paste('Media nacional:',tasa_nacional_casahabitacion), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_casahabitacion_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_casahabitacion)
dev.off()

#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(casahabitacion$delitos), 2)

g_total <- casahabitacion %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robos a casa habitación por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=casahabitacion,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_casahabitacion_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()


#NEGOCIO
#Crea el data set del mes en curso y colpasa por estados.
negocio <- incidencia_mes %>%
  filter(subtipo == 'Robo a negocio') %>%
  select(clave_entidad, mes) %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_negocio_nacional <- sum(negocio$delitos)
media_nacional_negocio <- round(mean(negocio$delitos),2)
tasa_nacional_negocio <-  round((total_negocio_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
negocio <-  left_join(negocio, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
negocio <- negocio %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_negocio_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_negocio <- c(0,1,5,10,15,20,
                  max((negocio$porcentaje+1), na.rm = TRUE))

g_arbol_negocio <- negocio %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_negocio, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución del robo a negocio por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_negocio_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_negocio)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_negocio <- negocio %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robo a negocio por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=negocio,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_negocio, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_negocio,
                label = paste('Media nacional:',tasa_nacional_negocio), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_negocio_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_negocio)
dev.off()

#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(negocio$delitos), 2)

g_total <- negocio %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robos a negocio por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=negocio,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_negocio_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()


#PERSONA
#Crea el data set del mes en curso y colpasa por estados.
persona <- incidencia_mes %>%
  filter(grepl('Robo a transe', subtipo)) %>% 
  select(clave_entidad, mes) %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_persona_nacional <- sum(persona$delitos)
media_nacional_persona <- round(mean(persona$delitos),2)
tasa_nacional_persona <-  round((total_persona_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
persona <-  left_join(persona, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
persona <- persona %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_persona_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_persona <- c(0,1,5,10,15,20,
                  max((persona$porcentaje+1), na.rm = TRUE))

g_arbol_persona <- persona %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_persona, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución del robo a persona por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_persona_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_persona)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_persona <- persona %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robo a persona por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=persona,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_persona, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_persona,
                label = paste('Media nacional:',tasa_nacional_persona), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_persona_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_persona)
dev.off()


#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(persona$delitos), 2)

g_total <- persona %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robos a persona por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=persona,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_persona_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()

#MOTOCICLETA
#Crea el data set del mes en curso y colpasa por estados.
motocicleta <- incidencia_mes %>%
  filter(modalidad == 'Robo de motocicleta Con violencia' |
           modalidad == 'Robo de motocicleta Sin violencia'	) %>%
  select(clave_entidad, mes) %>%
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_motocicleta_nacional <- sum(motocicleta$delitos)
media_nacional_motocicleta <- round(mean(motocicleta$delitos),2)
tasa_nacional_motocicleta <-  round((total_motocicleta_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
motocicleta <-  left_join(motocicleta, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
motocicleta <- motocicleta %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_motocicleta_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_motocicleta <- c(0,1,5,10,15,20,
                      max((motocicleta$porcentaje+1), na.rm = TRUE))

g_arbol_motocicleta <- motocicleta %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_motocicleta, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución del robo de motocicleta por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_motocicleta_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_motocicleta)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_motocicleta <- motocicleta %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robo de motocicleta por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=motocicleta,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_motocicleta, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_motocicleta,
                label = paste('Media nacional:',tasa_nacional_motocicleta), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_motocicleta_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_motocicleta)
dev.off()

#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(motocicleta$delitos), 2)

g_total <- motocicleta %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robos de motocicleta por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=motocicleta,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_motocicleta_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()

#BANCO
#Crea el data set del mes en curso y colpasa por estados.
banco <- incidencia_mes %>%
  filter(grepl('bancaria', subtipo)) %>% 
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_banco_nacional <- sum(banco$delitos)
media_nacional_banco <- round(mean(banco$delitos),2)
tasa_nacional_banco <-  round((total_banco_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
banco <-  left_join(banco, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
banco <- banco %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_banco_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_banco <- c(0,1,5,10,15,20,
                max((banco$porcentaje+1), na.rm = TRUE))

g_arbol_banco <- banco %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_banco, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución del robo a banco por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_banco_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_banco)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_banco <- banco %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robo a banco por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=banco,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_banco, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_banco,
                label = paste('Media nacional:',tasa_nacional_banco), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_banco_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_banco)
dev.off()


#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(banco$delitos), 2)

g_total <- banco %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Robos a banco por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=banco,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_banco_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()

#HOMICIDIO DOLOSO
#Crea el data set del mes en curso y colpasa por estados.
homicidio <- incidencia_mes %>%
  filter(subtipo == 'Homicidio doloso') %>% 
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_homicidio_nacional <- sum(homicidio$delitos)
media_nacional_homicidio <- round(mean(homicidio$delitos),2)
tasa_nacional_homicidio <-  round((total_homicidio_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
homicidio <-  left_join(homicidio, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
homicidio <- homicidio %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_homicidio_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_homicidio <- c(0,1,5,10,15,20,
                    max((homicidio$porcentaje+1), na.rm = TRUE))

g_arbol_homicidio <- homicidio %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_homicidio, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de los eventos de homicidio doloso por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_homicidio_eventos_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_homicidio)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_homicidio <- homicidio %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Eventos de homicidio doloso por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=homicidio,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_homicidio, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_homicidio,
                label = paste('Media nacional:',tasa_nacional_homicidio), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_homicidio_eventos_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_homicidio)
dev.off()


#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(homicidio$delitos), 2)

g_total <- homicidio %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Eventos de homicidio doloso por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=homicidio,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_homicidio_eventos_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()

#FEMINICIDIO
#Crea el data set del mes en curso y colpasa por estados.
feminicidio <- incidencia_mes %>%
  filter(subtipo == 'Feminicidio') %>% 
  group_by(clave_entidad) %>%
  summarise(delitos = sum(mes))

#Crea variables para el cálculo de la media nacional y de los porcentajes por estado.
poblacion_nacional <- sum(entidades$poblacion)
total_feminicidio_nacional <- sum(feminicidio$delitos)
media_nacional_feminicidio <- round(mean(feminicidio$delitos),2)
tasa_nacional_feminicidio <-  round((total_feminicidio_nacional/poblacion_nacional)*100000,2)

#Une el dataset con el de población.
feminicidio <-  left_join(feminicidio, entidades)

#Agrega el porcentaje de delitos cometidos por entidad y el porcentaje respecto al nacional.
feminicidio <- feminicidio %>% 
  mutate(tasa = round((delitos/poblacion)*100000,2),
         porcentaje =round((delitos/total_feminicidio_nacional)*100,2),
         is_entidad  = ifelse(entidad == estado, "1", "0"))

#Árbol de la distribución porcentual de la incidencia general por estado
brks_feminicidio <- c(0,1,5,10,15,20,
                      max((feminicidio$porcentaje+1), na.rm = TRUE))

g_arbol_feminicidio <- feminicidio %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_feminicidio, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de los eventos de feminicidio por estado como porcentaje del total nacional",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_feminicidio_eventos_arbol','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_feminicidio)
dev.off()

#Gráfica de la tasa de incidencia delictiva general.
g_tasa_feminicidio <- feminicidio %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Eventos de feminicidio por cada 100 mil habitantes",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=feminicidio,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_feminicidio, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_feminicidio,
                label = paste('Media nacional:',tasa_nacional_feminicidio), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_feminicidio_eventos_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_feminicidio)
dev.off()


#Gráfica de la incidencia delictiva general.
promedio_nacional <- round(mean(feminicidio$delitos), 2)

g_total <- feminicidio %>%
  ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('0' = '#00a5db', '1' = '#162342'), guide = FALSE)+
  coord_flip()+
  labs(title="Eventos de feminicidio por entidad",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=feminicidio,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional, label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_feminicidio_eventos_total','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total)
dev.off()


##VARIACIÓN RESPECTO AL MISMO MES DEL AÑO PASADO
#Incidencia total por mes evaluado
total_mes <- incidencia_mes_estatal %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_total_mes <- total_mes %>% 
  ggplot(aes(x = anio, y = delitos)) +
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Incidecia delictiva general",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por delitos del fuero común", size = 4)+
  geom_text(data=total_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_general_incidencia_total_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_total_mes)
dev.off()


#Incidencia total vehículo
vehiculo_mes <- incidencia_mes_estatal %>% 
  filter(modalidad == 'Robo de coche de 4 ruedas Con violencia' |
           modalidad == 'Robo de coche de 4 ruedas Sin violencia') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_vehiculo_mes <- vehiculo_mes %>% 
  ggplot(aes(x = anio, y = delitos)) +
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Robo de vehículo particular",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por robo de vehículo particular", size = 4)+
  geom_text(data=vehiculo_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_vehiculo_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_vehiculo_mes)
dev.off()

#Casa habitación
casahabitcion_mes <- incidencia_mes_estatal %>% 
  filter(grepl('casa', subtipo)) %>%
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_casahabitacion_mes <- casahabitcion_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Robo a casa habitación",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por robo a casa habitación", size = 4)+
  geom_text(data = casahabitcion_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_casahabitacion_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_casahabitacion_mes)
dev.off()

#Robo a negocio
negocio_mes <- incidencia_mes_estatal %>% 
  filter(subtipo == 'Robo a negocio') %>%
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_negocio_mes <- negocio_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Robo a negocio",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por robo a negocio", size = 4)+
  geom_text(data = negocio_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_negocio_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_negocio_mes)
dev.off()

#Robo a persona
persona_mes <- incidencia_mes_estatal %>% 
  filter(grepl('Robo a transe', subtipo)) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_persona_mes <- persona_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Robo a persona",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por robo a persona", size = 4)+
  geom_text(data = persona_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_persona_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_persona_mes)
dev.off()

#Robo de motocicleta
motocicleta_mes <- incidencia_mes_estatal %>% 
  filter(modalidad == 'Robo de motocicleta Con violencia' |
           modalidad == 'Robo de motocicleta Sin violencia') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_motocicleta_mes <- motocicleta_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Robo de motocicleta",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por robo de motocicleta", size = 4)+
  geom_text(data = motocicleta_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_motocicleta_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_motocicleta_mes)
dev.off()

#Robo a banco
banco_mes <- incidencia_mes_estatal %>% 
  filter(grepl('bancaria', subtipo)) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_banco_mes <- banco_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Robo a banco",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por robo a banco", size = 4)+
  geom_text(data = banco_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_banco_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_banco_mes)
dev.off()

#Homicidio doloso (eventos)
homicidio_mes <- incidencia_mes_estatal %>% 
  filter(subtipo == 'Homicidio doloso') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_homicidio_mes <- homicidio_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Homicidio doloso (eventos)",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por homicidio doloso", size = 4)+
  geom_text(data = homicidio_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_homicidio_eventos_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_homicidio_mes)
dev.off()

#Feminicidio (eventos)
feminicidio_mes <- incidencia_mes_estatal %>% 
  filter(subtipo == 'Feminicidio') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(mes))

g_feminicidio_mes <- feminicidio_mes %>% 
  ggplot(aes(x = anio, y = delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Feminicidio (eventos)",
       subtitle = paste(estado, anio_mes, sep = ', '),
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Carpetas de investigación iniciadas por feminicidio", size = 4)+
  geom_text(data = feminicidio_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(estado,'_feminicidio_eventos_incidencia_mes','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_feminicidio_mes)
dev.off()