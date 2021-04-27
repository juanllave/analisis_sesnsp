library(tidyverse)
library(readxl)
library(treemapify)
library(bbplot)
library(sf)

options(scipen = 999)

#Limpia ambiente y establece el directorio de trabajo.
rm(list = ls())
setwd('~/Desktop')
dir <- getwd()

#Define la variable "Estado" para efectuar el análisis. 
clave_ent <- 14
estado <- 'Jalisco'
anio <- '2020'

##ESTADOS Y POBLACIÓN
entidades <- read_csv('~/Documents/R/poblacion/2020_entidades_poblacion.csv') %>% 
  transmute(clave_entidad = as.numeric(clave),
            entidad = entidad,
            poblacion = as.numeric(poblacion))

poblacion_nacional <- sum(entidades$poblacion)

#POBLACION POR MUNICIPIO
municipios <- read_csv('~/Documents/R/poblacion/2020_poblacion_municipal.csv')

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
id_municipal <- read_csv('~/Documents/R/analisis_seguridad/datasets_seguridad/IDM_NM_dic2020.csv',
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
            diciembre = as.numeric(diciembre))


###VÍCTIMAS
victimas <- read_xlsx('~/Documents/R/analisis_seguridad/datasets_seguridad/Estatal-V¡ctimas-2015-2020_dic2020.xlsx')

nombres_columnas <- c('anio', 'clave_entidad', 'entidad', 
                      'categoria', 'tipo', 'subtipo', 'modalidad','sexo', 'rango_edad',
                      'enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio',
                      'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')

#Cambiar el nombre de las columnas.
colnames(victimas) <- nombres_columnas

#Ajustes necesarios previo al análisis.
victimas <- victimas %>%
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
            diciembre = as.numeric(diciembre))

#Crea directorio por estado para guardar las gráficas
dir.create(paste(dir,estado, sep = '/'))
setwd(paste(dir,estado, sep = '/'))

###INCIDENCIA DE CARPETAS DE INVESTIGACIÓN
#Crea el dataset de la incidencia de la entidad a analizar
incidencia_estatal <- id_municipal %>%
  filter(clave_entidad == clave_ent) %>%
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  select(-(c(2:5)), -(c(10:21))) 

##RÁNKING INCIDENCIA GENERAL
estatales_general <- id_municipal %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  group_by(clave_entidad) %>% 
  summarise(delitos = sum(total))

estatales_general <- left_join(entidades, estatales_general)

estatales_general <- estatales_general %>% 
  mutate(is_entidad = ifelse(entidad == estado, TRUE, FALSE))

promedio_nacional <- round(mean(estatales_general$delitos),2)

g_ranking_nacional <- estatales_general %>% 
ggplot(mapping = aes(x=reorder(entidad, delitos), delitos, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('TRUE' = 'steelblue1', 'FALSE' = 'steelblue4'), guide = FALSE)+
  coord_flip()+
  labs(title="Carpetas de investigación iniciadas por delitos del fuero común",
       subtitle = anio,
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Carpetas de investigación", size = 4)+
  geom_text(data=estatales_general,aes(label=delitos, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = promedio_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, promedio_nacional,
                label = paste('Media nacional:',promedio_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_ranking_nacional','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_ranking_nacional)
dev.off()


##INCIDENCIA GENERAL
incidencia_estatal_anual <- incidencia_estatal %>%
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_incidencia_estatal_anual <- incidencia_estatal_anual %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Incidencia delictiva general por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_incidencia_general_anual','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_incidencia_estatal_anual)
dev.off()

#TASA INCIDENCIA GENERAL
tasa_incidencia_general <- id_municipal %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  group_by(clave_entidad) %>% 
  summarise(delitos = sum(total))

tasa_incidencia_general <- left_join(entidades, tasa_incidencia_general)
tasa_incidencia_general <- tasa_incidencia_general %>% 
  mutate(tasa = round(((delitos/poblacion)*100000), digits = 2),
         is_entidad = ifelse(entidad == estado, TRUE, FALSE))

tasa_nacional <- round(mean(tasa_incidencia_general$tasa),2)
       
g_tasa_incidencia_general <- tasa_incidencia_general %>% 
  ggplot(mapping = aes(x=reorder(entidad, tasa), tasa, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('TRUE' = 'steelblue1', 'FALSE' = 'steelblue4'), guide = FALSE)+
  coord_flip()+
  labs(title="Carpetas de investigación por cada 100 mil habitantes",
       subtitle = anio,
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=tasa_incidencia_general,aes(label=tasa, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional,
                label = paste('Media nacional:',tasa_nacional), vjust = -1, hjust = -0.1))

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_tasa_general','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_incidencia_general)
dev.off()

##VEHÍCULOS
vehiculos <- incidencia_estatal %>%
  filter(modalidad == 'Robo de coche de 4 ruedas Con violencia' |
           modalidad == 'Robo de coche de 4 ruedas Sin violencia'	) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_vehiculos <- vehiculos %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Robos de vehículos particulares por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_vehiculos_anual','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_vehiculos)
dev.off()

##CASA HABITACIÓN
casahabitacion <- incidencia_estatal %>%
  filter(grepl('casa', subtipo)) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_casahabitacion <- casahabitacion %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Robos a casa habitación por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_casa_habitacion','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_casahabitacion)
dev.off()

##NEGOCIO
negocio <- incidencia_estatal %>%
  filter(subtipo == 'Robo a negocio') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_negocio <- negocio %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Robos a negocio por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_negocio','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_negocio)
dev.off()

##PERSONA
persona <- incidencia_estatal %>%
  filter(grepl('Robo a transe', subtipo)) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_persona <- persona %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Robos a persona por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_persona','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_persona)
dev.off()

##MOTOCICLETA
motocicleta <- incidencia_estatal %>%
  filter(modalidad == 'Robo de motocicleta Con violencia' |
           modalidad == 'Robo de motocicleta Sin violencia'	) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_motocicleta <- motocicleta %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Robos de motocicleta por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_motocicleta','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_motocicleta)
dev.off()

##BANCO
banco <- incidencia_estatal %>%
  filter(grepl('bancaria', subtipo)) %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_banco <- banco %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Robos a banco por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_banco','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_banco)
dev.off()

##HOMICIDIO DOLOSO
homicidio <- incidencia_estatal %>%
  filter(subtipo == 'Homicidio doloso') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_homicidio <- homicidio %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Eventos de homicidio doloso por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_homicidio_eventos','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_homicidio)
dev.off()


##FEMINICIDIO
feminicidio <- incidencia_estatal %>%
  filter(subtipo == 'Feminicidio') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_feminicidio <- feminicidio %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Eventos de feminicidio por año',
       subtitle = estado,
       x = 'Año', y = 'Carpetas de investigación',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_feminicidio_eventos','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_feminicidio)
dev.off()

#VÍCTIMAS
#Crea el dataset para análisis anual
victimas_h <- victimas %>%
  filter(clave_entidad == clave_ent) %>%
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  select(-(c(2:3)), -(c(10:21))) 


##HOMICIDIO DOLOSO
homicidio <- victimas_h %>%
  filter(subtipo == 'Homicidio doloso') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_homicidio <- homicidio %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Víctimas de homicidio doloso por año',
       subtitle = estado,
       x = 'Año', y = 'Víctimas',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_homicidio_victimas','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_homicidio)
dev.off()

##FEMINICIDIO
feminicidio <- victimas_h %>%
  filter(subtipo == 'Feminicidio') %>% 
  group_by(anio) %>% 
  summarise(delitos = sum(total))

g_feminicidio <- feminicidio %>%
  ggplot(aes(x=anio, y=delitos))+
  geom_col(colour = '#162342', fill = '#162342')+
  geom_text(aes(label=delitos, vjust=-1))+
  labs(title = 'Víctimas de feminicidio por año',
       subtitle = estado,
       x = 'Año', y = 'Víctimas',
       caption = 'Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_',estado,'_feminicidio_victimas','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_feminicidio)
dev.off()

###INCIDENCIA GENERAL MES CON MES
mensual <- id_municipal %>%
  filter(clave_entidad == clave_ent) %>% 
  group_by(anio) %>% 
  summarise(enero = sum(enero),
            febrero = sum(febrero),
            marzo = sum(marzo),
            abril = sum(abril),
            mayo = sum(mayo),
            junio = sum(junio),
            julio = sum(julio),
            agosto = sum(agosto),
            septiembre = sum(septiembre),
            octubre = sum(octubre),
            noviembre = sum(noviembre),
            diciembre = sum(diciembre))

mensual <- t(mensual)
mensual <- as_tibble(mensual)

mensual <- mensual %>% 
  transmute(dos15=as.numeric(V1),
            dos16=as.numeric(V2),
            dos17=as.numeric(V3),
            dos18=as.numeric(V4),
            dos19=as.numeric(V5),
            dos20=as.numeric(V6))

mensual <- cbind(Row.Names = rownames(mensual), mensual)
mensual <- mensual %>%
  filter(Row.Names  != 1)

meses <- c('01_Enero', '02_Febrero', '03_Marzo', '04_Abril', '05_Mayo', '06_Junio',
           '07_Julio', '08_Agosto', '09_Septiembre', '10_Octubre', '11_Noviembre', '12_Diciembre')

mensual <- mensual %>% 
  mutate(mes = meses) %>% 
  select(-1)

mean_2015 <- round(mean(mensual$dos15),2)
mean_2016 <- round(mean(mensual$dos16),2)
mean_2017 <- round(mean(mensual$dos17),2)
mean_2018 <- round(mean(mensual$dos18),2)
mean_2019 <- round(mean(mensual$dos19),2)
mean_2020 <- round(mean(mensual$dos20),2)

#Graficar
g_mensual <- ggplot(data = mensual) +
  geom_line(aes(x = mes, y = dos15, group =1), colour = "#003383", size = 0.5)+
  geom_line(aes(x = mes, y = dos16, group =1), colour = "#0075a9", size = 0.5)+
  geom_line(aes(x = mes, y = dos17, group =1), colour = '#8499a5', size = 0.5)+
  geom_line(aes(x = mes, y = dos18, group =1), colour = "#00a5db", size = 0.5)+
  geom_line(aes(x = mes, y = dos19, group =1), color = '#003893', size = 0.5)+
  geom_line(aes(x = mes, y = dos20, group =1), color = '#162342', size = 0.5)+
  geom_hline(yintercept = mean_2015, color = "#003383", linetype="dashed")+
  geom_text(aes(0, mean_2015, label = paste('Media 2015:',mean_2015), vjust = 1.2, hjust = -0.2),
            color = "#003383")+
  geom_hline(yintercept = mean_2016, color = "#0075a9", linetype="dashed")+
  geom_text(aes(0, mean_2016, label = paste('Media 2016:',mean_2016), vjust = 1.2, hjust = -0.2),
            color = "#0075a9")+
  geom_hline(yintercept = mean_2017, color = "#8499a5", linetype="dashed")+
  geom_text(aes(0, mean_2017, label = paste('Media 2017:',mean_2017), vjust = 1.2, hjust = -0.2),
            color = "#8499a5")+
  geom_hline(yintercept = mean_2018, color = "#00a5db", linetype="dashed")+
  geom_text(aes(0, mean_2018, label = paste('Media 2018:',mean_2018), vjust = 1.2, hjust = -0.2),
            color = "#00a5db")+
  geom_hline(yintercept = mean_2019, color = "#003893", linetype="dashed")+
  geom_text(aes(0, mean_2019, label = paste('Media 2019:',mean_2019), vjust = 1.2, hjust = -0.2),
            color = "#003893")+
  geom_hline(yintercept = mean_2020, color = "#162342", linetype="dashed")+
  geom_text(aes(0, mean_2020, label = paste('Media 2020:',mean_2020), vjust = 1.2, hjust = -0.2),
            color = "#162342")+
  labs(title = "Comportamiento de la incidencia delictiva general por año y por mes",
       subtitle = estado,
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x = "Carpetas de Investigación por mes", y = "Año")+
  scale_colour_manual("", breaks = c("2015", "2016", "2017", "2018", "2019", "2020"),
                      values = c("#003383", "#0075a9", '#8499a5', '#00a5db', '#003893', '#162342'))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+
  
png(filename =  paste0(estado,'_mensual','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_mensual)
dev.off()


##INCIDENCIA ACUMULADA POR MES
acumulada <- id_municipal %>%
  filter(clave_entidad == clave_ent) %>% 
  group_by(anio) %>% 
  summarise(enero = sum(enero),
            febrero = sum(febrero),
            marzo = sum(marzo),
            abril = sum(abril),
            mayo = sum(mayo),
            junio = sum(junio),
            julio = sum(julio),
            agosto = sum(agosto),
            septiembre = sum(septiembre),
            octubre = sum(octubre),
            noviembre = sum(noviembre),
            diciembre = sum(diciembre)) %>% 
  transmute(anio = anio,
            enero = enero,
            febrero = enero+febrero,
            marzo = febrero+marzo,
            abril = marzo+abril,
            mayo = abril+mayo,
            junio = mayo+junio,
            julio = junio+julio,
            agosto = julio+agosto,
            septiembre = agosto+septiembre,
            octubre = septiembre+octubre,
            noviembre = octubre+noviembre,
            diciembre = noviembre+diciembre)

acumulada <- t(acumulada)
acumulada <- as_tibble(acumulada)

acumulada <- acumulada %>% 
  transmute(dos15=as.numeric(V1),
            dos16=as.numeric(V2),
            dos17=as.numeric(V3),
            dos18=as.numeric(V4),
            dos19=as.numeric(V5),
            dos20=as.numeric(V6))

acumulada <- cbind(Row.Names = rownames(acumulada), acumulada)
acumulada <- acumulada %>%
  filter(Row.Names  != 1)

meses <- c('01_Enero', '02_Febrero', '03_Marzo', '04_Abril', '05_Mayo', '06_Junio',
           '07_Julio', '08_Agosto', '09_Septiembre', '10_Octubre', '11_Noviembre', '12_Diciembre')

acumulada <- acumulada %>% 
  mutate(mes = meses) %>% 
  select(-1)

acumulada_labs <- acumulada %>% 
  filter(mes == '12_Diciembre')
  

#Graficar
g_acumulada <- ggplot(data = acumulada) +
  geom_line(aes(x = mes, y = dos15, group =1), colour = "#003383", size = 0.2)+
  geom_line(aes(x = mes, y = dos16, group =1), colour = "#0075a9", size = 0.2)+
  geom_line(aes(x = mes, y = dos17, group =1), colour = '#8499a5', size = 0.2)+
  geom_line(aes(x = mes, y = dos18, group =1), colour = "#00a5db", size = 0.2)+
  geom_line(aes(x = mes, y = dos19, group =1), color = '#003893', size = 0.2)+
  geom_line(aes(x = mes, y = dos20, group =1), color = '#162342', size = 0.2)+
  labs(title = "Comportamiento de la incidencia delictiva general por año y por mes",
       subtitle = estado,
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x = "Carpetas de Investigación por mes", y = "Año")+
  geom_text(aes(x = mes, y = dos15, label = paste('2015: ',`dos15`), hjust = 1, vjust = -0.2),
            data = acumulada_labs, color = '#003383', size = 4)+
  geom_text(aes(x = mes, y = dos16, label = paste('2016: ',`dos16`), hjust = 1, vjust = -0.2),
            data = acumulada_labs, color = '#0075a9', size = 4)+
  geom_text(aes(x = mes, y = dos17, label = paste('2017: ',`dos17`), hjust = 1, vjust = -0.2),
            data = acumulada_labs, color = '#8499a5', size = 4)+
  geom_text(aes(x = mes, y = dos18, label = paste('2018: ',`dos18`), hjust = 1, vjust = -0.2),
            data = acumulada_labs, color = '#00a5db', size = 4)+
  geom_text(aes(x = mes, y = dos19, label = paste('2019: ',`dos19`), hjust = 1, vjust = -0.2),
            data = acumulada_labs, color = '#003893', size = 4)+
  geom_text(aes(x = mes, y = dos20, label = paste('2020: ',`dos20`), hjust = 1, vjust = -0.2),
            data = acumulada_labs, color = '#162342', size = 4)+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+
  
  png(filename =  paste0(estado,'_acumulada','.png'),
      width = 4096, height = 2048, units = "px", pointsize = 12,
      bg = "white",  res = 300)
print(g_acumulada)
dev.off()


###DELITOS DEL AÑO FINALIZADO
anual <- id_municipal %>%
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  filter(anio == '2020') %>% 
  group_by(clave_entidad) %>% 
  summarise(delitos = sum(total))

total_delitos_nacional <- sum(anual$delitos)
total_poblacion_nacional <- sum(anual$poblacion)
tasa_nacional_general <- round(((total_delitos_nacional/total_poblacion_nacional)*100000),2)

anual <- left_join(entidades, anual)

anual <- anual %>% 
  mutate(tasa = round(((delitos/poblacion)*100000), digits = 2),
         porcentaje = round(((delitos/total_delitos_nacional)*100), digits = 2))


brks_anual <- c(0,1,3,5,10,15,
                max((anual$porcentaje+1), na.rm = TRUE))

g_arbol_anual <- anual %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_anual, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(entidad,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general por estado como porcentaje del total nacional",
       subtitle = anio,
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_arbol_incidencia_general','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_anual)
dev.off()

#GRÁFICAS TASA VÍCTIMAS
victimas_homicidio <- victimas %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  filter(subtipo == 'Homicidio doloso') %>% 
  group_by(clave_entidad) %>% 
  summarise(homicidio = sum(total))

tasa_victimas <- left_join(entidades, victimas_homicidio)

victimas_feminicidio <- victimas %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  filter(subtipo == 'Feminicidio') %>% 
  group_by(clave_entidad) %>% 
  summarise(feminicidio = sum(total))

tasa_victimas <- left_join(tasa_victimas, victimas_feminicidio)

tasa_victimas <- tasa_victimas %>%
  mutate(tasa_homicidio = round((homicidio/poblacion)*100000,2),
         tasa_feminicidio = round((feminicidio/poblacion)*100000,2),
         is_entidad = ifelse(entidad == estado, TRUE,FALSE))

total_victimas_homicidio <- sum(tasa_victimas$homicidio)
tasa_nacional_homicidio <- round((total_victimas_homicidio/poblacion_nacional)*100000,2)
total_victimas_feminicidio <- sum(tasa_victimas$feminicidio)
tasa_nacional_feminicidio<- round((total_victimas_feminicidio/poblacion_nacional)*100000,2)

#VÍCTIMAS HOMICIDIO DOLOSO
g_tasa_homicidio_anual <- tasa_victimas %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa_homicidio), tasa_homicidio, fill = is_entidad))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = c('TRUE' = 'steelblue1', 'FALSE' = 'steelblue4'), guide = FALSE)+
  labs(title="Víctimas de homicidio doloso por cada 100 mil habitantes",
       subtitle = anio,
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=tasa_victimas,aes(label=tasa_homicidio, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_homicidio, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_homicidio,
                label = paste('Media nacional:',tasa_nacional_homicidio), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_tasa_homicidio','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_homicidio_anual)
dev.off()


#VÍCTIMAS FEMINICIDIO
g_tasa_feminicidio_anual <- tasa_victimas %>%
  ggplot(mapping = aes(x=reorder(entidad, tasa_feminicidio), tasa_feminicidio, fill = is_entidad))+
  geom_col()+
  scale_fill_manual(values = c('TRUE' = 'steelblue1', 'FALSE' = 'steelblue4'), guide = FALSE)+
  coord_flip()+
  labs(title="Víctimas de feminicidio por cada 100 mil habitantes",
       subtitle = anio,
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Estado", y="Tasa de incidencia", size = 4)+
  geom_text(data=tasa_victimas,aes(label=tasa_feminicidio, vjust = 0.5, hjust = 1), color = 'white')+
  geom_hline(yintercept = tasa_nacional_feminicidio, color = "red", linetype="dashed")+
  geom_text(aes(0, tasa_nacional_feminicidio,
                label = paste('Media nacional:',tasa_nacional_feminicidio), vjust = -1, hjust = -0.1))

png(filename =  paste0(estado,'_tasa_feminicidio','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_tasa_feminicidio_anual)
dev.off()


#ARBOL DISTRIBUCIÓN MUNICIPAL DEL ESTADO ANALIZADO
incidencia_municipal <- id_municipal %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  filter(entidad == estado) %>% 
  group_by(clave_municipio) %>% 
  summarise(delitos = sum(total))

incidencia_municipal <- left_join(municipios_estado, incidencia_municipal, by = c("clave_municipio"))
  

total_incidencia_estatal <- sum(incidencia_municipal$delitos)

incidencia_municipal <- incidencia_municipal %>% 
  mutate(porcentaje = round((delitos/total_incidencia_estatal)*100,2))
         
brks_total_incidencia_estatal <- c(0,1,3,5,10,15,
                max((incidencia_municipal$porcentaje+1), na.rm = TRUE))

g_arbol_incidencia_municipal <- incidencia_municipal %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_total_incidencia_estatal, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(municipio,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general por municipio como porcentaje del total estatal",
       subtitle = paste0(estado,': ',anio),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_arbol_incidencia_estatal','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_incidencia_municipal)
dev.off()


#ARBOL INCIDENCIA POR BIEN JURÍDICO AFECTADO NACIONAL
bien_afectado <- id_municipal %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  group_by(categoria) %>% 
  summarise(delitos = sum(total))

total_incidencia <- sum(bien_afectado$delitos)

bien_afectado <- bien_afectado %>% 
  mutate(porcentaje = round((delitos/total_incidencia)*100,2))

brks_bien_afectado <- c(0,1,10,25,
                                   max((bien_afectado$porcentaje+1), na.rm = TRUE))

g_arbol_bien_afectado <- bien_afectado %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_bien_afectado, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(categoria,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general según el bien jurídico afectado",
       subtitle = anio,
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_arbol_bien_afectado','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_bien_afectado)
dev.off()


#ARBOL INCIDENCIA POR BIEN JURÍDICO AFECTADO DEL ESTADO ANALIZADO
bien_afectado_estatal <- id_municipal %>% 
  filter(clave_entidad == clave_ent) %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  group_by(categoria) %>% 
  summarise(delitos = sum(total))

total_incidencia <- sum(bien_afectado_estatal$delitos)

bien_afectado_estatal <- bien_afectado_estatal %>% 
  mutate(porcentaje = round((delitos/total_incidencia)*100,2))

brks_bien_afectado_estatal <- c(0,1,10,25,
                        max((bien_afectado_estatal$porcentaje+1), na.rm = TRUE))

g_arbol_bien_afectado_estatal <- bien_afectado_estatal %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_bien_afectado_estatal, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(categoria,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  bbc_style()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general según el bien jurídico afectado",
       subtitle = paste0(estado,': ',anio),
       caption =  "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()

png(filename =  paste0(estado,'_arbol_bien_afectado_estatal','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_arbol_bien_afectado_estatal)
dev.off()


#MAPAS NACIONALES
#Incidencia delictiva general
tasa_municipal <- id_municipal %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  group_by(clave_municipio) %>% 
  summarise(delitos = sum(total))

tasa_municipal <- left_join(municipios, tasa_municipal, by = c('clave' = 'clave_municipio'))

tasa_municipal <- tasa_municipal %>% 
  mutate(tasa = round(((delitos/poblacion)*10000),2))

brks_tasa_municipal <- c(0,150,300,400,500,1000,1500,2000,3000,
                         max((tasa_municipal$tasa+1), na.rm = TRUE))

tasa_municipal <- tasa_municipal %>% 
  mutate(tasa_cut = cut(tasa, breaks = brks_tasa_municipal, right = FALSE))


tasa_municipal_mapa <- shapes %>%
  left_join(tasa_municipal,
            by = c('clave_municipio' = 'clave'))

mapa_tm <- tasa_municipal_mapa %>%
  ggplot(aes(fill = tasa_cut)) +
  geom_sf(colour = "dark grey", size = 0.05) +
  labs(title = "Tasa de inciencia delictiva por cada 10 mil habitantes",
       subtitle = anio,
       caption = "Fuente: elaboración propia con infromación del Sistema Nacional de Seguridad Pública y CONAPO") +  scale_fill_brewer("Tasa de incidencia", palette = "Blues") +
  theme_bw()

png(filename =  paste0(anio,'_mapa_tasa','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(mapa_tm)
dev.off()

#Víctimas de homicidio doloso


#MAPAS ESTATALES
#Incidencia delictiva general
tasa_municipal <- id_municipal %>% 
  mutate(total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre) %>% 
  filter(entidad == estado) %>% 
  group_by(clave_municipio) %>% 
  summarise(delitos = sum(total))

tasa_municipal <- left_join(municipios_estado, tasa_municipal, by = c('clave' = 'clave_municipio'))

tasa_municipal <- tasa_municipal %>% 
  mutate(tasa = round(((delitos/poblacion)*10000),2))

brks_tasa_municipal <- c(0,150,300,400,500,1000,1500,2000,3000,
                         max((tasa_municipal$tasa+1), na.rm = TRUE))

tasa_municipal <- tasa_municipal %>% 
  mutate(tasa_cut = cut(tasa, breaks = brks_tasa_municipal, right = FALSE))


tasa_municipal_mapa <- shapes_estado %>%
  left_join(tasa_municipal,
            by = c('clave_municipio' = 'clave'))

mapa_tm <- tasa_municipal_mapa %>%
  ggplot(aes(fill = tasa_cut)) +
  geom_sf(colour = "dark grey", size = 0.05) +
  labs(title = "Tasa de inciencia delictiva por cada 10 mil habitantes",
       subtitle = anio,
       caption = "Fuente: elaboración propia con infromación del Sistema Nacional de Seguridad Pública y CONAPO") +
  scale_fill_brewer("Tasa de incidencia", palette = "Blues") +
  theme_bw()

png(filename =  paste0(anio,'_mapa_tasa_estatal','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(mapa_tm)
dev.off()


