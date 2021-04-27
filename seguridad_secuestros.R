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
  filter(entidad != 'Entidad',
         entidad == 'Jalisco',
         tipo == 'Secuestro') %>%
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
            total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre)

sec_eventos_2021 <- id_municipal %>% 
  filter(anio == '2021') %>% 
  mutate(total = enero+febrero+marzo) %>% 
  group_by(anio) %>% 
  summarise(secuestros = sum(total)) %>% 
  transmute(anio = anio,
            secuestros = as.numeric(secuestros))


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
  filter(entidad != 'Entidad',
         entidad == 'Jalisco',
         tipo == 'Secuestro') %>%
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
            total = enero+febrero+marzo+abril+mayo+junio+julio+agosto+septiembre+octubre+noviembre+diciembre)



sec_victimas_2021 <- victimas %>% 
  filter(anio == '2021') %>% 
  mutate(total = enero+febrero+marzo) %>% 
  group_by(anio) %>% 
  summarise(secuestros = sum(total))



eventos <- id_municipal %>% 
  group_by(anio) %>% 
  summarise(secuestros = sum(total)) %>%
  filter(anio != '2021') %>% 1
  gather(eventos, sec_eventos_2021, key = 'anio', value = 'secuestros')

g_eventos_secuestro <- eventos %>% 
  ggplot(aes(x = anio, y = secuestros))+
  geom_col(colour = '#162342', fill = '#162342')+
  labs(title="Eventos de secuestro",
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Eventos de secuestro", size = 4)+
  geom_text(data = eventos,aes(label=secuestros, vjust = -0.5, hjust = 0.5))

png(filename =  paste0(eventos,'eventos_secuestro','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_eventos_secuestro)
dev.off()


vic_secuestro <- victimas %>%
  group_by(anio) %>% 
  summarise(secuestros = sum(total))


