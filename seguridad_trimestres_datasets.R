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


delitos <- id_municipal %>% 
  group_by(entidad, anio) %>% 
  summarise(q1 = sum(q1),
            q2 = sum(q2),
            q3 = sum(q3),
            q4 = sum(q4))

write_csv(delitos,'incidencia_total_trimeestres.csv')

homicidio <- victimas %>% 
  filter(subtipo == 'Homicidio doloso') %>% 
  group_by(entidad, anio) %>% 
  summarise(q1 = sum(q1),
            q2 = sum(q2),
            q3 = sum(q3),
            q4 = sum(q4))

write_csv(homicidio,'victimas_homicidio_trimestres.csv')

