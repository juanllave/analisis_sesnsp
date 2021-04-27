library(tidyverse)
library(treemapify)
library(sf)
library(cowplot)

#Carga dataset de municipios de Jaliscoy su población.
municipios <- read_csv('municipios_jal.csv') %>%
  transmute(clave_municipio = as.numeric(clave)+14000,
            municipio = municipio,
            poblacion = as.numeric(poblacion))

#Carga shapefiles para mapas
jal_map <- st_read("shapefiles/01_32_mun.shp") %>%
  filter(CVE_ENT == '14') %>%
  mutate(clave_municipio = as.numeric(CVEGEO))

#Cargar los datos de la incidencia delictiva. Se excluyen los nombres de las columnas porque generar un error.
id_municipal <- read_csv("Municipal-Delitos-2015-2020_oct2020.csv", 
                         col_names = FALSE)

#Crear vector con los nombres de las columnas.
nombres_columnas <- c('ano', 'clave_entidad', 'entidad', 'clave_municipio',
                      'municipio', 'categoria', 'tipo', 'subtipo', 'modalidad',
                      'enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio',
                      'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')

#Cambiar el nombre de las columnas.
colnames(id_municipal) <- nombres_columnas

#Ajustes necesarios previo al análisis.
id_municipal <- id_municipal %>%
  filter(entidad != 'Entidad') %>%
  transmute(ano = ano,
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
  
#Crea el dataset para el análisis del mes en curso.
octubre <- id_municipal %>%
  select(c(1:8), octubre) %>%
  filter(clave_entidad == '14', ano == '2020')

#Gráficas y mapas de la incidencia general
incidencia_total <- octubre %>%
  group_by(clave_municipio) %>%
  summarise(delitos = sum(octubre))

incidencia_total <- inner_join(municipios, incidencia_total)

total_jalisco <- sum(incidencia_total$delitos)

incidencia_total <- incidencia_total %>%
  mutate(porcentaje = round((delitos/total_jalisco)*100, digits = 2),
         delitos = delitos,
         tasa = round((delitos/poblacion)*100000, digits = 2))

#Árbol de la distribución de la incidencia general por municipio
brks_total <- c(0,1,5,10,15,20,
                max((incidencia_total$porcentaje+1), na.rm = TRUE))

g__arbol_total <- incidencia_total %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_total, right = FALSE))%>%
  ggplot(aes(area = delitos, fill = Porcentaje, label = paste(municipio,delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general por municipio como porcentaje del total estatal",
       subtitle = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_arbol_incidencia_general','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g__arbol_total)
dev.off()
  

#Mapa de la incidencia delcitiva general por cada 100,000 habitantes
incidencia_total <- left_join(incidencia_total, jal_map)

brks_tasa_general <- c(0,1,50,100,125, 150,175,200,
                        max((incidencia_total$tasa+1), na.rm = TRUE))

mapa_incidencia_general <- incidencia_total %>%
  mutate(tasa_cut = cut(tasa, breaks = brks_tasa_general, right = FALSE)) %>%
  ggplot(aes(fill = tasa_cut)) +
  geom_sf(aes(geometry = geometry), colour = "white", size = 0.07) +
  labs(title = "Incidencia delictiva general por cada 100,000 habitantes",
       caption = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer("Incidencia por cada 100,000 habtantes", palette = "Blues") +
  theme_bw()

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_mapa_incidencia_general','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(mapa_incidencia_general)
dev.off()

#Gráficas de incidencia por bien jurídico afectado
incidencia_categoria <- octubre %>%
  group_by(categoria) %>%
  summarise(count = sum(octubre))

categorias <- c('El Patrimonio', 'La familia', 'La libertad y la seguridad sexual', 'La sociedad',
                'La vida y la integridad corporal', 'La libertad personal', 'Otros bienes jurídicos afectados (del fuero común)')

incidencia_categoria <- incidencia_categoria %>%
  transmute(categoria = categorias,
            Delitos = count,
            porcentaje = round((count/total_jalisco)*100, digits = 2))

brks_categoria <- c(0,1,10,15,20,25,
                max((incidencia_categoria$porcentaje+1), na.rm = TRUE))

g__arbol_categoria <- incidencia_categoria %>%
  mutate(Porcentaje = cut(porcentaje, breaks = brks_categoria, right = FALSE))%>%
  ggplot(aes(area = Delitos, fill = Porcentaje, label = paste(categoria,Delitos,porcentaje,sep="\n")))+
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "topleft", size = 12, grow = FALSE)+
  labs(title = "Distribución de la incidencia delictiva general por bien jurídico afectado",
       subtitle = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública") +
  scale_fill_brewer(palette = "Blues")

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_arbol_incidencia_categoria','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g__arbol_categoria)
dev.off()

g_incidencia_general <- plot_grid(g__arbol_categoria, mapa_incidencia_general,g__arbol_total, align = 'h')

png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_graficas_incidencia_categoria','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_incidencia_general)
dev.off()

#Comparativa del mismo mes
mismo_mes <- id_municipal %>%
  select(c(1:9), octubre) %>%
  filter(clave_entidad == '14') %>%
  group_by(ano) %>%
  summarise(delitos = sum(octubre))

g_comportamiento_anual <- mismo_mes %>%
  ggplot(mapping = aes(x= ano, y = delitos))+
  geom_col(colour = 'dark blue', fill = 'dark blue')+
  labs(title="Comparativo de la inicidencia delictiva general respecto al mismo mes de los cinco años previos",
       subtitle = "Fuente: elaboración propia con información del Sistema Nacional de Seguridad Pública",
       x="Año", y="Incidencia delictiva general", size = 4)+
  geom_text(data=mismo_mes,aes(label=delitos, vjust = -0.5, hjust = 0.5))


png(filename =  paste0(format(Sys.Date(),'%Y%m%d'),'_comparativa_mensual','.png'),
    width = 4096, height = 2048, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(g_comportamiento_anual)
dev.off()
