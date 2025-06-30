# Información ----

# Este proyecto se propone analizar bases de datos de investigadores (https://datasets.datos.mincyt.gob.ar/dataset/personal-de-ciencia-y-tecnologia), instituciones (https://datasets.datos.mincyt.gob.ar/dataset/instituciones-del-sistema-nacional-de-ciencia-y-tecnologia-argentino), proyectos (https://datasets.datos.mincyt.gob.ar/dataset/proyectos-de-ciencia-tecnologia-e-innovacion) y publicaciones (https://datasets.datos.mincyt.gob.ar/dataset/producciones-cientifico-tecnologicas) del sistema cientìfico argentino, para construir redes de cooperación (en base a investigadores y proyectos) y coautoría (en base a investigadores y publicaciones) y poder analizarlas.

# Librerías ----

# Este archivo utiliza las siguientes librerías:

library(tidyverse)
library(readr)
library(dplyr)
library(igraph)

library(ComplexHeatmap)

# Datos ----


# Este archivo utiliza las siguientes tablas de datos (todas correspondientes al año 2011) y referencias:

setwd("C:\\Users\\facud\\Documents\\Proyecto PROPAI\\PIDAE-Branch_Colaboraciones")

# Personal
personas_2011 <- read_csv2("./Datos/Personal/personas_2011.csv")
personas_2012 <- read_csv2("./Datos/Personal/personas_2012.csv")
personas_2013 <- read_csv2("./Datos/Personal/personas_2013.csv")
personas_2014 <- read_csv2("./Datos/Personal/personas_2014.csv")
personas_2015 <- read_csv2("./Datos/Personal/personas_2015.csv")
personas_2016 <- read_csv2("./Datos/Personal/personas_2016.csv")
personas_2017 <- read_csv2("./Datos/Personal/personas_2017.csv")
personas_2018 <- read_csv2("./Datos/Personal/personas_2018.csv")
personas_2019 <- read_csv2("./Datos/Personal/personas_2019.csv")
personas <- read_csv2("./Datos/Personal/personas.csv")
ref_sexo <- read_csv2("./Datos/Personal/ref_sexo.csv")

# Instituciones

organizaciones <- read_csv2("./Datos/Instituciones/organizaciones.csv") %>% 
  rename(institucion_trabajo_id="organizacion_id")                       # En https://datasets.datos.mincyt.gob.ar/dataset/instituciones-del-sistema-nacional-de-ciencia-y-tecnologia-argentino/archivo/c7947acc-3df1-436b-971e-fa0a49b98eed.

# Proyectos 
Proyectos_2008_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2008.csv") #ciertos proyectos aparecen "acorregir" por un problema encontrado mas adelante en el codigo
Proyectos_2009 <-read_csv2("./Datos/Proyectos/proyectos_2009.csv")
Proyectos_2010_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2010.csv")
Proyectos_2011_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2011.csv")
Proyectos_2012_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2012.csv")
Proyectos_2013_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2013.csv")
Proyectos_2014_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2014.csv")
Proyectos_2015_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2015.csv")
Proyectos_2016_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2016.csv")
Proyectos_2017_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2017.csv")
Proyectos_2018_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2018.csv")
Proyectos_2019_acorregir <-read_csv2("./Datos/Proyectos/proyectos_2019.csv")
Proyecto_disciplina <-read_csv2("./Datos/Proyectos/proyecto_disciplina.csv")
Proyecto_beneficiario <- read.csv2("./Datos/Proyectos/proyecto_beneficiario.csv")
Proyecto_participante <- read.csv2("./Datos/Proyectos/proyecto_participante.csv")
Ref_moneda <-read.csv2("./Datos/Proyectos/Ref_moneda.csv")
Ref_tipo_proyecto <- read.csv2("./Datos/Proyectos/Ref_tipo_proyecto.csv")
Ref_disciplina <- read_csv2("./Datos/Proyectos/Ref_disciplina.csv")
Ref_funcion <- read.csv2("./Datos/Proyectos/Ref_funcion.csv")


# Código ----

#Primero usaremos las tablas principales de Instituciones, Personal y Proyectos.
#Luego 

# Instituciones

#se organizan las variables de cada tibble
#las organizaciones 3470, 7148, 16315, 17371 y 18134 estan duplicadas, las eliminaremos

##id 3470:
#GOBIERNO DE LA PROVINCIA DE BUENOS AIRES
#MINISTERIO DE SALUD
#SUBSECRETARIA DE PLANIFICACION Y CONTRALOR SANITARIO
#DIRECCION PROVINCIAL DEL INSTITUTO BIOLOGICO "DR. Tomas Peron"

##id 7148:
#GOBIERNO DE LA CIUDAD AUTONOMA DE BUENOS AIRES
#MINISTERIO DE CULTURA
#SUBSECRETARIA DE POLITCAS CULTURALES Y NUEVAS AUDIENCIAS
#CENTRO CULTURAL RECOLETA

##id 16315:
#GOBIERNO DE LA PROVINCIA DE MENDOZA
#DEPARTAMENTO GENERAL DE IRRIGACION
#SECRETARIA DE GESTION INSTITUCIONAL
#DIRECCION DE GESTION INSTITUCIONAL

##id 17371:
#GOBIERNO DE LA CIUDAD AUTONOMA DE BUENOS AIRES
#MINISTERIO DE CULTURA
#SUBSECRETARIA DE GESTION CULTURAL
#DIRECCION GENERAL DE PATRIMONIO, MUSEOS Y CASCO HISTORICO

##id 18134:
#MINISTERIO DE AGRICULTURA GANADERIA Y PESCA DE LA NACION
#SECRETARIA DE GOBIERNO DE AGROINDUSTRIA
#SERVICIO NACIONAL DE SANIDAD Y CALIDAD AGROALIMENTARIA
#OFICINA DE SENASA



anti_join(personas, personas_2011, by="persona_id")



organizaciones_limpia <- organizaciones %>%
  select(institucion_trabajo_id, institucion_nivel1_descripcion, institucion_nivel2_descripcion, institucion_nivel3_descripcion, institucion_nivel4_descripcion) %>% 
  rename(organizacion_id = institucion_trabajo_id) %>%
  rename(institucion_nivel1_desc = institucion_nivel1_descripcion) %>%
  rename(institucion_nivel2_desc = institucion_nivel2_descripcion) %>% 
  rename(institucion_nivel3_desc = institucion_nivel3_descripcion) %>% 
  rename(institucion_nivel4_desc = institucion_nivel4_descripcion) %>% 
  filter(!duplicated(.))

# Personal

personas_limpia <- personas %>%
  select(persona_id,
         nombre,
         apellido) %>%
  mutate(persona=paste(apellido, nombre, sep=", "))

#conectamos
personas_total <- personas_2011 %>%
  full_join(personas_2012) %>%
  full_join(personas_2013) %>%
  full_join(personas_2014) %>%
  full_join(personas_2015) %>%
  full_join(personas_2016) %>%
  full_join(personas_2017) %>%
  full_join(personas_2018) %>%
  full_join(personas_2019) %>%
  select(persona_id, institucion_trabajo_id,anio) %>%
  filter(!duplicated(.))


personas_joined <- personas_total%>% 
  left_join(personas_limpia,
            by="persona_id") %>% 
  distinct(persona_id, nombre, apellido, persona, institucion_trabajo_id,  .keep_all = TRUE) %>% #este distinct nos deja eliminar los duplicados cuando año a año se mantuvieron en el mismo lugar de trabajo. el anio que aparece es el de inicio en la institucion de trabajo
  select(persona_id,
         nombre,
         apellido,
         persona,
         institucion_trabajo_id,
         anio)

# Proyectos

#Juntamos toda la informacion anual de "proyectos_YYYY" 

glimpse(Proyectos_2008_acorregir)             #hay diferencias en las clases de datos para cada variable de cada tabla, lo corregimos porque no nos dejara joinear
glimpse(Proyectos_2009)                       #no hay que corregir nada en 2009
glimpse(Proyectos_2010_acorregir)
glimpse(Proyectos_2011_acorregir)  
glimpse(Proyectos_2012_acorregir)
glimpse(Proyectos_2013_acorregir)
glimpse(Proyectos_2014_acorregir)
glimpse(Proyectos_2015_acorregir)
glimpse(Proyectos_2016_acorregir) 
glimpse(Proyectos_2017_acorregir)
glimpse(Proyectos_2018_acorregir)
glimpse(Proyectos_2019_acorregir)


Proyectos_2008 <- Proyectos_2008_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(palabras_clave = as.character(palabras_clave)) %>% 
  mutate(cantidad_miembros_F = as.double(cantidad_miembros_F)) %>% 
  mutate(cantidad_miembros_M = as.double(cantidad_miembros_M)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4)) %>% 
  filter(!fecha_finalizacion <= 2010)

Proyectos_2009 <- Proyectos_2009 %>%
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))%>% 
  filter(!fecha_finalizacion <= 2010)

Proyectos_2010 <- Proyectos_2010_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))%>% 
  filter(!fecha_finalizacion <= 2010) 

Proyectos_2011 <- Proyectos_2011_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(monto_total_solicitado = as.double(monto_total_solicitado)) %>%
  mutate(monto_total_adjudicado = as.double(monto_total_adjudicado)) %>%   
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2012 <- Proyectos_2012_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2013 <- Proyectos_2013_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2014 <- Proyectos_2014_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2015 <- Proyectos_2015_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(monto_total_solicitado = as.double(monto_total_solicitado)) %>%
  mutate(monto_total_adjudicado = as.double(monto_total_adjudicado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2016 <- Proyectos_2016_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(monto_financiado_adjudicado = as.double(monto_financiado_adjudicado)) %>% 
  mutate(monto_total_solicitado = as.double(monto_total_solicitado)) %>%
  mutate(monto_total_adjudicado = as.double(monto_total_adjudicado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2017 <- Proyectos_2017_acorregir %>% 
  mutate(monto_financiado_solicitado = as.double(monto_financiado_solicitado)) %>% 
  mutate(monto_total_solicitado = as.double(monto_total_solicitado)) %>%
  mutate(monto_total_adjudicado = as.double(monto_total_adjudicado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2018 <- Proyectos_2018_acorregir %>% 
  mutate(monto_total_solicitado = as.double(monto_total_solicitado)) %>%
  mutate(monto_total_adjudicado = as.double(monto_total_adjudicado)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyectos_2019 <- Proyectos_2019_acorregir %>% 
  mutate(fondo_anpcyt = as.character(fondo_anpcyt)) %>% 
  mutate(fecha_inicio = substr(fecha_inicio, 1, 4)) %>% 
  mutate(fecha_finalizacion = substr(fecha_finalizacion, 1, 4))

Proyecto_participante <- Proyecto_participante %>% 
  mutate(fecha_inicio= as.Date(fecha_inicio)) %>% 
  mutate(proyecto_id= as.double(proyecto_id))

##joineamos todo
Proyectos_TOTAL<- Proyectos_2008 %>%
  full_join(Proyectos_2009) %>%
  full_join(Proyectos_2010) %>%
  full_join(Proyectos_2011) %>%
  full_join(Proyectos_2012) %>%
  full_join(Proyectos_2013) %>%
  full_join(Proyectos_2014) %>%
  full_join(Proyectos_2015) %>%
  full_join(Proyectos_2016) %>%
  full_join(Proyectos_2017) %>%
  full_join(Proyectos_2018) %>%
  full_join(Proyectos_2019) %>% 
  rename(fecha_iniciacion="fecha_inicio") 

# Analisis de consistencia en los datos

#Armaremos una red de colaboracion entre los proyectos y las organizaciones de sus investigadores involucrados
#la informacion sobre los proyectos se encuentra en la tabla "proyectos_YYYY" y de las personas y sus
#lugares de trabajo (organizaciones) en "personas_YYYY".
#Las conexiones entre tales organizaciones y proyectos estaran mediados por una tercera tabla
#esta tabla es "proyecto_participante"


#(1). #Tenemos datos de proyectos desde 2008 hasta 2019 y de personas y sus organizaciones entre 2011 y 2019
#Esto implica que no tenemos informacion sobre los lugares de trabajo de las personas que trabajaron en 
#proyectos iniciados en el periodo desde 2008 hasta 2010 

#Entre 2008 y 2019 tenemos: 
Proyecto_participante_2008 <-Proyectos_TOTAL  %>% 
  inner_join(Proyecto_participante,
  by="proyecto_id") %>% 
  select(proyecto_id, persona_id,funcion_id, fecha_inicio, fecha_fin)

Proyecto_participante_2008 %>% count(proyecto_id)                #14.998 Proyectos
Proyecto_participante_2008 %>% count(persona_id)                 #45.552 Personas
Proyecto_participante_2008 %>% left_join(personas_joined,        #3.396 Organizaciones
                                    by="persona_id") %>% 
  count(institucion_trabajo_id)                             

#¿Cuantos proyectos/personas/organizaciones dejamos de tener en cuenta al eliminar los que no pueden ser 
#vinculados a las instituciones de trabajo por un tema de temporalidad?

#Primero, descontamos proyectos de Proyecto participante que empicen antes de 2011


####Aca, en vez de fecha de iniciacion, considerar usar fecha de finalizacion. Esto, nos permitiria tener en cuenta todos los proyectos que estan activos a partir de 2011

Proyectos_TOTAL<- Proyectos_TOTAL %>% 
  filter(!fecha_iniciacion<2011)      

#Asi, nos queda:

Proyecto_participante <-Proyectos_TOTAL  %>% 
  inner_join(Proyecto_participante,
             by="proyecto_id") 

Proyecto_participante %>% count(proyecto_id)                #12.531 Proyectos, 2.467 menos
Proyecto_participante %>% count(persona_id)                 #43.020 Personas, 2.532 menos
Proyecto_participante %>% left_join(personas_joined,        #3.318 Organizaciones, 78 menos
                                    by="persona_id") %>% 
  count(institucion_trabajo_id) 

#¿En cuanto a la composicion de los numeros, cuantos de los 2467 proyectos se eliminaron por cual u otra causa?
#Veamos, si agarramos Proyecto_participante_2008, ¿cuantos proyectos son eliminados por personas que empezaron
#a trabajar en proyectos antes de 2011? ¿Cuantos proyectos son eliminados si dejamos de tener en cuenta
#los proyectos anteriores a 2011 para los cuales existen investigadores que empezaron a trabajar en ellos
#posteriormente a 2011?

#Primero, eliminamos las observaciones de personas con fecha de iniciacion en un proyecto anterior a 2011.

Proyecto_participante_2008 %>% count(proyecto_id)                # 14.998 Proyectos
Proyecto_participante_2008 %>% count(persona_id)                 # 45.442 Personas
Proyecto_participante_2008 %>% left_join(personas_joined,        # 3.396  Organizaciones
                                         by="persona_id") %>% 
  count(institucion_trabajo_id)   

Proyecto_participante_2008 <- Proyecto_participante_2008 %>% 
  filter(!fecha_inicio < as.Date("2011-01-01"))

Proyecto_participante_2008 %>% count(proyecto_id)                #12.755 Proyectos, 2.243 menos
Proyecto_participante_2008 %>% count(persona_id)                 #43.053 Personas, 2.389 menos
Proyecto_participante_2008 %>% left_join(personas_joined,        #3.320 Organizaciones, 76 menos
                                         by="persona_id") %>% 
  count(institucion_trabajo_id)                             


#Luego, nos queda ver los proyectos que empezaron antes de 2011 pero que tienen investigadores
#que empezaron a trabajar ahi despues de 2011. Lo podemos ver por descarte entre proyecto_participante_2008 y 
#proyecto participante habiendo eliminado los proyectos anteriores a 2011.

#Proyectos: 12.755 - 12.531 = 224 
#Personas: 43.053 - 43.020 = 33
#Organizaciones: 3.320 - 3.318 = 2

#(2). #Dado que las conexiones entre proyectos y personas las armamos en base a la tabla "proyecto_participante".
#primero veremos si todos los proyectos de "proyecto_YYYY" y personas en "persona_YYYY" estan incluidos en
#proyecto_participante. Proyecto_total denota la suma de los "proyecto_YYYY" desde 2008 a 2019.
#Luego, cuantas personas_id estan en "personas_YYYY" pero no en "proyecto_participante"?

personas_joined %>% count(persona_id)       #75.991 personas
Proyecto_participante %>% count(persona_id) #43.020 personas
inner_join(personas_joined,
           Proyecto_participante,
           by="persona_id") %>% 
  count(persona_id)                         #25.213 personas

#hay 50.788 personas en "personas_joined" que no estan en "proyecto_participante"
#hay 17.817 personas en "proyecto_participante" que no estan en "personas_joined"

#como necesitamos que las personas esten tanto en proyecto_participante como personas_joined para
#poder seleccionar sus lugares de trabajo y conectarlos a los respectivos proyectos, nos quedaremos
#con las personas en comun.
#para eso, hacemos que proyecto participante solo contenga a los proyectos con los cuales hay al menos 
#alguna persona con informacion de personas_joined

Proyecto_participante <- inner_join(Proyecto_participante,
                                    personas_joined,
                                    by="persona_id") %>% 
  select(proyecto_id, persona_id, nombre, apellido, persona,funcion_id, fecha_inicio, fecha_fin) #aca, todos los proyectos estan conectados a personas en personas_joined

#Por haber tenido en cuenta doble conexion con personas, cuantos proyectos, personas y organizaciones perdimos?

Proyecto_participante %>% count(proyecto_id)                #11.265 Proyectos, 1.266 menos
Proyecto_participante %>% count(persona_id)                 #25.213 Personas, 17.807 menos
Proyecto_participante %>% left_join(personas_joined,        #3.317 Organizaciones, 1 menos
                                    by="persona_id") %>% 
  count(institucion_trabajo_id) 


#Luego, tambien hay que verificar que estemos analizando los proyectos de proyecto_TOTAL que tambien estan en participante y viceversa

Proyectos_TOTAL %>% count(proyecto_id)          #16.007  proyectos
Proyecto_participante %>% count(proyecto_id)    #11.265  proyectos, 

Proyectos_TOTAL <- inner_join(Proyectos_TOTAL,
           Proyecto_participante,
           by="proyecto_id") %>% 
  select(everything(), -persona_id, -funcion_id, -fecha_inicio, -fecha_fin, -nombre, -apellido, -persona) %>% #eliminamos las duplicaciones causadas por la conexion con proyecto_participante
  filter(!duplicated(.)) 


#Por haber tenido en cuenta doble conexion con proyectos, cuantos proyectos, personas y organizaciones perdimos?

Proyecto_participante %>% count(proyecto_id)                #11.265 Proyectos, sin cambios en proyecto_participante
Proyecto_participante %>% count(persona_id)                 #25.213 Personas, sin cambios en proyecto_participante
Proyecto_participante %>% left_join(personas_joined,        #3.317 Organizaciones, sin cambios en proyecto_participante
                                    by="persona_id") %>% 
  count(institucion_trabajo_id) 


#(3) Luego, de proyectos solo nos interesan los que tengan conexiones a gente con la funcion de 
#1,2,3,5 y 6. Por lo tanto, mantenemos solo la conexion con esas personas.

Proyecto_participante <- Proyecto_participante %>%    #este codigo nos crea una tabla del personas mas amplio que trabaja en los proyectos
  filter(funcion_id %in% c(1,2,3,5,6))

#¿Cuantos proyectos, personas y organizaciones dejamos de tener en cuenta por esto?

Proyecto_participante %>% count(proyecto_id)                #En la interseccion, 11.254 Proyectos, 11 menos. Del total, no se pierden proyectos (sin los anteriores filtros no hay proyecto sin funciones 1,2,3,5,6)
Proyecto_participante %>% count(persona_id)                 #En la interseccion, 24.606 Personas, 607 menos. Del total, 2.355 menos
Proyecto_participante %>% left_join(personas_joined,        #En la interseccion, 3.274  Organizaciones, 43 menos. Del total, 41 menos (estas 41 hubieran sido organizaciones soletonas sin haber hecho este filtrado)
                                    by="persona_id") %>% 
  count(institucion_trabajo_id)

#Ahora, estos cambios que hicimos en proyecto_participante, debemos hacerlo en Proyectos_total para que los nodos (Proyecto_total) tengan
#el mismo criterio de filtrado que las conexiones (proyecto_participante)

Proyectos_TOTAL <- Proyectos_TOTAL %>% 
  inner_join(Proyecto_participante,
             by="proyecto_id") %>% 
  select(everything(), -persona_id, -funcion_id, -fecha_inicio, -fecha_fin, -nombre, -apellido, -persona) %>% #eliminamos las duplicaciones causadas por la conexion con proyecto_participante
  filter(!duplicated(.)) 

#(4).Por ultimo, necesitamos tener en cuenta la temporalidad de las conexiones en Proyecto_participante

Proyecto_participante_broad <- Proyecto_participante %>%
  mutate(fecha_fin = ifelse(fecha_fin=="", "9999/12/31" , fecha_fin)) 

conexiones_broad_a_corregir <-  Proyecto_participante_broad%>%
  left_join(personas_joined,
            by="persona_id") %>%
  filter(!fecha_fin < as.Date(paste0(anio, "-01-01"))) %>%
  select(proyecto_id,
         persona_id,
         institucion_trabajo_id,
         anio,
         fecha_fin) #advertencia, los que no tienen fecha fin me los toma como cero, entonces
#los que no finalizaron tienen siempre fecha_fin menor (igual a cero) que el anio de iniciacion en una organizacion y me lo descarta incorrectamente

fecha_fin_mayor <- filtereados2 %>% 
  filter(fecha_fin>fecha_finalizacion) %>% 
  select(fecha_fin, fecha_finalizacion) #estos son todas las personas con fecha de finalizacion en un proyecto mayor que la fecha de finalizacion del proyecto            

fecha_fin_mayor2 <- fecha_fin_mayor %>% 
  filter(!fecha_finalizacion==substr(fecha_fin,1,4))


conexiones_broad <- conexiones_broad_a_corregir %>%
  # Filtra las filas donde d es "9999/12/31"
  filter(fecha_fin == "9999/12/31") %>%
  # Agrupa por las columnas a y b
  group_by(proyecto_id, institucion_trabajo_id) %>%
  # Filtra la fila con el valor más alto en la columna c
  filter(anio == max(anio)) %>%
  # Combina las filas filtradas con las que no cumplen la condición inicial
  bind_rows(conexiones_broad_a_corregir %>% filter(fecha_fin != "9999/12/31")) %>%
  select(proyecto_id,
         persona_id,
         institucion_trabajo_id,
         anio)

#Aca finaliza el analisis de consistencia. De aca en adelante solo queda juntar los datos que nos interesan
#para luego armar y analizar la red bipartita

#seleccionamos y renombramos ciertas columnas de nuestras tablas

Proyecto_beneficiario_limpia <- Proyecto_beneficiario %>%
  rename(organizacion_financiadora="financiadora") %>%
  rename(organizacion_ejecutora="ejecutora") %>%
  mutate(porcentaje_financiamiento = ifelse(porcentaje_financiamiento =="" , 0, porcentaje_financiamiento))   #Parece haber un error en esta tabla de datos, en casilleros de procentaje de financiamiento donde una organizacion no financia un proyecto, a veces aparecen ceros (0) y a veces no hay ningun valor. Reemplazamos asi los valores faltantes por ceros. Podemos verificar que les corresponderia un cero porque sumando los porcentajes de financiacion de las demas organizaciones en el mismo proyecto, verificamos que suman 1 y, por lo tanto, no hay Missing Values (NA)

Ref_tipo_proyecto_limpia <- Ref_tipo_proyecto %>%
  rename(tipo_proyecto_id="id") %>%
  rename(tipo_proyecto_descripcion="descripcion")

Ref_moneda_limpia <- Ref_moneda %>%
  select(moneda_id,
         moneda_desc)

Ref_funcion_limpia <- Ref_funcion %>%
  rename(funcion_descripcion="funcion_desc")

Proyecto_participante_dir <- Proyecto_participante %>%     #la calificacion de directores nos sirve para agregar el director a lista_nodos
  left_join(Ref_funcion_limpia,
            by="funcion_id") %>%
  filter(funcion_id==1) %>%
  rename(Director="persona")

Proyecto_participante_broad <- Proyecto_participante_broad %>%    #este codigo nos crea una tabla del personas mas amplio que trabaja en los proyectos
  left_join(Ref_funcion_limpia,
            by="funcion_id")


#juntamos las tablas de proyectos con sus tablas de referencias (ej:59101)

Proyecto_participante_dir_joined <- Proyecto_participante_dir %>%                          #codigo para incorporar info de directores de proyectos a lista_nodos
  left_join(personas_limpia,                             #lo conectamos a personas_limpia porque es mas completa que joined y solo queremos nombre y apellido
            by="persona_id") %>%
  group_by(proyecto_id) %>%
  summarise_all(~paste(., collapse="; ")) %>%
  select(proyecto_id,
         persona_id,
         Director,
         funcion_id,
         fecha_inicio,
         fecha_fin) %>%
  mutate(Director=trimws(Director, which=c("left"), whitespace=";"))


#joineamos los proyectos con la informacion de disciplinas

Proyecto_disciplina_joined<- Proyecto_disciplina %>%
  left_join(Ref_disciplina, by="disciplina_id") %>%
  select(proyecto_id,
         gran_area_descripcion,
         area_descripcion,
         disciplina_descripcion) %>%
  group_by(proyecto_id) %>%
  summarise_all(~paste(., collapse = " ; "))


#armaremos una tabla donde se agrupen los financiadores y ejecutores para cada proyecto

organizacion_id_c <-Proyecto_beneficiario_limpia$organizacion_id                                 # lista de valores que reemplazan a los anteriores
col_repl <- c("organizacion_financiadora","organizacion_ejecutora")                              # indica columnas a modificar


Replacement_function <- function(x) {
  for (i in 1:length(col_repl)) {
    indices_Y <- which(x == "Y")
    indices_N <- which(x == "N")
    x[indices_Y] <- organizacion_id_c[indices_Y]
    x[indices_N] <- ""                                                                     #donde hay "N" reemplazamos por espacios vacios, esto es, para que cuando colapsemos las observaciones, solo tome en cuenta los que eran "Y"
  }
  return(x)
}

Proyecto_beneficiario_limpia[col_repl] <- sapply(Proyecto_beneficiario_limpia[col_repl],
                                                 Replacement_function)

#armamos la tabla final

Proyecto_beneficiario_limpia_TOTAL <- Proyectos_TOTAL %>%
  left_join(Proyecto_beneficiario_limpia,
            by="proyecto_id") %>%
  group_by(proyecto_id) %>%
  summarise_all(~paste(., collapse = ";")) %>%
  select(proyecto_id,
         organizacion_id,
         organizacion_financiadora,
         organizacion_ejecutora,
         porcentaje_financiamiento) %>%
  mutate(organizacion_ejecutora=trimws(organizacion_ejecutora, which=c("left"), whitespace=";")) %>%
  mutate(organizacion_ejecutora=trimws(organizacion_ejecutora, which=c("right"), whitespace=";")) %>%
  mutate(organizacion_financiadora=trimws(organizacion_financiadora, which=c("left"), whitespace=";")) %>%
  mutate(organizacion_financiadora=trimws(organizacion_financiadora, which=c("right"), whitespace=";")) %>%
  mutate(organizacion_id=trimws(organizacion_id, which=c("left"), whitespace=";")) %>%
  mutate(organizacion_id=trimws(organizacion_id, which=c("right"), whitespace=";"))


#juntamos las tablas de organizaciones con sus caracteristicas en organizaciones_limpia

organizaciones_lugartrabajo_previo <- Proyecto_participante_broad %>%
  left_join(personas_joined,
            by="persona_id") %>%
  filter(!is.na(institucion_trabajo_id)) %>%           #eliminamos NA porque refiere a los proyectos que no estan en proyecto_participante y, por lo tanto, no tienen valores en institucion_trabajo_id
  rename(organizacion_id=institucion_trabajo_id) %>%
  filter(!fecha_fin<=anio)                             #esto elimina todos los lugares de trabajo que tengan personas con fecha de finalizacion de trabajo en un proyecto anterior a la fecha de inicio en tal lugar de trabajo (hay que eliminarlo porque es incorrecto conectar al proyecto viejo con la nueva institucion)


organizaciones_lugartrabajo <- organizaciones_lugartrabajo_previo %>%
  left_join(organizaciones_limpia,
            by="organizacion_id") %>%
  select(organizacion_id,
         institucion_nivel1_desc,
         institucion_nivel2_desc,
         institucion_nivel3_desc,
         institucion_nivel4_desc) %>%
  filter(!duplicated(.))

#armamos nuestra tabla de proyectos de 2011



# Aca, ver de cambiar fecha incicio y fin por iniciacion y finalizacion. 
# Fin y inicio es de la participacion de los participantes en proyectos y finalizacion y iniciacion es sobre las fechas de los proyectos

Proyectos_total_joined <- Proyectos_TOTAL%>%
  left_join(Ref_moneda, by="moneda_id") %>%
  left_join(Proyecto_disciplina_joined, by="proyecto_id") %>%
  left_join(Ref_tipo_proyecto_limpia, by="tipo_proyecto_id") %>%
  left_join(Proyecto_beneficiario_limpia_TOTAL, by="proyecto_id") %>%
  left_join(Proyecto_participante_dir_joined, by="proyecto_id") %>%
  select(proyecto_id,
         fondo_anpcyt,
         proyecto_fuente,
         fecha_inicio,
         fecha_fin,
         Director,
         titulo,
         gran_area_descripcion,
         area_descripcion,
         disciplina_descripcion,
         tipo_proyecto_descripcion,
         tipo_proyecto_cyt_desc,
         moneda_desc,
         monto_total_solicitado,
         monto_total_adjudicado,
         organizacion_id,
         organizacion_financiadora,
         organizacion_ejecutora,
         porcentaje_financiamiento
  ) %>%
  mutate(Director=trimws(Director, which=c("left"), whitespace="NA,")) %>%      #Existen personas que pone el nombre y apellido en la casilla del nombre, para ello, removeremos los missing values
  rename(organizaciones="organizacion_id") %>%
  filter(!duplicated(.))

#Teniendo institucion_trabajo_id en la informacion de proyectos_total_joined, esta bueno saber cuantos proyectos elimino si elimino los que tengan
#NA en institucion_trabajo_id. Estos no nos sirven para el analisis, pero es cierto que si es mucha cantidad de proyectos, entonces los
#datos pierden mucha calidad. PASO de 15162 a 12738.

#¿Por que pierdo nodos al eliminar NA en institucion_trabajo_id si ya antes habia eliminado los proyecto_total que no estaban en proyecto_participante?
#no se, pero eliminarlos me saca como 1500 soletones de la bipartita, lo cual es bueno.
#debe ser por los directores que no tienen institucion_trabajo por no estar en personas_total
#de todas formas me parece una mala forma de fijarme ya que puede que no esten conectados por el director pero is por los colaboradores

#Proyectos_total_joined <- Proyectos_total_joined %>%
#  filter(!institucion_trabajo_id=="NA") %>%
#  filter(!proyecto_id=="16792")  #este proyecto_id tiene en vez de NA, el valor NA;NA, entonces el comando anterior no me lo elimina


#armamos las listas de nodos y conexiones

lista_nodos <- full_join(
  Proyectos_total_joined %>% mutate(id=paste0("pro-", proyecto_id)),
  organizaciones_lugartrabajo %>% mutate(id=paste0("org-", organizacion_id)),
  by="id") %>%
  mutate(type = ifelse(str_detect(id, "pro"), TRUE, FALSE))%>%
  select(id, everything(), -proyecto_id, -organizacion_id)

#los proyectos que estan en en proyectos_total. Esto es en el contexto de que nos interesan las conexiones con proyectos de
#proyectos_total porque son los proyectos de los cuales tenemos informacion

lista_conexiones_broad <-conexiones_broad %>%
  ungroup() %>%
  mutate(idpro=paste0("pro-", proyecto_id)) %>%
  mutate(idorg=paste0("org-", institucion_trabajo_id)) %>%
  select(idpro, idorg)


vertices_to_keep <- lista_nodos %>%
  select(id)

lista_conexiones_broad <- lista_conexiones_broad[lista_conexiones_broad$idpro %in% vertices_to_keep$id & lista_conexiones_broad$idorg %in% vertices_to_keep$id,]
