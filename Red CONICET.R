#algo importante a tener en cuenta de estas redes es que como resumen todos los años (2008-2019), no estan teniendo en cuenta la temporalidad
#del cambio de lugar de trabajo de los integrantes año a año


lista_nodos_CONICET <- full_join( 
  Proyectos_total_joined %>% mutate(id=paste0("pro-", proyecto_id)),
  organizaciones_lugartrabajo %>% mutate(id=paste0("org-", organizacion_id)),
  by="id") %>% 
  mutate(type = ifelse(str_detect(id, "pro"), TRUE, FALSE)) %>% 
  filter(proyecto_fuente=="CONICET" | is.na(proyecto_fuente)) %>% 
  select(id, everything(), -proyecto_id, -organizacion_id)  


lista_conexiones_lugardetrabajo_broad_CONICET <- left_join(
  Proyectos_total_joined %>% mutate(idpro=paste0("pro-", proyecto_id)),
  conexiones_broad %>%  mutate(idorg=paste0("org-", institucion_trabajo_id)),
  by="proyecto_id") %>% 
  select(idpro, idorg) %>% 
  filter(!is.na(idorg))

vertices_to_keep_CONICET <- lista_nodos_CONICET %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% vertices_to_keep_CONICET$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% vertices_to_keep_CONICET$id,]

#armamos la red

red_colaboracion_bipartita_CONICET <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET, 
                                                           vertices = lista_nodos_CONICET, directed = FALSE)

red_colaboracion_bipartita_CONICET<- delete_vertices(red_colaboracion_bipartita_CONICET, which(degree(red_colaboracion_bipartita_CONICET) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

#plot(red_colaboracion_bipartita_CONICET,
 #    vertex.size=0.0000001,
  #   edge_width=1000,
   #  vertex.label=NA)

grados <- degree(red_colaboracion_bipartita_CONICET)
nodos_aislados <- which(grados == 0)


red_colaboracion_unipartita_CONICET <- bipartite_projection(
  red_colaboracion_bipartita_CONICET,
  which = "FALSE") 

V(red_colaboracion_unipartita_CONICET)$name   # Esto verifica que los nodos son efectivamente las organizaciones

vcount(red_colaboracion_unipartita_CONICET) 
ecount(red_colaboracion_unipartita_CONICET)

#plot(red_colaboracion_unipartita_CONICET,
 #    vertex.size=0.0000001,
  #   edge_width=1000,
   #  vertex.label=NA)


grados <- degree(red_colaboracion_unipartita_CONICET)
nodos_aislados <- which(grados == 0)


#cuales son estos soletones? Estaria bueno armar una lista de organizaciones soletonas

grados_CONICET <- degree(red_colaboracion_unipartita_CONICET)

degree_CONICET <- grados_CONICET[order(-grados_CONICET)]

Conectividad_general_CONICET <- as.data.frame(degree_CONICET)

Conectividad_general_CONICET <- rownames_to_column(Conectividad_general_CONICET, var = "organizacion_id") %>% 
  mutate(organizacion_id = sub("org-", "", organizacion_id)) %>% 
  mutate(organizacion_id=as.double(organizacion_id)) %>% 
  left_join(organizaciones_limpia,
            by="organizacion_id") 

#Esta tabla toma como organizaciones distintas a cada suborganizacion de una organizacin
#Tomemos solamente el nivel 1 para analizar a nivel agregado

Conectividad_nivel_1_CONICET <- Conectividad_general_CONICET %>% 
  select(degree_CONICET, institucion_nivel1_desc) %>%
  group_by(institucion_nivel1_desc) %>% 
  summarise(degree=sum(degree))

#ME DA TODO DEGREE 97412, lo cual obviamente esta mal.
#recordar a futuro que el objetivo de toda esta parte del analisis es conocer cuales son los nodos
#centrales de las distitas redes. Nos puede llegar a aportar informacion valiosa.

#MAÑANA MARTES HACER EL INFORME QUE ME PIDEN DESDE LA SECRETARIA DE INVESTIGACION.



######################################## Ahora, año a año ################################################################################
par(mfrow=c(4,3), mar=c(1,1,1,1))

#2008##########################
CONICET_2008 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2008"|is.na(fecha_inicio))

VTK <- CONICET_2008 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET1 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2008_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET1, 
                                            vertices = CONICET_2008, directed = FALSE)

RED_CONICET_2008_bi<- delete_vertices(RED_CONICET_2008_bi, which(degree(RED_CONICET_2008_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2008_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)

#2009#########################
CONICET_2009 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2009"|is.na(fecha_inicio))

VTK <- CONICET_2009 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET2 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2009_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET2, 
                                            vertices = CONICET_2009, directed = FALSE)

RED_CONICET_2009_bi<- delete_vertices(RED_CONICET_2009_bi, which(degree(RED_CONICET_2009_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

grados <- degree(RED_CONICET_2009_bi)
nodos_aislados <- which(grados == 0)


plot(RED_CONICET_2009_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2010########################
CONICET_2010 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2010"|is.na(fecha_inicio))

VTK <- CONICET_2010 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET3 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2010_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET3, 
                                            vertices = CONICET_2010, directed = FALSE)

RED_CONICET_2010_bi<- delete_vertices(RED_CONICET_2010_bi, which(degree(RED_CONICET_2010_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2010_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2011#########################
CONICET_2011 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2011"|is.na(fecha_inicio))

VTK <- CONICET_2011 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET4 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2011_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET4, 
                                            vertices = CONICET_2011, directed = FALSE)

RED_CONICET_2011_bi<- delete_vertices(RED_CONICET_2011_bi, which(degree(RED_CONICET_2011_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2011_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2012########################
CONICET_2012 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2012"|is.na(fecha_inicio))

VTK <- CONICET_2012 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET5 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2012_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET5, 
                                            vertices = CONICET_2012, directed = FALSE)

RED_CONICET_2012_bi<- delete_vertices(RED_CONICET_2012_bi, which(degree(RED_CONICET_2012_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2012_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2013#######################
CONICET_2013 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2013"|is.na(fecha_inicio))

VTK <- CONICET_2013 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET6 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2013_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET6, 
                                            vertices = CONICET_2013, directed = FALSE)

RED_CONICET_2013_bi<- delete_vertices(RED_CONICET_2013_bi, which(degree(RED_CONICET_2013_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2013_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2014#########################
CONICET_2014 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2014"|is.na(fecha_inicio))

VTK <- CONICET_2014 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET7 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2014_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET7, 
                                            vertices = CONICET_2014, directed = FALSE)

RED_CONICET_2014_bi<- delete_vertices(RED_CONICET_2014_bi, which(degree(RED_CONICET_2014_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2014_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2015########################
CONICET_2015 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2015"|is.na(fecha_inicio))

VTK <- CONICET_2015 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET8 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2015_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET8, 
                                            vertices = CONICET_2015, directed = FALSE)

RED_CONICET_2015_bi<- delete_vertices(RED_CONICET_2015_bi, which(degree(RED_CONICET_2015_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2015_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2016########################
CONICET_2016 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2016"|is.na(fecha_inicio))

VTK <- CONICET_2016 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET9 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2016_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET9, 
                                            vertices = CONICET_2016, directed = FALSE)

RED_CONICET_2016_bi<- delete_vertices(RED_CONICET_2016_bi, which(degree(RED_CONICET_2016_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2016_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2017########################
CONICET_2017 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2017"|is.na(fecha_inicio))

VTK <- CONICET_2017 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET10 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                               & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2017_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET10, 
                                            vertices = CONICET_2017, directed = FALSE)

RED_CONICET_2017_bi<- delete_vertices(RED_CONICET_2017_bi, which(degree(RED_CONICET_2017_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2017_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2018#######################
CONICET_2018 <-lista_nodos_CONICET %>% 
  filter(fecha_inicio=="2018"|is.na(fecha_inicio))

VTK <- CONICET_2018 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_CONICET11 <- lista_conexiones_lugardetrabajo_broad_CONICET[lista_conexiones_lugardetrabajo_broad_CONICET$idpro %in% VTK$id 
                                                                                               & lista_conexiones_lugardetrabajo_broad_CONICET$idorg %in% VTK$id,]

RED_CONICET_2018_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_CONICET11, 
                                            vertices = CONICET_2018, directed = FALSE)

RED_CONICET_2018_bi<- delete_vertices(RED_CONICET_2018_bi, which(degree(RED_CONICET_2018_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente CONICET, los eliminamos

plot(RED_CONICET_2018_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
