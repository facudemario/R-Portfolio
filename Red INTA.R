#algo importante a tener en cuenta de estas redes es que como resumen todos los años (2008-2019), no estan teniendo en cuenta la temporalidad
#del cambio de lugar de trabajo de los integrantes año a año


lista_nodos_INTA <- full_join( 
  Proyectos_total_joined %>% mutate(id=paste0("pro-", proyecto_id)),
  organizaciones_lugartrabajo %>% mutate(id=paste0("org-", organizacion_id)),
  by="id") %>% 
  mutate(type = ifelse(str_detect(id, "pro"), TRUE, FALSE)) %>% 
  filter(proyecto_fuente=="INTA" | is.na(proyecto_fuente)) %>% 
  select(id, everything(), -proyecto_id, -organizacion_id)  


lista_conexiones_lugardetrabajo_broad_INTA <- left_join(
  Proyectos_total_joined %>% mutate(idpro=paste0("pro-", proyecto_id)),
  conexiones_broad %>%  mutate(idorg=paste0("org-", institucion_trabajo_id)),
  by="proyecto_id") %>% 
  select(idpro, idorg) %>% 
  filter(!is.na(idorg))

vertices_to_keep_INTA <- lista_nodos_INTA %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% vertices_to_keep_INTA$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% vertices_to_keep_INTA$id,]

#armamos la red

red_colaboracion_bipartita_INTA <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA, 
                                                           vertices = lista_nodos_INTA, directed = FALSE)

red_colaboracion_bipartita_INTA<- delete_vertices(red_colaboracion_bipartita_INTA, which(degree(red_colaboracion_bipartita_INTA) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(red_colaboracion_bipartita_INTA,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)

grados <- degree(red_colaboracion_bipartita_INTA)
nodos_aislados <- which(grados == 0)


red_colaboracion_unipartita_INTA <- bipartite_projection(
  red_colaboracion_bipartita_INTA,
  which = "FALSE") 

V(red_colaboracion_unipartita_INTA)$name   # Esto verifica que los nodos son efectivamente las organizaciones

vcount(red_colaboracion_unipartita_INTA) 
ecount(red_colaboracion_unipartita_INTA)

plot(red_colaboracion_unipartita_INTA,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)


grados <- degree(red_colaboracion_unipartita_INTA)
nodos_aislados <- which(grados == 0)


######################################## Ahora, año a año ################################################################################
par(mfrow=c(4,3), mar=c(1,1,1,1))

#2008##########################
INTA_2008 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2008"|is.na(fecha_inicio))

VTK <- INTA_2008 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA1 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2008_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA1, 
                                            vertices = INTA_2008, directed = FALSE)

RED_INTA_2008_bi<- delete_vertices(RED_INTA_2008_bi, which(degree(RED_INTA_2008_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2008_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)

#2009#########################
INTA_2009 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2009"|is.na(fecha_inicio))

VTK <- INTA_2009 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA2 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2009_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA2, 
                                            vertices = INTA_2009, directed = FALSE)

RED_INTA_2009_bi<- delete_vertices(RED_INTA_2009_bi, which(degree(RED_INTA_2009_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

grados <- degree(RED_INTA_2009_bi)
nodos_aislados <- which(grados == 0)


plot(RED_INTA_2009_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2010########################
INTA_2010 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2010"|is.na(fecha_inicio))

VTK <- INTA_2010 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA3 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2010_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA3, 
                                            vertices = INTA_2010, directed = FALSE)

RED_INTA_2010_bi<- delete_vertices(RED_INTA_2010_bi, which(degree(RED_INTA_2010_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2010_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2011#########################
INTA_2011 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2011"|is.na(fecha_inicio))

VTK <- INTA_2011 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA4 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2011_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA4, 
                                            vertices = INTA_2011, directed = FALSE)

RED_INTA_2011_bi<- delete_vertices(RED_INTA_2011_bi, which(degree(RED_INTA_2011_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2011_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2012########################
INTA_2012 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2012"|is.na(fecha_inicio))

VTK <- INTA_2012 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA5 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2012_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA5, 
                                            vertices = INTA_2012, directed = FALSE)

RED_INTA_2012_bi<- delete_vertices(RED_INTA_2012_bi, which(degree(RED_INTA_2012_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2012_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2013#######################
INTA_2013 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2013"|is.na(fecha_inicio))

VTK <- INTA_2013 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA6 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2013_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA6, 
                                            vertices = INTA_2013, directed = FALSE)

RED_INTA_2013_bi<- delete_vertices(RED_INTA_2013_bi, which(degree(RED_INTA_2013_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2013_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2014#########################
INTA_2014 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2014"|is.na(fecha_inicio))

VTK <- INTA_2014 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA7 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2014_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA7, 
                                            vertices = INTA_2014, directed = FALSE)

RED_INTA_2014_bi<- delete_vertices(RED_INTA_2014_bi, which(degree(RED_INTA_2014_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2014_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2015########################
INTA_2015 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2015"|is.na(fecha_inicio))

VTK <- INTA_2015 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA8 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2015_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA8, 
                                            vertices = INTA_2015, directed = FALSE)

RED_INTA_2015_bi<- delete_vertices(RED_INTA_2015_bi, which(degree(RED_INTA_2015_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2015_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2016########################
INTA_2016 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2016"|is.na(fecha_inicio))

VTK <- INTA_2016 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA9 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                              & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2016_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA9, 
                                            vertices = INTA_2016, directed = FALSE)

RED_INTA_2016_bi<- delete_vertices(RED_INTA_2016_bi, which(degree(RED_INTA_2016_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2016_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2017########################
INTA_2017 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2017"|is.na(fecha_inicio))

VTK <- INTA_2017 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA10 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                               & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2017_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA10, 
                                            vertices = INTA_2017, directed = FALSE)

RED_INTA_2017_bi<- delete_vertices(RED_INTA_2017_bi, which(degree(RED_INTA_2017_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2017_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2018#######################
INTA_2018 <-lista_nodos_INTA %>% 
  filter(fecha_inicio=="2018"|is.na(fecha_inicio))

VTK <- INTA_2018 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_INTA11 <- lista_conexiones_lugardetrabajo_broad_INTA[lista_conexiones_lugardetrabajo_broad_INTA$idpro %in% VTK$id 
                                                                                               & lista_conexiones_lugardetrabajo_broad_INTA$idorg %in% VTK$id,]

RED_INTA_2018_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_INTA11, 
                                            vertices = INTA_2018, directed = FALSE)

RED_INTA_2018_bi<- delete_vertices(RED_INTA_2018_bi, which(degree(RED_INTA_2018_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente INTA, los eliminamos

plot(RED_INTA_2018_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
