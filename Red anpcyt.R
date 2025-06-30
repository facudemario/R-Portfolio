#algo importante a tener en cuenta de estas redes es que como resumen todos los años (2008-2019), no estan teniendo en cuenta la temporalidad
#del cambio de lugar de trabajo de los integrantes año a año


lista_nodos_ANPCYT <- full_join( 
  Proyectos_total_joined %>% mutate(id=paste0("pro-", proyecto_id)),
  organizaciones_lugartrabajo %>% mutate(id=paste0("org-", organizacion_id)),
  by="id") %>% 
  mutate(type = ifelse(str_detect(id, "pro"), TRUE, FALSE)) %>% 
  filter(proyecto_fuente=="ANPCYT" | is.na(proyecto_fuente)) %>% 
  select(id, everything(), -proyecto_id, -organizacion_id)  


lista_conexiones_lugardetrabajo_broad_ANPCYT <- left_join(
  Proyectos_total_joined %>% mutate(idpro=paste0("pro-", proyecto_id)),
  conexiones_broad %>%  mutate(idorg=paste0("org-", institucion_trabajo_id)),
  by="proyecto_id") %>% 
  select(idpro, idorg) %>% 
  filter(!is.na(idorg))

vertices_to_keep_ANPCYT <- lista_nodos_ANPCYT %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% vertices_to_keep_ANPCYT$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% vertices_to_keep_ANPCYT$id,]

#armamos la red

red_colaboracion_bipartita_ANPCYT <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT, 
                                                           vertices = lista_nodos_ANPCYT, directed = FALSE)

red_colaboracion_bipartita_ANPCYT<- delete_vertices(red_colaboracion_bipartita_ANPCYT, which(degree(red_colaboracion_bipartita_ANPCYT) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

#plot(red_colaboracion_bipartita_ANPCYT,
 #    vertex.size=0.0000001,
  #   edge_width=1000,
   #  vertex.label=NA)

grados <- degree(red_colaboracion_bipartita_ANPCYT)
nodos_aislados <- which(grados == 0)


red_colaboracion_unipartita_ANPCYT <- bipartite_projection(
  red_colaboracion_bipartita_ANPCYT,
  which = "FALSE") 

V(red_colaboracion_unipartita_ANPCYT)$name   # Esto verifica que los nodos son efectivamente las organizaciones

vcount(red_colaboracion_unipartita_ANPCYT) 
ecount(red_colaboracion_unipartita_ANPCYT)

#plot(red_colaboracion_unipartita_ANPCYT,
 #    vertex.size=0.0000001,
  #   edge_width=1000,
   #  vertex.label=NA)


grados <- degree(red_colaboracion_unipartita_ANPCYT)
nodos_aislados <- which(grados == 0)


######################################## Ahora, año a año ################################################################################
par(mfrow=c(4,3), mar=c(1,1,1,1))

#2008##########################
ANPCYT_2008 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2008"|is.na(fecha_inicio))

VTK <- ANPCYT_2008 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT1 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2008_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT1, 
                                                           vertices = ANPCYT_2008, directed = FALSE)

RED_ANPCYT_2008_bi<- delete_vertices(RED_ANPCYT_2008_bi, which(degree(RED_ANPCYT_2008_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2008_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)

#2009#########################
ANPCYT_2009 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2009"|is.na(fecha_inicio))

VTK <- ANPCYT_2009 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT2 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2009_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT2, 
                                            vertices = ANPCYT_2009, directed = FALSE)

RED_ANPCYT_2009_bi<- delete_vertices(RED_ANPCYT_2009_bi, which(degree(RED_ANPCYT_2009_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

grados <- degree(RED_ANPCYT_2009_bi)
nodos_aislados <- which(grados == 0)


plot(RED_ANPCYT_2009_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2010########################
ANPCYT_2010 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2010"|is.na(fecha_inicio))

VTK <- ANPCYT_2010 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT3 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2010_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT3, 
                                            vertices = ANPCYT_2010, directed = FALSE)

RED_ANPCYT_2010_bi<- delete_vertices(RED_ANPCYT_2010_bi, which(degree(RED_ANPCYT_2010_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2010_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2011#########################
ANPCYT_2011 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2011"|is.na(fecha_inicio))

VTK <- ANPCYT_2011 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT4 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2011_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT4, 
                                            vertices = ANPCYT_2011, directed = FALSE)

RED_ANPCYT_2011_bi<- delete_vertices(RED_ANPCYT_2011_bi, which(degree(RED_ANPCYT_2011_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2011_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2012########################
ANPCYT_2012 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2012"|is.na(fecha_inicio))

VTK <- ANPCYT_2012 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT5 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2012_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT5, 
                                            vertices = ANPCYT_2012, directed = FALSE)

RED_ANPCYT_2012_bi<- delete_vertices(RED_ANPCYT_2012_bi, which(degree(RED_ANPCYT_2012_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2012_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2013#######################
ANPCYT_2013 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2013"|is.na(fecha_inicio))

VTK <- ANPCYT_2013 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT6 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2013_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT6, 
                                            vertices = ANPCYT_2013, directed = FALSE)

RED_ANPCYT_2013_bi<- delete_vertices(RED_ANPCYT_2013_bi, which(degree(RED_ANPCYT_2013_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2013_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2014#########################
ANPCYT_2014 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2014"|is.na(fecha_inicio))

VTK <- ANPCYT_2014 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT7 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2014_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT7, 
                                            vertices = ANPCYT_2014, directed = FALSE)

RED_ANPCYT_2014_bi<- delete_vertices(RED_ANPCYT_2014_bi, which(degree(RED_ANPCYT_2014_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2014_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2015########################
ANPCYT_2015 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2015"|is.na(fecha_inicio))

VTK <- ANPCYT_2015 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT8 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2015_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT8, 
                                            vertices = ANPCYT_2015, directed = FALSE)

RED_ANPCYT_2015_bi<- delete_vertices(RED_ANPCYT_2015_bi, which(degree(RED_ANPCYT_2015_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2015_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2016########################
ANPCYT_2016 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2016"|is.na(fecha_inicio))

VTK <- ANPCYT_2016 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT9 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2016_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT9, 
                                            vertices = ANPCYT_2016, directed = FALSE)

RED_ANPCYT_2016_bi<- delete_vertices(RED_ANPCYT_2016_bi, which(degree(RED_ANPCYT_2016_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2016_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2017########################
ANPCYT_2017 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2017"|is.na(fecha_inicio))

VTK <- ANPCYT_2017 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT10 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2017_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT10, 
                                            vertices = ANPCYT_2017, directed = FALSE)

RED_ANPCYT_2017_bi<- delete_vertices(RED_ANPCYT_2017_bi, which(degree(RED_ANPCYT_2017_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2017_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)
#2018#######################
ANPCYT_2018 <-lista_nodos_ANPCYT %>% 
  filter(fecha_inicio=="2018"|is.na(fecha_inicio))

VTK <- ANPCYT_2018 %>% 
  select(id)

lista_conexiones_lugardetrabajo_broad_ANPCYT11 <- lista_conexiones_lugardetrabajo_broad_ANPCYT[lista_conexiones_lugardetrabajo_broad_ANPCYT$idpro %in% VTK$id 
                                                                                             & lista_conexiones_lugardetrabajo_broad_ANPCYT$idorg %in% VTK$id,]

RED_ANPCYT_2018_bi <- graph_from_data_frame(d = lista_conexiones_lugardetrabajo_broad_ANPCYT11, 
                                            vertices = ANPCYT_2018, directed = FALSE)

RED_ANPCYT_2018_bi<- delete_vertices(RED_ANPCYT_2018_bi, which(degree(RED_ANPCYT_2018_bi) == 0))  #dado que hay organizaciones en broad que no nos sirven porque no estan conectadas a proyectos con proyecto_fuente anpcyt, los eliminamos

plot(RED_ANPCYT_2018_bi,
     vertex.size=0.0000001,
     edge_width=1000,
     vertex.label=NA)

##################################### STATISTICS #########################################################################





