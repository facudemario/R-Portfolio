################### La Red en general ##############################

red_colaboracion_bipartita <- graph_from_data_frame(d = lista_conexiones_broad, 
                                                    vertices = lista_nodos, directed = FALSE)

grados <- degree(red_colaboracion_bipartita)
nodos_aislados <- which(grados == 0)              #en la bipartita es lo correcto que no haya soletonas, ya que implica que hay organizaciones que no estan ejecutando ningun proyecto y que hay proyectos que no estan siendo ejecutados por ninguna organizacion

vcount(red_colaboracion_bipartita) 
ecount(red_colaboracion_bipartita)

#plot(red_colaboracion_bipartita,
 #    vertex.size=0.0000001,
  #   edge_width=1000,
   #  vertex.label=NA)


#proyeccion unipartita

red_colaboracion_unipartita <- bipartite_projection(
  red_colaboracion_bipartita,
  which = "FALSE")                  # esto proyecta Organizaciones como nodos y proyectos como conexiones


vcount(red_colaboracion_unipartita) 
ecount(red_colaboracion_unipartita)

#plot(red_colaboracion_unipartita,
  #   vertex.size=0.0000001,
   #  edge_width=1000,
    # vertex.label=NA)


grados <- degree(red_colaboracion_unipartita)
nodos_aislados <- which(grados == 0)



#cuales son estos soletones? Estaria bueno armar una lista de organizaciones soletonas

grados <- degree(red_colaboracion_unipartita)

degree <- grados[order(-grados)]

Conectividad_general <- as.data.frame(degree)

Conectividad_general <- rownames_to_column(Conectividad_general, var = "organizacion_id") %>% 
  mutate(organizacion_id = sub("org-", "", organizacion_id)) %>% 
  mutate(organizacion_id=as.double(organizacion_id)) %>% 
  left_join(organizaciones_limpia,
            by="organizacion_id") 

#Esta tabla toma como organizaciones distintas a cada suborganizacion de una organizacin
#Tomemos solamente el nivel 1 para analizar a nivel agregado

Conectividad_nivel_1 <- Conectividad_general %>% 
  select(degree, institucion_nivel1_desc) %>%
  group_by(institucion_nivel1_desc) %>% 
  summarise(degree=sum(degree))

#vizualicemos esto en un histograma

resumen_degree <- Conectividad_nivel_1[order(-Conectividad_nivel_1$degree), ] #esto, para ordenar de mayor a menor las instituciones por valores en degree

ggplot(Conectividad_nivel_1, 
       aes(x = factor(institucion_nivel1_desc, levels = resumen_degree$institucion_nivel1_desc), y=degree)) + 
  geom_point() +
  scale_x_discrete(labels = resumen_degree$institucion_nivel1_desc)



help(hist)

hist(Conectividad_general)



























