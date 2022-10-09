{source("parser.R")
objects <- parse()
arrival <- objects[[1]]
nodes <- objects[[2]]
ids <- objects[[3]]

#RECIBE EL NUMERO DE EXITS, LISTA DE IDS, EL NODO DEL QUE SALDRAN CLIENTES Y UN MAPA CON
simulateExits<-function(exits,ids,node,update){#CLAVE ID, VALOR NUMERO DE ENTRADAS
  if (exits >= 1) {
    rands <- runif(exits, 0, 1)#CREA UN RANDOM PARA SIMULAR A QUE NODO IRIA
    
    if (!is.null(update) && !is.na(update["Exit"])) {#
      update["Exit"]<-update["Exit"]+exits
    }
    else{
      update["Exit"]<-exits
    }
    if(is.null(node$T)){
      return(update)
    }
    #numero de salidas del sistema
    for (i in 1:exits) {
      prev = 0
      for (ID in ids) {#MIRA ENTRE QUE RANGO DE POSIBILIDADES HA CAIDO
        if (!is.na(node$T[ID])) {#SI PUEDE TRANSICIONAR A ESTE NODO
          if (rands[i] <= (node$T[ID] + prev)) {#SI EL RANDOM HA CAIDO EN EL RANGO
            #EL NUMERO DE CLIENTES QUE VAN A ID += 1
            if (!is.null(update) && !is.na(update[ID])) {
              update[ID] <- update[ID] + 1
            }
            else{
              update[ID] <- 1
            }
            update["Exit"]<-update["Exit"]-1
            break
          } else{
            prev <- prev + node$T[ID]
          }
        }
      }
    }
  }
  return(update)
}


sink("RESULTS2.txt")
cat()
sink(NULL)
entries<-0
it<-0
impresiones<-0
while(TRUE) {
  it<-it+1
  update <- names(c())#UPDATE ES UN MAP VACIO
  #SIMULAMOS LAS LLEGADAS AL SISTEMA DESDE FUERA
  new_entries<- rpois(1,arrival$lambda)
  entries<-entries +new_entries
  update<-simulateExits(new_entries,ids,arrival,update)
  m = 1
  for (node in nodes) {#SIMULAMOS LAS SALIDAS DE CADA NODO
    exits <- sum(rpois(node$c, node$mu))
    #SI HAY MENOS SALIDAS QUE CLIENTES EN EL SISTEMA LOS SERVIDORES ESTÁN SIEMPRE LLENOS
    if(exits<=node$Clients){
      node$Exits<-node$Exits + exits
      node$BruteBenefit <- node$BruteBenefit + (exits * node$Rev)
      node$BruteCost <-(node$BruteCost + (node$Clients-exits) * node$QCost + node$c*node$SCost)
      
      node$L<-node$L+node$Clients-exits/2#L es el sumatorio de la media de clientes por it
    }
    else{#SI LAS SALIDAS SUPERAN A LOS CLIENTES ESTUVO ALGUN SERVIDOR VACIO, COLA VACIA
      node$Exits<-node$Exits + node$Clients
      node$BruteBenefit <- node$BruteBenefit + (node$Clients * node$Rev)
      #HAY C SERVIDORES TRABAJANDO EL (CLIENTS/EXITS) DEL TIEMPO
      node$BruteCost <-(node$BruteCost + node$c*(node$Clients/exits)*node$SCost)
      node$L<-node$L+node$Clients/2
    }
    node$BruteCost<-node$BruteCost+node$ICost#Sumamos el coste de mantener la infraestructura
    
    if(exits==0){#W,Wq es el sumatorio del número de clientes/numero de salidas
      node$Wq<-node$Wq+max(0,(node$Clients-node$c))*2#"castigamos" en caso de que no salga nadie
      node$W<-node$W+node$Clients*2
    }
    else{
      node$Wq<-node$Wq+max(0,(node$Clients-node$c))/exits
      node$W<-node$W+node$Clients/exits#L,Lq,W,Wq los dividiremos entre el numero de iteraciones
    }
    exits<-min(node$Clients,exits)#no puede haber mas exits reales que clientes
    node$Clients <- node$Clients - exits #ACTUALIZAMOS EL NUMERO DE CLIENTES
    
    node$Lq<-node$Lq+node$Clients#Lq es el sumatorio de los que se quedan
    update<-simulateExits(exits,ids,node,update) # SIMULAMOS LAS SALIDAS
    nodes[[m]] <- node# ACTUALIZAMOS EL NODO
    m <- m + 1
  }
  if(entries>=200){
    sink("RESULTS2.txt",append=TRUE)
    cat("Datos obtenidos entre la llegada del cliente ", impresiones, "y el ", impresiones+entries, "\n")
    sink(NULL)
    impresiones<-impresiones+entries
    entries<-0
    {#inicializamos las variables globales a 0
      L=0
      Lq=0
      W=0
      Wq=0
      Benefit=0
      Exits=arrival$Exits#numero de salidas del sistema
      arrival$Exits<-0
      Rejected=0
      for (i in 1:length(nodes)){#sumamos a las variables generales e imprimimos las del nodo
        node<-nodes[[i]]
        L<- L + node$L
        Lq<- Lq + node$Lq
        W<- W + node$W*node$Exits
        Wq<- Wq + node$Wq*node$Exits
        Rejected<- Rejected + node$Rejected
        Benefit<-Benefit + node$BruteBenefit - node$BruteCost
        sink("RESULTS2.txt",append=TRUE)
        cat("##########################################","\n")
        cat("NODO ", node$id,"\n")
        cat("L: ", node$L/it,"\n")
        cat("Lq: ", node$Lq/it,"\n")
        cat("W: ", node$W/it,"\n")
        cat("Wq ", node$Wq/it,"\n")
        cat("Clientes servidos: ", node$Exits,"\n")
        cat("CLientes rechazados: ", node$Rejected,"\n")
        cat("Beneficio: ", node$BruteBenefit - node$BruteCost,"\n")
        sink(NULL) #restauramos a 0 todos los valores
        node$L<-0
        node$Lq<-0
        node$W<-0
        node$Wq<-0
        node$Exits<-0
        node$Rejected<-0
        node$BruteBenefit<-0
        node$BruteCost<-0
        nodes[[i]]<-node
      }
      sink("RESULTS2.txt",append=TRUE)#hacemos las medias
      cat("##########################################","\n")
      cat("GENERAL","\n")
      cat("L: ", L/it,"\n")
      cat("Lq: ", Lq/it,"\n")
      cat("W: ", W/(Exits*it),"\n")
      cat("Wq: ", Wq/(Exits*it),"\n")
      cat("Salidas exitosas del sistema: ", Exits,"\n")
      cat("Clientes forzados a salir: ", Rejected,"\n")
      cat("Beneficio: ", Benefit,"\n")
      cat("##########################################","\n")
      cat("##########################################","\n")
      sink(NULL)
    }
    it<-0
    entries<-0
  }
  m = 1
  if(!is.null(update)){
    for (node in nodes) {#SIMULAMOS LAS ENTRADAS
      if (!is.na(update[node$id])) {#SI TIENE ENTRADAS
        new_entries <- (node$Clients + update[node$id])
        if (node$k < new_entries) {#SI EL NUMERO NUEVO DE CLIENTES SUPERA LA CAPACIDAD
          node$Clients <- node$k
          node$Rejected <- (node$Rejected + new_entries - node$k)
        }
        else{#DE LO CONTRARIO ACTUALIZAMOS EL NUMERO DE CLIENTES
          node$Clients <- new_entries
        }
      }
      nodes[[m]] <- node #ACTUALIZAMOS LA LISTA DE NODOS
      m <- (m + 1)
    }
    arrival$Exits<-arrival$Exits+update["Exit"]
  }
}
}
