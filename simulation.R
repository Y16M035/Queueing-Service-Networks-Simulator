{source("parser.R")

moveTo <-function(id, user, nodes) {
  #DEVOLVEMOS EL PAR NODO ACTUALIZADO
  user$timeQ = 0
  for (j in 1:(length(nodes)-1)) {
    node <- nodes[[j]]
    if (node$id == id) {
      # SI EL NODO ES EL DESEADO
      for (i in 1:length(node$servers)) {
        #BUSCAMOS UN SERVIDOR VACIO
        if (is.null(node$servers[[i]])) {
          tiempo <- rexp(1, node$mu)#calculamos cuanto tardara en acabar
          user$timeSs <- user$timeSs + tiempo
          user$timeS <- tiempo#lo mismo al tiempo del cliente en el sistema
          node$servers[[i]] <- list(client = user, time = tiempo)
          nodes[[j]]<-node
          return(nodes)
        }
      }
      if (node$Queue$size + node$c < node$k) {
        #SI NO HAY SERVERS VACIOS, ENCOLAMOS SI CABE
        node$Queue <- push(node$Queue, list(user))
      }
      else{
        #SI NO CABE EN LA COLA ACTUALIZAMOS REJECTED
        node$Rejected <- (node$Rejected + 1)
      }
      nodes[[j]]<-node
      return(nodes)
    }
    nodes[[j]]<-node
  }
}

simulateExit <- function(node, ids) {
  prev <- 0
  rand <- runif(1, 0, 1)
  if(!is.null(node$T)){
    for (ID in ids) {
      #recolrre la lista de ids
      if (!is.na(node$T[ID])) {
        if (rand <= (node$T[ID] + prev)) {
          #si esta en el rango => va a ese nodo
          return(ID)
        } else{
          prev <- prev + node$T[ID]#actualizamos el limite inferior
        }
      }
    }
  }
  return(NULL)
}

update <- function(nodes, ids) {
  TIME <- Inf
  id <- NULL
  for (node in nodes) {
    #ENCONTRAR CLIENTE QUE VAYA A SALIR ANTES DE UN NODO
    for (i in 1:length(node$servers)) {
      if (!is.null(node$servers[[i]]) && node$servers[[i]]$time < TIME) {
        TIME <- node$servers[[i]]$time #cuanto tiempo va a pasar
        node_id <- node$id #de que nodo y de que servidor
        server_number <- i
      }
    }
  }
  if (TIME < Inf) {
    #SI HAY MOVIMIENTOS
    nodes[[length(nodes)]]$Total_time <-nodes[[length(nodes)]]$Total_time + TIME
    for (i in 1:(length(nodes))) {
      #calculamos el coste de que pase TIME
      node <- nodes[[i]]
      if (node$id != "arrival") {
        node$BruteCost <- node$BruteCost + (node$Queue$size * node$QCost * TIME)# en cola
        node$Lq <- node$Lq + node$Queue$size * TIME#L es el numero de clientes ponderado con TIME
        node$L <- node$L + node$Queue$size * TIME
        #ACTUALIZAMOS EL TIEMPO QUE LLEVAN ESPERANDO CADA CLIENTE EN LA COLA DEL NODO
        if (node$Queue$discipline != "PR") {
          pos <- 1
          while (pos <= node$Queue$size) {
            node$Queue$cola[[pos]]$timeQ <-(node$Queue$cola[[pos]]$timeQ + TIME)
            pos <- pos + 1
          }
        }
        else{
          for (c in 1:length(node$Queue$cola)) {
            pos <- 1
            while (pos <= length(node$Queue$cola[[c]])) {
              node$Queue$cola[[c]][[pos]]$timeQ <- (node$Queue$cola[[c]][[pos]]$timeQ + TIME)
              pos <- pos + 1
            }
          }
        }
      }#MIRAMOS DENTRO DE CADA SERVIDOR
      for (j in 1:length(node$servers)) {
        if (!is.null(node$servers[[j]])) {
          #en servidor calculamos el tiempo que le quedará a cada
          node$L <- node$L + TIME 
          node$servers[[j]]$time <- (node$servers[[j]]$time - TIME)
        }
        if (node_id == node$id && j == server_number) {
          CLIENT <- node$servers[[j]]$client
          if (node$id != "arrival") {
            node$Exits <- node$Exits + 1#una salida mas
            node$BruteBenefit <- node$BruteBenefit + node$Rev
            node$BruteCost <- (node$BruteCost + CLIENT$timeS * node$SCost)
            node$W <- node$W + CLIENT$timeQ + CLIENT$timeS
            node$Wq <- node$Wq + CLIENT$timeQ
          }
          #SI HAY CLIENTES EN LA COLA HACEMOS POP Y METEMOS EL RESULTADO EN EL SERVER
          if (node$Queue$size > 0) {
            POP <- pop(node$Queue)
            node$Queue <- POP[[1]]
            cliente <- POP[[2]]
            
            node$BruteCost <- (node$BruteCost + cliente$timeQ * node$QCost)
            
            expected <- rexp(1, node$mu)
            cliente$timeSs <- cliente$timeSs + expected
            cliente$timeS <- expected
            cliente$timeQs <- cliente$timeQs + cliente$timeQ
            
            node$servers[[j]]$client <- cliente
            node$servers[[j]]$time <- expected
          }
          #SI NO LOS HAY EL SERVIDOR SE VACÍA
          else{
            node$servers[j] <- list(NULL)
          }
          id <- simulateExit(node, ids)#guardamos a que nodo fue
          if (is.null(id)) {#si ha salido del sistema
            nodes[[length(nodes)]]$Wq <- nodes[[length(nodes)]]$Wq + CLIENT$timeQs
            nodes[[length(nodes)]]$W <- nodes[[length(nodes)]]$W + CLIENT$timeSs + CLIENT$timeQs
            nodes[[length(nodes)]]$Exits <- nodes[[length(nodes)]]$Exits + 1
          }
        }
      }
      nodes[[i]] <- node
    }
    if(!is.null(id)){#movemos el cliente a su destino
      nodes<-moveTo(id,CLIENT,nodes)
    }
  }
  return(nodes)
}

#creamos los objetos con los que trabajar
objects <- parse()
arrival <- objects[[1]]
nodes <- objects[[2]]
ids <- objects[[3]]

#añadimos arrival a la lista de nodes
nodes <- append(nodes, list(arrival))
nodes[[length(nodes)]]$W <- 0
nodes[[length(nodes)]]$Wq <- 0
identifier = 0

#hallamos el numero de prioridades
prio <- 1
for (node in nodes) {
  if (node$Queue$discipline == "PR") {
    prio <- max(prio, length(node$Queue$cola))
  }
}

sink("RESULTS.txt")
cat()
sink(NULL)

while (TRUE) {
  if (is.null(nodes[[length(nodes)]]$servers[[1]])) {
    #si no hay clientes
    #simulamos un cliente con id y nivel de prioridad entre 1 y el nº de prioridades
    cliente = list(
      id = identifier,
      timeQ = 0,
      timeS = 0,
      timeQs = 0,
      timeSs = 0,
      priority = floor(runif(1, 1, prio + 1))
    )
    #time es el tiempo que va a tardar en llegar el cliente al sistema
    #no cuánto tarda en llegar el siguiente cliente (por una implementacion más sencilla)
    nodes[[length(nodes)]]$servers[[1]] <-list(client = cliente, time = rexp(1, arrival$lambda))
    identifier <- identifier + 1
    if (identifier >= 200 && identifier %% 200 == 1) {
      L = 0
      Lq = 0
      W <- nodes[[length(nodes)]]$W
      Wq <- nodes[[length(nodes)]]$Wq
      nodes[[length(nodes)]]$W <- 0
      nodes[[length(nodes)]]$Wq <- 0
      Benefit = 0
      Exits = nodes[[length(nodes)]]$Exits
      nodes[[length(nodes)]]$Exits <- 0
      Rejected = 0
      time <- nodes[[length(nodes)]]$Total_time
      nodes[[length(nodes)]]$Total_time <- 0
      sink("RESULTS.txt", append = TRUE)
      cat("Datos obtenidos entre la llegada del cliente ",identifier - 201,"y el ",identifier - 1,"\n")
      sink(NULL)
      for (i in 1:(length(nodes) - 1)) {
        node <- nodes[[i]]
        L <- L + node$L
        Lq <- Lq + node$Lq
        Rejected <- Rejected + node$Rejected
        Benefit <-
          Benefit + node$BruteBenefit - node$BruteCost - node$ICost * time
        sink("RESULTS.txt", append = TRUE)
        cat("##########################################\n")
        cat("NODO ", node$id, "\n")
        cat("L: ", node$L / time, "\n")
        cat("Lq: ", node$Lq / time, "\n")
        cat("W: ", node$W / node$Exits, "\n")
        cat("Wq ", node$Wq / node$Exits, "\n")
        cat("Clientes servidos: ", node$Exits, "\n")
        cat("Clientes rechazados: ", node$Rejected, "\n")
        cat("Beneficio: ",
            node$BruteBenefit - node$BruteCost - node$ICost * time,
            "\n")
        sink(NULL)
        node$L <- 0
        node$Lq <- 0
        node$W <- 0
        node$Wq <- 0
        node$Exits <- 0
        node$Rejected <- 0
        node$BruteBenefit <- 0
        node$BruteCost <- 0
        nodes[[i]] <- node
      }
      sink("RESULTS.txt", append = TRUE)
      cat("##########################################\n")
      cat("GENERAL", "\n")
      cat("L: ", L / time, "\n")
      cat("Lq: ", Lq / time, "\n")
      cat("W: ", W / Exits, "\n")
      cat("Wq: ", Wq / Exits, "\n")
      cat("Salidas exitosas del sistema: ", Exits, "\n")
      cat("Clientes forzados a salir: ", Rejected, "\n")
      cat("Beneficio: ", Benefit, "\n")
      cat("##########################################\n")
      cat("##########################################\n")
      sink(NULL)
      
      nodes[[length(nodes)]]$Exits <- 0
      nodes[[length(nodes)]]$Total_time <- 0
    }
  }
  
  #corremos un movimiento de la simulacion
  nodes <- update(nodes, ids)
}
}
