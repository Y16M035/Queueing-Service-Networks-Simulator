source("queues.R")

#crea arrival
createArrival <- function(data, i){#queue y server eran necesarios para tratarlo como un nodo
  arrival <- list(id="arrival",
                  lambda = 1,
                  T=names(c()),
                  Queue = queue("FIFO"),
                  servers= vector(mode="list",length=1),
                  Total_time=0,
                  Exits=0)
  finished = 0
  #mientras no haya eof o encontremos un nodo
  while (finished == 0 && i < length(data) + 1) {
    line <- data[i]
    first <- trimws(strsplit(line, ":")[[1]][1])#variable
    second <- trimws(strsplit(line, ":")[[1]][2])#valor
    if (first == "lambda") {#si la variable es lambda la guardamos como un num
      arrival$lambda = as.numeric(second)
    }
    else if (first == "transitions") {#si la variable es las transiciones
      transitions <- strsplit(second, ",")[[1]]#obtenemos cada una
      for (entry in transitions) {#asociamos cada nodo con la posibillidad de que vaya ahi
        id <- trimws(strsplit(entry, "-")[[1]][1])
        p <- as.numeric(trimws(strsplit(entry, "-")[[1]][2]))
        arrival$T[id] = p
      }
    }
    else if (first == "NODE") {#si encontramos una declaracion de nodos paramos
      finished = 1
    }
    i <- i + 1
  }
  return(list(arrival, i))
}

createNode <- function(data, i, n) {
  node <-
    list(
      id = -1*i,
      IN = "M",
      OUT = "M",
      c = 1,
      k = Inf,
      Queue = queue("FIFO"),
      mu = 1,
      QCost = 0,
      SCost = 0,
      ICost = 0,
      Rev = 0,
      T = names(c()),
      Clients=0,
      Exits=0,
      L=0,
      Lq=0,
      W=0,
      Wq=0,
      Rejected=0,
      servers=list(),
      BruteCost=0,
      BruteBenefit=0
    )
  finished = 0
  while (finished == 0 && i < length(data) + 1) {
    line <- data[i]
    first <- trimws(strsplit(line, ":")[[1]][1])
    second <- trimws(strsplit(line, ":")[[1]][2])
    if (first == "id") {#establecemos su id
      node$id = second
    }
    else if (first == "arrival") {#su distribucion de llegadas
      node$IN = second
    }
    else if (first == "exit") {#su distribucion de salidas
      node$OUT = second
    }
    else if (first == "servers") {#el numero de servidores en paralelo y los creamos
      node$c = as.numeric(second)
      node$servers<- vector(mode="list",length=node$c)
    }
    else if (first == "capacity") {#capacidad del sistema
      node$k = as.numeric(second)
    }
    else if (first == "queue discipline") {#la cola que es
      dis<-trimws(strsplit(second, "-")[[1]][1])
      if (dis=="PR"){
        n<-as.numeric(trimws(strsplit(second, "-")[[1]][2]))
        node$Queue=prqueue(n)
      }
      else {
        node$Queue=queue(dis)
      }
    }
    else if (first == "mu") {#la tasa de salida de cada servidor
      node$mu = as.numeric(second)
    }
    else if (first == "queue cost per client") {#el coste e tener un cliente en cola por ud de tiempo
      node$QCost = as.numeric(second)
    }
    else if (first == "service cost per client") {#el coste de dar un servicio por ud de tiempo
      node$SCost = as.numeric(second)
    }
    else if (first == "infrastructure cost") {#coste de la infraestructura
      node$ICost = as.numeric(second)
    }
    else if (first == "service revenue per client") {#coste del beneficio por servicio
      node$Rev = as.numeric(second)
    }
    else if (first == "transitions") {#posibles transiciones
      transitions <- strsplit(second, ",")[[1]]
      for (entry in transitions) {
        id <- trimws(strsplit(entry, "-")[[1]][1])
        p <- as.numeric(trimws(strsplit(entry, "-")[[1]][2]))
        node$T[id] = p
      }
    }
    else if (first == "NODE") {#si encontramos otro nodo paramos
      finished = 1
    }
    i <- i + 1
  }
  return(list(node, i, node$id))
}

parse <- function() {
  data <- readLines("text2")#leemos el fichero
  data <- data[which(data != "")]
  i = 1
  if (data[1] == "ARRIVAL") {#si recibimos arrival de primero la creamos
    result <- createArrival(data, i + 1)
    arrival <- result[[1]]
    i <- result[[2]]
    nodes <- list()#creamos una lista de nodos
    ids <- list() #y de sus ids
    m = 1
    while (i < length(data) + 1) {#creamos cada nodo y lo metemos en la lista
      result <- createNode(data, i, arrival$nodes)
      node <- result[[1]]
      nodes[m] <- list(node)
      i <- result[[2]]
      ids[m] <- result[[3]]
      m = m + 1
    }
  }
  return(list(arrival, nodes, ids))#devolvemos el nodo arrival la lista de nodos y la de sus ids
}
