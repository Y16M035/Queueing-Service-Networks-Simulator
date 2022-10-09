queue <- function(x) { #crea un FIFO,LIFO o RSS
  return(list(
    cola = list(),
    discipline = x,
    size = 0
  ))
}

prqueue <- function(n) { #crea una Priority Queue con n niveles de prioridad
  a <- list(
    cola = vector(mode = "list", length = n),
    discipline = "PR",
    size = 0
  )
  for (i in 1:n) {
    a$cola[[i]] <- list()
  }
  return(a)
}

push <- function(q, e) {
  if (q$discipline == "FIFO" || q$discipline == "RSS") {
    q$cola <- append(q$cola, e)
  }
  else if (q$discipline == "LIFO") { #e se pone al principio de la cola
    q$cola <- append(q$cola, e, after = 0)
  }
  else if (q$discipline == "PR") { # e se pone al final de la lista correespondiente a su prioridad
    q$cola[[e[[1]]$priority]] <- append(q$cola[[e[[1]]$priority]], e)
  }
  q$size = q$size + 1
  return(q)
}

pop <- function(q) {
  if (q$discipline == "FIFO" || q$discipline == "LIFO") {
    e <- q$cola[[1]] #devolvemos el primer elemento
    if(q$size==1){ #si quedaba solo uno la lista se queda vacía
      q$cola<-list()
    }
    else{
      q$cola <- q$cola[2:q$size] #si aun quedan mas se quita el e
    }
  }
  else if (q$discipline == "RSS") {
    i <- floor(runif(1, 1, q$size+1)) #obtenemos un indice 100% random
    e <- q$cola[[i]]
    if (i == 1) {#si es el primer elemento
      if(q$size==1){# y la lista quedaria vacia
        q$cola<-list()
      }
      else{# eoc eliminamos el e
        q$cola <- q$cola[2:q$size]
      }
    }
    else if (i == q$size) { #si es el ultimo elemento y no es de un elemento recortamos el final
      q$cola <- q$cola[1:q$size - 1]
    }
    else{# eoc 
      q$cola <- append(q$cola[1:(i - 1)], q$cola[(i + 1):q$size])
    }
  }
  else if (q$discipline == "PR") {#si es priority
    for (i in 1:length(q$cola)) {
      if (length(q$cola[[i]]) > 0) {#buscamos la primera subcola vacia
        e <- q$cola[[i]][[1]]
        if (length(q$cola[[i]]) > 1) {
          q$cola[[i]] <- q$cola[[i]][2:length(q$cola[[i]])]
        }
        else{
          q$cola[[i]] <- list()
        }
        q$size<-q$size-1
        return(list(q, e))#hacemos el return aqui para evitar operaciones innecesarias
      }
    }
  }
  q$size = q$size - 1
  return(list(q,e))
}
