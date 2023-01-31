#Stworzyłem dwie funkcje: smol i big, bo dla big mniejsza liczba kolorów niekoniecznie dawała pewne przemieszanie.
#biblioteka grafowa
library(igraph)
#biblioteka kolorowa
library(randomcoloR)

#losowanie jednostajne dyskretne
rdu<-function(n,k){sample(1:k,n,replace=T)}

bounding_smol <- function(gr){
  
  #liczba wierzchołków
  n <- length(V(gr))
  
  #maksymalny stopień
  max_deg <- max(degree(gr,V(gr)))
  
  #liczba kolorów
  k <- ceiling(11*max_deg/6)
  
  #maksymalna liczba iteracji
  iterations <- 1000
  
  #możliwe kolory
  colors_graph <- seq(1:k)
  
  #łańcuch główny
  x <- array(dim=c(2,n))
  
  #łańcuch ograniczający
  y <- array(dim=c(2,n,k))
  
  
  #startujemy z każdego stanu o tym samym kolorze
  #x[1,] <- colors_graph[1]
  
  #opcja 1 na wystartowanie, każdemu dajemy cały zbiór kolorów
  for(i in 1:k){
    y[1,,i] <- colors_graph[i]
  }
  #opcja 2, każdemu dajemy 1 kolor
  #y[1,,1] <- colors_graph[1]
  
  x[2,] <- x[1,]
  y[2,,] <- y[1,,]
  
  for(j in 1:iterations){
    
    #kolejna iteracja bazuje na poprzedniej

    len_check <- 0
    for(v in length(V(gr))){
      if(length(y[2,v,!is.na(y[2,v,])])==1){
        len_check <- len_check+1
      }else{
        next
      }
    }
    if(len_check==length(V(gr))){
      break
    }else{
      v <- rdu(1,length(V(gr)))
      
      #sprawdzenie czy kolor jest już wybrany
      if(length(y[2,v,!is.na(y[2,v,])])==1){
        next
      }else{
        #zerowanie y(v)
        for(m in 1:k){
          y[2,v,m] <- NA
        }
        
        #sąsiedzi v
        Nv <- neighbors(gr,v)
        
        #suma mnogościowa sąsiadów
        sum_neighbor <- NULL
        for(l in Nv){
          sum_neighbor <- union(sum_neighbor,y[2,l,])
        }
        
        for(i in colors_graph){
          
          #wybieranie koloru
          col_curr <- rdu(1,k)
          
          #warunek stopowania pętli
          
          
          #sprawdzenie czy któryś z sąsiadów jest jednym kolorem, trochę naokoło
          singular_check <- 0
          for(m in Nv){
            if(identical(y[2,m,!is.na(y[2,m,])],c(col_curr))==TRUE){
              singular_check <- singular_check+1
            }
          }
          if(singular_check>=1){
            next
          }else{
            y[2,v,i] <- col_curr
            if(is.element(col_curr,sum_neighbor)==FALSE | length(y[2,v,!is.na(y[2,v,])])>max_deg){
              break
            }else{
              next
            }
          }
          
        }
        
        #dodanie koloru do x
        x[2,v] <- col_curr
        x[1,] <- x[2,]
        y[1,,] <- y[2,,]
      }
      
    }
  }
  return(x[2,])
}

bounding_big <- function(gr){
  
  #liczba wierzchołków
  n <- length(V(gr))
  
  #maksymalny stopień
  max_deg <- max(degree(gr,V(gr)))
  
  #dla dużych grafów
  k <- max_deg*(max_deg+2)
  
  #parametr ułatwiający znalezienie wskazanej liczby iteracji, działa dla dużych wielkości
  beta <- 1-(1-((max_deg+1)*max_deg)/(k-max_deg+1))/n
  theta <- 10000
  
  #maksymalna liczba iteracji
  iterations <- ceiling(log(n,base = beta))+theta
  
  #możliwe kolory
  colors_graph <- seq(1:k)
  
  #łańcuch główny
  x <- array(dim=c(2,n))
  
  #łańcuch ograniczający
  y <- array(dim=c(2,n,k))
  
  
  #startujemy z każdego stanu o tym samym kolorze
  #x[1,] <- colors_graph[1]
  
  #opcja 1 na wystartowanie, każdemu dajemy cały zbiór kolorów
  for(i in 1:k){
    y[1,,i] <- colors_graph[i]
  }
  #opcja 2, każdemu dajemy 1 kolor
  #y[1,,1] <- colors_graph[1]
  
  x[2,] <- x[1,]
  y[2,,] <- y[1,,]
  
  for(j in 1:iterations){
    
    #kolejna iteracja bazuje na poprzedniej

    len_check <- 0
    for(v in length(V(gr))){
      if(length(y[2,v,!is.na(y[2,v,])])==1){
        len_check <- len_check+1
      }else{
        next
      }
    }
    if(len_check==length(V(gr))){
      break
    }else{
      v <- rdu(1,length(V(gr)))
      
      #sprawdzenie czy kolor jest już wybrany
      if(length(y[2,v,!is.na(y[2,v,])])==1){
        next
      }else{
        #zerowanie y(v)
        for(m in 1:k){
          y[2,v,m] <- NA
        }
        
        #sąsiedzi v
        Nv <- neighbors(gr,v)
        
        #suma mnogościowa sąsiadów
        sum_neighbor <- NULL
        for(l in Nv){
          sum_neighbor <- union(sum_neighbor,y[2,l,])
        }
        
        for(i in colors_graph){
          
          #wybieranie koloru
          col_curr <- rdu(1,k)
          
          #warunek stopowania pętli
          
          
          #sprawdzenie czy któryś z sąsiadów jest jednym kolorem, trochę naokoło
          singular_check <- 0
          for(m in Nv){
            if(identical(y[2,m,!is.na(y[2,m,])],c(col_curr))==TRUE){
              singular_check <- singular_check+1
            }
          }
          if(singular_check>=1){
            next
          }else{
            y[2,v,i] <- col_curr
            if(is.element(col_curr,sum_neighbor)==FALSE | length(y[2,v,!is.na(y[2,v,])])>max_deg){
              break
            }else{
              next
            }
          }
          
        }
        
        #dodanie koloru do x
        x[2,v] <- col_curr
        x[1,] <- x[2,]
        y[1,,] <- y[2,,]
      }
      
    }
  }
  return(x[2,])
}

###
#funkcja na losowanie grafu, pierwszy parametr to liczba wierzchołków, drugi to prawdopodobieństwo wystąpienia krawędzi
g1 <- erdos.renyi.game(4,1)
plot(g1)
max_degg <- max(degree(g1,V(g1)))
k1 <- ceiling(11*max_degg/6)
k2 <- max_degg*(max_degg+2)
#tworzenie palety koloróW
paleta <- distinctColorPalette(k1) 
V(g1)$color <- paleta[bounding_smol(g1)]
plot(g1)

###
g2 <- erdos.renyi.game(13,1)
plot(g2)
max_degg <- max(degree(g2,V(g2)))
k1 <- ceiling(11*max_degg/6)
k2 <- max_degg*(max_degg+2)
paleta <- distinctColorPalette(k2) 
V(g2)$color <- paleta[bounding_big(g2)]
plot(g2)
length(unique(V(g2)$color))

###
g3 <- erdos.renyi.game(7,0.3)
plot(g3)
max_degg <- max(degree(g3,V(g3)))
k1 <- ceiling(11*max_degg/6)
k2 <- max_degg*(max_degg+2)
paleta <- distinctColorPalette(k2) 
V(g3)$color <- paleta[bounding_smol(g3)]
plot(g3)

###
g4 <- erdos.renyi.game(8,0.6)
plot(g4)
max_degg <- max(degree(g4,V(g4)))
k1 <- ceiling(11*max_degg/6)
k2 <- max_degg*(max_degg+2)
paleta <- distinctColorPalette(k2) 
V(g4)$color <- paleta[bounding_smol(g4)]
plot(g4)
length(unique(V(g4)$color))