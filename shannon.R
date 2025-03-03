#vector de abundancias de especies

#calcule el índice de Shannon y Pilou 
pilou <- function (x,y,z) {
  { total <- x + y + z
  p1 <- x/total
  p2 <- y/total
  p3 <- z/total
  shannon <- - (p1*log(p1) + p2*log(p2) + p3*log(p3))
  pil <- shannon/(log(3))
  }
 print(pil)
}  

pilou (6,4,10)
  
## Quisiera usar bien el vector de abundancias, en vez de definir el número de individuo por especie
abundancias <- c(3,4,4)

pilou2 <- function (abundancias) {
  { total <- sum (abundancias)
  p1 <- abundancias[1]/total
  p2 <- abundancias[2]/total
  p3 <- abundancias[3]/total #tengo que añadir un for porque sino está limitado por la lomngitud edl vector
  shannon <- - (p1*log(p1) + p2*log(p2) + p3*log(p3))
  pil2 <- shannon/(log(length(abundancias)))
  }
  print(pil2)
}  

pilou2 (abundancias) #necesito definir el máximo total
#pielou nos dice qué tan diverso es 

 
## Mi manera de hacerlo
#Mi función solo funciona con vectores
shan_pielo <- function(x) { {
  n <- length(x)
  total <- sum(x)
  shannon <- 0

  for (i in 1:n) {
    shannon <- shannon + ((-1) * ((x[i] / total) * log(x[i] / total))) #el ciclo for que necesitaba
  }
  
  pielou <- shannon / (log(n)) #x tiene que ser necesariamente un vector !!!!
}
  return (pielou)
  }

abundancias <- c(10, 5, 3)
shan_pielo (abundancias)



####        Github y R
#Git en ar hivos de programa
library (usethis)

usethis::git_sitrep()

usethis::use_git_config(user.name = 'jul-goher', 
                        user.email = 'juliegonzalezhrra@gmail.com')
##
##
