#' simulation d'une loi discrete n fois
#' @export
#' @param x vecteur numerique represente une valeur d'une variable aleatoire
#' @param p vecteur numerique represente les probabilites
#' @param n nombre d'iteration
rdistn <- function(x,p,n)
{
  y<- c(1:n)
for (i in 1:n) {
  z=rdist(x,p)
  y[i]<-z
}
  return(y)
}
