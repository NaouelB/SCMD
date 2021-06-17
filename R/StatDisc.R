#' statistique discriptive
#' @export
#' @param x vecteur numerique representant les valeurs de la variable aleatoire
SD <- function (x)
{
  par(mfrow = c(1,2))
  hist(x,main = "Histogramme de variable aleatoire",xlab = "V.A",ylab = "Frequence",col= rainbow(7))
  boxplot(x,col= 'red' )
  par( mfrow  = c(1,1))
  data.frame ( somme = sum(x),
               minimum = min(x),
               maximum  = max(x),
               mediane  = median(x),
               moyenne  = mean(x),
               variance = var( x ))
}
