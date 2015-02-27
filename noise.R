ricernd <- function(signal, noise){
        x = noise * rnorm(length(signal)) + signal
        y = noise * rnorm(length(signal))
        r = sqrt(x^2 + y^2)
}
