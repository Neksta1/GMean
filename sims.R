n_b <- 2
b <- c(0,800)
adc <- c(1:1000/1000000)
s01 <- 1
s02 <- c(1:1000/100)

sb1 <- s01*exp(-b[2]*adc)
sb2 <- s01*exp(-b[2]*adc[500])
adc_f <- (-log((sb1+sb2)/2)+log((s01+s01)/2))/800
adc_g <- ((-log(sb1)+log(s01))/800 + (-log(sb2)+log(s01))/800)/2
plot(adc/adc[500], adc_f/adc_g)

ssb1 <- s01*exp(-b[2]*adc[10])
ssb2 <- s02*exp(-b[2]*adc[500])
sadc_f <- (-log((ssb1+ssb2)/2)+log((s01+s02)/2))/800
sadc_g <- ((-log(ssb1)+log(s01))/800 + (-log(ssb2)+log(s02))/800)/2
plot(s02/s01, sadc_f/sadc_g)


adc1 <- 0.0003
adc2 <- 2*adc1
s01 <- 1
s02 <- 2*s01
sb1 <- s01*exp(-b[2]*adc1)
sb2 <- s02*exp(-b[2]*adc2)
adc_f <- (-log((sb1+sb2)/2)+log((s01+s02)/2))/b[2]
adc_g <- ((-log(sb1)+log(s01))/b[2] + (-log(sb2)+log(s02))/b[2])/2
adc_f/adc_g

#function for adc_f/adc_g
fn = 2 + 2*(log((sb1+sb2)/(sb1*sb2))-log(s01+s02/s01*s02))/(log((sb1*sb2)/(s01*s02)))

adc1 = adc[500]
adc2 = adc
fn2 = 2*(log((s01*exp(-b[2]*adc1)+s01*exp(-b[2]*adc2))/(s01+s01)))/(-b[2]*(adc1+adc2))


# signalverlauf monoexp

adc1 <- 0.002
adc2 <- 0.001
b <- c(0:1000)
s01 <- 1
s02 <- s01
sb1 <- s01*exp(-b*adc1)
sb2 <- s02*exp(-b*adc2)
sbr <- s01*exp(-b*(adc1+adc2)/2)
plot(b, log(sb1))
points(b, log(sb2))
points(b, log((sb1+sb2)/2))
points(b, (log(sb1)+log(sb2))/2)
bla <- exp((log(sb1)+log(sb2))/2)
fitf <- nls((sb1+sb2)/2 ~ S0f * exp(-b*ADCf), start = list(S0f = 1, ADCf = 0.001))
fitr <- nls(bla ~ S0r * exp(-b*ADCr), start = list(S0r = 1, ADCr = 0.001), trace = TRUE)
fitbf <- nls((sb1+sb2)/2 ~ a * exp(-b*D) + c * exp(-b*Db),
             start = list(D = 0.001, Db = 0.002, a= 1, c = 1), trace = TRUE)
fitbr <- nls(bla ~ a * exp(-b*D) + c * exp(-b*Db),
             start = list(D = 0.001, Db = 0.002, a= 1, c = 1), trace = TRUE)

# signalverlauf monoexp

adc1 <- 0.002
adc2 <- 0.001
b <- c(0:2000)
s01 <- 1
s02 <- s01
sb1 <- s01*exp(-b*adc1)
sb2 <- s02*exp(-b*adc2)
sbr <- s01*exp(-b*(adc1+adc2)/2)
plot(b, log(sb1))
points(b, log(sb2))
points(b, log((sb1+sb2)/2))
points(b, (log(sb1)+log(sb2))/2)

#rice rauschen
snr <- 1000
sdn <- 1/snr

norm1a <- rnorm(length(sb1), sb1*cos(2), sd = sdn)
norm1b <- rnorm(length(sb1), sb1*sin(2), sd = sdn)
rice1 <- sqrt(norm1a^2+norm1b^2)

norm2a <- rnorm(length(sb2), sb2*cos(2), sd = sdn)
norm2b <- rnorm(length(sb2), sb2*sin(2), sd = sdn)
rice2 <- sqrt(norm2a^2+norm2b^2)

sf <- (rice1+rice2)/2
sr <- exp((log(rice1)+log(rice2))/2)

fitf <- nls(sf ~ S0 * exp(-b*ADC),
            start = list(S0 = 1, ADC = 0.001), trace = TRUE)
fitr <- nls(sr ~ S0 * exp(-b*ADC),
            start = list(S0 = 1, ADC = 0.001), trace = TRUE)

fitbf <- nls(sf ~ S0 * (f * (exp(-b*(Db+D))-exp(-b*D)) + exp(-b*D)),
             start = list(S0 = 1, D = 0.001, Db = 0.002, f= 0.1),
             trace = TRUE,
             lower = c(0, 0, 0, 0), upper = c(10, 1, 1, 1), 
             algorithm = "port")
fitbr <- nls(sr ~ S0 * (f * (exp(-b*(Db+D))-exp(-b*D)) + exp(-b*D)),
             start = list(S0 = 1, D = 0.001, Db = 0.002, f= 0.01),
             trace = TRUE,
             lower = c(0, 0, 0, 0), upper = c(10, 1, 1, 1), 
             algorithm = "port")

fitkf <- nls(sf ~ S0 * (exp(-(b*D)+k*1/6*(b*D)^(2))),
             start = list(S0 = 1, D = 0.001, k= 0.1),
             trace = TRUE,
             lower = c(0, 0,-10), upper = c(10, 1, 10), 
             algorithm = "port")
fitkr <- nls(sr ~ S0 * (exp(-(b*D)+k*1/6*(b*D)^(2))),
             start = list(S0 = 1, D = 0.001, k= 0.1),
             trace = TRUE,
             lower = c(0, 0,-10), upper = c(10, 1, 10), 
             algorithm = "port")



