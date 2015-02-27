p.gap <- c(50,45,57,43,32,30,14,36,51)
p.ag <- c(43,24,52,46,28,17,7,18,29)
data <- as.data.frame(cbind(p.ag, p.gap))

#The model (using non-linear least squares regression):
fit.1.nls <- nls(formula=p.gap~beta1*p.ag^(beta2), start=list(beta1=5.065, beta2=0.6168))
summary(fit.1.nls)

#From the summary, I find the means and s.e's the two parameters, and develop their distributions:
beta1 <- rnorm(1000, 7.8945, 3.5689)
beta2 <- rnorm(1000, 0.4894, 0.1282)
coefs <- as.data.frame(cbind(beta1,beta2))

coeflines <-
        alply(as.matrix(coefs), 1, function(coef) {
                stat_function(fun=function(x){coef[1]*x^coef[2]}, colour="grey")
        })

plot(data$p.ag, data$p.gap, xlab="% agricultural land use",
     ylab="% of riparian buffer gap", xlim=c(0,130), ylim=c(0,130), pch=20, type="n")
for (i in 1:1000){curve(coefs[i,1]*x^(coefs[i,2]), add=T, col="grey")}
curve(coef(fit.1.nls)[[1]]*x^(coef(fit.1.nls)[[2]]), add=T, col="red")
points(data$p.ag, data$p.gap, pch=20)

ggplot(data, aes(x=p.ag, y=p.gap)) +
        scale_x_continuous(limits=c(0,100), "% ag land use") +
        scale_y_continuous(limits=c(0,100), "% riparian buffer gap") +
        coeflines +
        stat_function(fun=fit.mean, color="red") +
        geom_point()

