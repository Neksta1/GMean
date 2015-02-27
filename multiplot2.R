
```{r}
adccoefs <-  rnorm(100, 0.001, sd = 0.0002)
adclines <-
        alply(as.matrix(adccoefs), 1, function(adccoef) {
                stat_function(fun=function(x){S01*exp(-x*adccoef[1])}, colour="grey")
        })
```

```{r}
p <- ggplot(data = params)
p +     adclines +
        layer (stat = "function",
               fun = sb1,
               size = 1,
               alpha = 0.8,
               mapping = aes (color="sb1")
        ) + 
        layer (stat = "function",
               fun = sb2,
               size = 1,
               alpha = 0.8,
               mapping = aes (color="sb2")
        ) + 
        layer (stat = "function",
               fun = sbf,
               size = 1,
               alpha = 0.8,
               mapping = aes (color="sb3")
        ) + 
        layer (stat = "function",
               fun = sbr,
               size = 1,
               alpha = 0.8,
               mapping = aes (color="sb4")
        ) + 
        xlim(0,1000) +
        scale_y_log10() +
        xlab("b-value") + 
        ylab("Signal Intensity")
```
