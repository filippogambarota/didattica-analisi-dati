# Regressione Lineare

# https://lindeloev.github.io/tests-as-linear/

library(ggplot2)

# Simuliamo dei dati senza una relazione particolare

x <- rnorm(100)
y <- 10 + 0.5*x + rnorm(100)

plot(x, y, pch = 19)

# Anche se la relazione è simmetrica, nella relazione dobbiamo
# definire una variabile indipendente (x) e dipendente (y) y ~ x

fit0 <- lm(y ~ 1)
mean(y)

plot(x, y, pch = 19)
abline(fit0, col = "red") # media

fit <- lm(y ~ x)

coef(fit)[2]*(sd(x)/sd(y))

abline(fit, col = "blue")

x1 <- runif(100)
y <- 0.2 + 1 * x1 + 10 * x1^2 + rnorm(100, 0, 0.7)

qplot(x1, y) +
    stat_smooth(method='lm', formula = y ~ x, se = FALSE) +
    stat_smooth(method='lm', formula = y ~ poly(x,2), se = FALSE)

plot(x, yq, pch = 19)

fit2 <- lm(yq ~ x)  

abline(fit2)

plot(fit2)

residuals(fit)
round(y - fitted(fit), 3) == round(residuals(fit), 3)

1 - (sum(residuals(fit)^2) / sum((y - mean(y))^2))