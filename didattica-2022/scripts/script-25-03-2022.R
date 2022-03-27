# Residuals ---------------------------------------------------------------

fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)

summary(fit)

summary(residuals(fit))

curve(dnorm(x), -3, 3)

hist(residuals(fit))

# Dataframe ---------------------------------------------------------------

# Come ricodificare una variabile a seconda di condizioni logiche. Questo è molto
# utile per creare delle nuove colonne in un dataframe in base ai valori di una
# colonna esistente

x <- round(runif(30, 15, 70))
dat <- data.frame(x = x)

dat$x_chr <- ifelse(x < 18,
                    yes = "minorenne",
                    no = ifelse(x > 18 & x < 50, 
                                yes = "adulto", 
                                no = "anziano"))

# 1.12 --------------------------------------------------------------------

dat <- readxl::read_xls(path = "data/pazienti.xls")

median(dat$ansia) # 2 quartile
summary(dat$ansia) # cheat
quantile(dat$ansia, probs = c(0.25, 0.5, 0.75))

str(dat$cl.sociale)
dat$cl.sociale_factor <- factor(dat$cl.sociale)
dat$cl.sociale_ord <- ordered(dat$cl.sociale, levels = c("Bassa", "Media", "Alta"))
dat$cl.sociale_ord2 <- factor(dat$cl.sociale,
                              levels = c("Bassa", "Media", "Alta"),
                              ordered = TRUE)

quantile(dat$cl.sociale_ord2, probs = c(0.25, 0.5, 0.75), type = 1)
quantile(dat$cl.sociale_factor, probs = c(0.25, 0.5, 0.75), type = 1)

curve(dnorm(x, 0, 1), xlim = c(-3,3))
pnorm(1, mean = 0, sd = 1)
qnorm(0.5, mean = 0, sd = 1)

sort(dat$eta) # ordinare
mean(sort(dat$eta) <= 39)

median(dat$eta)
mean(dat$eta)
sd(dat$eta)
var(dat$eta)

# varianza

sum((dat$eta - mean(dat$eta))^2) / (length(dat$eta) - 1)

# devianza

sum((dat$eta - mean(dat$eta))^2)

# standard deviation

sqrt(sum((dat$eta - mean(dat$eta))^2) / (length(dat$eta) - 1))

# moda

ADati::moda(dat$cl.sociale)
ADati::moda(dat$eta)

barplot(table(dat$eta)) # visualizziamo le 4 mode

# boxplot

boxplot(dat$eta)

x <- rnorm(1000, 50, 40)
hist(x)
boxplot(x, ylim = c(0,120))
