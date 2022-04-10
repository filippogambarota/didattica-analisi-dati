p0 <- 0.5
nlanci <- 12
ns <- 0:12

plot(dbinom(ns, nlanci, 0.5), type = "h")

pbinom(3, 12, 0.5, lower.tail = FALSE)

qbinom(0.05, 12, 0.5)

binom.test(3, 12, 0.5)

esame <- scan("didattica-2022/data/esame.txt")

qqnorm(esame)
qqline(esame)

mean(esame)
se <- sd(esame)/sqrt(length(esame))
#labstatR::sigma2(esame$V1))/(sqrt(length(esame$V1))) # varianza non corretta

pnorm(esame, mean = 18, lower.tail = FALSE) 

pnorm(mean(esame), mean = 18, sd = se)

# mean_x - mean_p/se

t_value <- (mean(esame) - 18)/se

pt(t_value, df = length(esame)-1)
                            
t.test(esame, mu = 18, alternative = "less")$p.value

pbinom(6, 12, 0.5)