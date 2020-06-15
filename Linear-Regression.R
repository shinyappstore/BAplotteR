
library(ggplot2)
library(magrittr)
library(dplyr)


df_example_1 <- read.csv("df_SystBloodPressure.csv", na.strings = "")
df_example_2 <- read.csv("df_PlasmaVolume.csv", na.strings = "")
df_example_3 <- read.csv("df_T4.csv", na.strings = "")



df <- df_example_3

# df$Measurement_1 <- df$J1
# df$Measurement_2 <- df$R1

df$Measurement_1 <- df$T4A
df$Measurement_2 <- df$T4X


df <- df %>% mutate(Difference = Measurement_1-Measurement_2, Average = 0.5*Measurement_1+0.5*Measurement_2, Ratio=Measurement_1/Measurement_2, Percentage=100*Difference/(Average))


linearMod <- lm(Difference ~ Average, data=df)

b0 <- linearMod$coefficients[1]
b1 <- linearMod$coefficients[2]

linearMod$residuals

residuals(linearMod)

df$residuals <- linearMod$residuals
df$abs_residuals <- abs(df$residuals)

linearModRes <- lm(abs_residuals ~ Average, data=df)

linearModRes$coefficients

c0 <- linearModRes$coefficients[1]
c1 <- linearModRes$coefficients[2]

# b0+b1*Average±2.46*(c0+c1*A)

df <- df %>% mutate(LoA_regr_hi = b0+b1*Average+2.46*(c0+c1*Average), LoA_regr_lo = b0+b1*Average-2.46*(c0+c1*Average))


df$predicted <- predict(linearMod)

ggplot(df) +aes(x=Average)+aes(y=Difference)+geom_point()+geom_line(aes(x=Average,y=predicted))+geom_line(aes(x=Average,y=LoA_regr_hi))+geom_line(aes(x=Average,y=LoA_regr_lo))


ggplot(df) +aes(x=Average)+aes(y=residuals)+geom_point()

x <- round(confint(linearMod, level=0.95),2)
y <- (data.frame(lo=x[,1], hi=x[,2]))


if ((y$lo[1] < 0 && y$hi[1] > 0) || (y$lo[1] > 0 && y$lo[1] < 0)) {print("Intercept likely through zero")} else {print("Intercept unlikely through zero")}

if ((y$lo[2] < 0 && y$hi[2] > 0) || (y$lo[2] > 0 && y$lo[2] < 0)) {print("Slope likely zero")} else {print("Slope unlikely zero")}

print(paste("The 95% CI of the intercept ranges from", x[1,1], "to",x[1,2], sep=))

print(paste("The 95% CI of the slope ranges from", x[2,1], "to",x[2,2], sep=))
