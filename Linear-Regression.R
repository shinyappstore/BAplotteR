
library(ggplot2)
library(magrittr)
library(dplyr)


df_example_1 <- read.csv("df_SystBloodPressure.csv", na.strings = "")
df_example_2 <- read.csv("df_PlasmaVolume.csv", na.strings = "")
df_example_3 <- read.csv("df_T4.csv", na.strings = "")

df <- df_example_1

df$Measurement_1 <- df$J1
df$Measurement_2 <- df$R1

df <- df %>% mutate(Difference = Measurement_1-Measurement_2, Average = 0.5*Measurement_1+0.5*Measurement_2, Ratio=Measurement_1/Measurement_2, Percentage=100*Difference/(Average))


linearMod <- lm(Difference ~ Average, data=df)

linearMod$coefficients[1]

linearMod$residuals