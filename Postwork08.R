"Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente
los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en
su nivel socioeconómico, en si el hogar tiene recursos financieros extrar al ingreso y en si presenta
o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los
determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por
el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares
con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas
con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:"
  
#nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
#area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
#numpeho (Número de persona en el hogar)
#refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
#edadjef (Edad del jefe/a de familia)
#sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
#añosedu (Años de educación del jefe de familia)
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)
#IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"
library(dplyr)
library(DescTools)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
head(df)

df <- na.omit(df)
#df[is.na(df)] <- 0 
df$nse5f <- factor(df$nse5f, labels = c("Bajo", "Medio Bajo", "Medio", "Medio Alto", "Alto"))
df$area <- factor(df$area, labels = c("Zona Urbana", "Zona Rural"))
df$refin <- factor(df$refin, labels = c("No", "Si"))
df$sexojef <- factor(df$sexojef, labels = c("Hombre", "Mujer"))
df$IA <- factor(df$IA, labels = c("No Presenta IA", "Presenta IA"))

head(df)



"1.Plantea el problema del caso"
#Comprobar la relación entre el nivel socioeconómico de los hogares con los gastos en
#alimentos saludables y no saludables, así como si cuentan con recursos financieros extra,
#teniendo en cuenta si se presenta inseguridad alimentaria y los determinantes socioeconómicos asociados a ella


"2.Realiza un análisis descriptivo de la información"
summary(df)

(freq <- table(df$nse5f))
ggplot(df, aes(x = nse5f, fill = nse5f)) +
  geom_bar(aes(color = nse5f, fill = nse5f), alpha = 0.4) +
  labs(title = "Nivel socioeconómico de los hogares",x = "Nivel Socioeconómico", y = "Frecuencia") + 
  theme_light()
"Bajo  Medio Bajo    Medio   Medio Alto   Alto 
 3553        3927     4119         4364   4317"

(freq <- table(df$area))
ggplot(df, aes(x = area, fill = area)) +
  geom_bar(aes(color = area, fill = area), alpha = 0.4) +
  labs(title = "Zona de los hogares",x = "Zona", y = "Frecuencia") + 
  theme_light()
"Zona Urbana  Zona Rural 
       13959        6321 "

(freq <- table(df$refin))
ggplot(df, aes(x = refin, fill = refin)) +
  geom_bar(aes(color = refin, fill = refin), alpha = 0.4) +
  labs(title = "Recursos financieros extra en los hogares",x = "¿Cuenta con recursos financieros extra?", y = "Frecuencia") + 
  theme_light()
"No     Si 
 16421  3859"

(freq <- table(df$sexojef))
ggplot(df, aes(x = sexojef, fill = sexojef)) +
  geom_bar(aes(color = sexojef), alpha = 0.4) +
  scale_color_manual(values = c("#97DB4F", "#E55381")) +
  scale_fill_manual(values = c("#97DB4F", "#E55381")) +
  labs(title = "Sexo del jefe del hogar",x = "Sexo", y = "Frecuencia") + 
  theme_light()
"Hombre  Mujer 
  15887   4393"

(freq <- table(df$IA))
ggplot(df, aes(x = IA, fill = IA)) +
  geom_bar(aes(color = IA, fill = IA), alpha = 0.4) +
  scale_color_manual(values = c("#97DB4F", "#E55381")) +
  scale_fill_manual(values = c("#97DB4F", "#E55381")) +
  labs(title = "Hogares con inseguridad alimentaria",x = "¿Presenta inseguridad alimentario?", y = "Frecuencia") + 
  theme_light()
"No Presenta IA    Presenta IA 
           5853          14427 "


#General
(mean.als <- mean(df$ln_als)) #6.191992
(sd.als <- sd(df$ln_als)) #0.688553
(Mode(df$ln_als)[1]) #6.309918
(mean.alns <- mean(df$ln_alns)) #4.118845
(sd.alns <- sd(df$ln_alns)) #1.041476
(Mode(df$ln_alns)[1]) #3.401197

#Por nivel socioeconómico
(nivel.mean.sd <- df %>%
                  select(nse5f, ln_als, ln_alns) %>%
                  group_by(nse5f) %>%
                  summarize(mean_ln_als = mean(ln_als),
                            sd_ln_als = sd(ln_als),
                            mean_ln_alns = mean(ln_alns),
                            sd_ln_alns = sd(ln_alns)))
" nse5f      mean_ln_als sd_ln_als   mean_ln_alns sd_ln_alns
1 Bajo              5.80     0.765           3.69      0.943
2 Medio Bajo        6.03     0.669           3.91      0.948
3 Medio             6.18     0.608           4.05      0.982
4 Medio Alto        6.33     0.597           4.23      1.02 
5 Alto              6.54     0.584           4.61      1.06 "
boxplot(ln_als ~ nse5f,data = df)
boxplot(ln_alns ~ nse5f,data = df)




"3.Calcula probabilidades que nos permitan entender el problema en México"
pairs(~ nse5f + ln_als + ln_alns, 
      data = df, gap = 0.4, cex.labels = 1.5)

cor(exp(df$ln_als), df$nse5f)


prop.table(table(df$nse5f, df$refin),1)
"                 No        Si
Bajo       0.7863777 0.2136223
Medio Bajo 0.7950089 0.2049911
Medio      0.8052925 0.1947075
Medio Alto 0.8125573 0.1874427
Alto       0.8436414 0.1563586"
#En el nivel Bajo existe un 78.63% de probabilidad que el hogar no cuente con recursos extra pero un 21.36% de que si exista
#En el nivel Medio Bajo existe un 79.50% de probabilidad que el hogar no cuente con recursos extra pero un 20.49% de que si exista
#En el nivel Medio existe un 80.52% de probabilidad que el hogar no cuente con recursos extra pero un 19.47% de que si exista
#En el nivel Medio Alto existe un 81.25% de probabilidad que el hogar no cuente con recursos extra pero un 18.74% de que si exista
#En el nivel Alto existe un 84.36% de probabilidad que el hogar no cuente con recursos extra pero un 15.63% de que si exista
transform(table(df$nse5f, df$refin),
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))


prop.table(table(df$nse5f, df$IA),1)
"          No Presenta IA   Presenta IA
Bajo            0.1404447     0.8595553
Medio Bajo      0.1937866     0.8062134
Medio           0.2401068     0.7598932
Medio Alto      0.3279102     0.6720898
Alto            0.5033588     0.4966412"
#En el nivel Bajo existe un 14.04% de probabilidad que no tengan Inseguridad Alimentaria pero un 85.95% de que si exista
#En el nivel Medio Bajo existe un 19.37% de probabilidad que no tengan Inseguridad Alimentaria pero un 80.62% de que si exista
#En el nivel Medio existe un 24.01% de probabilidad no tengan Inseguridad Alimentaria pero un 75.98% de que si exista
#En el nivel Medio Alto existe un 32.79% de probabilidad que no tengan Inseguridad Alimentaria pero un 67.20% de que si exista
#En el nivel Alto existe un 50.33% de probabilidad que no tengan Inseguridad Alimentaria pero un 49.66% de que si exista
transform(table(df$nse5f, df$IA),
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))

{curve(dnorm(x, mean = mean.als, sd = sd.als), from = 0, to = 10, 
      col='blue', main = "Densidad Normal:\nln_als y ln_alns",
      ylab = "f(x)", xlab = "X")
  legend(x = 8.5, y = 0.5, legend=c("ln_als", "ln_alns"),
         col=c("blue", "red"), lty = 1, bty = "n", cex=0.8)
  curve(dnorm(x, mean = mean.alns, sd = sd.alns), from = 0, to = 10, 
        col='red', add = TRUE)
}


"4.Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México"
df.b <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

"Existe EEE de que el logaritmo natural del gasto en alimentos saludables es mayor a 6.1834?"
"Planteamiento de hipótesis:"
#Ho: mu <= 6.1834
#Ha: mu > 6.1834
{
  ttest <- t.test(x = df$ln_als, alternative = "greater", mu = 6.1834)
  p.value <- ttest$p.value
  if(p.value < 0.05) {
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es mayor a 6.1834")
  } else{
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es menor a 6.1834")
  }
}

"Con base en los datos, existe evidencia estadística para concluir que
el logaritmo natural en gastos de alimentos no saludables es mayor o igual 4.1298 ?"
#Ho: mu >= 4.1298
#Ha: mu < 4.1298
{
  ttest <- t.test(x = df$ln_alns, alternative = "less", mu = 4.1298)
  p.value <- ttest$p.value
  if(p.value < 0.05) {
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es mayor a 4.1298")
  } else{
    paste("NC 95%: Existe evidencia para rechazar Ho, el gasto es menor a 4.1298")
  }
}

"La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a
gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros
determinantes, lleva a que un hogar presente cierta inseguridad alimentaria"
var.test(df.b[df.b$nse5f > 3, "ln_alns"],
         df.b[df.b$nse5f < 3, "ln_alns"],
         ratio = 1, alternative = "two.sided")

"Planteamiento de hipótesis:
Ho: ln_alns_nse5f1-2 <= ln_alns_nse5f4-5
Ha: ln_alns_nse5f1-2 > ln_alns_nse5f4-5"

t.test(x = df.b[df.b$nse5f > 3, "ln_alns"],
       y = df.b[df.b$nse5f < 3, "ln_alns"],
       alternative = "greater", mu = 0, var.equal = FALSE) #p-value < 2.2e-16
#A nivel de confianza estándar, EEE para rechazar la Ho, el gasto en productos no saludables en los hogares
#con menor nivel socioeconómico es mayor al gasto de los hogares con mayor nivel socioeconómico


"5.Estima un modelo de regresión, lineal o logístico, para identificiar los determinantes de la inseguridad alimentaria en México"
attach(df.b)
#Regresión logística
#IA en relación con años de educación
y = df.b$IA
x = df.b$añosedu

logistic.1 <- glm(y ~ x, data = df.b, family = binomial)
plot(logistic.1)
summary(logistic.1)

par(mfrow = c(1, 1))
plot(IA ~ añosedu, data=df.b, xlim = c(0,50))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),add = TRUE)


#IA en relación con número de personas que viven en el hogar
x = df.b$numpeho
logistic.1 <- glm(y ~ x, data = df.b, family = binomial)

summary(logistic.1)

plot(IA ~ numpeho, data=df.b, xlim = c(0,20))
curve(predict(logistic.1, newdata = data.frame(x), type = "response"),add = TRUE)


"6.Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github."



"NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos tus resultados
para poder dar una conclusión final al problema planteado."