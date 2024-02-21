library(readxl)
Dane=read_excel("Projekttest.xlsx", sheet=5,range="A1:E769", col_types = "numeric") #zmie? zakresy


N=length(Dane)-1     #oblicza ilo?? zmiennych obja?niaj?cych
M=2^N-1              #oblicza ilo?? kombinacji 0-1

zm_obj=Dane[,1:N]    #tworzy macierz zmiennych obja?nij?cych
r=cor(zm_obj)               #tworzy macierz korelacji mi?dzy zmiennymi
r=as.matrix(abs(r))         #tworzy warto?ci bezwzgledne i macierz 
R=cor(zm_obj,Dane[,N+1])    #tworzy wektor korelacji Y z kazd? ze zmiennych
R=as.vector(R)              #zapisuje wektor jako wektor
R
r

tab=as.matrix(expand.grid(rep(list(0:1), N)))[-1,]     #tworzy macierz 0-1 kombinacji

colnames(tab)=colnames(Dane)[1:N]
View(tab)
wyniki=matrix(0,M,N)        #tworzy macierz 0 na wyniki cz?stkowe pojemnosci
colnames(wyniki)=colnames(Dane)[1:N]
for(i in 1:M)
{
  for(j in 1:N)
  {
    if(tab[i,j]!=0){wyniki[i,j]=(R[j]^2)/(tab[i,]%*%(as.vector(r[,j])))}
  }
} 

maks=which.max(rowSums(wyniki))
tab[maks,]

#Podgl?d kilku najlepszych wynik?w
wynikiS=cbind(wyniki,0)
wynikiS[,(N+1)]=rowSums(wyniki)
nazwy=colnames(as.data.frame(wyniki))
colnames(wynikiS)=c(nazwy,"hellwigP")

ind=order(wynikiS[,(N+1)],decreasing = TRUE)[1:150] #zwr?ci 15 najleprzych
najlepsze15=wynikiS[ind,]


library(car)

# Badanie symetrii skł. losowego

model <- lm(y ~ x5 + x7, data = Dane)
summary(model)
residuals <- residuals(model)
hist(residuals, main = "Histogram Reszt", xlab = "Reszty")
qqnorm(residuals)
qqline(residuals)
shapiro.test(residuals)

# Badanie losowości skł. losowego

acf(residuals)
Box.test(residuals, lag = 20, type = "Ljung-Box")
plot(residuals)

ks.test(residuals, "pnorm")


# Badanie normalności skł. Losowego

qqnorm(residuals)
qqline(residuals)
hist(residuals, main = "Histogram Reszt", xlab = "Reszty")

result <- shapiro.test(residuals_new)
result
p_value <- result$p.value

if (p_value < 0.05) {
  
  cat("Odrzucamy hipotezę zerową - dane nie pochodzą z rozkładu normalnego.")
  
} else {
  
  cat("Nie ma podstaw do odrzucenia hipotezy zerowej - dane mogą pochodzić z rozkładu normalnego.")
  
}

plot(residuals)
acf(residuals)

# Badanie heteroskedastyczności/homoskedastyczności
fitted_values <- fitted(model)
plot(fitted_values, residuals, main = "Wykres reszt", xlab = "Przewidywane wartości", ylab = "Reszty")
plot(residuals, type = "l", main = "Wykres losowości wariancji", ylab = "Reszty")

library(lmtest)

# Test Breuscha-Pagana
bptest(model)

# Test Goldfelda-Quandta
gqtest(model)


# Badanie autokorelacji skł. losowego

acf(residuals)
Box.test(residuals, lag = 20, type = "Ljung-Box")
plot(residuals)
cor(residuals[-length(residuals)], residuals[-1])

library(lmtest)

# Test Breuscha-Godfreya
bgtest(model)

# Statystyka Durbin-Watsona
durbinWatsonTest(model)

# Eliminacja autokorelacji poprzez dodanie lagów do modelu
model_with_lags <- lm(y ~ x5 + x7 + lag(residuals, 1), data = Dane)

residuals_new <- residuals(model_with_lags)
acf(residuals_new)

plot(residuals_new, type = "l", main = "Wykres losowości wariancji", ylab = "Reszty")

hist(residuals_new, main = "Histogram Reszt", xlab = "Reszty")

qqnorm(residuals_new)
qqline(residuals_new)



#Badanie autokorelacji skł. losowego

# Test Breuscha-Godfreya
bgtest(model_with_lags)

# Statystyka Durbin-Watsona
durbinWatsonTest(model_with_lags)








library(Metrics)

Y_values=read_excel("Projekttest.xlsx", sheet=7,range="D1:E769", col_types = "numeric")


# Mean Absolute Error (MAE)
mae <- mean(abs(Y_values$y - Y_values$`y^`))
print(paste("MAE:", mae))

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((Y_values$y - Y_values$`y^`)^2))
print(paste("RMSE:", rmse))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((Y_values$y - Y_values$`y^`) / Y_values$y)) * 100
print(paste("MAPE:", mape))

# Root Mean Squared Percentage Error (RMSPE)
rmspe <- sqrt(mean(((Y_values$y - Y_values$`y^`) / Y_values$y)^2)) * 100
print(paste("RMSPE:", rmspe))


aic_value <- AIC(model)
aicc_value <- AIC(model) + 2 * (length(coef(model)) + 1) * (nobs(model) / (nobs(model) - length(coef(model)) - 1))

print(paste("AIC:", aic_value))
print(paste("AICC:", aicc_value))


