library(psych)
library(Amelia)
require(Amelia)
library(car)
library(moments)
library(ggplot2)
library(corrplot)
library(gplots)
library(PerformanceAnalytics)
library(FactoMineR)
library(MASS)
library(plyr)
library(knitr)
library(GGally)
library(Metrics)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ggthemes)
library(caret)
library(RWeka)
library(shiny)


#IMPORTAZIONE DATASET
case.df <- read.csv("Case.csv")
View(case.df)

#PRIME INFORMAZIONI SUL DATASET
str(case.df)
head(case.df)
tail(case.df)
summary(case.df)
describe(case.df)
some(case.df) # esaminare una serie di righe casualmente

#PULIZIA DATI
missmap(case.df, 
        col = c("red", "blue"),
        main = "Mappa su eventuali missing values")

is.na(case.df)
any(is.na(case.df)) #non ci sono valori NA

case.df$date = NULL #tolta la colonna data in quanto poco utile
case.df$street = NULL #tolta la colonna data in quanto poco utile
case.df$view = NULL #tolta la colonna data in quanto poco utile
case.df$floors = NULL #tolta la colonna data in quanto poco utile
case.df$country = NULL #tolta la colonna data in quanto poco utile


case.df <- subset(case.df, price!= 0.0) #elimino le case con prezzo pari a 0

missmap(case.df, 
        col = c("red", "blue"),
        main = "Mappa su eventuali missing values")

str(case.df)
kable(summary(case.df))
#dataset pulito composto da 4551 oggetti
dev.off()

#EXPLORATORY ANALYSIS


#PREZZI
plot(case.df$price,type="p", xlab = "Numero casa osservata",
     ylab = "Prezzo in $", main = "Line Chart dei prezzi delle case")

plot(case.df$price,type="p", xlab = "Numero casa osservata", 
     ylab = "Prezzo in $", main = "Line Chart dei prezzi delle case",
     ylim = c(0, 1200000))

price.table <- table(case.df$price, deparse.level = 2)
plot(price.table)
plot(price.table, xlim = c(0, 1600000))

#eliminate le case con prezzo superiore a 1100000
case.df <- subset(case.df, price <= 1100000) 

case.df$price <- as.numeric(as.character(case.df$price))

hist(case.df$price, main = "Istogramma dei prezzi", xlab= "Prezzo",
     ylab = "Numero di case con quel prezzo", breaks = 50, ylim = c(0, 200))
price_q <- quantile(case.df$price)
abline(v=price_q[1], col="red", lwd=2) # 0% (min)
abline(v=price_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=price_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=price_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=price_q[5], col="red", lwd=2) # 100% (max)
density_price <- density(case.df$price)
density_price$y <- density_price$y * 70000000
lines(density_price, col = "red", lwd = 2, type = "l")
price.mean <- mean(case.df$price)
abline(v = price.mean, col = "orange", lwd = 2)

boxplot(case.df$price, xlab= "Prezzo", horizontal = T, 
        main ="Distribuzione dei prezzi")

price.sd <- sd(case.df$price)
price.var <- var(case.df$price) [1]
price.ks <- kurtosis(case.df$price)
price.sk <- skewness(case.df$price)

qqnorm(case.df$price, main = "qqplot dei prezzi")
qqline(case.df$price)


#CAMERE DA LETTO
hist(case.df$bedrooms, main = "Istogramma delle camere da letto",
     xlab= "Numero di camere", ylab = "Frequenza", breaks = 7)
axis(side=1, at=seq(0,8, by=1))
bed_q <- quantile(case.df$bedrooms)
abline(v=bed_q[1], col="red", lwd=2) # 0% (min)
abline(v=bed_q[2], col="blue", lwd=2) # 1st quartile 25%, coincide con 2nd 
abline(v=bed_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=bed_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=bed_q[5], col="red", lwd=2) # 100% (max)
bed.mean <- mean(case.df$bedrooms)
abline(v = bed.mean, col = "orange", lwd = 2)

boxplot(case.df$bedrooms, xlab= "Numero di camere da letto", horizontal = T,
        main ="Distribuzione del numero di camere da letto")

bed.sd <- sd(case.df$bedrooms)
bed.var <- var(case.df$bedrooms) [1]
bed.ks <- kurtosis(case.df$bedrooms)
bed.sk <- skewness(case.df$bedrooms)

qqnorm(case.df$bedrooms, main = "qqplot numero di camere da letto")
qqline(case.df$bedrooms)


#BAGNI
hist(case.df$bathrooms, main = "Istogramma dei bagni", xlab= "Numero di bagni",
     ylab = "Frequenza", breaks = 7)
bath_q <- quantile(case.df$bathrooms)
abline(v=bath_q[1], col="red", lwd=2) # 0% (min)
abline(v=bath_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=bath_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=bath_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=bath_q[5], col="red", lwd=2) # 100% (max)
bath.mean <- mean(case.df$bathrooms)
abline(v = bath.mean, col = "orange", lwd = 2)

boxplot(case.df$bathrooms, xlab= "Numero di bagni", horizontal = T,
        main ="Distribuzione del numero di bagni")

bath.sd <- sd(case.df$bathrooms)
bath.var <- var(case.df$bathrooms) [1]
bath.ks <- kurtosis(case.df$bathrooms)
bath.sk <- skewness(case.df$bathrooms)

qqnorm(case.df$bathrooms, main = "qqplot numero di bagni")
qqline(case.df$bathrooms)


#SUPERFICIE ABITABILE
hist(case.df$sqft_living, main = "Istogramma superficie abitabile", 
     xlab= "Numero di piedi quadrati", ylab = "Frequenza", breaks = 50)
liv_q <- quantile(case.df$sqft_living)
abline(v=liv_q[1], col="red", lwd=2) # 0% (min)
abline(v=liv_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=liv_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=liv_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=liv_q[5], col="red", lwd=2) # 100% (max)
liv.mean <- mean(case.df$sqft_living)
abline(v = liv.mean, col = "orange", lwd = 2)

boxplot(case.df$sqft_living, xlab= "Superficie abitabile", horizontal = T,
        main ="Distribuzione della superficie abitabile")

liv.sd <- sd(case.df$sqft_living)
liv.var <- var(case.df$sqft_living) [1]
liv.ks <- kurtosis(case.df$sqft_living)
liv.sk <- skewness(case.df$sqft_living)

qqnorm(case.df$sqft_living, main = "qqplot superficie abitabile")
qqline(case.df$sqft_living)


#SUPERFICIE LOTTO
hist(case.df$sqft_lot, main = "Istogramma superficie lotto", 
     xlab= "Numero di piedi quadrati", ylab = "Frequenza", breaks = 500)
hist(case.df$sqft_lot, main = "Istogramma superficie lotto", 
     xlab= "Numero di piedi quadrati", ylab = "Frequenza", breaks = 1000, 
     xlim = c(0,25000))
lot_q <- quantile(case.df$sqft_lot)
abline(v=lot_q[1], col="red", lwd=2) # 0% (min)
abline(v=lot_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=lot_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=lot_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=lot_q[5], col="red", lwd=2) # 100% (max)
lot.mean <- mean(case.df$sqft_lot)
abline(v = lot.mean, col = "orange", lwd = 2)

boxplot(case.df$sqft_lot, xlab= "Superficie lotto", horizontal = T,
        main ="Distribuzione della superficie del lotto", outline = TRUE)
boxplot(case.df$sqft_lot, xlab= "Superficie lotto", horizontal = T,
        main ="Distribuzione della superficie del lotto", outline = FALSE)


lot.sd <- sd(case.df$sqft_lot)
lot.var <- var(case.df$sqft_lot) [1]
lot.ks <- kurtosis(case.df$sqft_lot)
lot.sk <- skewness(case.df$sqft_lot)

qqnorm(case.df$sqft_lot, main = "qqplot dimensione superficie lotto")
qqline(case.df$sqft_lot)
qqnorm(case.df$sqft_lot, main = "qqplot dimensione superficie lotto", ylim = c(0,25000))
qqline(case.df$sqft_lot)



#CASE VICINO AL MARE
hist(case.df$waterfront, main = "Istogramma case posizionate vicino al mare", 
     xlab= "Vicinanza al mare", ylab = "Frequenza")
hist(case.df$waterfront, main = "Istogramma case posizionate vicino al mare", 
     xlab= "Vicinanza al mare", ylab = "Frequenza", ylim = c(0, 20))
tab_vicmar <- table(case.df$waterfront)
pie(tab_vicmar, main = "Case vicino al mare")


#CONDIZIONE
hist(case.df$condition, main = "Istogramma condizione delle case", 
     xlab= "Condizione (da 1 a 5)", ylab = "Frequenza", ylim = c(0, 3000))
cond_q <- quantile(case.df$condition)
abline(v=cond_q[1], col="red", lwd=2) # 0% (min)
abline(v=cond_q[2], col="blue", lwd=2) # 1st quartile 25%, coincide con 2nd 
abline(v=cond_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=cond_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=cond_q[5], col="red", lwd=2) # 100% (max)
cond.mean <- mean(case.df$condition)
abline(v = cond.mean, col = "orange", lwd = 2)
tab_cond <- table(case.df$condition)
pie(tab_cond, main = "Condizione delle case")

boxplot(case.df$condition, xlab= "Condizione della casa", horizontal = T,
        main ="Distribuzione delle condizioni")

cond.sd <- sd(case.df$condition)
cond.var <- var(case.df$condition) [1]
cond.ks <- kurtosis(case.df$condition)
cond.sk <- skewness(case.df$condition)

qqnorm(case.df$condition, main = "qqplot condizione della casa")
qqline(case.df$condition)


#SUPERFICIE PIANO TERRA
hist(case.df$sqft_above, main = "Istogramma superficie piano terra", 
     xlab= "Superficie in piedi quadrati", ylab = "Frequenza", breaks = 50)
ab_q <- quantile(case.df$sqft_above)
abline(v=ab_q[1], col="red", lwd=2) # 0% (min)
abline(v=ab_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=ab_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=ab_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=ab_q[5], col="red", lwd=2) # 100% (max)
ab.mean <- mean(case.df$sqft_above)
abline(v = ab.mean, col = "orange", lwd = 2)

boxplot(case.df$sqft_above, xlab= "Superficie piano terra", horizontal = T,
        main ="Distribuzione della superficie al piano terra")

ab.sd <- sd(case.df$sqft_above)
ab.var <- var(case.df$sqft_above) [1]
ab.ks <- kurtosis(case.df$sqft_above)
ab.sk <- skewness(case.df$sqft_above)

qqnorm(case.df$sqft_above, main = "qqplot superficie piano terra")
qqline(case.df$sqft_above)


#ANNO COSTRUZIONE
hist(case.df$yr_built, main = "Istogramma anno di costruzione", 
     xlab= "Anno di costruzione", ylab = "Frequenza", breaks = 50)
built_q <- quantile(case.df$yr_built)
abline(v=built_q[1], col="red", lwd=2) # 0% (min)
abline(v=built_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=built_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=built_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=built_q[5], col="red", lwd=2) # 100% (max)
built.mean <- mean(case.df$yr_built)
abline(v = built.mean, col = "orange", lwd = 2)

boxplot(case.df$yr_built, xlab= "Anno di costruzione", horizontal = T,
        main ="Distribuzione delle date di costruzione")

built.sd <- sd(case.df$yr_built)
built.var <- var(case.df$yr_built) [1]
built.ks <- kurtosis(case.df$yr_built)
built.sk <- skewness(case.df$yr_built)

qqnorm(case.df$yr_built, main = "qqplot anno di costruzione")
qqline(case.df$yr_built)
#siccome la media e' 1971 circa considerate vecchie (0) le case costruite prima 
#del 1971, nuove quelle costruite dopo

for (i in 1:4283){
  if(case.df$yr_built[i] < 1971){
    case.df$yr_built[i]=0
  }
  else case.df$yr_built[i] = 1
}

hist(case.df$yr_built, main = "Istogramma anno di costruzione", 
     xlab= "Superficie in piedi quadrati", ylab = "Frequenza", breaks = 50, 
     ylim = c(0, 2500))

tab_costr <- table(case.df$yr_built)
pie(tab_costr, main ="Case vecchie e case nuove")


#ANNO RISTRUTTURAZIONE
hist(case.df$yr_renovated, main = "Istogramma anno di ristrutturazione", 
     xlab= "Anno di ristrutturazione", ylab = "Frequenza", breaks = 50)
hist(case.df$yr_renovated, main = "Istogramma anno di ristrutturazione", 
     xlab= "Anno di ristrutturazione", ylab = "Frequenza", breaks = 1000, 
     xlim = c(1930, 2020), ylim = c(0,250))

#si puo' notare che molte case non sono state ristrutturate, quindi vengono  
#divise le case non ristrutturate (0) dalle case ristrutturate (1)
for (i in 1:4283){
  if(case.df$yr_renovated[i] > 0){
    case.df$yr_renovated[i]= 1
  }
}

hist(case.df$yr_renovated, main = "Istogramma anno di ristrutturazione", 
     xlab= "Anno di ristrutturazione", ylab = "Frequenza", breaks = 50, 
     ylim = c(0,2900))

tab_ristr <- table(case.df$yr_renovated)
pie(tab_ristr, main ="Case ristrutturate e non")


#DIMENSIONE PIANO INTERRATO
hist(case.df$sqft_basement, main = "Istogramma dimensione piano interrato", 
     xlab= "Dimensione piano interrato", ylab = "Frequenza", breaks = 50)
hist(case.df$sqft_basement, main = "Istogramma dimensione piano interrato", 
     xlab= "Dimensione piano interrato", ylab = "Frequenza", breaks = 50, 
     ylim = c(0, 150))

#si puo' notare che molte case non hanno il piano interrato, quindi vengono  
#divise le case senza piano interrato (0) dalle case con il piano interrato (1)
for (i in 1:4283){
  if(case.df$sqft_basement[i] > 0){
    case.df$sqft_basement[i]= 1
  }
}

hist(case.df$sqft_basement, main = "Istogramma dimensione piano interrato", 
     xlab= "Dimensione piano interrato", ylab = "Frequenza", breaks = 50, 
     ylim = c(0,2900))

tab_interrato <- table(case.df$sqft_basement)
pie(tab_interrato, main = "Case con e senza piano interrato")


#CORRELAZIONE TRA VARIABILI
corrplot.mixed(corr=cor(case.df[,c(1:11)],
                        use="complete.obs"),upper="circle", tl.pos="lt",
               lower.col = colorpanel(50, "red", "grey", "blue"),
               upper.col = colorpanel(50, "red", "grey", "blue"),
               tl.col = "black")

corrplot(cor(case.df[,c(4,5,8)]), method="pie", type = "lower", tl.col="black",
         tl.srt=45, addCoef.col = "black", diag = T, number.cex = 1.2)

pairs(cbind(case.df$sqft_living, case.df$sqft_above), labels = c("sqft_living",
                                                                 "sqft_above"))
#lm(case.df$sqft_above ~ case.df$sqft_living)


#trasformate in num le ultime due colonne
case.df$statezip <- as.numeric(case.df$statezip)
case.df$city <- as.numeric(case.df$city)

corrplot(cor(case.df[,c(1,12,13)]), method="color", type = "lower", tl.col="black",
         tl.srt=45, addCoef.col = "black", diag = T, number.cex = 1.2)

pairs(cbind(case.df$price, case.df$statezip), labels = c("price", "CAP"))
pairs(cbind(case.df$price, case.df$city), labels = c("price", "city"))

cor(case.df$price, case.df$statezip)
cor(case.df$price, case.df$city)

correlation_matrix <- cor(case.df)
heatmap(correlation_matrix,  main = "Heatmap della matrice di correlazione", 
        symm = F)

PCA.case<-PCA(case.df[,c(1:13)],quanti.sup = 1,graph = FALSE)
plot(PCA.case,choix='varcor')

# scatterplotMatrix(formula = ??? price + bedrooms + bathrooms +
#                     + sqft_living + sqft_lot +
#                     + condition + yr_built +
#                     + yr_renovated + city, 
#                    data=case.df, diagonal="histogram")


#rimossa una delle due colonne tra sqft_living e sqft_above in quanto
#essendo molto correlate ci sono molti valori duplicati
case.df$sqft_above = NULL

#ML ALGORITHM

# Divisione in training e test set 
# 80% Training, 3426 entries
# 20% Test, 857 entries

train_length <- round(0.80 * nrow(case.df))
train_label <- sample(1:nrow(case.df), train_length)
training_val <- case.df[train_label, ]
test_val <- case.df[-train_label, ]

#REGRESSIONE LINEARE MULTIVARIATA

#modello iniziale
InitFormula = price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
              + waterfront + condition + sqft_basement + yr_built +
              + yr_renovated + city + statezip
class(InitFormula)

CaseReg_train = lm(formula = InitFormula, data = training_val)
summary(CaseReg_train)

CaseReg_train$coefficients
#plot(CaseReg_train)

#adjusted r squared
adjusted_rsquared <- summary(CaseReg_train)$adj.r.squared

Caseprediction <-predict(CaseReg_train, newdata = test_val)

case_residuals <- test_val$price - Caseprediction

# Calcolo del Mean Absolute Error e dell Root Mean Square Error
MAE_Casereg <- mae(test_val$price, Caseprediction)
#il MAE puo' anche essere calcolato cosi'
#MAE_Casereg <- mean(abs(case_residuals))
MAE_Casereg
RMSE_Casereg <- rmse(test_val$price, Caseprediction)
RMSE_Casereg
#modello improved, eliminati yr_renovated, sqft_basement 
#in quanto poco significativi
ImprovedFormula = price ~ bedrooms + bathrooms + sqft_living + condition +  
                  + waterfront + sqft_lot + yr_built + city + statezip
class(ImprovedFormula)

CaseReg_train2 = lm(formula = ImprovedFormula, data = training_val)
summary(CaseReg_train2)

CaseReg_train2$coefficients
#plot(CaseReg_train2)

adjusted_rsquared2 <- summary(CaseReg_train2)$adj.r.squared

Caseprediction2 <-predict(CaseReg_train2, newdata = test_val)

case_residuals2 <- test_val$price - Caseprediction2

# Calcolo del Mean Absolute Error e dell Root Mean Square Error
MAE_Casereg2 <- mae(test_val$price, Caseprediction2)
#il MAE puo' anche essere calcolato cosi'
# MAE_Casereg2 <- mean(abs(case_residuals2))
MAE_Casereg2
RMSE_Casereg2 <- rmse(test_val$price, Caseprediction2)
RMSE_Casereg2

#CaseReg_train ha un adjusted R-squared piu' vicino a 1 rispetto a quello di 
#CaseReg_train2 quindi il primo modello e' migliore del secondo

#ALBERI DI REGRESSIONE
par(mfrow = c(1,1))

fit <- rpart(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
               + waterfront + condition + sqft_basement + yr_built +
               + yr_renovated + city + statezip, data = training_val) 
summary(fit)

rpart.plot(fit, digits = 2, fallen.leaves = FALSE, type = 3, 
           extra = 101, cex=0.9, 
           main = "Albero di regressione per predire i prezzi delle case")

#vedere quale sia la variabile piu' importante
importanza <- as.data.frame(cbind(fit$variable.importance))
importanza <- tibble::rownames_to_column(importanza, "x")
names(importanza) <- c("Variabili", "Valore")
ggplot(importanza, aes(Variabili, Valore))+
geom_segment(aes(xend=Variabili, yend=0))+
coord_flip()+
geom_point(size=20, color="black", fill = "yellow", shape = 21)+
labs(x = "", y = "Importanza della variabile")+
theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14))


fit_pred <- predict(fit, test_val)
MAE_albero <- mae(fit_pred, test_val$price)
MAE_albero
RMSE_albero <- rmse(fit_pred, test_val$price)
RMSE_albero

#test accuratezza dell'albero
#confusion.matrix = table(test_val$price, fit_pred)
#sum(diag(confusion.matrix))/sum(confusion.matrix)

#vedere a quale valore di size tree corrisponde l'errore minimo
plotcp(fit)
printcp(fit) # training error, cross validation error, std dev

#albero potato per evitare overfitting
prunedTree = prune(fit, cp=0.01)
fancyRpartPlot(prunedTree)

#WEKA CLASSIFIER
#https://www.rdocumentation.org/packages/RWeka/versions/0.1-0/topics/Weka_classifier_trees
#VALUTARE SE METTERLO
# M5P Regression Tree
fit_m5p <- M5P(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                 + waterfront + condition + sqft_basement + yr_built +
                 + yr_renovated + city + statezip, data = training_val)
summary(fit_m5p)
pred_m5p <- predict(fit_m5p, test_val)

MAE_m5p <- mae(pred_m5p, test_val$price)
MAE_m5p
RMSE_m5p <- rmse(pred_m5p, test_val$price)
RMSE_m5p

#####
#J48 e LMT non funzionano
#Error in .jcall(o, "Ljava/lang/Class;", "getClass") : 
#weka.core.UnsupportedAttributeTypeException: weka.classifiers.trees.J48: 
#Cannot handle numeric class!
# #J48
# fit_J48 <- J48(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
#                  + waterfront + condition + sqft_basement + yr_built +
#                  + yr_renovated + city + statezip, data = training_val)
# summary(fit_J48)
# pred_J48 <- predict(fit_J48, test_val)
# 
# MAE_J48 <- mae(pred_J48, test_val$price)
# RMSE_J48 <- rmse(pred_J48, test_val$price)
# 
# #LMT
# fit_LMT <- LMT(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
#                  + waterfront + condition + sqft_basement + yr_built +
#                  + yr_renovated + city + statezip, data = training_val)
# summary(fit_LMT)
# pred_LMT <- predict(fit_LMT, test_val)
# 
# MAE_LMT <- mae(pred_LMT, test_val$price)
# RMSE_LMT <- rmse(pred_LMT, test_val$price)


#altri metodi di cross validation per validare il modello migliore
#cross validation k-fold
set.seed(123)
train.control_k <- trainControl(method = "cv", number = 10)
model_k <- train(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                   + waterfront + condition + sqft_basement + yr_built +
                   + yr_renovated + city + statezip, 
                   data = case.df, method = "lm", trControl = train.control_k)
print(model_k)

#repeated cross validation
set.seed(123)
train.control_krep <- trainControl(method = "repeatedcv", number = 10, 
                                   repeats = 3)
model_krep <- train(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                      + waterfront + condition + sqft_basement + yr_built +
                      + yr_renovated + city + statezip, data = case.df,
                    method = "lm", trControl = train.control_krep)
print(model_krep)

#loocv (Leave one out cross validation)
set.seed(123)
train.control_loocv <- trainControl(method = "LOOCV")
model_loocv <- train(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + 
                       + waterfront + condition + sqft_basement + yr_built +
                       + yr_renovated + city + statezip, data = case.df,
                     method = "lm", trControl = train.control_loocv)

print(model_loocv)


#WEB APPLET
source(paste(getwd(), "Server.R", sep="/"))
source(paste(getwd(), "UI.R", sep="/"))

shinyApp(ui = ui, server = server)   
