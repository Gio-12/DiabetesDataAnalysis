df = read.csv("winequality-red.csv") #LETTURA DEL FILE E CREAZIONE DATAFRAME
num.df = df # BACKUP DEL DF

df.Feauture = df #BACKUP DF PER MODEL SELECTION

df = na.omit(df) #ELIMINAZIONE NA

sapply(colnames(df), function(x) class(df[[x]])) #CONTROLLO VARIABILI

df$quality=as.factor(df$quality) #CAMBIO IN FACTOR DELLA VARIABILE QUALITY

N = nrow(df) #INDICIZZAZIONE 

N.train = 1100 #SUDDIVISIONE DEL DATASET 
N.val = 250
N.test = 249

train.sample = sample(N, N.train) 
df.train = df[train.sample, ] 
df.test = df[-train.sample, ] 

val.sample = sample(N.test + N.val, N.val)
df.val = df.test[val.sample, ]
df.test = df.test[-val.sample, ]

df.TestFeauture = df.test #BACKUP DF PER MODEL SELECTION 

df = df.train #DIMENTICO IL TEST E VALIDATION 
N = nrow(df) #INDICIZZAZIONE

install.packages("corrplot") #PARTE VISIVA
library("corrplot")

cor.matrix = cor(num.df)
corrplot(cor.matrix, method = "circle")

library(ggplot2)
ggplot(data=df, aes(x=alcohol, y=quality)) + geom_point() + 
  labs(x="Alcohol", y="Quality", title="WINE VALUATION OVER % ALCOHOL", 
       caption = "Data from library Kaggle.com") + 
  theme_classic() + 
  theme(axis.title = element_text( color="red4", size=15), axis.text = element_text(color="black", size=10),
        plot.title=element_text(face='italic', colour='red4', size=20),
        plot.background = element_rect(fill = "orange4"), 
        panel.background = element_rect(fill = "darkolivegreen1")
  )

ggplot(data=df, aes(x=pH, y=fixed.acidity)) + geom_point() + 
  labs(x="PH", y="Fixed Acidity", title="FIXED ACIDITY WITH PH", 
       caption = "Data from library Kaggle.com") + 
  theme_classic() + 
  theme(axis.title = element_text( color="red4", size=15), axis.text = element_text( color="black", size=10),
        plot.title=element_text(face='italic', colour='red4', size=20),
        plot.background = element_rect(fill = "orange4"), 
        panel.background = element_rect(fill = "darkolivegreen1")
  )

ggplot(data=df, aes(x=free.sulfur.dioxide, y=total.sulfur.dioxide)) + geom_point() + 
  labs(x="Free Sulfur", y="Total Sulfur", title="FREE AND TOTAL SULFUR", 
       caption = "Data from library Kaggle.com") + 
  theme_classic() +
  theme(axis.title = element_text( color="red4", size=15), axis.text = element_text( color="black", size=10),
        plot.title=element_text(face='italic', colour='red4', size=20),
        plot.background = element_rect(fill = "orange4"), 
        panel.background = element_rect(fill = "darkolivegreen1")
  )

ggplot(data=df, aes(x=sulphates, y=chlorides)) + geom_point() +
  labs(x="Sulphates", y="Chlorides", title="SULPHATES AND CHLORIDES", 
       caption = "Data from library Kaggle.com") + 
  theme_classic() +
  theme(axis.title = element_text( color="red4", size=15), axis.text = element_text( color="black", size=10),
        plot.title=element_text(face='italic', colour='red4', size=20),
        plot.background = element_rect(fill = "orange4"), 
        panel.background = element_rect(fill = "darkolivegreen1")
  )

install.packages("e1071")
library(e1071)
  
model.SVM = svm(quality ~ ., df.train, kernel="radial", cost=2, gamma=0.5) #CREAZIONE DEL SUPPORT VECTOR MACHINE
summary(model.SVM)

y.pred = predict(model.SVM, df.train)
y.pred

MR = function(y.pred, y.true) { #CREAZIONE DELLA FUNCTION PER CALCOLARE L'ERRORE
  res = mean(y.pred != y.true)
  return(res)
}

Acc = function(y.pred, y.true) { #CREAZIONE DELLA FUNCTION PER CALCOLARE L'ACCURATEZZA 
  Acc = 1 - mean(y.pred != y.true)
  return(Acc)
}

MR.linear = MR(y.pred, df.train$quality)
MR.linear

Acc.linear = Acc(y.pred, df.train$quality)
Acc.linear


MR.total = 1:10 #D INCOGNITA CERCO D MIGLIORE
MR.val = 1:10
for (d in 1:10) {
  model.SVM = svm(quality ~ ., df, kernel="radial", cost=2, gamma = 1/d)
  y.pred = predict(model.SVM, df)
  MR.poly = MR(y.pred, df.val$quality)
  MR.total[d] = MR.poly
  
  y.pred=predict(model.SVM, df.test)
  MR.poly=MR(y.pred, df.val$quality)
  MR.val[d]=MR.poly
}

plot(MR.total, type='p', xlab = "gamma" , ylim = c(0,1)) #GRAFICI DI CONFRONTO
points(MR.val, type="p", col="red")

model.SVM = svm(quality ~ ., df, kernel="radial", cost=2, gamma=0.10) #APPLICO L'SVM AL TEST SET 
summary(model.SVM)

y.pred = predict(model.SVM, df.test)
y.pred

MR.test = MR(y.pred, df.test$quality)
MR.test

Acc.test = Acc(y.pred, df.test$quality)
Acc.test

#PARTE DI SOSTITUZIONE DELLA COLONNA QUALITY 6-CLASSI CON TARGET BINARIA

df$quality = as.numeric(df$quality) 
df$target = rep(0, N) 
df$target[df$quality >= 4] = 1  
df$quality = NULL  
df$target = as.factor(df$target) 
    
df.test$quality = as.numeric(df.test$quality)
df.test$target = rep(0, nrow(df.test)) 
df.test$target[df.test$quality >= 4] = 1  
df.test$quality = NULL  
df.test$target = as.factor(df.test$target) 

df.val$quality = as.numeric(df.val$quality)
df.val$target = rep(0, nrow(df.val)) 
df.val$target[df.val$quality >= 4] = 1  
df.val$quality = NULL  
df.val$target = as.factor(df.val$target) 

SVM.probs = svm(target~ ., data=df, kernel="radial", cost=2, gamma=0.10, probability=TRUE) #CREO SVM PER CALCOLARE LA PROBABILITà
y.pred = predict(SVM.probs, df.test, probability=TRUE)
y.probs = attr(y.pred, "probabilities")

y.probs = y.probs [, 1] # (nel caso della curva di roc è al contrario 2 -> 1)
y.probs


y.total = rep(0,249)
y.total[y.probs > 0.5] = 1
y.total


table(y.total, df.test$target) #TABELLA DEI VALORI 

#CURVA DI ROCR 

install.packages("ROCR")
library(ROCR)

pred = prediction(y.probs, df.test$target)
perf = performance(pred, "tpr", "fpr")
auc = performance(pred, "auc")
auc = auc@y.values[[1]]

plot(perf, colorize=TRUE, main=auc)

#MODEL SELECTION DEL PROGETTO (DC. HO PROVATO ALCUNE COMBINAZIONI, SE VUOI FABIO TENTANE ALTRE NON SOLO CON 2 COLONNE, E TIENITI QUELLE CHE HANNO LA RELAZIONE MIGLIORE ERROR PIU BASSO)

library(e1071)

df.Feauture
df.Feauture$quality=as.factor(df.Feauture$quality)
df.TestFeauture

#1
model.SVM = svm(quality ~ citric.acid + alcohol , df.Feauture, kernel="radial", cost=2, gamma=1)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#2
model.SVM = svm(quality ~ alcohol + fixed.acidity , df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#3
model.SVM = svm(quality ~ sulphates + alcohol, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#4
model.SVM = svm(quality ~ volatile.acidity + total.sulfur.dioxide, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#5
model.SVM = svm(quality ~ total.sulfur.dioxide + sulphates, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#6
model.SVM = svm(quality ~ total.sulfur.dioxide + sulphates + chlorides, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#7
model.SVM = svm(quality ~ density + chlorides + sulphates, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#8
model.SVM = svm(quality ~ fixed.acidity + volatile.acidity + citric.acid, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#9
model.SVM = svm(quality ~ residual.sugar + chlorides + density + alcohol, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)
#10
model.SVM = svm(quality ~ total.sulfur.dioxide + free.sulfur.dioxide + pH + citric.acid, df.Feauture, kernel="radial", cost=2, gamma=0.10)
y.pred = predict (model.SVM, df.TestFeauture)
MR(y.pred, df.TestFeauture$quality)
Acc(y.pred, df.TestFeauture$quality)

#ATTENZIONE PARTE 11 DEL PROGETTO AVVIARE SU UN ALTRO SCRIPT

#DATI OTTENUTI DA 10 SIMULAZIONI

install.packages("e1071")

Data.Finale = data.frame(MR.linear=double(), Acc.linear=double(), MR.test=double(), Acc.test=double())

for (i in 1:10) {df = read.csv("winequality-red.csv") #LETTURA DEL FILE E CREAZIONE DATAFRAME
num.df = df # BACKUP DEL DF

df = na.omit(df) #ELIMINAZIONE NA

df$quality=as.factor(df$quality) #CAMBIO IN FACTOR DELLA VARIABILE QUALITY

N = nrow(df) #INDICIZZAZIONE 

N.train = 1100 #SUDDIVISIONE DEL DATASET 
N.val = 250
N.test = 249

train.sample = sample(N, N.train) 
df.train = df[train.sample, ] 
df.test = df[-train.sample, ] 

val.sample = sample(N.test + N.val, N.val)
df.val = df.test[val.sample, ]
df.test = df.test[-val.sample, ]

df = df.train #DIMENTICO IL TEST E VALIDATION 
N = nrow(df) #INDICIZZAZIONE

library(e1071)

model.SVM = svm(quality ~ ., df.train, kernel="radial", cost=2, gamma=0.5) #CREAZIONE DEL SUPPORT VECTOR MACHINE
summary(model.SVM)

y.pred = predict(model.SVM, df.train)
y.pred

MR = function(y.pred, y.true) { #CREAZIONE DELLA FUNCTION PER CALCOLARE L'ERRORE
  res = mean(y.pred != y.true)
  return(res)
}

Acc = function(y.pred, y.true) { #CREAZIONE DELLA FUNCTION PER CALCOLARE L'ACCURATEZZA 
  Acc = 1 - mean(y.pred != y.true)
  return(Acc)
}

MR.linear = MR(y.pred, df.train$quality)
MR.linear

Acc.linear = Acc(y.pred, df.train$quality)
Acc.linear


MR.total = 1:10 #D INCOGNITA CERCO D MIGLIORE
MR.val = 1:10
for (d in 1:10) {
  model.SVM = svm(quality ~ ., df, kernel="radial", cost=2, gamma = 1/d)
  y.pred = predict(model.SVM, df)
  MR.poly = MR(y.pred, df.val$quality)
  MR.total[d] = MR.poly
  
  y.pred=predict(model.SVM, df.test)
  MR.poly=MR(y.pred, df.val$quality)
  MR.val[d]=MR.poly
}

plot(MR.total, type='p', xlab = "gamma" , ylim = c(0,1)) #GRAFICI DI CONFRONTO
points(MR.val, type="p", col="red")

model.SVM = svm(quality ~ ., df, kernel="radial", cost=2, gamma=0.10) #APPLICO L'SVM AL TEST SET 
summary(model.SVM)

y.pred = predict(model.SVM, df.test)
y.pred

MR.test = MR(y.pred, df.test$quality)
MR.test

Acc.test = Acc(y.pred, df.test$quality)
Acc.test

i = data.frame(MR.L=c(MR.linear), Accuracy.L=c(Acc.linear), MR.T=c(MR.test), Accuracy.T=c(Acc.test))
print(i)
Data.Finale = rbind(Data.Finale, i)
}

Data.Finale

#CALCOLO DI VALORI STATISTICI

mean(Data.Finale$MR.L)
mean(Data.Finale$Accuracy.L)

median(Data.Finale$MR.L)
median(Data.Finale$Accuracy.L)

quantile(Data.Finale$MR.L)
quantile(Data.Finale$Accuracy.L)

mean(Data.Finale$MR.T)
mean(Data.Finale$Accuracy.T)

median(Data.Finale$MR.T)
median(Data.Finale$Accuracy.T)

quantile(Data.Finale$MR.T)
quantile(Data.Finale$Accuracy.T)

#GRAFICI DI VALORI STATISTICI

library(ggplot2)
ggplot(data=Data.Finale, aes(x=MR.L))+geom_histogram(bins = 20)
ggplot(data=Data.Finale, aes(x=MR.L))+geom_boxplot()

ggplot(data=Data.Finale, aes(x=MR.T))+geom_histogram(bins = 30)
ggplot(data=Data.Finale, aes(x=MR.T))+geom_boxplot()

#CALCOLO MEDIA E INTERVALLO 

mean(Data.Finale$MR.T)

alpha = 0.05
n = 10
mu = 0.25
sigma = sd (Data.Finale$MR.T)

x = Data.Finale$MR.T
m = mean (x)
m
zalfa = qnorm(1 - alpha /2, 0.1)
c1 = m - zalfa *sigma/sqrt(n)
c2 = m + zalfa *sigma/sqrt(n)
if (mu < c1 | mu > c2) {
  message("L'ipotesi è stata rifiutata")
}else{
  message("L'ipotesi non è stata rifiutata")
}

ConfidenceInt = function(x,alpha=0.05)
{
  x <- x[!is.na(x)]
  m <- mean(x)
  z <- qnorm(1-alpha/2)
  s <- sd(x)/sqrt(length(x))
  confidence <- c("inf"=m-z*s,"sup"=m+z*s)
  return(confidence)
}
ConfidenceInt(Data.Finale$MR.T)

