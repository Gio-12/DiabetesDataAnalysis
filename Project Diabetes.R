df = read.csv("diabetes.csv") 

#salviamo un backup del dataframe
num.df = df 

#eliminiamo i na/nan 
df = na.omit(df)  

#analizziamo gli imput e output e cambiamo la outcome in factor 
df$Outcome=as.factor(df$Outcome) 
levels(df$Outcome) 

N = nrow(df) 

#Dividiamo il dataset
N.train = 540  
N.val = 114
N.test = 114

train.sample = sample(N, N.train) 
df.train = df[train.sample, ] 
df.test = df[-train.sample, ] 

val.sample = sample(N.test + N.val, N.val)
df.val = df.test[val.sample, ]
df.test = df.test[-val.sample, ]

#faccio si che il sistemi dimentichi df test e validation
df = df.train 
N = nrow(df) 

#matrice di correlazione 
install.packages("corrplot") 
library("corrplot")

cor.matrix = cor(num.df)
corrplot(cor.matrix, method = "circle")

#grafici per avere una visione migliore del dataset 
library(ggplot2)
ggplot(data=df, aes(x=Age, y=Glucose)) + geom_point() + 
  labs(x="Age", y="Glucose", title="GLUCOSE ON AGING PERSON", 
       caption = "Data from library Kaggle.com") + 
  theme(plot.title=element_text(face='bold', colour='sienna', size=18),
        plot.background = element_rect(fill = "gray63"), 
        panel.background = element_rect(fill = "khaki2", colour="black"),
        axis.title = element_text( color="sienna", size=15, face=2),
        axis.text = element_text( angle = 90, color="gray27", size=10, face=2)
  )

ggplot(data=df, aes(x=BMI, y=BloodPressure)) + geom_point() + 
  labs(x="BMI", y="Blood Pressure", title="BODY MASS INDEX WITH BLOOD PRESSURE", 
       caption = "Data from library Kaggle.com") + 
  theme(plot.title=element_text(face='bold', colour='sienna', size=18),
        plot.background = element_rect(fill = "gray63"), 
        panel.background = element_rect(fill = "khaki2", colour="black"),
        axis.title = element_text( color="sienna", size=15, face=2),
        axis.text = element_text( angle = 90, color="gray27", size=10, face=2)
  )

ggplot(data=df, aes(x=Insulin, y=SkinThickness)) + geom_point() + 
  labs(x="Insulin", y="Skin Thickness", title="INSULIN AND SKIN THICKNESS", 
       caption = "Data from library Kaggle.com") + 
  theme(plot.title=element_text(face='bold', colour='sienna', size=18),
        plot.background = element_rect(fill = "gray63"), 
        panel.background = element_rect(fill = "khaki2", colour="black"),
        axis.title = element_text( color="sienna", size=15, face=2),
        axis.text = element_text( angle = 90, color="gray27", size=10, face=2)
  )

ggplot(data=df, aes(x=Age, y=Pregnancies)) + geom_point() +
  labs(x="Age", y="Pregnancies", title="AGE AND PREGNANCIES", 
       caption = "Data from library Kaggle.com") + 
  theme(plot.title=element_text(face='bold', colour='sienna', size=18),
        plot.background = element_rect(fill = "gray63"), 
        panel.background = element_rect(fill = "khaki2", colour="black"),
        axis.title = element_text( color="sienna", size=15, face=2),
        axis.text = element_text( angle = 90, color="gray27", size=10, face=2)
  )

install.packages("e1071")
library(e1071)

#SVM inziamo a mettere le basi per il machine learning 
model.SVM = svm(Outcome ~ ., df.train, kernel="polynomial", cost=1, degree=5) 
summary(model.SVM)

y.pred = predict(model.SVM, df.train)
y.pred

#funzione per il calcolo errore
MR = function(y.pred, y.true) { 
  res = mean(y.pred != y.true)
  return(res)
}

#funzione per il calcolo accuratezza
Acc = function(y.pred, y.true) { 
  Acc = 1 - mean(y.pred != y.true)
  return(Acc)
}

MR.linear = MR(y.pred, df.train$Outcome)
MR.linear

Acc.linear = Acc(y.pred, df.train$Outcome)
Acc.linear

#Come da testo cerchiamo il degree migliore utilizzando il validation set 
MR.total = 1:10 
MR.val = 1:10
for (d in 1:10) {
  model.SVM = svm(Outcome ~ ., df, kernel="polynomial", cost=1, degree = d)
  y.pred = predict(model.SVM, df)
  MR.poly = MR(y.pred, df.val$Outcome)
  MR.total[d] = MR.poly
  
  y.pred=predict(model.SVM, df.test)
  MR.poly=MR(y.pred, df.val$Outcome)
  MR.val[d]=MR.poly
}

plot(MR.total, type='p', xlab = "Degree" , ylim = c(0,1)) 
points(MR.val, type="p", col="red")

#Andiamo a vedere come si comparta il SVM con il test set 
model.SVM = svm(Outcome ~ ., df, kernel="polynomial", cost=1, degree=2) 
summary(model.SVM)

y.pred = predict(model.SVM, df.test)
y.pred

MR.test = MR(y.pred, df.test$Outcome)
MR.test

Acc.test = Acc(y.pred, df.test$Outcome)
Acc.test

#Calcoliamo le probabilità 
SVM.probs = svm(Outcome~ ., data=df, kernel="polynomial", cost=1, degree=2, probability=TRUE)
y.pred = predict(SVM.probs, df.test, probability=TRUE)
y.probs = attr(y.pred, "probabilities")

y.probs = y.probs [, 2] #cambiare in 1 se la curva viene inversa
y.probs

y.total = rep(0,114)
y.total[y.probs > 0.8] = 1
y.total

table(y.pred, df.test$Outcome) 

#ROCR 
install.packages("ROCR")
library(ROCR)
pred = prediction(y.probs, df.test$Outcome)
perf = performance(pred, "tpr", "fpr")
auc = performance(pred, "auc")
auc = auc@y.values[[1]]

plot(perf, colorize=TRUE, main=auc)

#Model selection 

install.packages("e1071")
library(e1071)
model.SVM = svm(Outcome ~ Insulin + Glucose, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ Age + Glucose, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ BMI + Glucose, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ Pregnancies + Age, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ Glucose + BMI + Pregnancies, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ BloodPressure + DiabetesPedigreeFunction + Age, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ DiabetesPedigreeFunction + Insulin + Glucose, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ Pregnancies + Glucose + BloodPressure, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ SkinThickness + Insulin + BMI + DiabetesPedigreeFunction, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

model.SVM = svm(Outcome ~ Insulin + Glucose + BloodPressure + SkinThickness, df, kernel="polynomial", cost=1, degree=2)
y.pred = predict (model.SVM, df.test)
MR(y.pred, df.test$Outcome)
Acc(y.pred, df.test$Outcome)

#Per la parte 10-11 utilizziamo il machine learning diluito 

install.packages("e1071")

Data.CicloFor = data.frame(MR.linear=double(), Accuracy.linear=double(), MR.Test=double(), Accuracy.Test=double())

for (i in 1:10) {df = read.csv("diabetes.csv") #LETTURA DEL FILE E CREAZIONE DATAFRAME
num.df = df # BACKUP DEL DF

df = na.omit(df)  #ELIMINAZIONE NA

df$Outcome=as.factor(df$Outcome) #CAMBIO IN FACTOR DELLA VARIABILE 

N = nrow(df) #INDICIZZAZIONE 

N.train = 540  #SUDDIVISIONE DEL DATASET
N.val = 114
N.test = 114

train.sample = sample(N, N.train) 
df.train = df[train.sample, ] 
df.test = df[-train.sample, ] 

val.sample = sample(N.test + N.val, N.val)
df.val = df.test[val.sample, ]
df.test = df.test[-val.sample, ]

df = df.train #DIMENTICO IL TEST E VALIDATION 
N = nrow(df) #INDICIZZAZIONE

library(e1071)

model.SVM = svm(Outcome ~ ., df.train, kernel="polynomial", cost=1, degree=5) #CREAZIONE DEL SUPPORT VECTOR MACHINE
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

MR.linear = MR(y.pred, df.train$Outcome)
MR.linear

Acc.linear = Acc(y.pred, df.train$Outcome)
Acc.linear

MR.total = 1:10 #D INCOGNITA CERCO D MIGLIORE
MR.val = 1:10
for (d in 1:10) {
  model.SVM = svm(Outcome ~ ., df, kernel="polynomial", cost=1, degree = d)
  y.pred = predict(model.SVM, df)
  MR.poly = MR(y.pred, df.val$Outcome)
  MR.total[d] = MR.poly
  
  y.pred=predict(model.SVM, df.test)
  MR.poly=MR(y.pred, df.val$Outcome)
  MR.val[d]=MR.poly
}

plot(MR.total, type='p', xlab = "Degree" , ylim = c(0,1)) #GRAFICI DI CONFRONTO
points(MR.val, type="p", col="red")

model.SVM = svm(Outcome ~ ., df, kernel="polynomial", cost=1, degree=2) #APPLICO L'SVM AL TEST SET
summary(model.SVM)

y.pred = predict(model.SVM, df.test)
y.pred

MR.test = MR(y.pred, df.test$Outcome)
MR.test

Acc.test = Acc(y.pred, df.test$Outcome)
Acc.test

i = data.frame(MR.linear=c(MR.linear), Accuracy.linear=c(Acc.linear), MR.Test=c(MR.test), Accuracy.Test=c(Acc.test))
print(i)
Data.CicloFor  = rbind(Data.CicloFor , i)
}

Data.CicloFor 

#Calcolo punto 11 

library(ggplot2)

mean(Data.CicloFor$MR.linear)
mean(Data.CicloFor$Accuracy.linear)

median(Data.CicloFor$MR.linear)
median(Data.CicloFor$Accuracy.linear)

quantile(Data.CicloFor$MR.linear)
quantile(Data.CicloFor$Accuracy.linear)

ggplot(data=Data.CicloFor, aes(x=MR.linear))+geom_histogram(bins = 40)
ggplot(data=Data.CicloFor, aes(x=MR.linear))+geom_boxplot()

mean(Data.CicloFor$MR.Test)
mean(Data.CicloFor$Accuracy.Test)

median(Data.CicloFor$MR.Test)
median(Data.CicloFor$Accuracy.Test)

quantile(Data.CicloFor$MR.Test)
quantile(Data.CicloFor$Accuracy.Test)

ggplot(data=Data.CicloFor, aes(x=MR.Test))+geom_histogram(bins = 40)
ggplot(data=Data.CicloFor, aes(x=MR.Test))+geom_boxplot()

#Intervallo di confidenza

ConfidenceInt = function(x,alpha=0.05)
{
  x <- x[!is.na(x)]
  m <- mean(x)
  z <- qnorm(1-alpha/2)
  s <- sd(x)/sqrt(length(x))
  confidence <- c("inf"=m-z*s,"sup"=m+z*s)
  return(confidence)
}
ConfidenceInt(Data.CicloFor$MR.Test)


