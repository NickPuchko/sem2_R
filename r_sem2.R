#ввод данных 
library(readxl)
library(psych)
install.packages("git")

matrix_data <- as.matrix(read_excel("test_cor.xlsx", range = "B1:D10", col_names = FALSE)) 
data <- as.numeric(matrix_data) 
deathrate <- data[1:10] 
healthcare <- data[11:20] 
fruits_consumption <- data[21:30] 

#Проверка значимости корреляции при p-уровне значимости 0.05 
cor.test(healthcare, deathrate) 
cor.test(fruits_consumption, deathrate) 
#Нулевая гипотеза о нулевой корреляции в обоих случаях отклоняется => есть существенная зависимость 

#Отступы данных близки к нормальным => можем строить корреляционную модель 
qqplot(healthcare, deathrate) 
qqplot(fruits_consumption, deathrate) 

#Регрессионая модель на полях корреляции 
scatter.smooth(x = healthcare, y = deathrate, main = "healthcare ~ deathrate") 
scatter.smooth(x = fruits_consumption, y = deathrate, main = "fruits consumption ~ deathrate") 

#Коэффициенты ковариации 
cov(healthcare, deathrate) 
cov(fruits_consumption, deathrate) 
#Матрица ковариации 
cova <- cov(matrix_data) 

#Матрица корреляции (получена из матрицы ковариации) 
cov2cor(cova)

#Оценим корректность корреляции с помощью таблицы  частных корреляций
pairs.panels(matrix(data, ncol = 3),
             method = "pearson",
             hist.col = "cornflowerblue",
             density = T, ellipses = F)
#Очевидно, что существует зависимость между двумя независмыми переменными (предикторами)
# =>  Необходимо выбрать более точную модель

#Оценка  корректности моделей корреляции
summary(lm(healthcare + fruits_consumption ~ deathrate))
summary(lm(healthcare ~ deathrate))
summary(lm(fruits_consumption ~ deathrate))
#Исправленный коэффициент детерминации оказался наибольшим при использовании только столбца healthcare 
# => при построении регрессионной модели показазатель fruits_consumption лучше не учитывать

#Финальная наиболее точная модель регрессии
scatter.smooth(healthcare, deathrate)
