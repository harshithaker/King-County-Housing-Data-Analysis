kc_data = read.csv('/Users/harshithaker/Desktop/IMT 572/kc_house_data.csv')
library(tidyverse)
library(lubridate)
install.packages('geosphere')
library(geosphere)
install.packages('geodist')
library(geodist)
install.packages('corrplot')
library(caret)
library(neuralnet)

str_replace(kc_data$date, 'T000000', '')

kc_data$date = as.Date(kc_data$date, format = '%Y%m%d')
view(kc_data)
mean(kc_data$price)
kc_zip = kc_data %>% filter(zipcode == 98034)
mean(kc_zip$price)
sd(kc_data$price)

kc_data$yr_sold = year(kc_data$date)
kc_data$age = kc_data$yr_sold - kc_data$yr_built
kc_data$renovated = ifelse(((kc_data$yr_sold - kc_data$yr_renovated) <= 10) | (kc_data$age <= 5), 1, 0)


#geodist( x,
#y,
#paired = FALSE,
#sequential = FALSE,
#pad = FALSE,
#measure = "cheap"
#)

kc_dist_df = data.frame(lat = 47.6062, long = -122.3321)
  
for(i in 1:nrow(kc_data)){
  vec <- c(47.6062, -122.3321)
  kc_dist_df[i,] <- vec
}
kc_dist_df  

latitude = kc_data$lat
longitude = kc_data$long

kc_data_dist_df = data.frame(latitude, longitude)

kc_data$dist = geodist(kc_dist_df, kc_data_dist_df, paired = TRUE, 
                       sequential = FALSE, pad = FALSE, measure = "haversine")
view(kc_data)   
  
#renovated, dist, age, 
write.csv(kc_data,'/Users/harshithaker/Desktop/IMT 572/kc_house_data.csv', row.names = TRUE)

nrow(kc_data)

kc_data = kc_data %>% filter(bedrooms <= 11)

cor(kc_data)

kc_corr = kc_data %>% select(price,bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, grade, 
                             sqft_above,sqft_living15, sqft_lot15, age, renovated,dist)

corr = cor(kc_corr)

library(corrplot)

corrplot(corr, type = "lower", order = "hclust", 
         tl.col = "black", tl.srt = 45)

kc_data_model = lm(price ~ sqft_living + grade + bathrooms + dist + view - 1, kc_data)
summary(kc_data_model)
plot(kc_data_model)


#supervised clustering 11.28.21
#single = bedrooms <= 1, bathrooms <= 1
#small family = 1 < bedrooms <= 4, 1 < bathrooms <= 4.5
#big family = bedrooms > 4, bathrooms > 4.5


#kc_data_class = kc_data
#if(kc_data_class$bedrooms <= 1 & kc_data_class$bathrooms <= 1){
#  kc_data_class$fam_size = 0
#} else if(1 < kc_data_class$bedrooms <= 4 & 1 < kc_data_class$bathrooms <= 4.5){
#  kc_data_class$fam_size = 1
#} else if(kc_data_class$bedrooms > 4 & kc_data_class$bathrooms > 4.5){
#  kc_data_class$fam_size = 2
#}

#kc_data_class$fam_size = ifelse(kc_data_class$bedrooms <= 1 & kc_data_class$bathrooms <= 1, 0, 
#                                ifelse((kc_data_class$bedrooms <= 4 & kc_data_class$bedrooms > 1) & (kc_data_class$bedrooms > 1 & kc_data_class$bathrooms <= 4.5), 1,
#                                       ifelse(kc_data_class$bedrooms > 4 & kc_data_class$bathrooms > 4.5, 2, 5)))

#kc_data_class %>% filter(fam_size == 5) %>% nrow()
#view(kc_data_class)

waterfront_model = glm(kc_data$waterfront ~ kc_data$view, family = 'binomial')
summary(waterfront_model)

kc_data$waterfront_predict = predict(waterfront_model)
boxplot(kc_data$waterfront_predict ~ kc_data$waterfront)

threshold_wf = -5
kc_data$wf = ifelse(kc_data$waterfront_predict <= threshold_wf, 0, 1)

confusionMatrix(as.factor(kc_data$waterfront),
                as.factor(kc_data$wf))

view(kc_data)

#unsupervised clustering
library(cluster)
kc_cluster_data = kc_data %>% select(bedrooms,bathrooms,sqft_living,floors,waterfront,price)  

set.seed(1234)

kc_cluster_data$cluster = kmeans(kc_cluster_data, 3)$cluster
clusplot(kc_cluster_data, kc_cluster_data$cluster)
view(kc_cluster_data)
kmeans_clust = kc_cluster_data
summary(kc_cluster_data %>% filter(cluster == 1))
summary(kc_cluster_data %>% filter(cluster == 2))
summary(kc_cluster_data %>% filter(cluster == 3))
distance = dist(kc_cluster_data[,1:6])
cl = hclust(distance)
plot(cl)
scatter.smooth(cl)

(x1 - mean(x1))/sd(x1)
(y1 - mean(y1))/sd(y1)
scale(x1)
scale(y1)

a = scale(kc_data$sqft_living)
b = scale(kc_data$grade)
c = scale(kc_data$bathrooms)
d = scale(kc_data$dist)
e = scale(kc_data$view)

f = scale(kc_data$price)

kc_data_scaled = data.frame(a, b, c, d, e, f)

kc_neural_model = neuralnet(f ~ a + b, kc_data_scaled, hidden = c(5,5,5,5), threshold = 2.5)
summary(kc_neural_model)

plot(kc_neural_model)







