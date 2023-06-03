#Objective1 
#k-means clustering
#Sanjula Jayawardana w1790259



###########loarding libaries 

library(tidyverse)

library(readxl)

library(NbClust)

library(knitr)

library(tidymodels)

library(flexclust)

library(funtimes)
library(scales)

theme_set(theme_light())

#library(plyr)

#############################################

# Read excel datafile

vehiclesList <- read_excel("E:/2nd year 2nd sem/Machine Learning/Final Submission/vehiclesSheet.xlsx") %>%
  janitor::clean_names() %>%
  mutate(class = as_factor(class))

########### summary of the dataset looks like ###########

summary(vehiclesList)

########### Detecting outliers ############

#########outliers of van

vehiclesList %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Detecting ouliers for: 'van'")


#########outliers of bus

vehiclesList %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Detecting ouliers for: 'bus'")



#########outliers of saab

vehiclesList %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Detecting ouliers for: saab")


#########outliers of opel

vehiclesList %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Detecting ouliers for: opel")

######################


########## filtering the list

bus_data = vehiclesList %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

van_data = vehiclesList %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

opel_data = vehiclesList %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

saab_data = vehiclesList %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

########## data combing

dataCombined = bind_rows(list(bus_data,opel_data,saab_data,van_data)) %>%
  arrange(samples)



print(dataCombined)

dataCombined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Cleared out Outliers for: 'van'")



dataCombined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Cleared out Outliers for: 'bus'")



dataCombined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Cleared out Outliers for: saab")



dataCombined %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Cleared out Outliers for: opel")



############Checking only numerical values

vehicles_non_data = dataCombined %>%
  select(-samples, -class)

############performing scaling the data

vehicles_data_scale = vehicles_non_data %>%
  mutate(across(everything(), scale))

#summary(vehicles_data_scale)



############performing PCA across the data



pc <- princomp(vehicles_data_scale)
plot(pc)
plot(pc, type='l')
summary(pc)


pc <- prcomp(vehicles_data_scale)

# getting First two principal components
comp <- data.frame(pc$x[,1:2])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

#################finding out cluster count

getClusterCount <- function(data, nc=15, seed=1234){
  wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(comp, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
getClusterCount(df)



##############clusering out the dataset for the analyze 

library(cluster)
clusplot(pam(comp,3))





############## Finding out coordinates of each centre for each clustering group


set.seed(1234)
fit.km <- kmeans(vehicles_data_scale, centers=4,  nstart=25)
fit.km$size

fit.km$centers
plot(fit.km$centers)

fit.km2 <- kmeans(vehicles_data_scale, centers=3,  nstart=25)
fit.km2$size

fit.km2$centers
plot(fit.km2$centers)

fit.km3 <- kmeans(vehicles_data_scale, centers=2,  nstart=25)
fit.km3$size

fit.km3$centers
plot(fit.km3$centers)





####################### NBclust function used for kmeans clustering


#######getting distance from euclidean option 

set.seed(123)

model_euclidean = NbClust(comp,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

table(model_euclidean$Best.partition,vehiclesList$class)

barplot(table(model_euclidean$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters")
table(model_euclidean$Best.n[1,])



#confusionMatrix(as.factor(fit.km$cluster), vehicles_original)

#######getting distance from manhattan option 


# Use manhattan for distance

model_manhattan = NbClust(comp,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")

barplot(table(model_manhattan$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters for manhatten distance")
table(model_manhattan$Best.n[1,])


table(model_manhattan$Best.partition,vehiclesList$class)


#######getting distance from maximum option 


model_maximum =NbClust(comp,distance="maximum", min.nc=2,max.nc=15,method="kmeans",index="all")

barplot(table(model_maximum$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters for maximum distance")
table(model_maximum$Best.n[1,])



table(model_maximum$Best.partition,vehiclesList$class)


#############getting exciting datset values for clusters 

km <- kmeans(vehicles_data_scale, 3, nstart = 20)
table(data.frame(vehiclesList$class, km$cluster))


#####################






