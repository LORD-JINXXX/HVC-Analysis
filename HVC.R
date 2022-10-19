#### HVC Project

## Import data

data = read.csv(file.choose())

View(data)

nrow(data)

library(dplyr)

data_new = na.omit(data %>% select(-X))

nrow(data_new)

data_new$CustomerID

# Recency
# date of analysis

data_new$inv_date = as.Date(data_new$InvoiceDate, "%d-%b-%y")

data_new$inv_date[1:10]

data_new$InvoiceDate[1:10]

date_of_analysis = max(data_new$inv_date) +1

recency = function(x){
  max_date = max(x)
  return(as.integer(date_of_analysis - max_date))
}

recency

# Frequency 
# Monetary 

summary(data_new[c("Quantity","UnitPrice")])

data_new = data_new %>% 
  filter(UnitPrice > 0, Quantity >0)

summary(data_new)

data_new['Total_Price'] = data_new$UnitPrice * data_new$Quantity

data_new

### RFM

rfm_data = data_new %>% 
  group_by(CustomerID) %>% 
  summarise(Recency = recency(inv_date),
            Frequency = n(),
            Monetary = sum(Total_Price))


rfm_data 


##Scaling data

scaled_rfm_data = rfm_data%>% scale()

### K means clustering

library(NbClust)

nbc = NbClust(rfm_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = 'kmeans')

nbc

summary(nbc)


### According to majority role, 6 proposed ideal no of cluster is 2

km2 = kmeans(scaled_rfm_data, 2)

km2

### Since the no of observation in one cluster is significantly huge than the other i.e (1090,3248), again checking for
### the 2nd best no of cluster according to majority rule.

### 6 proposed ideal no of cluster is 3

km3 = kmeans(scaled_rfm_data, 3)

km3


rfm_data_km = rfm_data

rfm_data_km['cluster_label'] = km3$cluster

table(rfm_data_km$cluster_label)


### Aggregate Cluster Profile

profile = rfm_data_km%>%
  select(-CustomerID) %>%
  group_by(cluster_label) %>%
  summarise_all(mean) %>%
  data.frame()

counts = as.vector(table(rfm_data_km$cluster_label))

km_profile = cbind(profile[1], counts, profile[-1])

km_profile



rfm_data_km[rfm_data_km$cluster_label == 1, 'CustomerID']

rfm_data_km[rfm_data_km$cluster_label == 2, 'CustomerID']

rfm_data_km[rfm_data_km$cluster_label == 3, 'CustomerID']

### Plot the Kmeans Cluster

cluster::clusplot(scaled_rfm_data, 
                  rfm_data_km$cluster_label, color = TRUE, labels = 2, main = 'K Means')


factoextra::fviz_cluster(km3 , data = scaled_rfm_data, method = "depth")


### Hierarchical Clustering

dis_mat = dist(scaled_rfm_data, method = 'euclidean')

dis_mat

h_clus = hclust(dis_mat, method = 'centroid')

h_clus

summary(h_clus)

plot(h_clus, labels = rfm_data$CustomerID, hang = -1)

rect.hclust(hclus, 3, border = 'red')

rfm_data_hc = rfm_data

rfm_data_hc['cluster_label'] = cutree(h_clus, k = 3)

rfm_data_hc

### Aggregate Cluster Profile

profile_hc = rfm_data_hc%>%
  select(-CustomerID) %>%
  group_by(cluster_label) %>%
  summarise_all(mean) %>%
  data.frame()

counts = as.vector(table(rfm_data_hc$cluster_label))

hc_profile = cbind(profile_hc[1], counts, profile_hc[-1])

hc_profile


rfm_data_hc[rfm_data_hc$cluster_label == 1, 'CustomerID']

rfm_data_hc[rfm_data_hc$cluster_label == 2, 'CustomerID']

rfm_data_hc[rfm_data_hc$cluster_label == 3, 'CustomerID']

### Plot the Hierarchical Cluster

cluster::clusplot(scaled_rfm_data, 
                  rfm_data_hc$cluster_label, color = TRUE, labels = 2, main = 'Hierarchical')


### Checking silhouette score to find out the best model

sl_mean_km = mean(cluster::silhouette(km3$cluster, dist(scaled_rfm_data))[,3])

sl_mean_hclus = mean(cluster::silhouette(rfm_data_hc$cluster_label, dist(scaled_rfm_data))[,3])



## Silhouette score of Kmeans model is 0.3863765

## Silhouette score of Hierarchical model is 0.9139706

## According to Silhouette score Hierarchical algorithm is providing better clustuering model


### To find out the most valuable customer, let check the models in terms of cluster level with respect to count,

### recency, frequency and monetary value



## Kmeans

km_profile

## Hierarchical

hc_profile

### According to Kmeans model, cluster 1 customers are high valuable customer as the monetary and frequency values
### are higher than other clusters.

rfm_data_km[rfm_data_km$cluster_label == 1, 'CustomerID']

### But according to Hierarchical model, Cluster3 customers are highly valuable customer in terms of monetary value
### than other clusters.

rfm_data_hc[rfm_data_hc$cluster_label == 3, 'CustomerID']

























