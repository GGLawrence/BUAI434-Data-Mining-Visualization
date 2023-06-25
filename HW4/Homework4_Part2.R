###################################
## Cluster Analysis ##
###################################

#NOTE: Prepared with R version 3.6.0

#set the working directory to appropriate folder on your machine, so as to access the data
#files.

#load the required libraries/packages for this chapter
#Install the package(s) below once on your machine. To do so, uncomment the
#install.packages line(s) below.

#install.packages("caret")
library(caret)



## Problem 3 University Ranking
##The dataset on American College and University Rankings contains information on 1302 American colleges and
##universities offering an undergraduate program. For each university, there
##are 17 measurements, including continuous measurements (such as tuition and
##graduation rate) and categorical measurements (such as location by state and
##whether it is a private or public school).
##Note that many records are missing some measurements. Our first goal is to
##estimate these missing values from "similar" records. This will be done by
##clustering the complete records and then finding the closest cluster for
##each of the partial records.
##The missing values will be imputed from the information in that cluster.

# load the data
universities.df <- read.csv("/Users/zeyaoyang/Downloads/Datasets/Universities.csv")
head(universities.df)
str(universities.df)
##First we need to remove all records with missing measurements from the dataset.

# non-missing records
records.missing <- rowSums(is.na(universities.df)) > 0
universities.df.nonmissing <- universities.df[!records.missing, ]

##3.a. For all the continuous measurements, run hierarchical clustering
##using complete linkage and Euclidean distance. Make sure to normalize the
##measurements. From the dendrogram: How many clusters seem reasonable for
##describing these data?
str(universities.df.nonmissing)
# cluster analysis

categorical <- c('State', 'Public..1...Private..2.', 'College.Name')
continuous <-
  setdiff(colnames(universities.df.nonmissing), categorical)
data_normal <-
  as.data.frame(scale(universities.df.nonmissing[, continuous]))
dist <- dist(data_normal, method = 'euclidean')
model <- hclust(dist, method = "complete")
plot(model, cex = 0.4)

# 6 clusters seem reasonable for describing these data.

##3.b. Compare the summary statistics for each cluster and describe each
##cluster ##in this context (e.g., "Universities with high tuition, low acceptance
##rate."). Hint: To obtain cluster statistics for hierarchical clustering, use
##the aggregate() function.

cut <- cutree(model, k = 6)
universities.df.nonmissing['cluster'] <- as.factor(cut)
sum <-
  aggregate(universities.df.nonmissing[continuous], by = universities.df.nonmissing['cluster'], mean)

# Cluster1: Universities with low board and low estimated book costs.
# Cluster2: Universities with low PT.undergrad number, high new student from top10 number, high tuition, high graduation rate,
# Cluster3: Universities with high application received number, high FT. undergrad number.
# Cluster4: Universities with low application number, high board, low addition fee, high estimated book costs, low estimated personal costs.
# Cluster5: Universities with high PT.undergrad number, low in-state tuition, low room, high estimated personal costs.
# Cluster6: Universities with high application number, low out-of-state tuition, high addition fees, high % fac. w/PHD, high stud./fac. ratio.


##3.c. What other external information can explain the contents of some or
##all of these clusters?

# Large differences in admissions, students, tuition, and fees between clusters.
# Cluster 4,5,6 have higher book costs than cluster 1,2,3.




#############################

##problem 4 Marketing to Frequent Fliers.
##The file EastWestAirlinesCluster.csv contains information on 3999 passengers
##who belong to an airline's frequent flier program. For each passenger, the
##data include information on their mileage history and on different ways they
##accrued or spent miles in the last year. The goal is to try to identify
##clusters of passengers that have similar characteristics for the purpose of
##targeting different segments for different types of mileage offers.

##4.a Apply hierarchical clustering with Euclidean distance and Ward's
##method. Make sure to normalize the data first. How many clusters appear?

# load the data
airlines.df <- read.csv("/Users/zeyaoyang/Downloads/EastWestAirlinesCluster.csv")
head(airlines.df)

airlines.df_n <- scale(airlines.df)
d <- dist(airlines.df_n[, 2:12], method = "euclidean")
model_airline <- hclust(d, method = 'ward.D2')
summary(model_airline)
plot(model_airline, cex = 0.4)
abline(h = 80)
# 5 clusters appear.

##4.b. What would happen if the data were not normalized?


# If the data were standardized, the scale of each variable cannot be unified,
#  and the calculation of inter-individual distance would be dominated by the variables with large scale and cause errors




##4.c. Compare the cluster centroid to characterize the different clusters,
##and try to give  each cluster a label.


cut2 <- cutree(model_airline, k = 5)

airlines.df['cluster'] <- as.factor(cut2)
sum_airlines <-
  aggregate(airlines.df[, 2:12], airlines.df['cluster'], mean)

# Cluster1 : Passengers with low balance, low Qual_miles, low both flight and Bonus miles or trans, low days_since_enroll, low award.
# Cluster2 : Passengers with high cc1_miles, high days_since_enroll.
# Cluster3 : Passengers with low cc1_miles, high cc2_miles.
# Cluster4 : Passengers with high balance, high Qual_miles, high bonus_trans, high flight flight miles and trans, high award.
# Cluster5 : Passengers with high cc3_miles, high bonus miles.


##4.d. Use k-means clustering with the number of clusters that you found
##above. Does the same picture emerge?

model_kmeans <- kmeans(x = airlines.df[, 2:12], centers = 5)
centers.df1 <- data.frame(model_kmeans$centers)

centers.df1


# The same picture does not emerge.

