############### This is data for Fig 3A pre-HSCT ###############
library(philentropy)
library(cluster)
library(vegan)
library(dplyr)
library(ggplot2)
load("Fig3A_pre.RData")
js_distance <- distance(as.matrix(metagenome), method = "jensen-shannon")
js_dist_matrix <- dist(js_distance)
hc <- hclust(js_dist_matrix)
js_dist_matrix <- as.data.frame(js_dist_matrix)
wss <- (nrow(js_dist_matrix) - 1) * sum(apply(js_dist_matrix, 2, var))
for (i in 1:20) {wss[i] <- sum(kmeans(js_dist_matrix, centers = i)$withinss)}
plot(1:20,wss,type = "b",xlab = "Number of Clusters",ylab = "Within groups sum of squares")
k <- 4
pam_res <- pam(as.matrix(js_distance), k)
bray_distance <- vegdist(metagenome, method = "bray")
pcoa_coords <- as.data.frame(cmdscale(bray_distance, k = 2))
pcoa <- cbind(pcoa_coords,cluster = pam_res$clustering)
pcoa$cluster <- as.factor(pcoa$cluster)
taxa_abundance <- as.data.frame(t(metagenome))
taxa_abundance$family <- setNames(name$family,name$genus)[rownames(taxa_abundance)]
family_abundance <- taxa_abundance %>% group_by(family) %>% summarise(across(everything(), sum, na.rm = TRUE))
family_abundance <- as.data.frame(family_abundance)
rownames(family_abundance) <- family_abundance$family
family_abundance$family <- NULL
family_abundance <- as.data.frame(t(family_abundance))
family_abundance$cluster <- pcoa$cluster
cluster_family_abundance <- family_abundance %>% group_by(cluster) %>% summarise(across(everything(),~ mean(.x, na.rm = TRUE),.names = "{col}"))
cluster_family_abundance <- cluster_family_abundance[, -1]
cluster_family_abundance <- as.data.frame(t(cluster_family_abundance))
dominant_family <- apply(cluster_family_abundance,2,function(x) {rownames(cluster_family_abundance)[which.max(x)]})
print(dominant_family)
pcoa$cluster <- ifelse(pcoa$cluster == 1, "f__Prevotellaceae",
                       ifelse(pcoa$cluster == 2, "f__Micrococcaceae",
                              ifelse(pcoa$cluster == 3, "f__Streptococcaceae",
                                     ifelse(pcoa$cluster == 4, "f__Prevotellaceae",pcoa$cluster))))
metadata$cluster <- pcoa[rownames(metadata), "cluster"]
# figure generation
Fig3A_pre <- plot_pcoa_cluster(
  metagenome = metagenome,
  metadata = metadata,
  group = "cluster",
  shape = "Study",
  col = col,
  sha = sha)
Fig3A_pre$plot