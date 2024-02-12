library(vegan)
library(Hotelling)
library(readxl)

data = as.data.frame(read_excel("tables/2020.08.24.raresample.xlsx"), sheet = "rare")
#colnames(data) = data[1,]
rownames(data) = data[,1]
data$plotid = NULL

head(data)

#Rarefaction
sp.abund <- rowSums(data)  #gives the number of individuals found in each plot
raremax <- min(rowSums(data))  #rarefaction uses the smallest number of observations (individuals here) per sample to extrapolate the expected number if all other samples only had that number of observations
raremax
Srare <- rarefy(data, raremax)
par(mfrow = c(1, 1))
plot(sp.abund, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
rarecurve(data, col = "blue")

#Code from Alison----
#get richness estimators (for each sample, cumulative)
pool.aurora <- poolaccum(data)
#plot all: obs richness and  estimators
plot(pool.aurora)

#build the species accumulation curve & rarefaction curve (expected)
aurora.specaccum <- specaccum(data,method = "rarefaction")
#plot the curve with some predefined settings
plot(aurora.specaccum,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

#build a expected curve (randomization for boxplot comparison)
aurora.specaccum.rand <- specaccum(data, "random")
#plot both curves ("observed" vs "randomized")
plot(aurora.specaccum,ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(aurora.specaccum.rand, col="yellow", add=TRUE, pch="+")

library(iNEXT)
library(readxl)

incid = list()

incid[[1]] = as.data.frame(read_excel("tables/2020.08.25.raresample.xlsx", sheet = "field1"))
incid[[2]] = as.data.frame(read_excel("tables/2020.08.25.raresample.xlsx", sheet = "field2"))
incid[[3]] = as.data.frame(read_excel("tables/2020.08.25.raresample.xlsx", sheet = "field3"))
incid[[4]] = as.data.frame(read_excel("tables/2020.08.25.raresample.xlsx", sheet = "water1"))
incid[[5]] = as.data.frame(read_excel("tables/2020.08.25.raresample.xlsx", sheet = "water2"))

names(incid) = c("field1", "field2", "field3", "water1")

rownames(incid[["field1"]]) = incid[["field1"]][["plot"]]; incid[["field1"]][["plot"]] = NULL
head(incid[["field1"]])

rownames(incid[["field2"]]) = incid[["field2"]][["plot"]]; incid[["field2"]][["plot"]] = NULL
rownames(incid[["field3"]]) = incid[["field3"]][["plot"]]; incid[["field3"]][["plot"]] = NULL
rownames(incid[["water1"]]) = incid[["water1"]][["plot"]]; incid[["water1"]][["plot"]] = NULL
rownames(incid[["water2"]]) = incid[["water2"]][["plot"]]; incid[["water2"]][["plot"]] = NULL

##This is the iNEXT that actually works ----
incid = list()

incid[[1]] = as.data.frame(read_excel("tables/subplotspecies_master.xlsx", sheet = "field.incid"))
incid[[2]] = as.data.frame(read_excel("tables/subplotspecies_master.xlsx", sheet = "water.incid"))
incid[[3]] = as.data.frame(read_excel("tables/subplotspecies_master.xlsx", sheet = "forest.incid"))
names(incid) = c("field", "riparian", "forest")
rownames(incid[["field"]]) = incid[["field"]][["plot"]]; incid[["field"]][["species"]] = NULL
rownames(incid[["riparian"]]) = incid[["riparian"]][["plot"]]; incid[["riparian"]][["species"]] = NULL
rownames(incid[["forest"]]) = incid[["forest"]][["plot"]]; incid[["forest"]][["species"]] = NULL

rare.inext = iNEXT(incid, datatype = "incidence_raw", endpoint = 56)

png("inext.speciesdiversity.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 1)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()

png("inext.samplecoverage.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 2)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()

incid = list()
incid[[1]] = c(13, 4,7,4,9,8,11,10,9,16,9,12,7,7) #field 1
incid[[2]] = c(11, 5, 7, 11,9, 15,15,14,6,10,9,9) #field 2
incid[[3]] = c(4, 6,6,13,11) #field 3
incid[[4]] = c(7, 18,26,27,20,18,29,9) #water 1


names(incid) = c("field1", "field2", "field3", "water1", "water2")

data.t = t(data)
head(data.t)

#Sum abundances for the aurora site; i.e. consider the aurora data as a single site with spp abundances (summed over the sampling days)
aurora.sum <- rowSums(data.t)

#apply `iNEXT` main function
aurora.sum.inext <- iNEXT(incid, q = 0, datatype = "incidence_freq")
#look at the data
aurora.sum.inext
#plot the results
ggiNEXT(aurora.sum.inext)

#iNEXT Quadrat Abundance ----
quad = list()

quad[[1]] = as.data.frame(read_excel("tables/quadrats_master.xlsx", sheet = "relativecover.field"))
quad[[2]] = as.data.frame(read_excel("tables/quadrats_master.xlsx", sheet = "relativecover.water"))
quad[[3]] = as.data.frame(read_excel("tables/quadrats_master.xlsx", sheet = "relativecover.forest"))
names(quad) = c("field", "water", "forest")
rownames(quad[["field"]]) = quad[["field"]][["plot"]]; quad[["field"]][["species"]] = NULL
rownames(quad[["water"]]) = quad[["water"]][["plot"]]; quad[["water"]][["species"]] = NULL
rownames(quad[["forest"]]) = quad[["forest"]][["plot"]]; quad[["forest"]][["species"]] = NULL

quad.inext = iNEXT(quad)
ggiNEXT(quad.inext)

png("inext.speciesdiversity.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 1)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()

png("inext.samplecoverage.png", width = 8, height = 6, units = "in", res = 300)
ggiNEXT(rare.inext, type = 2)+
  scale_color_manual(values = c("goldenrod", "olivedrab4", "dodgerblue4"))+
  theme_classic()+
  theme(legend.position = "bottom",panel.border = element_rect(colour = "black", fill=NA))
dev.off()