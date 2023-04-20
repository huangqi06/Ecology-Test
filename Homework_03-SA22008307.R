####exercise 1
# read in the doubs 
rm (list = ls ())
library("ade4")
data(doubs)
doubs

# reading the fish data and remove site with no data
spe <- doubs$fish
spe <- spe[-8,]   

total_species <- rowSums(spe > 0)
total_species 
barplot(total_species, main = "Species richness",
        xlab = "Sites",ylab = "Number of species",
        col = "gray",las = 1)
# Which site has the most species (and how many species)?
# answer：site 29 has the most species with 26 species

widespread_species <- colSums(spe > 0)
widespread_species
barplot(widespread_species, main = "Species sites",
        xlab = "Species",ylab = "Number of sites",
        col = "gray",las = 1)
# Which species is the most widespread (found in the most sites)?
# answer：species named Lece is the most widespread with 25 sites


####exercise 2
library("vegan") 
spe_pa <- decostand(spe,method = "pa") # scale into presence/absence scale(0/1)
dis_spe_pa <- vegdist(spe_pa,method = "bray")

# Create a distance matrix from Hellinger transformed Doubs river data and 
# compute the single linkage clustering
# Apply Hellinger transformation to correct for the double zero problem
spe_hel <- decostand(spe,method = "hellinger")
spe_chi <- decostand(spe, method = "chi.square")

dis_spe_hel <- vegdist(spe_hel,method = "euclidean")
dis_spe_hel_single <- hclust(dis_spe_hel,method = "single")
plot(dis_spe_hel_single)

# reading the env data and remove site with no data
env <- doubs$env
env <- env[-8,] 

# clustering
env_hel <- decostand(env,method = "hellinger")
env_chi <- decostand(env, method = "chi.square")

dis_env_he1 <- vegdist(env_hel,method = "euclidean")
dis_env_he1_single <- hclust(dis_env_he1,method = "single")
plot(dis_env_he1_single)
# Which groups of species are related to these groups of sites?
# answer：21 and 22、27 and 28


####exercise 3
# PCA and RDA
# PCA - species 
pca_spe_hel <- rda(spe_hel)    # PCA on the hellinger-transformed data
pca_spe_hel
# extract the eigenvalues 
eigenvalues_spe <- pca_spe_hel$CA$eig    
# select the eigenvalues above average
eigenvalues_spe [eigenvalues_spe > mean(eigenvalues_spe)]
n <- length(eigenvalues_spe)

barplot(eigenvalues_spe, main = "Eigenvalues_spe",col = "grey",las = 1)

abline(h = mean(eigenvalues_spe),col = "blue",lwd = 2)

legend("topright", "Average Eigenvalue_spe",
       lwd = 2,  col = "blue")

biplot(pca_spe_hel,main = "pca_spe_hel")

# PCA - environment
sd_env <- decostand(env, method = "standardize")
pca_env <- rda(sd_env)
# select the eigenvalues above average
eigenvalues_env <- pca_env$CA$eig    
# select the eigenvalues above average
eigenvalues_env [eigenvalues_env > mean(eigenvalues_env)]
n <- length(eigenvalues_env)

barplot(eigenvalues_env,main = "eigenvalues_env",col = "grey",las = 1)

abline(h = mean(eigenvalues_env), col = "blue",lwd = 2)

legend("topright", "Average Eigenvalues_env",
       lwd = 2, col = "blue")

biplot(pca_env,main = "pca_env")

# NMDS - species
spe_NMDS <- metaMDS(spe,distance = "bray", k = 2)
spe_NMDS$stress
stressplot(spe_NMDS, main = "Shepard plot")

# Do RDA analysis
# examine whether RDA is suitable for RDA：Axis lengths
dca_spe <- decorana(spe)
dca_spe    #Axis lengths = 3.855

dca_spe_hel <- decorana(spe_hel)
dca_spe_hel    #Axis lengths = 3.8978

# standardize the variables
sd_env <- decostand(env, method = "standardize")

# check the correlations between variables
# remove the collinerity variables
heatmap(abs(cor(env)),
        col = rev(heat.colors(6)),
        Colv = NA,
        Rowv =NA)
eigen(abs(cor(env)))
# remove the dfs
sd_env <- sd_env[,2:11]

# RDA model
spe_rda <- rda(spe_hel ~ ., data = sd_env)
plot(spe_rda)

fwd_sel <- ordiR2step(rda(spe_hel ~ 1, data = sd_env),
                      scope = formula(spe_rda),
                      direction = "forward",
                      R2scope = TRUE,
                      pstep = 1000,
                      trace = FALSE)
fwd_sel$call
# variables: alt + oxy + bdo

# new RDA model
spe_rda_new <- rda(spe_hel ~ alt + oxy + bdo, data = sd_env)

anova.cca(spe_rda_new, step = 1000, by = "axis")

plot(spe_rda_new)
# Which environmental variables cause a community to 
# vary across a landscape?
# answer：alt 、 oxy 、 bdo