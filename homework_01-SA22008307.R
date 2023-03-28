# exercise one
install.packages("tidyverse")   #Loading library of tidyverse 
library("tidyverse")            
install.packages("ade4")        #Loading library of ade4
library("ade4")                 
data(doubs)                     #Loading the doubs data into R
doubs                           #Checking the data looks like
str(doubs)                      
class(doubs)                    #The class of the data


# exercise two
df.env <- doubs$env                 
                          #Turning the row names into a column called site
df.env$site <- row.names(df.env)
view(df.env)
df.env <- as_tibble(df.env)     #Convert the data frame to a tibble
class(df.env)
env_tb <- df.env                #Named tibble env_tb
view(env_tb)
class(env_tb)
  

# exercise three
# 3.1    Extract and remain the data of the dfs with more than 1000 km
dfs1000 <- env_tb$dfs   
dfs1000 <- subset(dfs1000, dfs1000 > 1000)    
view(dfs1000)

# 3.2    Select columns for further analysis
env_final <- env_tb %>% select(site, dfs, slo, flo, pH, nit, oxy)   
class(env_final)  

# 3.3    Rename these columns
colnames(env_final)                       
names(env_final)[2] <- "distsour"
names(env_final)[3] <- "slope"
names(env_final)[4] <- "flowrate"
names(env_final)[6] <- "nitrogen"
names(env_final)[7] <- "oxygen"

# 3.4
env_final <- env_final[order(env_final$slope),]       
                             #Arrange the data by slope in ascending order
env_final <- env_final[order(env_final$pH,decreasing = T),] 
                             #Arrange the data by pH in descending order
view(env_final)