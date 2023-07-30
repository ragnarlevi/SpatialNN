

library(tidyverse)


# ------ load ------


data <- read.csv("BVDatabase.csv", sep = ";")
adj_matrix <- read.csv("Adj_Table.csv", sep = ";")
adj_matrix <- adj_matrix[,2:ncol(adj_matrix)]
rownames(adj_matrix) <- names(adj_matrix)
dim(adj_matrix)

head(data)

ggplot(data) + geom_bar(aes(TClaimsNo))+ theme_bw()
ggplot(data[data$Avg_Cost>0,]) + geom_bar(aes(TClaimsNo))+ theme_bw()

ggplot(data) + geom_bar(aes(TClaimsNo, fill = Pcause))+ theme_bw()
ggplot(data[data$Avg_Cost>0,]) + geom_bar(aes(TClaimsNo, fill = Pcause))+ theme_bw()

# ----- aggregate -----
# Ask about Floors_No, can we have one policy covering multiple floors. For example a large apartment building. If that is the case, how to calculate the exposure.
# is the exposure the number of apartments within the policy or simply 1?
# Maybe not a less of an issue for number of claims

# How to aggregate capital insured? Split into segments?

# Only use residental (C1) itemTypeCd?

# How do we understand corine?


# Agregate Athens into one?

ggplot(data) + geom_density(aes(x = TCapital_Insurred))+ theme_bw()
ggplot(data[data$TCapital_Insurred<1e6,]) + geom_density(aes(x = TCapital_Insurred))+ theme_bw()


# Freq per build year

ggplot(data %>% group_by(builtYear) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                  Expsoure = sum(TExposure))) +
  geom_bar(aes(x = builtYear, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 0.5, fill = "#116E8A")+ 
  geom_point(aes(x = builtYear,y = Freq))+theme_bw()


# Freq per nrOfFloors - what is 0 number of floors?

ggplot(data %>% group_by(nrOfFloors) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                  Expsoure = sum(TExposure))) +
  geom_bar(aes(x = nrOfFloors, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 0.5, fill = "#116E8A")+ 
  geom_point(aes(x = nrOfFloors,y = Freq))+theme_bw()

# Freq per  postalCode - How to aggregate?
length(unique(data$postalCode))
ggplot(data %>% group_by(postalCode) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                   Expsoure = sum(TExposure))) +
  geom_bar(aes(x = postalCode, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = postalCode,y = Freq))+theme_bw()


# Freq per  floodvulnerability
ggplot(data %>% group_by(floodvulnerability) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                   Expsoure = sum(TExposure))) +
  geom_bar(aes(x = floodvulnerability, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = floodvulnerability,y = Freq))+theme_bw()


ggplot(data %>% group_by(floodvulnerability, Pcause) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                           Expsoure = sum(TExposure))) +
  geom_bar(aes(x = floodvulnerability, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = floodvulnerability,y = Freq, color = Pcause))+theme_bw()


# Freq per  windvulnerability
ggplot(data %>% group_by(windvulnerability) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                           Expsoure = sum(TExposure))) +
  geom_bar(aes(x = windvulnerability, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = windvulnerability,y = Freq))+theme_bw()


ggplot(data %>% group_by(windvulnerability, Pcause) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                                   Expsoure = sum(TExposure))) +
  geom_bar(aes(x = windvulnerability, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = windvulnerability,y = Freq, color = Pcause))+theme_bw()

# Freq per  Location
ggplot(data %>% group_by(Location) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                          Expsoure = sum(TExposure))) +
  geom_bar(aes(x = Location, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = Location,y = Freq))+theme_bw()

# Freq per  Item_Type_Description
ggplot(data %>% group_by(Item_Type_Description) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                 Expsoure = sum(TExposure))) +
  geom_bar(aes(x = Item_Type_Description, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = Item_Type_Description,y = Freq))+theme_bw()

# Freq per  SubItemType_C
ggplot(data %>% group_by(SubItemType_C) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                              Expsoure = sum(TExposure))) +
  geom_bar(aes(x = SubItemType_C, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = SubItemType_C,y = Freq))+theme_bw()

# Freq per  Construction_Material_C
ggplot(data %>% group_by(Construction_Material_C) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                      Expsoure = sum(TExposure))) +
  geom_bar(aes(x = Construction_Material_C, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = Construction_Material_C,y = Freq))+theme_bw()

# Freq per  FloorsType_C
ggplot(data %>% group_by(FloorsType_C) %>% summarise(Freq = sum(TClaimsNo)/sum(TExposure),
                                                                Expsoure = sum(TExposure))) +
  geom_bar(aes(x = FloorsType_C, y = 0.05*Expsoure/max(Expsoure)), stat = 'identity', alpha = 1, fill = "#116E8A")+ 
  geom_point(aes(x = FloorsType_C,y = Freq))+theme_bw()


# Tcapital insurred
plot(data[data$Avg_Cost>0,]) + geom_boxplot(aes(x = as.character(TClaimsNo), y = TCapital_Insurred)) + theme_bw()

# 

data_aggregated <- data %>% group_by(postalCode, builtYear, nrOfFloors, floodvulnerability,
                                     windvulnerability, Location, Item_Type_Description,
                                     Pcause, SubItemType_C, Construction_Material_C, FloorsType_C) %>% 
  summarise(TExposure = sum(TExposure),
            TPremiums = sum(TPremiums), 
            TClaimsNo = sum(TClaimsNo),
            TIncurred = sum(TIncurred),
            Avg_cost = sum(Avg_cost))

dim(data_aggregated)




# ----- plot -----


ggplot(data[data$Avg_Cost>0,]) + geom_point(aes(x = builtYear, y = Avg_Cost)) +
  geom_smooth(aes(x = builtYear, y = Avg_Cost))


ggplot(data[data$Avg_Cost>0,]) + geom_point(aes(x = as.character(Floors_No), y = Avg_Cost)) +
  geom_smooth(aes(x = Floors_No, y = Avg_Cost))









