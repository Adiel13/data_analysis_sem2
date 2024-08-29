install.packages("arules")
library(arules)
install.packages("genero")
library(genero)

data <- read.csv('C:/Users/kevin/OneDrive/Documentos/data.csv', sep = ";", fileEncoding = "latin1")

data$cui <- format(data$cui, scientific = FALSE)

subset(data, nota > 60)

data$nota[data$nota == "SDE"] <- -1
data$final[data$final == "SDE"] <- -1
data$final[data$final == "NSP"] <- -2


data$nombre1 <- sapply(strsplit(data$nombre, " "), `[`, 1)
data$nombre2 <- sapply(strsplit(data$nombre, " "), `[`, 2)

genero("SARA")

data$genero <- genero(data$nombre1)

subset(data, is.na(data$genero))

data$genero <- ifelse(is.na(data$genero), genero(data$nombre2), data$genero)

data[77, "genero"] <- "male"
data[113, "genero"] <- "male"
data[119, "genero"] <- "female"
data[120, "genero"] <- "male"
data[179, "genero"] <- "female"
data[185, "genero"] <- "male"
data[202, "genero"] <- "male"
data[225, "genero"] <- "male"
data[250, "genero"] <- "male"
data[276, "genero"] <- "female"
data[363, "genero"] <- "female"
data[473, "genero"] <- "female"
data[487, "genero"] <- "male"
data[566, "genero"] <- "male"


data$genero <- ifelse(data$genero == "male", 1, 2)

data$anio_carne <- substr(data$carne, start=1, stop = 4)

subset(data, anio_carne >8000)

data[47, "anio_carne"] <- 1992
data[91, "anio_carne"] <- 1986
data[92, "anio_carne"] <- 1996
data[287, "anio_carne"] <- 1992
data[391, "anio_carne"] <- 1983
data[392, "anio_carne"] <- 1992
data[393, "anio_carne"] <- 1995
data[394, "anio_carne"] <- 1996
data[496, "anio_carne"] <- 1983
data[497, "anio_carne"] <- 1995
data[498, "anio_carne"] <- 1996


data$edad <- as.integer(data$anio) - as.integer(data$anio_carne) + 18

data

data_research <- data[,c("lab", "zona", "final", "nota", "anio", "sem", "genero", "edad")]

reglas <- apriori(data_research, parameter = list(support=0.2, confidence=0.5))
inspect(reglas[0:100])

data_g <- subset(data_research, data$genero==1)
data_g <- data_g[,-7]
reglas_g <- apriori(data_g, parameter = list(support=0.2, confidence=0.5))
inspect(reglas_g[0:100])

install.packages("ggplot2")
library(ggplot2)


data_research_k <- data_research

cluster_k <- kmeans(data_research_k, centers = 4)


ggplot(data_research_k, aes(x = edad, y = zona , color = as.factor(cluster_k$cluster))) +
  geom_point() + 
  geom_point(data = as.data.frame(cluster_k$centers), aes(x = edad, y = zona), color = "black", size = 4, shape = 17) +
  labs(title = "Edad vs Lab") +
  theme_minimal()

write.csv(data_research, file = "C:/Users/kevin/OneDrive/Documentos/data_research_red.csv", row.names = FALSE)
