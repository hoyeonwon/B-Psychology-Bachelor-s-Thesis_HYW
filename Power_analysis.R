install.packages("pwr")
library(pwr)

n <- pwr.anova.test(k = 6, f = 0.30, sig.level = .05, power = .8) # one way anova 


# calcuations for  GLIMMPSE 
data <- read.csv("teo_results.csv", header = T)
ps <- c(0.1, 0.2, 1)
View(data)

means <- data.frame()
for(m in 1:2) {
  for(j in 1:3) {
    for(i in 1:4) {
      
      x <- data[which(data[2] == ps[j] & data[3] == (m-1)), 3+i]
      means[j+(m-1)*3, i] <- mean(x)
      
    }
    
  }
  
}
colnames(means) <- c(paste("Trial", 1:4))
rownames(means) <- c(paste("Control", ps), paste("Yoked", ps))
sd(sapply(means, FUN = mean))
View(means)

var_in_pred <- data.frame()
for(m in 1:2) {
  for(j in 1:3) {
    for(i in 1:2) {
      
      x <- data[which (data[2] == ps[j] & data[3] == (m-1)), 9+(2*i)]
      var_in_pred[j+(m-1)*3, i] <- mean(x)
      
    }
    
  }
  
}
for (i in unique(data_pc1$ID)){
  
}
colnames(var_in_pred) <- c(paste("Half", 1:2))
rownames(var_in_pred) <- c(paste("Control", ps), paste("Yoked", ps))
sd(sapply(var_in_pred, FUN = mean))
View(var_in_pred)

direct_con_rat <- data.frame()
for(m in 1:2) {
  for(j in 1:3) {
    for(i in 1:2) {
      
      x <- data[which (data[2] == ps[j] & data[3] == (m-1)), i+13]
      direct_con_rat[j+(m-1)*3, i] <- mean(x)
      
    }
    
  }
  
}
colnames(direct_con_rat) <- c(paste("Half", 1:2))
rownames(direct_con_rat) <- c(paste("Control", ps), paste("Yoked", ps))
sd(sapply(direct_con_rat, FUN = mean))



meanVSvari_vec <- c(cor(data[, 4], data[, 11]), cor(data[, 5], data[, 11]), cor(data[, 6], data[, 13]), cor(data[, 7], data[, 13]))
meanVSvari <- mean(meanVSvari_vec)

meanVSexp_vec <- c(cor(data[, 4], data[, 14]), cor(data[, 5], data[, 14]), cor(data[, 6], data[, 15]), cor(data[, 7], data[, 15]))
meanVSexp <- mean(meanVSexp_vec)

dirVSexp_vec <- c(cor(data[, 11], data[, 14]), cor(data[, 13], data[, 15]))
dirVSexp <- mean(dirVSexp_vec)



one_two <- cor(data[, 4], data[, 5])
one_thr <- cor(data[, 4], data[, 6])
one_fou <- cor(data[, 4], data[, 7])
two_thr <- cor(data[, 5], data[, 6])
two_fou <- cor(data[, 5], data[, 7])
thr_fou <- cor(data[, 6], data[, 7])