library(ggplot2)

data = read.csv('probResults.csv', header = T)

df <- data.frame(matrix(ncol = 6, nrow = 9))
x <- c("Epsilon", "COMP", "C", "Free", "Fake", "D")
colnames(df) <- x
df$Epsilon <- c(2:10)/10

f <- function (x){
  i <- x*10 -1
  df$C[i] <-     mean(data[data$eps == x, 'count.patches.with..genome....C..'])
  df$COMP[i] <-  mean(data[data$eps == x, 'count.patches.with..genome....COMP..'])
  df$Free[i] <-  mean(data[data$eps == x, 'count.patches.with..genome....FREE..'])
  df$Fake[i] <-  mean(data[data$eps == x, 'count.patches.with..genome....FAKE..'])
  df$D[i] <-  mean(data[data$eps == x, 'count.patches.with..genome....D..'])
  print(df)
}

for (x in c(2:10)/10){
  df <- f(x)
}

ggplot(df, aes(x = Epsilon)) + 
  geom_line(aes(y = COMP), colour="red", size = 2) + 
  geom_line(aes(y = Free), colour = "blue", size = 2) + 
  geom_line(aes(y = C), colour = "green", size = 2) + 
  geom_line(aes(y = D), colour = "black", size = 2) + 
  geom_line(aes(y = Fake), colour = "grey", size = 2) + 
  ylab(label="Number") + 
  xlab("Epsilon")