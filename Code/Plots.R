H_n <- function(n){
  ans <- 0
  for(i in 1:n){
    ans <- ans + 1/i
  }
  return(ans)
}

my_fxn <- function(y){
  n <- length(y)
  ans <- numeric(n)
  ans[1] <- 1
  for(i in 2:n){
    ans[i] <- ans[i-1] + 1/i
  }
  for(i in 1:n){
    ans[i] <- ans[i]*i
  }
  return(ans)
}

my_fxn_diff <- function(y){
  n <- length(y)
  ans <- numeric(y[n])
  ans[1] <- 1
  for(i in 2:y[n]){
    ans[i] <- ans[i-1] + 1/i
  }
  need <- numeric(n)
  for(i in 1:n){
    need[i] <- ans[y[i]]*y[i]
  }
  return(need)
}

library(ggplot2)
colnames(New_dat) <- c("N", "Proposals")


######## Scatterplot for the Number of Proposals #############

colnames(DATA) <- c("N", "Proposals")
ggplot(DATA, aes(x = N, y = Proposals)) + 
  geom_point() + 
  xlim(1,3100) + 
  ylim(0, 35e3) + 
  geom_line(aes(N, 1.5*N*log(N), col = "1.5*NlogN vs N"), linewidth = 1) +
  geom_line(aes(N, N^2, col = "N^2 vs N"), linewidth = 1)+
  geom_line(aes(N, my_fxn(N), col = "N*H_N vs N"), linewidth = 1) +
  guides(color = guide_legend("Diff fxns"), linewidth = 1) + 
  ggtitle(label = "Plot of proposals vs N") +
  theme_bw()


######## Scatterplot for the log Number of Proposals #############

ggplot(DATA, aes(x = log(N), y = log(Proposals))) + 
  geom_point() + 
  xlim(1,8) + 
  ylim(0, 11) + 
  geom_line(aes(log(N), log(1.5*N*log(N)), col = "log(1.5*NlogN) vs logN"), linewidth = 1) +
  geom_line(aes(log(N), log(N^2), col = "2*log(N) vs logN"), linewidth = 1)+
  geom_line(aes(log(N), log(my_fxn(N)), col = "log(N*H_n) vs logN"), linewidth = 1)+
  guides(color = guide_legend("Diff Log fxns")) +
  ggtitle(label = "Logarithmic graph")+
  theme_bw()

#### Number of Proposals for N = 10, 1000 times
### Plot when N = 10  ##############################

ggplot(data = count_10, mapping = aes(x = index, y = count))  + 
  geom_point() + 
  geom_line(aes(y = mean(count), col = "Average"), linewidth = 1) + 
  geom_line(aes(y = 10*H_n(10), col = "nH_n"), linewidth = 1) +
  geom_line(aes(y = max(count), col = "Maximum"), linewidth = 1) +
  geom_line(aes(y = 1.1*10*H_n(10), col = "110% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 1.5*10*H_n(10), col = "150% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 2*10*H_n(10), col = "200% of nH_n"), linewidth = 1) +
  guides(color = guide_legend("Comparison")) +
  ggtitle(label = "Number of Proposals when N = 10")+
  theme_minimal()

diff_10 <- 10*H_n(10) - mean(count_10$count)
diff_10

avg_10 <- 10*H_n(10)
exceed_10_10 <- mean(count_10$count > 1.1*avg_10)
exceed_20_10 <- mean(count_10$count > 1.2*avg_10)
exceed_50_10 <- mean(count_10$count > 1.5*avg_10)
exceed_100_10 <- mean(count_10$count > 2*avg_10)

mean(count_10$count)
max(count_10$count)
sd(count_10$count)

#### Number of Proposals for N = 100, 1000 times
### Plot when N = 100  ###############################

ggplot(data = count_100, mapping = aes(x = index, y = count))  + 
  geom_point() + 
  geom_line(aes(y = mean(count), col = "Average"), linewidth = 1) + 
  geom_line(aes(y = 100*H_n(100), col = "nH_n"), linewidth = 1) +
  geom_line(aes(y = max(count), col = "Maximum"), linewidth = 1) +
  geom_line(aes(y = 1.1*100*H_n(100), col = "110% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 1.5*100*H_n(100), col = "150% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 2*100*H_n(100), col = "200% of nH_n"), linewidth = 1) +
  guides(color = guide_legend("Comparison")) + 
  ggtitle(label = "Number of Proposals when N = 100")+
  theme_minimal()

avg_100 <- 100*H_n(100)
exceed_10_100 <- mean(count_100$count > 1.1*avg_100)
exceed_20_100 <- mean(count_100$count > 1.2*avg_100)
exceed_50_100 <- mean(count_100$count > 1.5*avg_100)
exceed_100_100 <- mean(count_100$count > 2*avg_100)

mean(count_100$count)
max(count_100$count)
sd(count_100$count)

## when N = 100, 
diff_100 <- 100*H_n(100) - mean(count_100$count)
diff_100

#### Number of Proposals for N = 1000, 1000 times
### Plot when N = 1e3 ##########################

ggplot(data = count_1000, mapping = aes(x = index, y = count))  + 
  geom_point() + 
  geom_line(aes(y = mean(count), col = "Average"), linewidth = 1) + 
  geom_line(aes(y = 1000*H_n(1000), col = "nH_n"), linewidth = 1) +
  geom_line(aes(y = max(count), col = "Maximum"), linewidth = 1) +
  geom_line(aes(y = 1.1*1000*H_n(1000), col = "110% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 1.5*1000*H_n(1000), col = "150% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 2*1000*H_n(1000), col = "200% of nH_n"), linewidth = 1) +
  guides(color = guide_legend("Comparison")) + 
  ylim(5000, 18000) +
  ggtitle(label = "Number of Proposals when N = 1000")+
  theme_minimal()

diff_1000 <- 1000*H_n(1000) - mean(count_1000$count)
diff_1000

avg_1000 <- 1000*H_n(1000)
exceed_10_1000 <- mean(count_1000$count > 1.1*avg_1000)
exceed_20_1000 <- mean(count_1000$count > 1.2*avg_1000)
exceed_50_1000 <- mean(count_1000$count > 1.5*avg_1000)
exceed_100_1000 <- mean(count_1000$count > 2*avg_1000)

mean(count_1000$count)
max(count_1000$count)
sd(count_1000$count)

#### Number of Proposals for N = 10000, 100 times ##############
### Plot when N = 1e4  ############################

ggplot(data = count_10000, mapping = aes(x = index, y = count))  + 
  geom_point() + 
  geom_line(aes(y = mean(count), col = "Average"), linewidth = 1) + 
  geom_line(aes(y = 10000*H_n(10000), col = "nH_n"), linewidth = 1) +
  geom_line(aes(y = max(count), col = "Maximum"), linewidth = 1) +
  geom_line(aes(y = 1.1*10000*H_n(10000), col = "110% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 1.5*10000*H_n(10000), col = "150% of nH_n"), linewidth = 1) +
  geom_line(aes(y = 2*10000*H_n(10000), col = "200% of nH_n"), linewidth = 1) +
  guides(color = guide_legend("Comparison")) + 
  ylim(7e4, 20e4) +
  ggtitle(label = "Number of Proposals when N = 10000")+
  theme_minimal()


diff_10000 <- 10000*H_n(10000) - mean(count_10000$count)
diff_10000

avg_10000 <- 10000*H_n(10000)
exceed_10_10000 <- mean(count_10000$count > 1.1*avg_10000)
exceed_20_10000 <- mean(count_10000$count > 1.2*avg_10000)
exceed_50_10000 <- mean(count_10000$count > 1.5*avg_10000)
exceed_100_10000 <- mean(count_10000$count > 2*avg_10000)

mean(count_10000$count)
max(count_10000$count)
sd(count_10000$count)

## Table for different N

data <- matrix(c(24.1, 549.7, 7813.4, 123176.2, 25.77, 515.18, 8392.9,110621.1, 25.083, 540.411, 8207, 0), byrow = TRUE, nrow = 3, ncol = 4)
colnames(data) <- c(10, 100, 1000, 10000)
rownames(data) <- c(10, 100, 1000)
data
proposal <- c(541, 8392, 18832, 28559,
         38416,
         51100,
         62653,
         77271,
         82836,
         94673,
         110621)
n <- c(100, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)
v <- cbind(n, proposal)
v

barplot(proposal, names.arg=n, col = "skyblue", xlab = "N", ylab = "Proposals")

dbb <- data.frame(
  n, proposal
)


# Barplot
ggplot(dbb, aes(x=n, y=proposal)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = after_stat(proposal)), vjust = -1)+
  ggtitle(label = "Barplot for Average proposals vs n") +
  theme_bw()

### Values of n_Hn
for(i in seq(1000, 10000, 1000)){
  #print(i*H_n(i))
  print(i)
}

### Boxplot for N = 10

quantile(count_10$count, 0.25)
quantile(count_10$count, 0.5)
quantile(count_10$count, 0.75)

ggplot(data = count_10, mapping = aes(x = index, y = count))  + 
  geom_boxplot(fill = "hotpink") + 
  geom_jitter(color = "purple", size = 0.5) +
  coord_flip()+
  annotate("text", y=29, x= 0, label= "29", size = 3.5) +
  annotate("text", y=24, x= 0, label= "24", size = 3.5) +
  annotate("text", y=20, x= 0, label= "20", size = 3.5) +
  annotate("text", y=29, x= 1000, label= "75%", size = 3.5) +
  annotate("text", y=24, x= 1000, label= "50%", size = 3.5) +
  annotate("text", y=20, x= 1000, label= "25%", size = 3.5) +
  geom_hline(yintercept = 10*H_n(10), color = "green", show.legend = "nH_n")+
  ggtitle(label = "Boxplot when N = 10")+
  theme_bw()

### Boxplot for N = 1e2

quantile(count_100$count, 0.25)
quantile(count_100$count, 0.5)
quantile(count_100$count, 0.75)

ggplot(data = count_100, mapping = aes(x = index, y = count))  + 
  geom_boxplot(fill = "hotpink") + 
  geom_jitter(color = "purple", size = 0.5) +
  coord_flip()+
  annotate("text", y=604.25, x= 0, label= "604.25", size = 3.5) +
  annotate("text", y=522, x= 0, label= "522", size = 3.5) +
  annotate("text", y=456, x= 0, label= "456", size = 3.5) +
  annotate("text", y=604.25, x= 1000, label= "75%", size = 3.5) +
  annotate("text", y=522, x= 1000, label= "50%", size = 3.5) +
  annotate("text", y=456, x= 1000, label= "25%", size = 3.5) +
  geom_hline(yintercept = 100*H_n(100), color = "green", show.legend = "nH_n")+
  ggtitle(label = "Boxplot when N = 100")+
  theme_bw()

### Boxplot for N = 1e3

quantile(count_1000$count, 0.25)
quantile(count_1000$count, 0.5)
quantile(count_1000$count, 0.75)

ggplot(data = count_1000, mapping = aes(x = index, y = count))  + 
  geom_boxplot(fill = "hotpink") + 
  geom_jitter(color = "purple", size = 0.5) +
  coord_flip()+
  annotate("text", y=8893.25, x= 0, label= "8893.25", size = 3.5) +
  annotate("text", y=7980, x= 0, label= "7980", size = 3.5) +
  annotate("text", y=7217.5, x= 0, label= "7217.5", size = 3.5) +
  annotate("text", y=8893.25, x= 1000, label= "75%", size = 3.5) +
  annotate("text", y=7980, x= 1000, label= "50%", size = 3.5) +
  annotate("text", y=7217.5, x= 1000, label= "25%", size = 3.5) +
  ylim(5000, 13000) +
  geom_hline(yintercept = 1000*H_n(1000), color = "green", show.legend = "nH_n")+
  ggtitle(label = "Boxplot when N = 1000")+
  theme_bw()

### Boxplot for N = 1e4

quantile(count_10000$count, 0.25)
quantile(count_10000$count, 0.5)
quantile(count_10000$count, 0.75)

ggplot(data = count_10000, mapping = aes(x = index, y = count))  + 
  geom_boxplot(fill = "hotpink") + 
  geom_jitter(color = "purple", size = 0.5) +
  coord_flip()+
  annotate("text", y=117042.8 , x= 0, label= "117042.8", size = 3.5) +
  annotate("text", y=108540, x= 0, label= "108540", size = 3.5) +
  annotate("text", y=99666.25 , x= 0, label= "99666.25 ", size = 3.5) +
  annotate("text", y=117042.8, x= 100, label= "75%", size = 3.5) +
  annotate("text", y=108540, x= 100, label= "50%", size = 3.5) +
  annotate("text", y=99666.25 , x= 100, label= "25%", size = 3.5) +
  geom_hline(yintercept = 10000*H_n(10000), color = "green", show.legend = "nH_n")+
  ggtitle(label = "Boxplot when N = 10000")+
  theme_bw()
