library(tictoc)
wprefermovermpair <- function(n, w, m1, m2){
  for(i in 1:n){
    if(prefer[w, i] == m1) return(1)
    if(prefer[w, i] == m2) return(0)
  }
}

set.seed(100)

gale_shapley <- function(n){
  
  #### preference list of women
  #### rejection list of man(woman already rejected that man)
  ### woman_pref, man_reject
  count_iter <- 0
  partner_woman <- rep(-1, n)
  free_man <- rep(1, n) ### 1 denotes that (i-1)th index man is free
  count_free_man <- n
  count_free_man
  while(count_free_man > 0){
    ### choosing a man to be paired who is not already paired
    for(i in 1:n){
      #count_iter <- count_iter + 1
      if(free_man[i] == 1){
        m <- i
        break
      }
    }
    #cat("man", m)
    #print(m)
    while(free_man[m] == 1){
      count_iter <- count_iter + 1
      woman <- sample(c(1:n), size = 1)
      if(partner_woman[woman] == -1){
        free_man[m] = 0
        count_free_man = count_free_man - 1
        partner_woman[woman] = m
      }
      else {  ### if woman is already paired
        mpair = partner_woman[woman]
        if(wprefermovermpair(n, woman, m , mpair) == 1){
          free_man[m] = 0
          partner_woman[woman] = m
          free_man[mpair] = 1
        }
      }
      #cat("woman", woman)
      #cat("partner", partner_woman[woman], "\n")
      #print(woman)
      #print(partner_woman[woman])
    }
    
  }
  return(list(partner_woman, count_iter))
  
}

### 1st row is preference list of 1st woman and so on
#prefer <- matrix(c(1,2,3,4, 3,1,2,4, 1,2,4,3, 4,1,3,2), nrow = 4, ncol = 4, byrow = TRUE)

tic()
t <- gale_shapley(4)
toc()

count <- numeric(1e3)
tic()
for(i in 1:1e3){
  prefer <- matrix(c(sapply(1:i, function(x) sample(1:i, i, replace = FALSE))), nrow = i, ncol = i, byrow = TRUE)
  count[i] <- gale_shapley(i)[[2]]
}
toc()

dat <- as.data.frame(cbind(1:1e3, count))
colnames(dat) <- c("N", "Time")
### Plotting the number of proposals
library(ggplot2)
fxn <- function(x){x^log(x)}
ggplot(dat, aes(x = N, y = Time)) + 
  geom_point() + 
  xlim(0,1e3) + 
  ylim(0, 1e4) + 
  geom_line(aes(N, N*log(N)), col = "red") +
  geom_line(aes(N, N^(4/3)), col = "blue") + 
  geom_smooth(method="nls", se=FALSE, formula=y~a*x*log(x)+k,
              method.args=list(start=c(a=1, k=1)), col = "gold") +
  geom_line(aes(N, N^(5/4)), col = "green") + 
  geom_line(aes(N, N^(7/5)), col = "brown") + 
  geom_line(aes(N, N^2), col = "black") 
plot(count)


n <- 5
counts <- numeric(1e3)
for(i in 1:1e3){
  prefer <- matrix(c(sapply(1:n, function(x) sample(1:n, n, replace = FALSE))), nrow = n, ncol = n, byrow = TRUE)
  counts[i] <- gale_shapley(n)[[2]]
}
plot(counts)
abline(h = 25, col = "red")
