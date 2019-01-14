##########################
#
# Urban Simulation - Introduction to R
#
##########################


# Some useful data analysis tools and techniques in R

# A vector, different ways of generating
v <- c(1,2,3)
v
v[1]
v[1] + 2

?rep
v <- rep(0, 10)
v
v <- rep(c(1,2), 10)
v

?seq
v <- seq(0,1,length.out = 10)
v
v <- seq(0,2,0.1)
v
length(v)
v[21]

v <- 1:10
v
v <- 10:1
v

sort(v)
sort(v, decreasing = T)


# Dataframes
df <- data.frame(a=v, b=100:109)
df

df[,1]
df[,'a']
df[,'b']
df[,c('a','b')]
df$b

df[3,'a']
df[3,'a'] + 2

df[seq(2,6,1), 'a']
df[2:6, 'b']

?which
which(c(T,F,T,T,F))
which(c(1,2,4,5)>2)
df[which(df$a >4),]

ncol(df)
nrow(df)

# Add new column
?runif
?floor
df$rand <- floor(runif(10,0,100))
df

?order
order(-df$rand)
df[order(df$rand, decreasing = T),]


# Matrices
m <- matrix(data = 1:25, nrow = 5, ncol = 5)
m
m.2 <- matrix(data = 1:20, nrow = 5, ncol = 4)
m.2
m.3 <- matrix(data = 1:5, nrow = 5, ncol = 1)
m.3

m.4 <- m%*%m.3
m.4

m.3 <- matrix(1:5, nrow = 1, ncol = 5)
m.3

m%*%m.3 # Errors, wrong dim of vector
m.3%*%m

m[5,2]


# Lists
l <- list('a','b','c')
l
l[1]
class(l[1]) # Also a list
l[[1]]
class(l[[1]]) # Character

l <- list(1:10)
l[1]
l <- list(1:10, 1:10, 1:10)
l
l[3]
class(l[3][1]) # List. The third item in the overall list is also a list
class((1:10)[1]) # This is an integer though - weird
l[[3]][1]


# For loop
for (i in 1:length(v)) {
  v[i] <- v[i] * 2
}
v

# Can do this wthout an explicit for loop
v <- 1:10
v
v*2

df$a <- df$a + 5
df

df$sum_ab <- df$a + df$b
df$sum_ab


# If clause
if (all(v==7)) {
  print("Vector is all 7")
} else {
  print(42)
}


# Probability distributions

# Uniform distribution
x <- runif(10, min = 1, max = 5)
x

# Normal
x <- rnorm(10, mean = 2.5, sd = 2)
x

# Binomial
x <- rbinom(10, size = 1, p = 0.8)
x

# Sampling
x <- 1:15
sample(x, size = 5, replace = FALSE, prob = NULL)
x
sample(x, size = 5, replace = T, prob = NULL) # Can sample the same value twice
x

# Produce weights vector to sample by
w <- (15:1)/sum(15:1)
w
sample(x, size = 5, replace = F, prob = w)


# Plotting data
plot(df)
plot(df[,c('a','b')])
plot(df[,c('a','rand')])
plot(v)

palette <- c("steelblue4","slateblue3","palevioletred3","lightcoral","orange","chartreuse2","aquamarine4","turquoise3","royalblue1")
x1 <- rnorm(1000, mean = 5, sd = 1)
y1 <- runif(1000, min = 11, max = 19)
x2 <- rnorm(1000, mean = 15, sd = 1)
y2 <- rnorm(1000, mean = 5, sd = 2)
x3 <- runif(1000, min = 20, max = 30)
y3 <- rnorm(1000, mean = 20, sd = 5)
x4 <- runif(1000, min = 0, max = 10)
y4 <- runif(1000, min = 0, max = 3)

plot(x1,y1, pch = 1, cex = 0.5, col = palette[5], xlim = c(0,30), ylim = c(0, 30), main = 'Title', xlab = 'x-axis', ylab = 'y-axis')
points(x2,y2,col=palette[7], pch = 4, cex = 0.5)
points(x3,y3,col=palette[1], pch = 4, cex = 0.5)
points(x4,y4,col=palette[2], pch = 4, cex = 0.5)


# Linear regression model
m <- lm(a ~ b, data = df)
m
plot(df[,c('a','b')])

?cat # Like print
cat("R=",(cor(df, use="complete.obs", method="pearson"))[2] ,", R2=",summary(m)$r.squared,", adjusted R2=",summary(m)$adj.r.squared,", p-value=",anova(m)$'Pr(>F)'[1]," slope=",m$coefficients[[2]]," intercept=",m$coefficients[[1]])

library(ggplot2)
ggplot(df, aes(x=a, y=b)) + geom_smooth(method = 'lm', formula = y ~ x) + geom_point()


# Functions

# Why using equals sign here?
getCorrelation <-  function(x,y,logged = T){
  if(logged){
    x <- log(x)
    y <- log(y)
  }
  df <-  data.frame(y=y, x=x)
  m <- lm(y ~ x, data = df)
  cat("R=",(cor(df, use="complete.obs", method="pearson"))[2] ,", R2=",summary(m)$r.squared,", adjusted R2=",summary(m)$adj.r.squared,", p-value=",anova(m)$'Pr(>F)'[1]," slope=",m$coefficients[[2]]," intercept=",m$coefficients[[1]])
  print(ggplot(data = df, aes(x=x, y=y)) + geom_smooth(method = 'lm', formula = y ~ x) + geom_point())
  
}

getCorrelation(df$a, df$b)
getCorrelation(df$a, df$b, logged = F)


# Remainging exercises are more advanced data manipulations like merging dataframes together and performing aggregations
