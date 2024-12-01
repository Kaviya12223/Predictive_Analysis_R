#ANN
#mtacrs-inbuild dataset
#1/12/2024

installed.packages("neuralnet")
library(neuralnet)

s = mtcars
head(s)

s = s[,1:7]

ind = sample(2, nrow(s), 0.7*nrow(s))
train = s[ind, ]
test = s[-ind, ]

n = function(x){
  (x-min(x)) / (max(x) - min(x))
}

tr = as.data.frame(lapply(train[,1:7],n))
te = as.data.frame(lapply(test[,1:7],n))

ann = neuralnet(mpg ~ ., data = train, hidden = c(5,3), linear.output = T)
plot(ann)
