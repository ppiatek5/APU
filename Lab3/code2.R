setwd("C:/Users/mrbre/Desktop/Studia/APU/Lab3/")
install.packages("neuralnet")
library("neuralnet")

traininginput <- as.data.frame(matrix(c(8,6,12,8,6,6,8,8,6,6,8,8,8,16,16,
                                        128, 128, 512, 128, 128, 128, 256, 256, 128, 128, 128, 64, 64, 128, 128), nrow=15, ncol=2))
trainingoutput <- c(2099, 1649, 6599, 2399, 1899, 1099, 3499, 4499, 2999, 1199, 3999, 1599, 1999, 2599, 2999)

maxs <- apply(traininginput,2,max)
mins <- apply(traininginput,2,min)


scaled.traininginput <- as.data.frame(scale((traininginput), center = mins, scale = maxs - mins))

print(head(scaled.traininginput, 15))

trainingdata <- cbind(scaled.traininginput, trainingoutput)
trainingdata <- cbind(scaled.traininginput, as.data.frame(scale(trainingoutput, center = min(trainingoutput), scale = max(trainingoutput - min(trainingoutput)))))
colnames(trainingdata) <- c("RAM", "Pamiec", "Cena")

print(trainingdata)

net.mathfunc <- neuralnet(Cena ~ RAM+Pamiec, trainingdata, hidden = c(7,20,20,10,5), threshold = 0.001, stepmax = 1e7)

print(net.mathfunc)
plot(net.mathfunc)

testdata <- as.data.frame(matrix(
  c(8,8,128,64),
  nrow = 2,
  ncol = 2
))

scaled.testdata <- as.data.frame(scale(testdata, center = mins, scale = maxs - mins))

net.results <- compute(net.mathfunc, scaled.testdata)
ls(net.results)
print(net.results$net.result * max(trainingoutput - min(trainingoutput)) + min(trainingoutput))

