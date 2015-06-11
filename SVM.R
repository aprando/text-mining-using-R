install.packages("e1071")
library(e1071)

day = c(0,1,2,3,4,5,6)
weather = c(1,0,0,0,0,0,0)
happy = factor(c(T,F,F,F,F,F,F))

d = data.frame(day=day, weather=weather, happy=happy)
model = svm(happy ~ day + weather, data = d)
plot(model, d)
