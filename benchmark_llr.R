source("C:/Users/LIYIJIA/Desktop/STAT-610/Homework/hw6/llr_functions.R")
# Try out functions to see if it works on some data:
library(reshape2) # package that contains "french_fries"
data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 2)
plot(z, fits)

# Loop through different omega values
for (i in 1:10){
  fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = i)
  plot(z, fits, main = paste("llr with ω =", i),  xlab= expression(z), ylab = expression(hat(f)(z)))
}

library(microbenchmark) 
result <- microbenchmark(
  fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 1)
  )
cat("microbenchmark result:\n")
print(result)
