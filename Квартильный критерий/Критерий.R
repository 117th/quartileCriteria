getQuartiles <- function(x, y){
  merged = c(x, y)
  merged = sort(merged)
  
  a = vector()
  
  step = (length(x) + length(y))/4
  
  index = 1
  
  for(i in 1:4){
    a[i] = 0
    
    for(j in(((i-1)*step + 1):(i*step))){
      if(merged[j] == x[index]){
        a[i] = a[i] + 1
        index = index + 1
      }
    }
  }
  
  return(a)
}

x = c(1, 3, 5, 7, 9, 11, 13,
      18, 21, 23, 25, 30, 32, 34, 35, 37)
y = c(-8, -7, 4, 5, 6, 7, 8,
      12, 14, 15, 18, 19, 20, 21, 23, 25)

alpha = 0.9

m = length(x)
n = length(y)

a = getQuartiles(x, y)

s = a[1] + a[4]
dZero = a[4] - a[1]
dOne = a[3] - a[2]

varS = m*n/(4*(m + n -1))
varD = m*n/(2*(m + n - 1))

sMean = (s - m/2)/((varS)^(1/2))
dZeroMean = dZero/(varD^(1/2))
dOneMean = dOne/(varD^(1/2))

D = sMean^2 + dZeroMean^2 + dOneMean^2

chiSq = qchisq(alpha, 3, FALSE)

#H0: совпадение
#H1: различие

result = if(D > chiSq) "H1" else "H0"