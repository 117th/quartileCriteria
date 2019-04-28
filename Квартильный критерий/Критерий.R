getQuartiles <- function(x, y){
  merged = c(x, y)
  merged = sort(merged)
  
  a = vector()
  
  print("MERGED VECTOR:")
  print(merged)
  
  step = (length(x) + length(y))/4
  
  x = sort(x)
  index = 1
  
  for(i in 1:4){
    a[i] = 0
    
    for(j in(((i-1)*step + 1):(i*step))){
      if(index > length(x)) break
      if(merged[j] == x[index]){
        a[i] = a[i] + 1
        index = index + 1
      }
    }
  }
  
  return(a)
}

x = c(1, 2, 3, 5, -2, 1, 2, 20)
y = c(103, -23, -103, 1, -100, -1, -1, -105, 23, 54, 53, 98)

alpha = 0.95

m = length(x)
n = length(y)

if((m + n) %% 4 != 0) stop("m + n %% 4 != 0")

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

print(paste("X = ", x))
print(paste("Y = ", y))
print(paste("a = ", a))
print(paste("s = ", s))
print(paste("d0 = ", dZero))
print(paste("d1 = ", dOne))
print(paste("s~ = ", sMean))
print(paste("d0~ = ", dZeroMean))
print(paste("d1~ = ", dOneMean))
print(paste("D = ", D))
print(paste("X^2 = ", chiSq))
print(paste("ACCEPT: ", result))