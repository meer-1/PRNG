################################################################################
#'  Logistic map
#'
#'  This is the most used chaotic map . The map is sensitive for the value of the parameter greater than 3.568
#' @param x0 the seed value range from 0 to 1
#' @param a   the parameter ranging from 3.5 to 4
#'
#' @return  the map returns the a*x(1-x) for input x
#' @export
#'
#' @examples logistic_map(0.26,3.5)
#'
#'
#'
logistic_map <- function(x0, a) {
  x <- as.numeric(a) * as.numeric(x0) * (1.0 - as.numeric(x0))
  return(x)
}












################################################################################
#' Saw tooth map
#'
#'
#'  saw tooth map is a family of maps as f(x)=b*x mod1
#' @param x0 seed value ranging from 0 to 1
#'
#' @return (3*x) mod(1)
#'
#' @export
#' @examples saw_tooth(0.6)
saw_tooth <- function(x0) {
  x <- (3 * as.numeric(x0)) %% 1
  return(x)
}



################################################################################
#' Baker map
#'
#'
#' this is a chaotic map with the sensitive for the parameter value greater than 0.5
#' @param x0 seed value
#' @param a parameter of the map range is greater than  0.5
#'
#' @return for 0<=x=<1/2 the map returns 2ax
#'         for 1/2<=x<=1 the map returns a(2x-1)mod1
#'
#' @export
#'
#' @examples baker_map(0.3,0.56)
baker_map <- function(x0, a) {
  if (x0 >= 0 & x0 < 1/2) {
    return(2 * a * x0)
  } else if (x0 >= 1/2 & x0 <= 1) {
    return(a * (2 * x0 - 1) %% 1)
  }
}

################################################################################


#' Linear congruence map
#'
#' the map is a member  of the family of the maps f(x)=(ax+b) mod(n)
#' @param x0 seed value
#'
#' @return the map gives an integer ax+b mod(n)
#'
#' @export
#'
#'
#' @examples linear_con(5)
#'
#'
linear_con <- function(x0) {
  x <- ((7^5) * x0) %% (2^31 - 1)
  return(x)
}





######################################################################3
#' time function
#'
#' This function is used to generate a time of the system to be used for generating time dependent random numbers   precise upto micro-seconds
#'
#' @return t fractional value of the time
#' @export
#'
#' @examples time()
time<-function()
{
  time_with_fractional_seconds <- format(Sys.time(), "%OS6")
  
  
  # Extract the fractional seconds
  fractional_seconds <- sub("^0:", "", time_with_fractional_seconds)
  
  # return the fractional part as numeric value
  t=as.numeric(fractional_seconds)%%1
  
  return(t)
  
}




###########################################################################
#' Uniformly  Pseudo random  number generator
#'
#'this function generates random numbers which follow uniform distribution \code{[0,1]}

#'
#'
#'
#'
#' @param a1 parameter of logistic map the value takes from 3.5 to 4
#' @param a2 parameter of baker map the value it takes values greater than or equalt to 0.5
#' @param x00 seed value of saw-tooth map values from 0 to 1
#' @param x01 seed value of logistic map values from 0 to 1
#' @param n0  seed value of linear congruence map   it can take value of any  natural number
#' @param x02  seed value of baker map
#' @param N How many  numbers  are required
#'@param Time if enabled TRUE the numbers are time dependent
#'
#' @return gives a vector  of pseudo random numbers generated of desired length
#'
#' @export
#' @examples   runf(10)
#' runf(10,Time=TRUE)
#' runf(10,Time=TRUE)
#' runf(10,Time=TRUE)
#' runf(10,2)
#' runf(10,Time=TRUE,2)
#' runf(10,Time=TRUE,2)
#'
#' runf(10,5,0.52)
#'  runf(15,2,0.352)
#'
#'   runf(10,2,0.652,0.235)
#'    runf(10,Time=TRUE,2,0.652,0.235)
#'   runf(9,7,0.52,0.4235,0.389)
#'   runf(10,Time=TRUE,2,0.752,0.235,0.351,3.8)

runf <- function(N = 100, Time = TRUE, n0 = 5, x00 = 0.5362, x01 = 0.357, x02 = 0.235, a1 = 3.69, a2 = 0.7) {
  
  if (!is.numeric(N) || N <= 0) {
    stop("N must be a positive integer")
  }
  if (!is.numeric(x00) || x00 < 0 || x00 > 1) {
    stop("x00 must be in the range [0, 1]")
  }
  if (!is.numeric(x01) || x01 < 0 || x01 > 1) {
    stop("x01 must be in the range [0, 1]")
  }
  if (!is.numeric(x02) || x02 < 0 || x02 > 1) {
    stop("x02 must be in the range [0, 1]")
  }
  if (!is.numeric(a1) || a1 < 3.5 || a1 > 4) {
    stop("a1 must be in the range [3.5, 4]")
  }
  if (!is.numeric(a2) || a2 < 0.5) {
    stop("a2 must be >= 0.5")
  }
  
  
  
  
  N <- N + 10
  if (Time == FALSE) {
    x000 <- x00
    x010 <- x01
    x020 <- x02
  } else {
    t = time()
    x000 <- (x00 + t) %% 1
    x010 <- (x01 + t) %% 1
    x020 <- (x02 + t) %% 1
  }
  
  a10 <- a1
  a20 <- a2
  n00 <- n0
  output <- c()
  
  for (j in 1:N) {
    a10 <- a10 + 1 / sqrt(N)
    a20 <- a20 + 1 / sqrt(N)
    
    n <- linear_con(n00)
    n1 <- n %% 3
    
    if (n1 == 0) {
      x <- saw_tooth(x000)
      z <- as.numeric(x) %% 1
      output <- c(output, z)
      x000 <- x
    } else if (n1 == 1) {
      x <- logistic_map(x010, a10)
      z <- as.numeric(x) %% 1
      output <- c(output, z)
      x010 <- z
    } else {
      x <- baker_map(x020, a20)
      z <- as.numeric(x) %% 1
      output <- c(output, z)
      x020 <- z
    }
    n00 <- n
  }
  return(output[-c(1:10)])
}

#
###################################################################
###################################################################
###################################################################



#' Generating numbers form    Normal distribution
#' here we use  Box Muler transform to obtain normal random variable

#' @param n number required
#'
#' @return a list of pseudo random numbers from normal distribution
#' @export
#'
#' @examples rnorm(10)
#'  rnorm(100)
#'
rnorm<- function(n)
{
  u1=runf(n)
  u2=runf(n)
  return(sqrt(-2*log(u1))*cos(2*pi*u2))
}

###################################
##################################
## Random bits generator##########
###################################


#' Random Bit generator
#'
#'this function generates random bits of desired length
#' @param n number of bits required
#' @param Time it is a boolean value of TRUE/FALSE if we want to generate time dependent random bits.i.e each time we call the function with same input different output will be generated.
#'
#' @return returns a vector of random bits of length n
#' @export
#'
#' @examples rbits(2)
#' rbits(2)
#' rbits(2,Time=FALSE)
#' rbits(2,Time=FALSE)
#'  rbits(10)
#'
rbits<-function(n,Time=TRUE)
{
  a=runf(n,Time)
  
  b=rep(0,length(a))
  
  b[a>=0.5]=1
  return (b)
}



#########################################
######Exponentail distribution###########
#########################################
#' Exponentail distribution
#'
#' This function generates random numbers from exponentail distribution
#'
#' @param n how many numbers we need
#' @param Time  time dependent or not
#'
#' @return  a vector of n numbers from exponential distribution
#' @export
#'
#' @examples  rexp(10)
#'    rexp(10)
#'    rexp(10,FALSE)
#'    rexp(10,FALSE)
rexp<-function(n,Time=TRUE)
{
  return(-log(1-runf(n,Time),base = exp(1))/5)
}



#########################################
######Cauchy distribution###########
#########################################
#' Cauchy distribution
#'
#'This function generates random numbers from standard  cauchy distribution
#' @param n How many numbers we want
#'
#' @param Time time dependent or not
#'
#' @return   a vector of n numbers from cauchy distribution
#' @export
#'
#' @examples
#' rcauchy(10)
#' rcauchy(10,Time=TRUE)
#' rcauchy(10,Time=TRUE)
rcauchy<-function(n,Time=TRUE)
{
  
  return(tan(pi*(runf(n,Time)-0.5)))
}














