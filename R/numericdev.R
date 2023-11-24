
#the different functions of the numericdev package
#the package allows:
#to derive functions numerically
#to represent the graph of functions
#to calculate the truncation, rounding errors and the total error
#function to calculate the numerical limit of a function

#limit of a function and numerical derivation
#we want to calculate the derivative numerically of the function f:
# we give ourselves a very small step h and we calculate the limit
#of the expression (f(x+h)-f(x))/h when x tends towards a x0 to
#obtain f'(x0) which is the the desired derivative

limit <- function(f,x0,h=10e-15) {


limval <- function(x){
  (f(x+h)-f(x))/h
}

lim <- limval(x0)
return(lim)

   }

#example :

# Calculate the limit of f(x) when x tends to 0

#f <- function(x) {
  #return(1/x)
#}

# Calculate the limit of f(x) when x tends to 0

#lim <- limit(f, 0)
#print(lim)

#In this example, the limit function takes the function f as an argument,
#the value x0 towards which x tends, and a small step h (by default at 1e-9)
#to calculate the numerical derivative of the function in x0. The digital derivative
#is an approximation of the limit.

#Make sure to define the function f you want to study,
#then call the limit function with this function and the value towards
#which you want to calculate the limit.


#function which allows us to trace a function and represents its tangent f'
#if it exists


#Representation of f and its tangent with the derivative found as slope



devplot <- function(f,x0)

  {

#tracing the curve and its tangent

curve(f,from = -10,to= 10,col="blue" , xlab = "abscises ",ylab = "ordinates"
      ,main= "curve of the function and its tangent")

pente <- limit(f,x0)


abline(a= f(x0)-pente*x0, b=pente,col= "red" )

}



#example

#f <- function(x){exp(x)-x}
#devplot(f,2)
#on obtient le graphe de la fonction et sa tangente


#calculating rounding and truncation error

#1- rounding error

 Ear <- function(f,x0,h=10^-9,delta=10^-12){

   rap <- function(x){
    2*delta*abs(f(x)/h)
   }

   return(rap(x0))
 }

 #x0 represents the point at which x tends
 #the relative precision of the machine


#2-truncation error


Etr <- function(f,x0,h=10^-9){

  limsecond <- function(x){
    (f(x+h)-2*f(x)+f(x-h))/h^2
  }
  tr <- (abs(h)*abs(limsecond(x0)))/2
  return(tr)
}

#This function requires before calculating the second derivative.
#x0 represents the point at which x tends


#3- Total error

Eto <- function(Ear,Etr){

  sum(Ear,Etr)
}

#this function allows you to obtain the total error which
#is the sum of the absolute error and the relative error
#when in an error there is the exponential e have rewritten "exp(x)"
#in the argument of Eto

