# Bot301, April 2018
# With Kath Smart
# From the lecture (with a bit of expansion)
#---------------------------------------------------------------------------

# Simple dialouge
1+1
1/2
sqrt(9)
pi
cos(pi)
3x9 #This does not work for multiplication, why? Let's find help:
?+ #We know plus worked, so let's start there 
  
# Assignment
x<-1
x #view x

X<-2
X #view capital X - R is case sensitive

x+X #simple arithmetic with objects

# Naming
MyObject<-1
My.object<-2
my_object<-3
my-object<-4


#---------------------------------------------------------------------------
#Functions

#Use the sum function, sum() to add up numbers
x<-sum(1,2,3)
?sum

#Use the replicate function, rep() to replicate an argument
rep(1, times=3)
?rep
rep(MyObject, 3)

#---------------------------------------------------------------------------
# Vectors + functions

# Exploring numeric vectors
#create a vector with the concatenate function, c()
x <- c(1, 2, 3, 4) # create a vector containing the four values, 1, 2, 3, 4 
# and store it with the name "x"
x
class(x) # check the data type
length(x) # length() gives you the number of elements in the vector
?length
# alternatives are nrow() or ncol() for working with matrice or dataframes

y <- c(x, x) # create a new object 'y' by combining 'x' twice
length(y)

z<-cbind(x,x) #create a new object 'z' using cbind()
##this wil only work if the elements are the same length for both 'x' and 'x'
class(z)
z.v1<-cbind(col1=x, col2=x) #create a different version of z and tell R to change column names to col1 and col2
z
?cbind #what does cbind() do?

zz<-rbind(x,x) #using the alternative usage listed in the help file, rbind()
zz #this arranges the objects into rows instead of columns

# Now lets add some characters into the mix
a<-c('a','b','c','d') # notice that characters must be in quotation marks
class(a)

xa<-c(x,a)
xa # view xa
#notice that by concatentating x and a, R converts the numbers to characters
class(xa)
typeof(xa)

as.numeric(xa) # you can try to convert it all to numbers, but you loose information

matrix.xa<-cbind(x,a)
matrix.xa
dataframe.xa<-data.frame(x,a)
dataframe.xa 
# but we seem to have lost the quotation marks around the characters...
# data.frame() converts the class of the data, from character to factor 

#---------------------------------------------------------------------------
# Some more basic arithmetic and simple functions
x + 10
x/10
sqrt(x)
min(x)
max(x)
range(x)
mean(x)
median(x)
sd(x)
summary(x)
