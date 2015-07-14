author-neeraj kumar
--------

### Data Structures and Subsetting

#### Explain your code where appropriate.

1. Create vectors of each of the different primitive types. i.e., integer, double, logical, and character. Create matrices by attaching `dim` attributes to those vectors. Look up the help for `dimnames` and attach `dimnames` to these resulting matrices.

* integer vector

```{r}
#look up help for dimnames.
help(dimnames)
?dimnames

#create vectors

x <- c(1:6)
as.integer(x)
is.integer(x) #to check if it is integer
is.vector(x) #to check if vector
dim(x)<-c(2,3) 
is.matrix(x) #to check if it is matrix
colnames(x)<-c("a","b","c")
rownames(x)<-c("A","B")
dimnames(x)
```

* double vector

```{r}
y <- c(1,2, 2.5, 4.5)
is.numeric(y)
is.double(y) #checking
dim(y)<-c(2,2)
is.matrix(y)
colnames(y)<-c("w","z")
dimnames(y) #only column names, No rownames, hence Null.
```


* logical vector

```{r}
z<-c(TRUE, TRUE, TRUE,FALSE,FALSE, FALSE)
is.logical(z) #check logical
dim(z)<-c(2,3)
rownames(z)<-(c("Y","N")) #only rownames, No column names, hence NULL.
dimnames(z)
```


* character vector

```{r}
w<-c("This", "is", "a", "vector")
is.character(w) #check
dim(w)<-c(2,2)
rownames(w)<-c("character" , "vector")
colnames(w)<-c("character" , "vector")
dimnames(w)
w
```

2. Create a list of length 4 and then add a `dim` attribute to it. What happens?


* the dimension attribute sets lists to list-matrices or list-arrays, these are relatively esoteric data structures, but can be useful if you want to arrange objects into a grid-like structure.


```{r}
list_example <- list(c("Masters of data science"), data.frame(c(1:6), 1:6, "TRUE"), 1:6, "FALSE")
length(list_example) #check length is 4
is.list(list_example) #check list
dim(list_example)<-c(2,2)
list_example
```


3. Look up the help page for `data.frame` and use the example code to create a small data frame containing numeric variables and factors.

```{r}
#look up help for dimnames.
help(data.frame)

#example code for data.frame used to create a small dataset by replacing letters by numeric variable and creating a factor for it.

Nvect <- c(1, 3, 17, 4, 5)
is.numeric(Nvect)

fac <- factor(sample(Nvect, 10, replace = TRUE))
is.factor(fac)

(d <- data.frame(x = 1, y = 1:10, fac = fac))
class(d$fac)
```

4. Use the `seq` function to generate a subscript vector that selects those elements of a vector that have even-numbered subscripts.

```{r}
#even-numbered subscripts.

Eseq <- seq(0, 10, by = 2)
Eseq
```

5. Verify that vectors can have duplicated names and that if a subscript matches a duplicated name, only the first value is returned. What happens with `x[NA]`?


* We can see that vectors can have duplicated names and if a subscript matches a duplicated name, only the first value is returned and with `x[NA]` we got what he had expected. NA values returned at all places.


```{r}
x <- 1:10
names(x) <- letters[1:10]
x[c("a", "b", "c", "d")]
names(x)[3] <- "a"
x["a"]
x[c("a","a")]
names(x)
x[NA]

```



6. Use logical subscripts to extract the even-numbered elements of the `letters` vector.

```{r}
##Even numbered elements of the 'letter' vector
l <- letters

#False for Odd, True for Even.

l[c(FALSE,TRUE)]

length(l[c(FALSE,TRUE)])

# Using match command we see and check for correctness of extraction of even numbered elements.

match(l[c(FALSE,TRUE)],l)
```

7. Let `x` be a vector of length 10 generated by `1:10` and suppose it has a dimension attribute so that it is a matrix with 2 columns and 5 rows. What is the matrix location of the 7th element of `x`? That is, which row and column is it in? Alternatively, which element of `x` is in the second row, first column?

```{r}
x<-c(1:10)
dim(x)<-c(5,2)
#2nd row and 1st column gives "2"
x[2,1]
#7the element is in 2nd row and 2nd col.
x[2,2]
```

8. What does `as.matrix()` do when applied to a data frame with columns of different types?


* The matrix container wants to store data all of the same type, hence our numeric andlogical values are being coerced into strings. What we thought was numeric and logical data has been converted to character data, which we subsequently try to add.

```{r}
#lets take an example.
d <- c(1,2,3,4) #double
e <- c("Data", "Science", "Stats", NA) #integer
f <- c(TRUE,TRUE,TRUE,FALSE) #logical
mydata <- data.frame(d,e,f) #dataframe of different type
as.matrix(mydata)
```

9. Fix each of the following common data frame subsetting errors:


* mtcars[mtcars$cyl = 4, ]


```{r}

#corrected 
mtcars[mtcars$cyl == 4, ]
```


* mtcars[-1:4, ] 


```{r}

#corrected.


#if we are trying to extract the rows except first four, then correct way is this
mtcars[(-1:4), ]

#if we are trying to correct the rows for 1st 4 rows in extraction, then
mtcars[1:4, ]
```


* mtcars[mtcars$cyl <= 5]


```{r}

#corrected.
mtcars$cyl
#get everything less than or equal to 5, which is 4 only.
mtcars$cyl[mtcars$cyl <= 5]
```


* mtcars[mtcars$cyl == 4 | 6, ] #this did not work because it selected 4,6,8 while we wanted 4 or 6.


```{r}


#corrected.
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]
```

10. Consider the linear model: `mod <- lm(mpg ~ wt, data = mtcars)`. Describe the data structure of `mod`, including its componets. Extract the coefficients, residuals, and the residual degrees of freedom. Extract the R squared from the model summary, i.e., from `summary(mod)`.

```{r}
mod <- lm(mpg ~ wt, data = mtcars)
```


* structure of the model.

We see the list of 12 elements, followed by the model definition that contains 32 obs for 2 variables. Attribute attached with each definition are like intercepts, coefficients, environment, order, classes etc.

```{r}
str(mod)
```


* coefficient of the model.

```{r}
coef(mod) #base coefficient.
coef(summary(mod)) #extended coefficients of the model.
summary(mod)$coef
```


* Residual of the model.


```{r}
residuals(mod)

#or

summary(mod)$residuals
```


* Residual degree of freedom in the model.

```{r}
df.residual(mod)

#we can chekc this by calling summary function on the model.
summary(mod)$df

#df=30
```


* R-squared value of the model.

```{r}
summary(mod)$r.squared
summary(mod)$adj.r.squared
```
