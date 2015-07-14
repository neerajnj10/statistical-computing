author-neeraj kumar
--------

###  Functions

#### Explain your code where appropriate.

1. Create a function to perform a one-way analysis of variance. The input should be a list consisting of (possibly) named components, one for each group. The output should be list containing components for the between SS, the within SS, the between degrees of freedom, and the within degrees of freedom. 
```{r}
oneway <- function(z) {
    col1 <- z[,1]  # list column 1 
    col2 <- z[,2]  # list column 2 
    nn <- tapply(col1, col2, length)
    means <- tapply(col1, col2, mean)
    vars <- tapply(col1,col2, var)
    level <- levels(col2)
    a <- length(levels(col2))  # number of groups
    N <- length(col1)           # number of observations
    TSS <- sum(col1^2)          # total SS
    CFSS <- sum(col1)^2/N
    Dfa <- a-1    # between degress of freedom
    Df <- N-a      # within degree of freedom  
    sum_a <- tapply(col1, col2, sum)
    aSS <- sum((sum_a^2)/nn)
    SSa <- aSS - CFSS   # between SS
    SSe <- TSS - aSS    # within SS
    list( Dfa = Dfa, Df = Df, SSa = SSa, SSe = SSe)   #output
   
}
```

2. Create a function to summarize the output in a one-way ANOVA table, including the F test. The input is the output list in the previous question. The output should be one-way ANOVA table.
```{r}
oneway.table <- function(x) {
    x <- oneway(x)
    MSa <- x$SSa/x$Dfa   # between MS
    MSe <- x$SSe/x$Df     # within MS
    F.value <- signif(MSa/MSe, digits = 4)   # F test
    p.value <- signif(1 - pf(F.value, x$Dfa, x$Df), digits = 4) # p value
 
  cat("Df SumSquare MeanSquare f-value pr(>F):\n")
  cat(paste(x$Dfa,x$SSa,MSa,F.value,p.value, sep="    "))
  cat("\n")
  cat(paste(x$Df,x$SSe,MSe,sep="   "))


    }
```

3. Your functions should be illustrated with the `coagulation` data set. The data consists of blood coagulation times for 24 animals randomly assigned to four different diets.
```{r}
library(faraway)
data(coagulation)
head(coagulation)
oneway.table(coagulation)

```
