

## Installing packages in R
#install.packages("dplyr")
#install.packages("ggplot2)

## Importing packages in R
library(dplyr)
library(ggplot2)
library(parallel)


## PART ONE: Answer / write code for all the TODOs. This is best for people who this is the first time programming
## and will teach you the basics of R

partOne <- function() {
  print("STARTING PART ONE")
  ## Reading data
  ## TODO: move this into an external CSV file and rewrite this
  a <- "a,b,c\n1,3,2\n4,5,6\n7,8,9\n10,11,12\n15,20,25"
  con <- textConnection(a)
  data <- read.csv(con)
  close(con)


  print(data[4,]) #print the 4th row
  print(data[,"a"]) #print the "a" column
  rownames(data) = c("A", "B", "C", "D", "E") #name the rows
  print(data["A",]) #print the "A" row
  sub_data <- filter(data, b %% 2 == 0) #only keep any rows where the b value is even
  print(nrow(sub_data)) # how many rows are still left?
  
  ## TODO: add a new row to the data, name it, and print the new row!
  ## TODO: modify the filter statement above so that it now prints out rows where b is even 
  ## and c is a multiple of three
  
  ## TODO: Use ggplot2 to make a basic scatter plot, with the "a" variable on the x-axis and "b" variable on the y-axis  
  
  reg_c <- lm(c ~ a + b, data)
  reg_c_alternative <- lm(data$c ~ data$a + data$b) #alternative way to run the same regression
  reg_c_no_intercept <- lm(c ~ 0 + a + b, data) # Regress c on a + b with no intercepts 
  print(reg_c) #print results
  print(reg_c_no_intercept)
  print(summary(reg_c_no_intercept)) # print summary of regression
  
  #TODO: Write a regression of c = beta_1*a + beta_2*a^2 + beta_3*b
  
  # sum the numbers from 1 to 6
  sum = 1
  sum = sum + 2
  sum = sum + 3
  sum = sum + 4
  sum = sum + 5
  sum = sum + 6
  print(sum)
  # TODO: think about what's wrong with this? how would you rewrite it?
  # TODO: Suppose I wanted the sum for all numbers from 1 to 100 (without using math tricks). How would you write this?
  # TODO: Suppose I wanted the sum of f(1) to f(100) for some function f. How would you write this?
  # TODO: Suppose I wanted the sum of (f(1) + 1 + f(2) + 2 +... + f(100) + 100).
  # TODO: Suppose I changed 100 to some N, write your code so that this only requires changing the value of a variable.

  # TODO: write a function that prints YES if some value passed to the function is even and NO if that value is odd
  
  #TODO: understand (roughly) why the output isn't 0 (relevant readings: https://blog.codinghorror.com/why-do-computers-suck-at-math/, http://fabiensanglard.net/floating_point_visually_explained/)
  a <- 1.0 - 0.9 - 0.1
  print(a)
  
  
  ## TODO: think about why are the comments I'm leaving bad in general, but good in this context
}



partOne()




## PART 2: Answer / write code for all the TODOs. This is a bit harder than the one above and more intended for people who have more programming experience


print("STARTING PART TWO")

## TODO: Write this function that, given the return object from summary(lm(...)), 
## pulls out the regression coefficient for some variable passed into the function
## e.g. for y = beta_0 + beta_1 *x, akcd(reg, "x") = beta_1
## ALSO TODO: Rename the function to something sensible.
akcd <- function() {
}

## TODO: Write this function that, given the return object from summary(lm(...))
## checks if some value passed into the function is in the 95% confidence interval for the regression
## e.g. for y = beta_0 + beta_1*x, bkcd(reg_summ, "x", 2) = TRUE
## ALSO TODO: rename the function to something sensible.
bkcd <- function() {
  critical_value <- 1.96
}


## Suppose we are given that y = 2x + u, where u ~ N(0, 1), x ~ Exp(1) 
## and want to generate enough simulations of y, x, and u that when we run lm(y ~ x) we get a coefficient of 2
## Below I've implemented a naive version of this, but our goal is to rewrite it
##

naive_version <- function (num_samples) {
  x_vals <- c()
  u_vals <- c()
  y_vals <- c()
  for (i in 1:num_samples) {
    x_vals <- c(x_vals, rexp(1))
    u_vals <- c(u_vals, rnorm(1))
    y_vals <- c(y_vals, 2 * x_vals[i] + u_vals[i])
  }
  reg <- lm(y_vals ~ x_vals)
  
  #TODO implement bkcd and akcd from above
  res <- bkcd(summary(reg))
  print(reg)
  ## TODO: print YAY if 2 is in the resulting confidence band around 'x' and NAY if not.
}


# TODO: Keep the for loop in the naive function, speed up the function above by making the portion inside the for-loop more efficient
less_naive <- function(num_samples) {
}

# TODO: Now, refactor the less_naive function to be able to use the apply function
# Docs for apply: https://www.rdocumentation.org/packages/base/versions/3.4.3/topics/apply
even_less_naive <- function(num_samples) {
}

# TODO: Read about the vectorization (http://www.noamross.net/blog/2014/4/16/vectorization-in-r--why.html). Now, rewrite
# the same function as above but in a vectorized manner.

the_least_naive_function <- function(num_sample) {
}

# TODO: Convince yourself that parallelization in this context is a good idea.
# What about this context allows us to parallelize? Think about the contexts where parallelization doesn't make sense and read up briefly on the idea
# behind Amdahl's Law (https://en.wikipedia.org/wiki/Amdahl%27s_law)

# TODO: now let's implement a (simple) parallelized version of our even_less_naive code using the parallel package (https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf)

not_so_naive <- function(num_samples) {
}


# TODO: run this without modification...does it finish?
NUM_SAMPLES <- 1000000
start_time <- Sys.time()
naive_version(NUM_SAMPLES) # change this for different versions of the implementation
end_time <- Sys.time()
cat("Simulation took", difftime(end_time, start_time, units = "secs"), "seconds")

# TODO: think of a good way to rename the different functions above


# TODO: after you've written all the code above, let's see what the tradeoffs for parallelization are: 

# Compute the time difference between the not_so_naive and even_less_naive functions for ONE run of the following values of NUM_SAMPLES: (100, 10000, 1000000, 5000000)
# Do you ever see that the parallelized version is slower than the non-parallelized version (why would this be the case?)?

# TODO: Final question! Why is the method above a bad method of benchmarking (relevant question to ask yourself: are runtimes across runs deterministic?)? If you wanted to benchmark
# properly what are some things you could change?


