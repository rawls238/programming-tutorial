

## Installing packages in R
#install.packages("dplyr")
#install.packages("ggplot2)

## Importing packages in R
library(dplyr)
library(ggplot2)
library(parallel)

WORKING_DIRECTORY <- "/Users/garidor/Desktop/" #you should change this to the directory on your computer that you are correctlyw orking in!


## PART ONE: Answer / write code for all the TODOs. This is best for people who this is the first time

partOne <- function() {
  print("STARTING PART ONE")
  ## Reading data
  ## TODO: move this into an external CSV file and rewrite this
  data <- read.csv(paste(WORKING_DIRECTORY, "tutorial_dat.csv", sep=""))
  
  
  print(data[4,]) #print the 4th row
  print(data[,"a"]) #print the "a" column
  rownames(data) = c("A", "B", "C", "D", "E") #name the rows
  print(data["A",]) #print the "A" row
  sub_data <- filter(data, b %% 2 == 0) #only keep any rows where the b value is even
  print(nrow(sub_data)) # how many rows are still left?
  data["new",] = c(1, 1, 1, 1, 1)
  print(data["new",])
  sub_sub_data <- filter(data, b%%2 == 0 & c %%3 == 0)
  print(sub_sub_data)
  
  ## TODO: add a new row to the data, name it, and print the new row!
  ## TODO: modify the filter statement above so that it now prints out rows where b is even 
  ## and c is a multiple of three
  
  ## TODO: Use ggplot2 to make a basic scatter plot, with the "a" variable on the x-axis and "b" variable on the y-axis  
  
  qplot(data['a'], data['b'])
  reg_c <- lm(c ~ a + b, data)
  reg_c_alternative <- lm(data$c ~ data$a + data$b) #alternative way to run the same regression
  reg_c_no_intercept <- lm(c ~ 0 + a + b, data) # Regress c on a + b with no intercepts 
  print(reg_c) #print results
  print(reg_c_no_intercept)
  print(summary(reg_c_no_intercept)) # print summary of regression
  
  data['b_2'] <- data['b']^2
  print(lm(c ~ a + b + b_2, data))
  #TODO: Write a regression of c = beta_1*a + beta_2*a^2 + beta_3*b
  
  # sum the numbers from 1 to 6
  N <- 6
  s <- 0.0
  f <- function(x) { return(x^2)}
  for (i in 1:N) {
    s = s + f(i) + i
  }
  cat('Sum from 1 to', N, 'is', s, '\n')
  # TODO: think about what's wrong with this? how would you rewrite it?
  # TODO: Suppose I wanted the sum for all numbers from 1 to 100 (without using math tricks). How would you write this?
  # TODO: Suppose I wanted the sum of f(1) to f(100) for some function f. How would you write this?
  # TODO: Suppose I wanted the sum of (f(1) + 1 + f(2) + 2 +... + f(100) + 100).
  # TODO: Suppose I changed 100 to some N, write your code so that this only requires changing the value of a variable.
  
  # TODO: write a function that prints YES if some value passed to the function is even and NO if that value is odd
  
  #TODO: understand (roughly) why the output isn't 0 (relevant reading: https://blog.codinghorror.com/why-do-computers-suck-at-math/, http://fabiensanglard.net/floating_point_visually_explained/)
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
get_coeff <- function(reg_result, coef_name) {
  return(reg_result$coefficients[coef_name, 1])
}

## TODO: Write this function that, given the return object from summary(lm(...))
## checks if some value passed into the function is in the 95% confidence interval for the regression
## e.g. for y = beta_0 + beta_1*x, bkcd(reg_summ, "x", 2) = TRUE
## ALSO TODO: rename the function to something sensible.
estimate_in_confidence_interval <- function(reg_summary_result, coef_name, test_val) {
  CV <- 1.96
  estimate <- get_coeff(reg_summary_result, coef_name)
  se <- reg_summary_result$coefficients[coef_name, 2]
  return(test_val >= (estimate - CV*se) && test_val <= (estimate + CV*se))
}


## Suppose we are given that y = 2x + u, where u ~ N(0, 1), x ~ Exp(1) 
## and want to generate enough simulations of y, x, and u that when we run lm(y ~ x) we get a coefficient of 2
## Below I've implemented a naive version of this, but our goal is to rewrite it
##

naive_monte_carlo <- function (NUM_SAMPLES) {
  x_vals <- c()
  u_vals <- c()
  y_vals <- c()
  for (i in 1:NUM_SAMPLES) {
    x_vals <- c(x_vals, rexp(1))
    u_vals <- c(u_vals, rnorm(1))
    y_vals <- c(y_vals, 2 * x_vals[i] + u_vals[i])
  }
  reg <- lm(y_vals ~ x_vals)
  
  res <- estimate_in_confidence_interval(summary(reg), 'x_vals', 2)
  print(reg)
  if (res) {
    print('YAY')
  } else {
    print('NAY')
  }
}


# TODO: Keep the for loop in the naive function, speed up the function above by making the portion inside the for-loop more efficient
naive_monte_carlo_2 <- function(NUM_SAMPLES) { # naming this is tough!
  y_vals <- c()
  x_vals <- rexp(NUM_SAMPLES)
  u_vals <- rnorm(NUM_SAMPLES)
  for (i in 1:NUM_SAMPLES) {
    y_vals <- c(y_vals, 2 * x_vals[i] + u_vals[i])
  }
  reg <- lm(y_vals ~ x_vals)
  
  res <- estimate_in_confidence_interval(summary(reg), 'x_vals', 2)
  print(reg)
  if (res) {
    print('YAY')
  } else {
    print('NAY')
  }
}

# TODO: Now, refactor the less_naive function to be able to use the apply function
# Read this - docs for apply: https://www.rdocumentation.org/packages/base/versions/3.4.3/topics/apply
apply_monte_carlo <- function(NUM_SAMPLES) {
  x_vals <- rexp(NUM_SAMPLES)
  u_vals <- rnorm(NUM_SAMPLES)
  vals <- cbind(x_vals, u_vals)
  
  ## Write a function to calculate the y value given an x and u value
  calculate_y <- function(x) {
    return (2*x[1] + x[2])
  }
  
  y_vals <- apply(vals, 1, calculate_y)
  reg <- lm(y_vals ~ x_vals)
  
  res <- estimate_in_confidence_interval(summary(reg), 'x_vals', 2)
  print(reg)
  if (res) {
    print('YAY')
  } else {
    print('NAY')
  }
}


# TODO: Read about the vectorization (http://www.noamross.net/blog/2014/4/16/vectorization-in-r--why.html). Now, rewrite
# the same function as above but in a vectorized manner.

vectorized_monte_carlo <- function(NUM_SAMPLES) {
  x_vals <- rexp(NUM_SAMPLES)
  u_vals <- rnorm(NUM_SAMPLES)
  y_vals <- 2 * x_vals + u_vals
  reg <- lm(y_vals ~ x_vals)
  
  res <- estimate_in_confidence_interval(summary(reg), 'x_vals', 2)
  print(reg)
  if (res) {
    print('YAY')
  } else {
    print('NAY')
  }
  
}
# TODO: Convince yourself that parallelization in this context is a good idea.
# What about this context allows us to parallelize? Think about the contexts where parallelization doesn't make sense and read up briefly on the idea
# behind Amdahl's Law (https://en.wikipedia.org/wiki/Amdahl%27s_law)

# TODO: now let's implement a (simple) parallelized version of our even_less_naive code using the parallel package (https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf)

parallelized_monte_carlo <- function(NUM_SAMPLES) {
  x_vals <- rexp(NUM_SAMPLES)
  u_vals <- rnorm(NUM_SAMPLES)
  vals <- cbind(x_vals, u_vals)
  
  ## Write a function to calculate the y value given an x and u value
  calculate_y <- function(x) {
    return (2*x[1] + x[2])
  }
  
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  
  y_vals <- parRapply(cl, vals, calculate_y)
  
  reg <- lm(y_vals ~ x_vals)
  print(summary(reg))
  
  res <- estimate_in_confidence_interval(summary(reg), 'x_vals', 2)
  print(reg)
  if (res) {
    print('YAY')
  } else {
    print('NAY')
  }
  stopCluster(cl)
}


# TODO: after you've written all the code above, let's see what the tradeoffs for parallelization are: 

# Compute the time difference between the not_so_naive and even_less_naive functions for ONE run of the following values of NUM_SAMPLES: (100, 10000, 1000000, 5000000)
# Do you ever see that the parallelized version is slower than the non-parallelized version?

# TODO: Final question! Why is the method above a bad method of benchmarking (relevant question to ask yourself: are runtimes across runs deterministic?)? If you wanted to benchmark
# properly what are some things you could change?

NUM_SAMPLES <- 1000000
start_time <- Sys.time()
parallelized_monte_carlo(NUM_SAMPLES)
end_time <- Sys.time()
cat("Simulation took", difftime(end_time, start_time, units = "secs"), "seconds")


