# part 1 ####

# suppose there are 30 students in a class. estimate the probability that at least one pair
# students has the same birthday.

# you can ignore the possibility of people being born on Feb 29 during a leap-year.

# first, i'll write a function to generate a new class and return whether there are people
# with the same birthday

new.class <- function(class.size){
  # since there are 365 days in a year, we can use the integers 1:365 to represent
  # birthdays
  birthdays <- sample(1:365, class.size, replace=T)
  
  # now we need to check if any two values in the birthdays array are the same.
  # there are many ways to do this. we could do it with a for loop:
  for(day in birthdays){
    number.of.matches <- sum(day == birthdays)
    if(number.of.matches > 1){
      return(TRUE)
    }
  }
  
  # if we go through the whole for loop and never return TRUE it means there are no matches
  # so we can return FALSE at this point in the code.
  return(FALSE)
}

# to estimate the probability for a class of size 30, we need to run the function many times
results <- replicate(10000, new.class(30))

sum(results) / length(results)

# the estimated probability is around 70%

# part 2 ####

# estimate the probability for class sizes from 5-60, and plot the resulting curve 
# (x axis is class size, y axis is probability of at least one shared birthday)

# let's start by making an array to hold the class size:

class.size <- 5:60

# now we need to run a function for each element of this array. there are lots of 
# ways to do this.

probabilities <- sapply(class.size, function(s){
  res <- replicate(10000, new.class(s))
  return(sum(res) / length(res))
})

# make the plot!

plot(class.size, probabilities, type="o")
