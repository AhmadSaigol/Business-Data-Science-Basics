D <- 1000
K <- 5
h <- 0.25

Q <- sqrt(2*D*K/h)

# functions

my_fnt <- function(faces=1:6){
  die <- 1:6
  #dice <- sample(die, size = 2, replace = TRUE)
  dice <- sample(faces, size = 2, replace = TRUE)
  sum(dice)
  #op
}

my_fnt()

#Exercise
my_fnt2 <- function(faces=1:6, num_dice = 2){
  die <- 1:6
  #dice <- sample(die, size = 2, replace = TRUE)
  dice <- sample(faces, size = num_dice, replace = TRUE)
  sum(dice)
  #op
}

my_fnt2()


#Exercise

calc_EOQ <- function(D=1000){
  sqrt(2*D*5/0.25)
}

calc_EOQ(D=4000)

#Exercise
roll3 <- function(faces=1:6, num_dices=1){
  prob_vector <- c(0.1,0.1,0.1,0.1,0.1,0.5)
  dice <- sample(x=faces, size=num_dices, prob = prob_vector, replace = TRUE)
  sum(dice)
}
results <- replicate(n=100, expr = roll3(), simplify = TRUE)
hist(results)
