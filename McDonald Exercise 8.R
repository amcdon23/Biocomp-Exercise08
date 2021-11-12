####Anna McDonald Biocomputing Exercise 8####

###Question 1: Sports Analysis 

#read in the data 
sports <- read.table("UWvMSU_1-22-13.txt", header = TRUE)

#create a new matrix with an empty column for each team to add up 
#the score as time goes on
scorematrix <- matrix(ncol=2, nrow = 50, dimnames=list(c(1:50), c("UW", "MSU")))

#create a loop to add together 
for(i in 1:nrow(sports)){
  if(sports[i,2]=='UW'){
    if (i>1){
      scorematrix[i,1]=sports[i,3]+scorematrix[(i-1),1]
      scorematrix[i,2]=scorematrix[(i-1),2]
    }else{
      scorematrix[i,1]=sports[i,3]+0
      scorematrix[i,2]=0
    }
  }else{
    if (i>1){
      scorematrix[i,2]=sports[i,3]+scorematrix[(i-1),2]
      scorematrix[i,1]=scorematrix[(i-1),1]
    }else{
      scorematrix[i,2]=sports[i,3]+0
      scorematrix[i,1]=0
  }
  }
}

#create the plot with the line for each team and assign to a variable
sportsPlot <- plot(x=sports[,1], y=scorematrix[,1], type='l') 
sportsPlot <- lines(x=sports[,1], y=scorematrix[,2])
sportsPlot

###Question 2: Guess My Number Game

#create a function for the whole game
GuessingGame <- function(){
  
#create x as some random number 1-100
 x <- sample(1:100,1)
 
#print original starting phrase
print("I'm thinking of a number 1-100...")

#run loop for max 10 times in which you guess until correct (stops if correct)
  for(i in 1:10){
  guess <- readline(prompt='Guess:')
  if(guess==x){
  print("Correct!") + break
  }else if(guess>x){
    print("Lower")
  }else{
    print("Higher")
  
  }
  }
}

#run "GuessingGame" to play
GuessingGame 
