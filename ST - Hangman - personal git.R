# Olivia Paunkoff
# Special Topics in Biomedical Comm. 
# Assignment 3 - Hangman 

library(readr)
library(stringr)

# Read hangman dictionary file into r

hangman_dict <- read.table(file = "/Users/olivia/Desktop/hangman_dict1.txt", header = F)
hangman_words <- hangman_dict$V1

# Choose a random element from the dictionary
rand_word <- sample(1:length(hangman_words), 1)

# Converts the selected element from a row number to the word of the row
rand_word2 <- hangman_words[rand_word]

# Convert the rand_word to a vector of its elements 
rand_word3 <- strsplit(hangman_words[rand_word], split = '')
rand_word3 <- unlist(rand_word3)

# Inform the user about instructions and the number of wrong guesses/tries allowed

cat("\nWelcome to Hangman! How to play: when prompted, please guess one letter at a time.\n")
cat("\n If the letter guess is not in the word, you lose a life.You may also guess the full word at anytime for a win!\n")
cat("\n Quit the game anytime by typing 'quit'. You have 10 guesses. Good luck!\n")

# Tell the user how many characters in the word
num_char <- nchar(hangman_words[rand_word])
print(paste0("Hint:your word is ", num_char, " letters long."))

# Create a function to track progress of letter guessing when an input letter is guessed correctly
# Will add it to the correct spot in the word 
progress_word <- c()
for (input_lettr in rand_word3) {
  progress_word <- append(progress_word, '_')
}

# Function to inform user of their progress on guessing the mystery word
user_progress <- function(progress) {
  cat("\n-> Your progress: ")
  cat(paste(progress, collapse = ' '))
  cat("\nGuess again!\n")
}

# Create a function to store number of lives user has left 
lives <- 10

# Created an empty vector to store guessed letters
guessed_letters <- c()

# Create a loop that checks if the the user still has lives left - when lives are greater than zero 
# this loop will function
while (lives > 0) {

# Ask user for their guess
  input_lettr <- readline("Please input a letter guess: ") 
  
# Create statement for exiting the game upon user request
  if (input_lettr == "quit") {
    (print("Exiting game now, please come again!"))
    break
  }
# Create statement for when player guesses correct answer by typing in full word, ends game
  else if (input_lettr == rand_word2) {
    cat("\n WINNER ALERT!!!You guessed correctly!\n")
    cat(paste("The word was:", rand_word2, "\n"))
    cat("\n You have won the game! Thank you for playing and come again.\n\n")
    break
  }
# Create if statement for dealing with invalid inputs (characters other than upper or lower case letters)
  else if (grepl("^[A-Za-z]+$", input_lettr, perl = T) == F) {
    cat("\n-> Please input letters ONLY!\n")
# Blocks users from putting in more than one letter at a time that is not "quit" or the mystery word
  } else if (nchar(input_lettr) > 1) {
    cat("\nHELLO! Please only guess ONE letter at a time!\n\n")
  } else {
# Statement that checks if the user guessed the letter correctly 
    if (grepl(input_lettr, rand_word2, ignore.case = T)) {
      print("Correct guess! Good job.")
      # Adds the letter to the list of letters guesses
      guessed_letters <- append(guessed_letters, input_lettr)
      cat("Letters guessed: ")
      cat(paste(guessed_letters, collapse = ', '))
      # Displays user progress and places it in the correct position in the mystery word
      user_progress(progress_word) 
      numb_positions <- which(rand_word3 == input_lettr)
      for (position in numb_positions) {
        progress_word[position] <- input_lettr
      }
    } else { # Statement for incorrect guesses, lose one life each time
      print(paste("Oh no!" , input_lettr, " is not in the secret word!"))
      lives <- lives - 1 
      print(paste("You have ", lives, " lives left!")) 
      # Adds the letter to list of letters guessed
      guessed_letters <- append(guessed_letters, input_lettr)
      cat("-> Letters guessed: ")
      cat(paste(guessed_letters, collapse = ', '))
    }   
# Statement for if a user guesses the word correctly with individual letter guesses instead of guessing the whole word in one go
    if (paste(progress_word, collapse = '') == rand_word2) {
      cat("\n WOOHOO!! You guessed correctly!\n")
      cat(paste("The word was:", rand_word2, "\n"))
      cat("\n You won! Thank you for playing and come again. \n\n")
      break
    }
# Checks if lives are left, no lives left game will quit
    if (lives == 0) {
      cat("\nYou are out of lives! :(\n")
      print(paste("The word was", rand_word2, "!"))
      cat("\nBetter luck next time! Exiting game.\n")
    }
  }
}


