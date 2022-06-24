# Olivia Paunkoff
# Special Topics in Biomedical Comm. 
# Assignment 3 - Hangman 

library(readr)
library(stringr)

# Read hangman dictionary file into r

hangman_dict <- read.table(file = "/Users/olivia/Desktop/hangman_dict1.txt", header = F)
hangman_words <- hangman_dict$V1

# Create a prompt for them to enter that starts game ??
# Choose a random element from the dictionary
rand_word <- sample(1:length(hangman_words), 1)

# Converts the selected element from a row number to the word of the row
rand_word2 <- hangman_words[rand_word]

# Convert the rand_word to a vector of its elements 
rand_word3 <- strsplit(hangman_words[rand_word], split = '')
rand_word3 <- unlist(rand_word3)

# Give user hint about length of letters in word
# Combine these two so they automatically happen together .... select random word
# and then prints below 


# Inform the user about instructions and the number of wrong guesses/tries allowed

cat("\nWelcome to Hangman! How to play: when prompted, please guess one letter at a time.\n")
cat("\n If the letter guess is not in the word, you lose a life.You may also guess the full word at anytime for a win!\n")
cat("\n Quit the game anytime by typing 'quit'. You have 10 guesses. Good luck!\n")

# Tell the user how many characters in the word
num_char <- nchar(hangman_words[rand_word])
print(paste0("Hint:your word is ", num_char, " letters long.")) # returning that its 9 letters long

# Create a vector to store progress of letter guessing 

progress_word <- c()
for (letter in rand_word3) {
  progress_word <- append(progress_word, '_')
}

# Function to inform user of their progress on guessing secret word
user_progress <- function(progress) {
  cat("\n-> Your progress so far: ")
  cat(paste(progress, collapse = ' '))
  cat("\n-> Guess again!\n")
  cat("\n\n")
}

# Create a function to display number of lives left
lives <- 10

letters <- nchar(rand_word2)

#
guessed_letters <- c()

# Create a function that allows user to keep track of letters guessed
lettrs_guessed <- "letters:"

# Ask for user input, only allow one character to be entered 
while (lives > 0) {
  
  input_lettr <- readline("Please input a letter guess: ") 
  
  # Create statement for exiting the game upon user request
  if (input_lettr == "quit") {
    (print("Exiting game now, please come again!"))
    break
  }
  # Create statement for when player guesses correct answer  
  else if (input_lettr == rand_word2) {
    cat("\n You guessed correctly!\n")
    cat(paste("The word was:", rand_word2, "\n"))
    cat("\n You have won the game! Thank you for playing and come again. \n\n")
    break
  }
  # Create if statement for dealing with invalid inputs
  else if (grepl("^[A-Za-z]+$", input_lettr, perl = T) == F) {
    cat("\n-> Please input letters ONLY!\n")

  } else if (nchar(input_lettr) > 1) {
    cat("\n-> Please only guess ONE letter at a time!\n\n")
  } else {
    # Create a statement for when the player guesses a letter they have already guessed 
    # Statement for when the player guesses the letter correctly, if the user guesses incorrectly 
    if (grepl(input_lettr, rand_word2, ignore.case = T)) {
      print("Correct guess! Good job.")
      guessed_letters <- append(guessed_letters, input_lettr)
      cat("-> Letters guessed: ")
      cat(paste(guessed_letters, collapse = ', '))
      cat("\n")
      letters < letters - 1
    } else { # Code handling incorrect guesses. Subtract 1 life for each.
      print(paste("Oh no!" , input_lettr, " is not in the secret word!"))
      lives <- lives - 1
      print(paste("You have ", lives, " lives left!"))
      guessed_letters <- append(guessed_letters, input_lettr)
      cat("-> Letters guessed: ")
      cat(paste(guessed_letters, collapse = ', '))
      cat("\n")
    }   
    # Statement for if a user guesses the word correctly with individual letter guesses
    #... instead of guessing the whole word in one go
    
    if (letters == 0) {
      cat("\n You guessed correctly!\n")
      cat(paste("The word was:", rand_word2, "\n"))
      cat("\n You have won the game! Thank you for playing and come again. \n\n")
      break
    }
    # Checking if the user has run out of lives, if so, game will quit
    if (lives == 0) {
      print("You have run out of lives!")
      print(paste("The mystery word was", rand_word2, "!"))
      print("Better luck next time! ;)")
    }
  }
}


