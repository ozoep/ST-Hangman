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

cat("\nWelcome to Olivia's Hangman game!\n")
cat("\n How to play: when prompted, please guess one letter at a time.\n")
cat("\n If the letter guess is not in the word, you lose a life.\n")
cat("\n You may also guess the full word at anytime for a win!")
cat("\n Quit the game anytime by typing 'quit'. You have 10 guesses. Good luck!\n")

# Tell the user how many characters in the word
num_char <- nchar(hangman_words[rand_word])
print(paste0("Hint:your word is ", num_char, " letters long.")) # returning that its 9 letters long

# Create a vector to store progress of letter guessing 

#lettr_prog <- c()
#for (letter in rand_word3) {
# lettr_prog <- append(lettr_prog, "_")
#}

# Create a function to display number of lives left
lives <- 10

letters <- nchar(rand_word2)

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
    # Create a statement for if the user inputs 2 or more characters that arent the guess or the exit 
    # must be after the exit and the full word input 
  } else if (nchar(input_lettr) > 1) {
    cat("\n-> Please only guess ONE letter at a time!\n\n")
  } else {
    # Create a statement for when the player guesses a letter they have already guessed 
    # Statement for when the player guesses the letter correctly, if the user guesses incorrectly 
    if (grepl(input_lettr, rand_word2, ignore.case = T)) {
      # If the letter is guessed correctly, locate the position in the secret word where the letter is 
      #lettr_position <- which(rand_word3 == input_lettr)
      #lettrs_guessed <- paste(lettrs_guessed, input_lettr)
      lettrs_guessed <- append(lettrs_guessed, input_lettr)
      print(lettrs_guessed)
      print("Correct guess! Good job.")
      # Place the correctly guessed letter in the letter progress vector - gives user visual 
      # of where their correct guesses are 
      #for (pos in lettr_position) {
      #lettr_prog[pos] <- input_lettr
      # next
      letters < letters - 1
    } 
    else {
      print(paste("Oh no!" , input_lettr, " is not in the secret word!"))
      lives <- lives - 1
      print(paste("You have ", lives, " lives left!"))
      lettrs_guessed <- paste(lettrs_guessed, input_lettr)
    }   
    # Statement for if a user guesses the word correctly with individual letter guesses
    #... instead of guessing the whole word in one go
    
    if (letters == 0) {
      cat("\n You guessed correctly!\n")
      cat(paste("The word was:", rand_word2, "\n"))
      cat("\n You have won the game! Thank you for playing and come again. \n\n")
      break
    }
    # Code handling incorrect guesses. Subtract 1 life for each.
    # Checking if the user has run out of lives, if so, game will quit
    if (lives == 0) {
      print("You have run out of lives!")
      print(paste("The mystery word was", rand_word2, "!"))
      print("Better luck next time! ;)")
    }
  }
}


# Displaying progress to user after each unsuccessful guess.


