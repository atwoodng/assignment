# 6.00 Problem Set 3
# 
# Hangman game

import random
import string

WORDLIST_FILENAME = "c:\xxx\words.txt"

def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = string.split(line)
    print "  ", len(wordlist), "words loaded."
    return wordlist

def chooseWord(wordlist):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

def isWordGuessed(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: boolean, True if all the letters of secretWord are in lettersGuessed;
      False otherwise
    '''

    sepword=[]
    for i in secretWord:
        sepword.append(i)
    def test(sepword,lettersGuessed,no):
        if no>=len(secretWord):
            return True
        else:
            if sepword[no] in lettersGuessed:
                return True and test(sepword,lettersGuessed,no+1)
            else:
                return False
    return test(sepword,lettersGuessed,0)

def getGuessedWord(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters and underscores that represents
      what letters in secretWord have been guessed so far.
    '''

    sepword=[]
    for i in secretWord:
        sepword.append(i)
    x=""
    length=0
    def test(sepword,lettersGuessed,length,x):
        if length==len(sepword):
            return x
        else:
            if sepword[length] in lettersGuessed:
                x+=sepword[length]
                return test(sepword,lettersGuessed,length+1,x)
            else:
                x+="_ "
                return test(sepword,lettersGuessed,length+1,x)
    return test(sepword,lettersGuessed,0,x)
    
    
def getAvailableLetters(lettersGuessed):
    '''
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters that represents what letters have not
      yet been guessed.
    '''

    import string
    sepword=[]
    for i in string.ascii_lowercase:
        sepword.append(i)
    x=""
    length=0
    def test(sepword,lettersGuessed,length,x):
        if length==len(string.ascii_lowercase):
            return x
        else:
            if sepword[length] in lettersGuessed:
                return test(sepword,lettersGuessed,length+1,x)
            else:
                x+=sepword[length]
                return test(sepword,lettersGuessed,length+1,x)
    return test(sepword,lettersGuessed,0,x)
    

def hangman(secretWord):
    '''
    secretWord: string, the secret word to guess.

    Starts up an interactive game of Hangman.

    * At the start of the game, let the user know how many 
      letters the secretWord contains.

    * Ask the user to supply one guess (i.e. letter) per round.

    * The user should receive feedback immediately after each guess 
      about whether their guess appears in the computers word.

    * After each round, you should also display to the user the 
      partially guessed word so far, as well as letters that the 
      user has not yet guessed.

    Follows the other limitations detailed in the problem write-up.
    '''
    
    print "Welcome to the game Hangman!"
    print "I am thinking of a word that is "+str(len(secretWord))+" letters long."
    print "-----------"
    round=8
    lettersGuessed=[]
    sepword=[]
    for i in secretWord:
            sepword.append(i)
    def tryanderror(secretWord,lettersGuessed,round,sepword):  
        if round>0:
            if isWordGuessed(secretWord, lettersGuessed):
                print "Congratulations, you won!"
            else:
                print "You have "+str(round)+ " guesses left."
                print "Available Letters: " + getAvailableLetters(lettersGuessed)
                entered=raw_input("Please guess a letter: ").lower()
                if entered in sepword and entered not in lettersGuessed:
                    lettersGuessed.append(entered)
                    print "Good guess: "+ getGuessedWord(secretWord, lettersGuessed)
                    print"-----------"
                    return tryanderror(secretWord, lettersGuessed,round,sepword)
                elif entered in lettersGuessed:
                    lettersGuessed.append(entered)
                    print "Oops! You've already guessed that letter:"+getGuessedWord(secretWord, lettersGuessed)
                    print"-----------"
                    return tryanderror(secretWord, lettersGuessed,round,sepword)
                else:
                    lettersGuessed.append(entered)
                    print "Oops! That letter is not in my word: "+ getGuessedWord(secretWord, lettersGuessed)
                    print"-----------"
                    return tryanderror(secretWord, lettersGuessed,round-1,sepword)
        else:
            if isWordGuessed(secretWord, lettersGuessed):
                print "Congratulations, you won!"
            else:
                print "Sorry, you ran out of guesses. The word was else. "
    return tryanderror(secretWord,lettersGuessed,round,sepword)
        

wordlist=loadWords()                        
hangman(chooseWord(wordlist))
            
            
        