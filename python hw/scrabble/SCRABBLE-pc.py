from SCRABBLE import *
import time


## warning. it take long time to run if you choose the pc to play the game
# Computer chooses a word

def compChooseWord(hand, wordList, n):
    """
    Given a hand and a wordList, find the word that gives 
    the maximum value score, and return it.

    This word should be calculated by considering all the words
    in the wordList.

    If no words in the wordList can be made from the hand, return None.

    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)

    returns: string or None
    """
    bestword=""
    for i in wordList:
        if isValidWord(i,hand, wordList) and getWordScore(i,n)>getWordScore(bestword,n):
            bestword=i
    if bestword=="":
        return None
    else:
        return bestword
        


# Computer plays a hand

def compPlayHand(hand, wordList, n):
    """
    Allows the computer to play the given hand, following the same procedure
    as playHand, except instead of the user choosing a word, the computer 
    chooses it.

    1) The hand is displayed.
    2) The computer chooses a word.
    3) After every valid word: the word and the score for that word is 
    displayed, the remaining letters in the hand are displayed, and the 
    computer chooses another word.
    4)  The sum of the word scores is displayed when the hand finishes.
    5)  The hand finishes when the computer has exhausted its possible
    choices (i.e. compChooseWord returns None).
 
    hand: dictionary (string -> int)
    wordList: list (string)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    """
    
    def displayHand2(hand):
        savedprinthand=""
        for letter in hand.keys():
            for j in range(hand[letter]):
                savedprinthand=savedprinthand+letter+" "
        return savedprinthand

    newhand=hand.copy()
    score=0
    print "Current Hand: "+ str(displayHand2(newhand))
    superword=compChooseWord(newhand, wordList, n)
    while superword!=None:                      
        score+=getWordScore(superword, n)
        newhand=updateHand(newhand, superword)
        print "\""+str(superword)+"\" earned "+str(getWordScore(superword, n))+" points. Total: "+str(score)+" points"
        print 
        if displayHand2(newhand)!="":
            print "Current Hand: "+ str(displayHand2(newhand))
        superword=compChooseWord(newhand, wordList, n)
    else:
        print "Total score: "+str(score)+" points" 
             

# Playing a game

def playGame(wordList):
    """
    Allow the user to play an arbitrary number of hands.
 
    1) Asks the user to input 'n' or 'r' or 'e'.
        * If the user inputs 'e', immediately exit the game.
        * If the user inputs anything that's not 'n', 'r', or 'e', keep asking them again.

    2) Asks the user to input a 'u' or a 'c'.
        * If the user inputs anything that's not 'c' or 'u', keep asking them again.

    3) Switch functionality based on the above choices:
        * If the user inputted 'n', play a new (random) hand.
        * Else, if the user inputted 'r', play the last hand again.
      
        * If the user inputted 'u', let the user play the game
          with the selected hand, using playHand.
        * If the user inputted 'c', let the computer play the 
          game with the selected hand, using compPlayHand.

    4) After the computer or user has played the hand, repeat from step 1

    wordList: list (string)
    """
    def new(wordList,HAND_SIZE,hand):
        userplay=raw_input("Enter u to have yourself play, c to have the computer play: ")
        if userplay=="u":
            hand=dealHand(HAND_SIZE)
            playHand(hand, wordList, HAND_SIZE)
            return selection(wordList,HAND_SIZE,hand)
        elif userplay=="c":
            hand=dealHand(HAND_SIZE)
            compPlayHand(hand, wordList, HAND_SIZE)
            return selection(wordList,HAND_SIZE,hand)
        else:
            print "Invalid command."
            return new(wordList,HAND_SIZE,hand)
            
    def old(wordList, HAND_SIZE,hand):
        if len(hand)>0:
            userplay=raw_input("Enter u to have yourself play, c to have the computer play: ")
            if userplay=="u":
                playHand(hand, wordList, HAND_SIZE)
                return selection(wordList,HAND_SIZE,hand)
            elif userplay=="c":
                compPlayHand(hand, wordList, HAND_SIZE)
                return selection(wordList,HAND_SIZE,hand)
            else:
                print "Invalid command."
                return old(wordList,HAND_SIZE,hand)
                    
        else:
            print "You have not played a hand yet. Please play a new hand first!"
            return selection(wordList,HAND_SIZE,hand)
    
    
    def selection(wordList,HAND_SIZE,hand):
        userinput=raw_input("Enter n to deal a new hand, r to replay the last hand, or e to end game: ")
        if userinput=="n":
            new(wordList,HAND_SIZE,hand)    
        elif userinput=="r":
            old(wordList,HAND_SIZE,hand)
        elif userinput=="e":
            return None
        else:
            print "Invalid command."
            return selection(wordList,HAND_SIZE,hand)
                   
    hand={}
    selection(wordList,HAND_SIZE,hand)

        

# Build data structures used for entire session and play game

if __name__ == '__main__':
    wordList = loadWords()
    playGame(wordList)


