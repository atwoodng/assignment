# -*- coding: cp1252 -*-
# 6.00x Problem 

import random
import string

VOWELS = 'aeiou'
CONSONANTS = 'bcdfghjklmnpqrstvwxyz'
HAND_SIZE = 7

SCRABBLE_LETTER_VALUES = {
    'a': 1, 'b': 3, 'c': 3, 'd': 2, 'e': 1, 'f': 4, 'g': 2, 'h': 4, 'i': 1, 'j': 8, 'k': 5, 'l': 1, 'm': 3, 'n': 1, 'o': 1, 'p': 3, 'q': 10, 'r': 1, 's': 1, 't': 1, 'u': 1, 'v': 4, 'w': 4, 'x': 8, 'y': 4, 'z': 10
}



WORDLIST_FILENAME = "C:\xxx\words.txt"

def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # wordList: list of strings
    wordList = []
    for line in inFile:
        wordList.append(line.strip().lower())
    print "  ", len(wordList), "words loaded."
    return wordList

def getFrequencyDict(sequence):
    """
    Returns a dictionary where the keys are elements of the sequence
    and the values are integer counts, for the number of times that
    an element is repeated in the sequence.

    sequence: string or list
    return: dictionary
    """
    # freqs: dictionary (element_type -> int)
    freq = {}
    for x in sequence:
        freq[x] = freq.get(x,0) + 1
    return freq
	




#  Scoring a word

def getWordScore(word, n):
    """
    Returns the score for a word. Assumes the word is a valid word.

    The score for a word is the sum of the points for letters in the
    word, multiplied by the length of the word, PLUS 50 points if all n
    letters are used on the first turn.

    Letters are scored as in Scrabble; A is worth 1, B is worth 3, C is
    worth 3, D is worth 2, E is worth 1, and so on (see SCRABBLE_LETTER_VALUES)

    word: string (lowercase letters)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    returns: int >= 0
    """
    score=0
    for k in word:
        score+=SCRABBLE_LETTER_VALUES[k]
    if len(word)<n:
        score=score*len(word)
    elif len(word)>=n:
        score=score*len(word)+50
    return score
    

        




def displayHand(hand):
    """
    Displays the letters currently in the hand.

    For example:
    >>> displayHand({'a':1, 'x':2, 'l':3, 'e':1})
    Should print out something like:
       a x x l l l e
    The order of the letters is unimportant.

    hand: dictionary (string -> int)
    """
    for letter in hand.keys():
        for j in range(hand[letter]):
             print letter,              
    print                              


def dealHand(n):
    """
    Returns a random hand containing n lowercase letters.
    At least n/3 the letters in the hand should be VOWELS.

    Hands are represented as dictionaries. The keys are
    letters and the values are the number of times the
    particular letter is repeated in that hand.

    n: int >= 0
    returns: dictionary (string -> int)
    """
    hand={}
    numVowels = n / 3
    
    for i in range(numVowels):
        x = VOWELS[random.randrange(0,len(VOWELS))]
        hand[x] = hand.get(x, 0) + 1
        
    for i in range(numVowels, n):    
        x = CONSONANTS[random.randrange(0,len(CONSONANTS))]
        hand[x] = hand.get(x, 0) + 1
        
    return hand


# Update a hand by removing letters

def updateHand(hand, word):
    """
    Assumes that 'hand' has all the letters in word.
    In other words, this assumes that however many times
    a letter appears in 'word', 'hand' has at least as
    many of that letter in it. 

    Updates the hand: uses up the letters in the given word
    and returns the new hand, without those letters in it.

    Has no side effects: does not modify hand.

    word: string
    hand: dictionary (string -> int)    
    returns: dictionary (string -> int)
    """
    newhand=hand.copy()
    for k in word:
        newhand[k]=newhand[k]-1
    return newhand



#
# Problem #3: Test word validity
#
def isValidWord(word, hand, wordList):
    """
    Returns True if word is in the wordList and is entirely
    composed of letters in the hand. Otherwise, returns False.

    Does not mutate hand or wordList.
   
    word: string
    hand: dictionary (string -> int)
    wordList: list of lowercase strings
    """
    
    
    def match(word,hand):
        hello=True
        worddict=getFrequencyDict(word)
        for i in word:
            if worddict[i]<=hand.get(i,0):
                 hello=True
            else:
                hello=False
                return hello
                break
        return True
                    
    
    if word in wordList and match(word,hand):
        return True
    else:
        return False
                
        
        
        
        



# Problem #4: Playing a hand

def calculateHandlen(hand):
    """ 
    Returns the length (number of letters) in the current hand.
    
    hand: dictionary (string-> int)
    returns: integer
    """
    total=0
    for i in hand:
        total+=hand[i]
    return total



def playHand(hand, wordList, n):
 
    def superscore(hand, wordList, n, score):
        if calculateHandlen(hand)>0:
            print displayHand(hand)    
            word=raw_input("Enter word, or a \".\" to indicate that you are finished: ")        
            if word==".":
                print "Goodbye! Total score: "+str(score)+" points."
            else:
                if isValidWord(word, hand, wordList):
                    score+=getWordScore(word, n)
                    print str(word)+"earned "+str(getWordScore(word, n))+" points. Total: "+str(score)+" points."
                    return superscore(updateHand(hand, word), wordList, n, score)
                else:
                    print "Invalid word, please try again."
                    return superscore(hand, wordList, n, score)
        else:
            print "Run out of letters. Total score: "+str(score)+" points."
            
    
    score=0
    superhand=hand.copy()
    superscore(superhand, wordList, n, score)
            
        


def playGame(wordList):
    
    def selection(wordList,HAND_SIZE,hand):
        
        userinput=raw_input("Enter n to deal a new hand, r to replay the last hand, or e to end game: ")
        if userinput=="n":
            hand=dealHand(HAND_SIZE)
            playHand(hand, wordList, HAND_SIZE)
            return selection(wordList,HAND_SIZE,hand)
        elif userinput=="r":
            if len(hand)>0:
                playHand(hand, wordList, HAND_SIZE)
                return selection(wordList,HAND_SIZE,hand)
            else:
                print "You have not played a hand yet. Please play a new hand first!"
                return selection(wordList,HAND_SIZE,hand)
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
