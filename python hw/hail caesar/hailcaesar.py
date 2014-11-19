# 6.00x Problem Set 6
## part1: encrypting plaintext
## part2:decoding Caesar Cipher encrypted text 

import string
import random

WORDLIST_FILENAME = "C:\xxx\words.txt"


def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    inFile = open(WORDLIST_FILENAME, 'r')
    wordList = inFile.read().split()
    print "  ", len(wordList), "words loaded."
    return wordList

def isWord(wordList, word):
    """
    Determines if word is a valid word.

    wordList: list of words in the dictionary.
    word: a possible word.
    returns True if word is in wordList.

    Example:
    >>> isWord(wordList, 'bat') returns
    True
    >>> isWord(wordList, 'asdf') returns
    False
    """
    word = word.lower()
    word = word.strip(" !@#$%^&*()-_+={}[]|\\:;'<>?,./\"")
    return word in wordList

def randomWord(wordList):
    """
    Returns a random word.

    wordList: list of words  
    returns: a word from wordList at random
    """
    return random.choice(wordList)

def randomString(wordList, n):
    """
    Returns a string containing n random words from wordList

    wordList: list of words
    returns: a string of random words separated by spaces.
    """
    return " ".join([randomWord(wordList) for _ in range(n)])

def randomScrambled(wordList, n):
    """
    Generates a test string by generating an n-word random string
    and encrypting it with a sequence of random shifts.

    wordList: list of words
    n: number of random words to generate and scamble
    returns: a scrambled string of n random words

    NOTE:
    This function will ONLY work once you have completed your
    implementation of applyShifts!
    """
    s = randomString(wordList, n) + " "
    shifts = [(i, random.randint(0, 25)) for i in range(len(s)) if s[i-1] == ' ']
    return applyShifts(s, shifts)[:-1]

def getStoryString():
    """
    Returns a story in encrypted text.
    """
    return open("C:\xxx\story.txt", "r").read()



#  Encryption

def buildCoder(shift):
    """
    Returns a dict that can apply a Caesar cipher to a letter.
    The cipher is defined by the shift value. Ignores non-letter characters
    like punctuation, numbers and spaces.
    shift: 0 <= int < 26
    returns: dict
    """
    import string
    lowerstring=string.ascii_lowercase
    upperstring=string.ascii_uppercase
    caesar={}
    for i in range(0,len(lowerstring)):
        if ord(lowerstring[i])+shift>122:
            caesar[lowerstring[i]]=chr(ord(lowerstring[i])+shift-26)
        else:
            caesar[lowerstring[i]]=chr(ord(lowerstring[i])+shift)
    for i in range(0,len(upperstring)):
         if ord(upperstring[i])+shift>90:
            caesar[upperstring[i]]=chr(ord(upperstring[i])+shift-26)
         else:
            caesar[upperstring[i]]=chr(ord(upperstring[i])+shift)
    return caesar




def applyCoder(text, coder):
    """
    Applies the coder to the text. Returns the encoded text.

    text: string
    coder: dict with mappings of characters to shifted characters
    returns: text after mapping coder chars to original text
    """
    dictionary=coder
    superstring=""
    for i in range(0,len(text)):
        if text[i] in dictionary.keys():
            superstring+=dictionary[text[i]]
        else:
            superstring+=text[i]
    return superstring
            
    
    

def applyShift(text, shift):
    """
    Given a text, returns a new text Caesar shifted by the given shift
    offset. Lower case letters should remain lower case, upper case
    letters should remain upper case, and all other punctuation should
    stay as it is.

    text: string to apply the shift to
    shift: amount to shift the text (0 <= int < 26)
    returns: text after being shifted by specified amount.
    """

    return applyCoder(text,buildCoder(shift))

#---------------------------end of code for encryption--------------------------


#  Decryption

def findBestShift(wordList, text):
    """
    Finds a shift key that can decrypt the encoded text.

    text: string
    returns: 0 <= int < 26
    """
    def supersplit(texts):
        newtext=""
        for i in range(0,len(texts)):
            if text[i] in string.ascii_lowercase or text[i] in string.ascii_uppercase or text[i] in " ":
                newtext+=texts[i]
        return newtext
    newtext=supersplit(text).lower()
    Largestvalue=0
    bestfit=int()
    for i in range(0,26):
        superlist=applyShift(newtext,i).split(" ")
        valid=0
        for j in superlist:
            if j in wordList:
                valid+=1
        if valid>Largestvalue:
            Largestvalue=valid
            bestfit=i
    return bestfit
    
                
                
        
        
        
    
    

def decryptStory():
    """
    Using the methods you created in this problem set,
    decrypt the story given by the function getStoryString().
    Use the functions getStoryString and loadWords to get the
    raw data you need.

    returns: string - story in plain text
    """
    story=getStoryString()
    shift=findBestShift(loadWords(),story)
    return applyShift(story, shift)



# Build data structures used for entire session and run encryption


if __name__ == '__main__':
    # To test findBestShift:
    wordList = loadWords()
    # To test decryptStory, comment the above four lines and uncomment this line:
    print decryptStory()
