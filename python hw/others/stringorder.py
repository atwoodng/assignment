## in this program, the longest substring of "s" in alphabetical order will be printed out 

def cont(s):
    n=0
    k=s[0]
    if len(s)>=2:
        while ((ord(s[n])-ord(s[n+1]))<=0):
            k=k+s[n+1]
            n=n+1
            if n==len(s)-1:
                break
        return k
    else:
        return s