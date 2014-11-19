"""program that calculates the minimum fixed monthly payment needed in order pay off a credit card balance within 12 months
Here is the math equation being used
Monthly interest rate = (Annual interest rate) / 12.0
Monthly unpaid balance = (Previous balance) - (Minimum monthly payment)
Updated balance each month = (Monthly unpaid balance) + (Monthly interest rate x Monthly unpaid balance) 
"""
## please define the variable "balance" and "annualInterestRate" yourself
##balance = 3329
##annualInterestRate = 0.2

def balance_month(balance,pay,annualInterestRate):
    outcome=(balance-pay)*(1+annualInterestRate/12)
    return outcome
    
def balance_year(balance,pay,annualInterestRate):
    balance=balance
    pay=pay
    annualInterestRate=annualInterestRate
    for i in range(1,13):
        balance=balance_month(balance,pay,annualInterestRate)
    return balance


counter=0
high=balance
low=0
pay=(float(high)+low)/2

while abs(balance_year(balance,pay,annualInterestRate))>1:
    counter+=1
    pay=(float(high)+low)/2
    if balance_year(balance,pay,annualInterestRate)>0:
        low=pay
    if balance_year(balance,pay,annualInterestRate)<0:
        high=pay
    if counter>300:
        break
        
if (pay-round(pay,-1))>0:
    pay = round(pay,-1)+10
    print "Lowest Payment: ",int(pay)
else:
    print round(pay,-1)