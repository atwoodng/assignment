""" similar to pervious program, this program is to calculates the minimum fixed monthly payment needed in order pay off a credit card balance within 12 months using bisection search"""
## for example: 
##balance = 320000
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
high=(balance*(1+annualInterestRate/12)**12)/12
low=balance/12
pay=(high+low)/2
bigger=True
smaller=True

while bigger or smaller:
    pay=(float(high)+low)/2
    if balance_year(balance,pay,annualInterestRate)>0:
        low=pay
    if balance_year(balance,pay,annualInterestRate)<0:
        high=pay
    bigger=bool(balance_year(balance,pay,annualInterestRate)>0)
    smaller=bool(balance_year(balance,pay,annualInterestRate)<-1)
    
        
    
print "Lowest Payment: ",round(pay,2)