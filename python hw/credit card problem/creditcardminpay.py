""" this program is to calculate the credit card balance after one year if a person only pays the minimum monthly payment required by the credit card company each month.
Here is the math equation being used
Monthly interest rate= (Annual interest rate) / 12.0
Minimum monthly payment = (Minimum monthly payment rate) x (Previous balance)
Monthly unpaid balance = (Previous balance) - (Minimum monthly payment)
Updated balance each month = (Monthly unpaid balance) + (Monthly interest rate x Monthly unpaid balance)
"""

#please define the following variable, for example 
#balance = 4213
#annualInterestRate = 0.2
#monthlyPaymentRate = 0.04


def balance_month(balance,annualInterestRate,monthlyPaymentRate):
    outcome=(balance-balance*monthlyPaymentRate)*(1+annualInterestRate/12)
    print "Minimum monthly payment: "+"%.2f"%(balance*monthlyPaymentRate)
    print "Remaining balance: " + "%.2f"%outcome
    return outcome

total_paid=float()
for i in range(1,13):
    print "Month: "+str(i)
    total_paid+=balance*monthlyPaymentRate
    balance=balance_month(balance,annualInterestRate,monthlyPaymentRate)
print "Total paid: "+"%.2f"%total_paid
print "Remaining balance: "+"%.2f"%balance