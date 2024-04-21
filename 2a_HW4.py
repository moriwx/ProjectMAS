import tempfile
with tempfile.TemporaryFile() as fp:
    fp.write(b'''id,1,2,3,4,5,6,7,y
1,1,1,1,1,1,1,1,2.05
2,1,1,1,2,2,2,2,2.24
3,1,2,2,1,1,2,2,2.44
4,1,2,2,2,2,1,1,1.10
5,2,1,2,1,2,1,2,1.50
6,2,1,2,2,1,2,1,1.35
7,2,2,1,1,2,2,1,1.26
8,2,2,1,2,1,1,2,2.00''')
    fp.seek(0)
    data = pd.read_csv(fp)

a=data.y.values
4*((a[0:4].mean()-a.mean())**2+(a[4:].mean()-a.mean())**2)

data.y.sum()

for i in range(1,8): print(data.groupby(str(i))['y'].sum())

for i in range(1,8):
    a = data.groupby(str(i))['y'].sum()
    print(abs(a.values[0]-a.values[1]))

from decimal import Decimal
x = Decimal('4.2')
y = Decimal('3.1')
# printing the sum of both the variables
print("x + y =", x + y)
# checking if the sum is both the variables is equal to 7.3 using by passing the sum to the Decimal Function
print((x + y) == Decimal('7.3'))
