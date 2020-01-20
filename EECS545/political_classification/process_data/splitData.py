
with open('SPARSE1960.dat') as f:
    content = f.readlines()

i = 0

train_data1960 = open("train_data1960.dat", "w")
test_data1960 = open("test_data1960.dat", "w")

i = 0
for thisline in content:
   i += 1
   if (i%3 == 0):
      test_data1960.write(thisline)
   else:
      train_data1960.write(thisline)


with open('SPARSE2008.dat') as g:
    content = g.readlines()

i = 0

train_data2008 = open("train_data2008.dat", "w")
test_data2008 = open("test_data2008.dat", "w")

i = 0
for thisline in content:
   i += 1
   if (i%3 == 0):
      test_data2008.write(thisline)
   else:
      train_data2008.write(thisline)


with open('SPARSE2016.dat') as h:
    content = h.readlines()

i = 0

train_data2016 = open("train_data2016.dat", "w")
test_data2016 = open("test_data2016.dat", "w")

i = 0
for thisline in content:
   i += 1
   if (i%3 == 0):
      test_data2016.write(thisline)
   else:
      train_data2016.write(thisline)
