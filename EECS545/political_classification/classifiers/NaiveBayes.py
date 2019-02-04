import numpy as np
import heapq
from sklearn.model_selection import train_test_split

def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

file_types  = ['wf', 'bool', 'tfidf']
train_years = ['1960','2008','2016']
test_years = ['1960','2008','2016']
printlines = False

for file_type in file_types:
 tokens_size = file_len('../data/processed_'+file_type+'/ALLYEARS/TOKEN_LIST')
 for train_year in train_years:
  for test_year in test_years:

   print('file type is '+file_type)
   numspam = 0
   numgood = 0
   guessgoodfiles = 0
   guessbadfiles = 0

   print("train year is " + train_year + "; test year is " +test_year)
   linenum = 0
   probwordgivspam, probwordgivgood, spamsum, goodsum, logprobs = [0]*tokens_size, [0]*tokens_size, [0]*tokens_size, [0]*tokens_size, [0]*tokens_size

   with open('../data/processed_'+file_type+'/ALLYEARS/train_data'+train_year+'.dat') as f:
   #get labels, calculate frequency sums
    for i, line in enumerate(f):
     label = line.split('  ')[0]
     text = line.split('  ')[1]
     for cell in text.strip().split(' '):
      word, freq = cell.split(':')
      if int(label) == 1:
       spamsum[int(word)-1] += float(freq)
       numspam += 1
      if int(label) == -1:
       goodsum[int(word)-1] += float(freq)
       numgood += 1

   #get probability of good & spam
   probspam = numspam / (numspam + numgood)
   probgood = numgood / (numspam + numgood)

 #use frequency sums to get conditionals
   for i in range(0, tokens_size):
    probwordgivspam[i] = (1 + spamsum[i])/(tokens_size+sum(spamsum))
    probwordgivgood[i] = (1 + goodsum[i])/(tokens_size+sum(goodsum))
    logprobs[i] = np.log(probwordgivspam[i]/probwordgivgood[i])

    numdocs = 0
    misclass = 0

   print("Democratic-est words are at")
   Democraticest = heapq.nlargest(5, range(len(logprobs)), logprobs.__getitem__)
   print(Democraticest + np.ones(len(Democraticest)))

   print("Republican-est words are at")
   Republicanest = heapq.nsmallest(5, range(len(logprobs)), logprobs.__getitem__)
   print(Republicanest + np.ones(len(Republicanest)))

   #get labels
   with open('../data/processed_'+file_type+'/ALLYEARS/test_data'+test_year+'.dat') as g:
    for i, line in enumerate(g):
     linenum += 1
     numdocs += 1
     guesslabel = 0
     logpost = np.log(probspam/probgood)
     testwords = []
     testlabel = line.split('  ')[0]
     testtext = line.split('  ')[1]
     for cell in testtext.strip().split(' '):
      testword, testfreq = cell.split(':')
      testwords.append(int(testword)-1)
     for test in testwords:
      logpost += np.log(probwordgivspam[test]/probwordgivgood[test])
     if (logpost > 0):
      guesslabel = 1
      guessgoodfiles += 1
     elif (logpost < 0):
      guesslabel = -1
      guessbadfiles += 1
     if (int(testlabel) != guesslabel):
      misclass += 1
      if printlines:
       print(linenum)

   accuracy = float((1-(misclass/numdocs))*100.0)

   print("accuracy is")
   print(accuracy)
   print("republican speeches number guess is")
   print(guessbadfiles)
   print("democrat speeches number guess is")
   print(guessgoodfiles)

