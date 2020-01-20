import numpy as np
import heapq

numspam = 0
numgood = 0
tokens_size = 1448


#I construct a Naive Bayes classifier "spam filter" to detect spam emails
probwordgivspam, probwordgivgood, spamsum, goodsum, logprobs = [0]*tokens_size, [0]*tokens_size, [0]*tokens_size, [0]*tokens_size, [0]*tokens_size

with open('SPARSE.TRAIN.1400') as f:
#get labels, calculate frequency sums
 for i, line in enumerate(f):
  label = line.split('  ')[0]
  text = line.split('  ')[1]
  for cell in text.strip().split(' '):
   word, freq = cell.split(':')
   if int(label) == 1:
    spamsum[int(word)-1] += int(freq)
    numspam += 1
   if int(label) == -1:
    goodsum[int(word)-1] += int(freq)
    numgood += 1

 #get probability of good & spam
probspam = numspam / (numspam + numgood)
probgood = numgood / (numspam + numgood)

 #use frequency sums to get conditionals
for i in range(0, tokens_size):
 probwordgivspam[i] = (1 + spamsum[i])/(len(spamsum)+sum(spamsum))
 probwordgivgood[i] = (1 + goodsum[i])/(len(goodsum)+sum(goodsum))
 logprobs[i] = np.log(probwordgivspam[i]/probwordgivgood[i])

with open('SPARSE.TEST') as g:
 numdocs = 0
 misclass = 0
#get labels
 for i, line in enumerate(g):
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
  elif (logpost < 0):
   guesslabel = -1
  if (int(testlabel) != guesslabel):
   misclass += 1

error = (misclass/numdocs)*100
print("error is")
print(error)
print("spammiest words are at")

spammiest = heapq.nlargest(5, range(len(logprobs)), logprobs.__getitem__)
print(spammiest + np.ones(len(spammiest)))
