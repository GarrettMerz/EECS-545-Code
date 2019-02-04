import numpy as np
import heapq

# tokens_size = 10797 # 1960
# tokens_size = 11232 # 2008
# tokens_size = 6005 # 2012
# tokens_size = 6949 # 2016
tokens_size = 16243 # all
probwordgivR, probwordgivD, Rsum, Dsum, logprobs = [0]*tokens_size, [0]*tokens_size, [0]*tokens_size, [0]*tokens_size, [0]*tokens_size

with open('data/processed_tfidf/ALLYEARS/SPARSE1960.dat') as f:
   #get labels, calculate frequency sums
   for i, line in enumerate(f):
      label = line.split('  ')[0]
      text = line.split('  ')[1]
      # print i
      for cell in text.strip().split(' '):
         word, freq = cell.split(':')
         if int(label) == 1:
            Rsum[int(word)-1] += float(freq)
         if int(label) == -1:
            Dsum[int(word)-1] += float(freq)

with open('data/processed_tfidf/ALLYEARS/SPARSE2008.dat') as f:
   #get labels, calculate frequency sums
   for i, line in enumerate(f):
      label = line.split('  ')[0]
      text = line.split('  ')[1]
      # print i
      for cell in text.strip().split(' '):
         word, freq = cell.split(':')
         if int(label) == 1:
            Rsum[int(word)-1] += float(freq)
         if int(label) == -1:
            Dsum[int(word)-1] += float(freq)

with open('data/processed_tfidf/ALLYEARS/SPARSE2016.dat') as f:
   #get labels, calculate frequency sums
   for i, line in enumerate(f):
      label = line.split('  ')[0]
      text = line.split('  ')[1]
      # print i
      for cell in text.strip().split(' '):
         word, freq = cell.split(':')
         if int(label) == 1:
            Rsum[int(word)-1] += float(freq)
         if int(label) == -1:
            Dsum[int(word)-1] += float(freq)

 #use frequency sums to get conditionals
for i in range(0, tokens_size):
   probwordgivR[i] = (1 + Rsum[i])/(len(Rsum)+sum(Rsum))
   probwordgivD[i] = (1 + Dsum[i])/(len(Dsum)+sum(Dsum))
   logprobs[i] = np.log(probwordgivR[i]/probwordgivD[i])

republican = heapq.nlargest(5, range(len(logprobs)), logprobs.__getitem__)
print(republican + np.ones(len(republican)))

democrat = heapq.nsmallest(5, range(len(logprobs)), logprobs.__getitem__)
print(democrat + np.ones(len(democrat)))