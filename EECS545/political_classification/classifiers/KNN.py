import numpy as np
import scipy.io as sio
from collections import Counter

def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

file_types = ['wf','tfidf','bool']
train_years = ['1960','2008','2016']
test_years = ['1960','2008','2016']

for file_type in file_types:
 tokens_size = file_len('../data/processed_'+file_type+'/ALLYEARS/TOKEN_LIST')
 for train_year in train_years:
  for test_year in test_years:
   print('file type is ' + file_type)
   print("train year is " + train_year + "; test year is " +test_year)
   train_images = np.zeros((file_len('../data/processed_'+file_type+'/ALLYEARS/train_data'+train_year+'.dat'), tokens_size))
   test_images = np.zeros((file_len('../data/processed_'+file_type+'/ALLYEARS/test_data'+test_year+'.dat'), tokens_size))
   train_labels = np.zeros(file_len('../data/processed_'+file_type+'/ALLYEARS/train_data'+train_year+'.dat'))
   test_labels = np.zeros(file_len('../data/processed_'+file_type+'/ALLYEARS/test_data'+test_year+'.dat'))

   with open('../data/processed_'+file_type+'/ALLYEARS/train_data'+train_year+'.dat') as f:
   #get labels, calculate frequency sums
    for i, line in enumerate(f):
     label = line.split('  ')[0]
     train_labels[i] = int(label)
     text = line.split('  ')[1]
     for cell in text.strip().split(' '):
      word, freq = cell.split(':')
      train_images[i,int(word)-1] = float(freq)

   with open('../data/processed_'+file_type+'/ALLYEARS/test_data'+test_year+'.dat') as g:
   #get labels, calculate frequency sums
    for i, line in enumerate(g):
     label = line.split('  ')[0]
     test_labels[i] = int(label)
     text = line.split('  ')[1]
     for cell in text.strip().split(' '):
      word, freq = cell.split(':')
      test_images[i,int(word)-1] = float(freq)

   klist = [1]
   for k in klist:
    errorcount = 0
    repguesses = 0
    demguesses = 0
    for i in range(test_labels.size):
     test_image = test_images[i]
     test_label = test_labels[i]
     distances = [(np.linalg.norm((test_image - image), 1), label) for (image, label) in zip(train_images, train_labels)]
     distsort = sorted(distances, key = lambda tup: tup[0])
     k_labels = [label for (_, label) in distsort[0:k]]
     win_label, freq = Counter(k_labels).most_common()[0]
     if (int(win_label) != int(test_label)):
      errorcount += 1
     if int(win_label) == 1:
      demguesses += 1
     if int(win_label) == -1:
      repguesses += 1
    print('for k =', end = ' ')
    print(k, end = ' ')
    print('the accuracy is ', end = ' ')
    print((1-(errorcount/test_labels.size)) * 100.0)
    print('Republican guesses is')
    print(repguesses)
    print('Democratic guesses is')
    print(demguesses)
