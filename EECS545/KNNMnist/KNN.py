import numpy as np
import scipy.io as sio
from collections import Counter

mnist_data = sio.loadmat('mnist_data.mat')
train_data = mnist_data['train']
test_data = mnist_data['test']

train_images = np.asarray(train_data[:,1:785])
train_labels = np.asarray(train_data[:,0])
test_images = np.asarray(test_data[:,1:785])
test_labels = np.asarray(test_data[:,0])

errorcount = 0
klist = [1, 5, 9, 13]
for k in klist:
 for i in range(100):
  randnum = np.random.choice(test_labels.size)
  test_image = test_images[randnum]
  test_label = test_labels[randnum]
  distances = [(np.linalg.norm((test_image - image), 1), label) for (image, label) in zip(train_images, train_labels)]
  distsort = sorted(distances, key = lambda tup: tup[0])
  k_labels = [label for (_, label) in distsort[0:k]]
  win_label, freq = Counter(k_labels).most_common()[0]
  if (int(win_label) != int(test_label)):
   errorcount += 1
 print('for k =', end = ' ')
 print(k, end = ' ')
 print('the error is ', end = ' ')
 print(errorcount/100)
