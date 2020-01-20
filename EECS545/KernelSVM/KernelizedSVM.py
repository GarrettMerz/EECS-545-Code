from PIL import Image
import numpy as np
import pandas as pd
import random
from sklearn import svm
from sklearn import metrics
import csv
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

traindata= pd.read_csv(open("digits_training_data.csv"), header=None, sep = ' ').as_matrix()
trainlabels= pd.read_csv(open("digits_training_labels.csv"), header=None, sep = ' ').as_matrix()
testdata= pd.read_csv(open("digits_test_data.csv"), header=None, sep = ' ').as_matrix()
testlabels= pd.read_csv(open("digits_test_labels.csv"), header=None, sep = ' ').as_matrix()

trainlabels = trainlabels - 8 * np.ones((trainlabels.shape[0], 1))
testlabels = testlabels - 8 * np.ones((testlabels.shape[0], 1))

param_grid = dict(C=np.linspace(0.1, 2, 20))

print("Soft-Margin SVM:")
SoftClf = svm.SVC(C=3.0,kernel='rbf')
SoftClf.fit(traindata,trainlabels.ravel())
SoftTrainGuess=SoftClf.predict(traindata)
errstrain = metrics.accuracy_score(trainlabels,SoftTrainGuess)
print("The training accuracy is %f"
 % (errstrain))
SoftTestGuess=SoftClf.predict(testdata)
errstest = metrics.accuracy_score(testlabels,SoftTestGuess)
print("The test accuracy is %f"
 % (errstest))

for i in range(testlabels.shape[0]):
 if (SoftTestGuess[i] != testlabels[i]):
  plt.imshow(np.transpose(np.reshape(testdata[i,:],(28,28))), interpolation='nearest')
  print(SoftTestGuess[i])
  plt.show()
