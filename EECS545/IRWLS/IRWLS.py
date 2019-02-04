import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

epsilon = 1e-10
error = 1000000000

df=pd.read_csv('synthetic.csv', sep=',',header=None)
datavals=df.values
data=datavals.astype(np.float)

x = data[:,0]
y = data[:,1]
xtilde = np.hstack((np.ones((len(x),1)), x.reshape((len(x),1))))

def weight(r):
 return 1/ np.sqrt(1+r*r)

def WLS(X, C, y):
 return np.linalg.inv(X.transpose() @ np.diag(C) @ X) @ X.transpose() @ np.diag(C) @ y

w = np.zeros(xtilde.shape[1])
iter = 0

print("IRWLS iter, error, weight is")
while error > epsilon:
 iter += 1
 wold = w
 r = y - (xtilde @ w)
 C = weight(r)
 w= WLS(xtilde, C, y)
 error = np.linalg.norm(w-wold)
 print(iter, w, error)

plt.scatter(x, y)
xest = np.array([[1, x.min()], [1,x.max()]])
w_corr = [5, 10]
w_ols = WLS(xtilde, np.ones_like(C), y)
print("OLS weight is")
print(w_ols)

y_corr = xest @ w_corr
y_ols = xest @ w_ols
y_robust = xest @ w
plt.plot(xest[:,1], y_corr, label = "True Line", color = 'm')
plt.plot(xest[:,1], y_ols, label = "OLS Regression", color = 'r')
plt.plot(xest[:,1], y_robust, label = "Robust Regression", color = 'y')
plt.xlabel('x')
plt.ylabel('y')
green_patch = mpatches.Patch(color='m', label='True')
red_patch = mpatches.Patch(color='red', label='OLS')
blue_patch = mpatches.Patch(color='y', label='Robust')
plt.legend(handles=[green_patch, red_patch,blue_patch])
plt.show()
