import numpy as np
import matplotlib.pyplot as plt
from PIL import Image
numlist = [2, 10, 40]


#Perform Singular Value Decomposition for the image attached. This is a useful tool for image compression; we note that the first 40 values capture the majority of the image content.
X=np.asarray(Image.open('harvey-saturday-goes7am.jpg').convert('L'))
U, sigma, V = np.linalg.svd(X)

for i in numlist:
    print("i is "),
    print(i)
    plt.figure(i)
    minsize = min(U.shape[0], V.shape[0])
    signew = np.zeros((U.shape[0], V.shape[0]))
    signew[:i, :i] = np.diag(sigma[:i])
    newimg = np.matrix(U)*np.matrix(signew)*np.matrix(V)
    plt.imshow(newimg, cmap='gray')
    title = "n = %s" % i
    plt.title(title)

    diffnorm =np.linalg.norm(X-newimg)/np.linalg.norm(X)
    print("Normalized difference is "),
    print(diffnorm)
plt.show()
