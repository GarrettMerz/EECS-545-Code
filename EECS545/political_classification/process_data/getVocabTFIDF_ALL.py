from nltk.corpus import stopwords, names
from nltk import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.stem.porter import PorterStemmer
from sklearn.feature_extraction.text import CountVectorizer
import io, unicodedata, re
import numpy as np

class LemmaTokenizer(object):
    def __init__(self):
        self.wnl = WordNetLemmatizer()
        self.ps = PorterStemmer()
    def __call__(self, doc):
        temp = [self.wnl.lemmatize(t) for t in word_tokenize(doc)]
        return [self.ps.stem(t) for t in temp]

s1 = set(stopwords.words('english'))
s2 = set(names.words('male.txt'))
s3 = set(names.words('female.txt'))
s = s1 | s2 | s3
s = {x.lower() for x in s}

s.add('{')
s.add('}')
s.add('!')
s.add('$')
s.add("'")
s.add("''")
s.add("'d")
s.add("'ll")
s.add("'m")
s.add("'re")
s.add("'s")
s.add("'splainin")
s.add("'ve")
s.add('(')
s.add(')')
s.add(',')
s.add('.')
s.add('/')
s.add('\\')
s.add(':')
s.add('?')
s.add(';')
s.add('[')
s.add(']')
s.add('%')
s.add('&')
s.add('``')
s.add('`')
s.add('#')
s.add('`we')
s.add('_')
s.add('uh')
s.add('q')

corpus = []
party = []
year = []

with io.open('filelists/files_1960.list', 'r') as filelist:
    allfiles = filelist.readlines()

for thisfile in allfiles:
    year.append(1960)
    if "/R/" in str(thisfile):
        party.append(-1)
    else:
        party.append(1)
    with io.open(str(thisfile).strip(), 'r') as myfile:
        txt = myfile.read().replace('\n', ' ')
        txt = ''.join([i if ord(i) < 128 else ' ' for i in txt])
        # txt = unicodedata.normalize('NFKD', txt).encode('ascii', 'ignore')
        txt = re.sub("\d+", " ", txt)
        txt = re.sub("-", " ", txt)
        txt = re.sub("'", " ", txt)
        txt = re.sub("\.", " ", txt)
        txt = re.sub("\*", " ", txt)
        corpus.append(txt)

with io.open('filelists/files_2008.list', 'r') as filelist:
    allfiles = filelist.readlines()

for thisfile in allfiles:
    year.append(2008)
    if "/R/" in str(thisfile):
        party.append(-1)
    else:
        party.append(1)
    with io.open(str(thisfile).strip(), 'r') as myfile:
        txt = myfile.read().replace('\n', ' ')
        txt = ''.join([i if ord(i) < 128 else ' ' for i in txt])
        # txt = unicodedata.normalize('NFKD', txt).encode('ascii', 'ignore')
        txt = re.sub("\d+", " ", txt)
        txt = re.sub("-", " ", txt)
        txt = re.sub("'", " ", txt)
        txt = re.sub("\.", " ", txt)
        txt = re.sub("/", " ", txt)
        txt = re.sub("\*", " ", txt)
        corpus.append(txt)

with io.open('filelists/files_2016.list', 'r') as filelist:
    allfiles = filelist.readlines()

for thisfile in allfiles:
    year.append(2016)
    if "/R/" in str(thisfile):
        party.append(-1)
    else:
        party.append(1)
    with io.open(str(thisfile).strip(), 'r') as myfile:
        txt = myfile.read().replace('\n', ' ')
        txt = ''.join([i if ord(i) < 128 else ' ' for i in txt])
        # txt = unicodedata.normalize('NFKD', txt).encode('ascii', 'ignore')
        txt = re.sub("\d+", " ", txt)
        txt = re.sub("-", " ", txt)
        txt = re.sub("'", " ", txt)
        txt = re.sub("\.", " ", txt)
        txt = re.sub("/", " ", txt)
        txt = re.sub("\*", " ", txt)
        corpus.append(txt)

# print party

vectorizer = CountVectorizer(stop_words=s, tokenizer=LemmaTokenizer(), max_features=5000)
X = vectorizer.fit_transform(corpus)
all_tokens = vectorizer.get_feature_names()

print all_tokens
print X.toarray()

Y = X.toarray()

N = len(Y)
nt = np.zeros(len(Y[0]))

for i in range(0, len(Y)):
    for j in range(0, len(Y[i])):
        if Y[i][j] != 0:
            nt[j] += 1

outfile1960 = open("SPARSE1960.dat", "w")
outfile2008 = open("SPARSE2008.dat", "w")
outfile2016 = open("SPARSE2016.dat", "w")

for i in range(0, len(Y)):
    if year[i] == 1960:
        thisline = '' + str(party[i]) + '  '
        for j in range(0, len(Y[i])):
            if Y[i][j] != 0:
                thisweight = Y[i][j] * np.log(1 + float(N)/nt[j])
                thisline += str(j+1) + ':' + str(thisweight) + ' '
        thisline += '\n'
        outfile1960.write(thisline)

    elif year[i] == 2008:
        thisline = '' + str(party[i]) + '  '
        for j in range(0, len(Y[i])):
            if Y[i][j] != 0:
                thisweight = Y[i][j] * np.log(1 + float(N)/nt[j])
                thisline += str(j+1) + ':' + str(thisweight) + ' '
        thisline += '\n'
        outfile2008.write(thisline)

    elif year[i] == 2016:
        thisline = '' + str(party[i]) + '  '
        for j in range(0, len(Y[i])):
            if Y[i][j] != 0:
                thisweight = Y[i][j] * np.log(1 + float(N)/nt[j])
                thisline += str(j+1) + ':' + str(thisweight) + ' '
        thisline += '\n'
        outfile2016.write(thisline)

outfile1960.close()
outfile2008.close()
outfile2016.close()

outfile2x1960 = open("DENSE.X.1960.dat", "w")
outfile2y1960 = open("DENSE.Y.1960.dat", "w")
outfile2x2008 = open("DENSE.X.2008.dat", "w")
outfile2y2008 = open("DENSE.Y.2008.dat", "w")
outfile2x2016 = open("DENSE.X.2016.dat", "w")
outfile2y2016 = open("DENSE.Y.2016.dat", "w")

for i in range(0, len(Y)):
    if year[i] == 1960:
        thislineY = '' + str(party[i]) + '\n'
        thislineX = ''
        for j in range(0, len(Y[i])):
            if Y[i][j] != 0:
                thisweight = Y[i][j] * np.log(1 + float(N)/nt[j])
                thislineX += str(thisweight) + ','
            else:
                thislineX += '0,'
        thislineX += '\n'
        outfile2x1960.write(thislineX)
        outfile2y1960.write(thislineY)

    elif year[i] == 2008:
        thislineY = '' + str(party[i]) + '\n'
        thislineX = ''
        for j in range(0, len(Y[i])):
            if Y[i][j] != 0:
                thisweight = Y[i][j] * np.log(1 + float(N)/nt[j])
                thislineX += str(thisweight) + ','
            else:
                thislineX += '0,'
        thislineX += '\n'
        outfile2x2008.write(thislineX)
        outfile2y2008.write(thislineY)

    elif year[i] == 2016:
        thislineY = '' + str(party[i]) + '\n'
        thislineX = ''
        for j in range(0, len(Y[i])):
            if Y[i][j] != 0:
                thisweight = Y[i][j] * np.log(1 + float(N)/nt[j])
                thislineX += str(thisweight) + ','
            else:
                thislineX += '0,'
        thislineX += '\n'
        outfile2x2016.write(thislineX)
        outfile2y2016.write(thislineY)

outfile2x1960.close()
outfile2y1960.close()
outfile2x2008.close()
outfile2y2008.close()
outfile2x2016.close()
outfile2y2016.close()

token_list = open("TOKEN_LIST", "w")

for i in range(0, len(all_tokens)):
    thisline = '' + str(i+1) + ' ' + all_tokens[i] + '\n'
    token_list.write(thisline)

token_list.close()

