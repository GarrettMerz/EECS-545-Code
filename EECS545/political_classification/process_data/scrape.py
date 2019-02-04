# execution:
# python scrape.py '[webpage to scrape]' [campaign year (e.g. 2012)] [party (D/R)] [speaker initials (e.g. BO)]

from lxml import html
import requests
import sys
from bs4 import BeautifulSoup

print sys.argv[1]

page = requests.get(str(sys.argv[1])).text

bs = BeautifulSoup(page, "lxml")
possible_links = bs.find_all('a')

all_links = []

for link in possible_links:
    if link.has_attr('href'):
        if 'pid' not in link.attrs['href']:
            continue
        all_links.append('http://www.presidency.ucsb.edu/' + link.attrs['href'][3:])

k = 0

flist = open('data/rawtext/%d/%s/files.list' % (int(sys.argv[2]), str(sys.argv[3])), 'a+')

for link in all_links:
    page = requests.get(link)
    tree = html.fromstring(page.content)

    print 'getting text from %s' % str(link)
    f = open('data/rawtext/%d/%s/%s_%d' % (int(sys.argv[2]), str(sys.argv[3]), str(sys.argv[4]), k),'w')
    flist.write("data/rawtext/%d/%s/%s_%d\n" % (int(sys.argv[2]), str(sys.argv[3]), str(sys.argv[4]), k))
    k += 1

    temp = tree.xpath('//span[@class="displaytext"]/text()')

    for j in range(0, len(temp)):
        f.write(temp[j].encode('utf-8'))

        f.write('\n\n')

    for i in range(1, 1000):
        temp = tree.xpath('//span[@class="displaytext"]//p[%d]/text()' % i)
        if (temp == []):
            break
        for j in range(0, len(temp)):
            f.write(temp[j].encode('utf-8'))

        f.write('\n\n')

    f.close()
