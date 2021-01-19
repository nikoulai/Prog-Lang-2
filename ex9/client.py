#!/usr/bin/env python3

import sys
import requests
import re
#from html.parser import HTMLParser

questionTag = "<code class=\"block\" id=\"question\">"

def lps(str):
    n = len(str)

    L = [[0 for x in range(n)]for y in range(n)]

    for i in range(n):
        L[i][i] = 1

    for cl in range( 2, n+1):
        for i in range(n - cl + 1):
            j = i + cl - 1
            if (str[i] == str[j] and cl == 2):
                L[i][j] = 2
            elif (str[i] == str[j]):
                L[i][j] = L[i + 1][j - 1] + 2
            else:
                L[i][j] = max(L[i][j - 1],L[i + 1][j])

    return L[0][n - 1]
#def getString(html):

url = sys.argv[1]

ses = requests.Session()

req = ses.get(url)
#print(req.text)
round = 1
while (round<=10):
   # print(req.text.find('code.question'))
    regex = r'<code\s* class\s*=\s*"block"\s* id\s*=\s*"question"\s*>\s*(.*)\s*</code>'
    titleURL = re.findall(regex, req.text)
    print("Round " + str(round) +", length:"+ str(len(titleURL[0])) +","+titleURL[0])
    answer = len(titleURL[0]) - lps(titleURL[0])
    print("Answer:"+ str(answer))
    data = { "answer": answer }
    req = ses.post(url, data = data)
    if 'class="right"' in req.text:
        print('Right :-)')
        req = ses.post(url,data={"again":"continue"})
        round = round + 1
    else:
        print('Wrong :-(')
        req = ses.post(url,data={"again":"continue"})
    #print(r.text)
    print("\n")
