import nltk
#nltk.download('omw-1.4')
from nltk.corpus import sentiwordnet as swn


'''jazz = swn.senti_synset('jazz.n.01')
print(jazz)'''

#slow_list=list(swn.senti_synsets('jazz'))
#print(slow_list)



all = swn.all_senti_synsets()
print([swn.senti_synsets(x) for x in all if x in ['jazz','blues', 'pop']])
### NON FUNZIONA ANCORA