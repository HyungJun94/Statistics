#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#naver news crawling 
from bs4 import BeautifulSoup 
import urllib.request


# In[ ]:


import numpy as np
import pandas as pd


# In[ ]:


# URL 주소 [정치] 
URL1 = np.array([])
for i in np.arange(100):
  num = 3053599 - i
  new_URL  = np.array(['https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=100&oid=081&aid=000{}'.format(num)])
  URL1 = np.concatenate((URL1,new_URL))

# URL 주소 [경제] 
URL2 = np.array([])
for i in np.arange(100):
  num = 2962982 - i
  new_URL  = np.array(['https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=101&oid=025&aid=000{}'.format(num)])
  URL2 = np.concatenate((URL2,new_URL))

# URL 주소 [사회] 
URL3 = np.array([])
for i in np.arange(100):
  num = 7876 - i
  new_URL  = np.array(['https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=102&oid=629&aid=000000{}'.format(num)])
  URL3 = np.concatenate((URL3,new_URL))

# URL 주소 [문화] 
URL4 = np.array([])
for i in np.arange(100):
  num = 4542990 - i
  new_URL  = np.array(['https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=103&oid=018&aid=000{}'.format(num)])
  URL4 = np.concatenate((URL4,new_URL))

URL = np.array([URL1, URL2, URL3, URL4])
# 12/25 01:30 AM 기준 


# In[ ]:


URL1[0]


# In[ ]:


# 크롤링 함수
def get_text(URL):
    source_code_from_URL = urllib.request.urlopen(URL)
    soup = BeautifulSoup(source_code_from_URL, 'lxml', from_encoding='utf-8')
    text = ''
    for item in soup.find_all('div', id='articleBodyContents'):
        text = text + str(item.find_all(text=True))
    return text


# In[ ]:


# 데이터 클리닝 함수
import re 

#클린 함수  (영어 및 특수 기호 제거)
def clean(before):
  after = re.sub('[a-zA-Z]', '', before)
  after = re.sub('[\{\}\[\]\/?.,;:|\)*~`!^\-_+<>@\#$%&\\\=\(\'\"]','', after)
  return after


# In[ ]:


#데이터 프래임으로 만들기 
import pandas as pd

genre = ['정치','경제','사회','문화']
text = np.array([])
category = np.array([])
for i in np.arange(4):
  for j in np.arange(100):
    cat = np.array([genre[i]])
    category = np.concatenate((category, cat))
    news = np.array([clean(get_text(URL[i][j]))])
    text = np.concatenate((text, news))

data0 = pd.DataFrame(np.array([text, category]), index=['text','category'])
data0 = data0.T


# In[ ]:


from google.colab import drive
drive.mount('/content/drive')


# In[ ]:


get_ipython().run_line_magic('pwd', '')


# In[ ]:


get_ipython().run_line_magic('cd', '/content/drive/My Drive')


# In[ ]:


# csv 파일로 저장
data0.to_csv("data0.csv")


# In[ ]:


#csv 파일 불러오기 
data0 = pd.read_csv('data0.csv')


# In[ ]:


data0 = data0.loc[:,['text','category']]
data0


# In[ ]:


data0


# In[ ]:


# target에 숫자로 label 부여 

from sklearn.preprocessing import LabelEncoder 
labeling = LabelEncoder()
y = labeling.fit_transform(data0['category'].values) 
y


# In[ ]:


# 결측값 대체
# text가 결측일 경우 전 기사의 text 가져오기

for i in np.arange(400):
  if str(data0['text'][i]) == 'nan':
    data0['text'][i] = data0['text'][i-1]

data0['text'].head(10)


# In[ ]:


# 추가 데이터 정제 

def addclean(text):
  text = re.sub('[0-9]','',text)
  text = re.sub('▶','',text)
  text = re.sub('■','',text)
  text = re.sub('△','',text)
  text = re.sub('◇','',text)
  text = re.sub('ⓒ', '', text)
  text = re.sub('\s본문','', text)
  text = re.sub('\s내용','', text)
  text = re.sub('\s오류를','', text)
  text = re.sub('\s플레이어','', text)
  text = re.sub('\s함수','', text)
  text = re.sub('\s우회하기','', text)
  text = re.sub('\s함수','',text)
  text = re.sub('\s위한','', text)
  text = re.sub('\s추가','', text)
  return text

for i in np.arange(400):
  data0['text'][i] = addclean(data0['text'][i])

# feature variable X 생성
X = data0['text'].values


# In[ ]:


# 문장 vector화 하는 vectorizer 

from sklearn.feature_extraction.text import TfidfVectorizer
vectorizer = TfidfVectorizer()
text = [data0['text'][7]]
voc = vectorizer.fit_transform(text)
print(vectorizer.get_feature_names())
print(voc.shape)


# In[ ]:


# 장르 별 최빈단어 10개씩 찾기 

vocab1 = np.array([])
for i in np.arange(100):
  text = np.array([X[i]])
  new_vocab = vectorizer.fit_transform(text)
  vocab1 = np.concatenate((vocab1,vectorizer.get_feature_names()))


  


# In[ ]:


# train, test set 나누기

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, stratify=y)


# In[ ]:


# 단어를 분할하는 tokenizer  - 안써도 될듯 
def tokenizer(text):
    return text.split()


# In[ ]:


#LogisticRegression

from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import TfidfVectorizer

tfidf = TfidfVectorizer(strip_accents=None,lowercase=False,preprocessor=None)
param_grid = [{'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer,None],'clf__penalty':['l1', 'l2'],'clf__C': [1.0, 10.0, 100.0]},
               {'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer, None],'vect__use_idf':[False],
                'vect__norm':[None], 'clf__penalty': ['l1', 'l2'],'clf__C': [1.0, 10.0, 100.0]}]
lr_tfidf = Pipeline([('vect', tfidf),('clf', LogisticRegression())])
gs_lr_tfidf = GridSearchCV(lr_tfidf, param_grid,scoring='accuracy',cv=5, verbose=1,n_jobs=1)
gs_lr_tfidf.fit(X_train, y_train) 


# In[ ]:


print(gs_lr_tfidf.best_params_)

y_train_pred1 = gs_lr_tfidf.predict(X_train) 
y_test_pred1 = gs_lr_tfidf.predict(X_test)

from sklearn.metrics import accuracy_score
print(accuracy_score(y_train, y_train_pred1)) # train data에 대한 accuracy
print(accuracy_score(y_test, y_test_pred1)) # test data에 대한 accuracy


# In[ ]:


# 분류 결과 logistic
from sklearn.metrics import confusion_matrix
print(confusion_matrix(y_test, y_test_pred1))


# In[ ]:


#KNN

from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfVectorizer

from sklearn.neighbors import KNeighborsClassifier 
tfidf = TfidfVectorizer(strip_accents=None,lowercase=False,preprocessor=None)
param_grid = [{'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer,None],'knn__n_neighbors':[3,5,10],'knn__p': [1,2]},
               {'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer, None],'vect__use_idf':[False],
                'vect__norm':[None], 'knn__n_neighbors':[3,5,10],'knn__p': [1,2]}]
knn_tfidf = Pipeline([('vect', tfidf),('knn', KNeighborsClassifier())])
gs_knn_tfidf = GridSearchCV(knn_tfidf, param_grid,scoring='accuracy',cv=5, verbose=1,n_jobs=1)
gs_knn_tfidf.fit(X_train, y_train) 


# In[ ]:


print(gs_knn_tfidf.best_params_)

y_train_pred2 = gs_knn_tfidf.predict(X_train) 
y_test_pred2 = gs_knn_tfidf.predict(X_test)

from sklearn.metrics import accuracy_score
print(accuracy_score(y_train, y_train_pred2)) # train data에 대한 accuracy
print(accuracy_score(y_test, y_test_pred2)) # test data에 대한 accuracy


# In[ ]:


# 분류 결과 KNN
from sklearn.metrics import confusion_matrix
print(confusion_matrix(y_test, y_test_pred2))


# In[ ]:


#SVM 

from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfVectorizer

from sklearn.svm import SVC  
tfidf = TfidfVectorizer(strip_accents=None,lowercase=False,preprocessor=None)
param_grid = [{'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer,None],'svc__C':[1e3,5e3,1e4,5e4,1e5],
               'svc__gamma': [0.005,0.01,0.1], 'svc__kernel':['poly','rbf','sigmoid']},
               {'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer, None],'vect__use_idf':[False],
                'vect__norm':[None],'svc__C':[1e3,5e3,1e4,5e4,1e5],
               'svc__gamma': [0.005,0.01,0.1], 'svc__kernel':['poly','rbf','sigmoid']}]
svc_tfidf = Pipeline([('vect', tfidf),('svc', SVC())])
gs_svc_tfidf = GridSearchCV(svc_tfidf, param_grid,scoring='accuracy',cv=5, verbose=1,n_jobs=1)
gs_svc_tfidf.fit(X_train, y_train) 


# In[ ]:


print(gs_svc_tfidf.best_params_)

y_train_pred3 = gs_svc_tfidf.predict(X_train) 
y_test_pred3 = gs_svc_tfidf.predict(X_test)

from sklearn.metrics import accuracy_score
print(accuracy_score(y_train, y_train_pred3)) # train data에 대한 accuracy
print(accuracy_score(y_test, y_test_pred3)) # test data에 대한 accuracy


# In[ ]:


# 분류 결과 SVC
from sklearn.metrics import confusion_matrix
print(confusion_matrix(y_test, y_test_pred3))


# In[ ]:


#Random Forest

from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer

from sklearn.ensemble import RandomForestClassifier
tfidf = TfidfVectorizer(strip_accents=None,lowercase=False,preprocessor=None)
param_grid = [{'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer,None],
               'rf__n_estimators':[50,100,200,500],'rf__max_depth': [3,5,10]},
               {'vect__ngram_range': [(1,1)],'vect__tokenizer': [tokenizer, None],'vect__use_idf':[False],
                'vect__norm':[None],'rf__n_estimators':[50,100,200,500],'rf__max_depth': [3,5,10]}]
rf_tfidf = Pipeline([('vect', tfidf),('rf', RandomForestClassifier())])
gs_rf_tfidf = GridSearchCV(rf_tfidf, param_grid,scoring='accuracy',cv=5, verbose=1,n_jobs=1)
gs_rf_tfidf.fit(X_train, y_train) 


# In[ ]:


print(gs_rf_tfidf.best_params_)

y_train_pred4 = gs_rf_tfidf.predict(X_train) 
y_test_pred4 = gs_rf_tfidf.predict(X_test)

from sklearn.metrics import accuracy_score
print(accuracy_score(y_train, y_train_pred4)) # train data에 대한 accuracy
print(accuracy_score(y_test, y_test_pred4)) # test data에 대한 accuracy


# In[ ]:


# 분류 결과 RandomForest
from sklearn.metrics import confusion_matrix
print(confusion_matrix(y_test, y_test_pred4))

