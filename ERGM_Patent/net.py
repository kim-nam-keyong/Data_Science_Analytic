#!/usr/bin/env python
# coding: utf-8

# ### 데이터 로드 

# #### INDEX
# 
# 1. 공동출원 : 출원연도, 공동특허, 출원기관수 , 출원인구분, 출원인명 , ipc분류(섹션-서브클래스) , cpc분류(섹션-서브클래스) 
# 2. 인용특허 : 출원연도, 출원인구분, 출원인명, 유형구분 (1,2,3), ipc분류(섹션-서브클래스) ,cpc분류 , 인용문헌번호 , 피인용문헌수 
# 3. 융합특허 : 출원연도, 출원인구분, 출원인명, 유형구분(1,2,3), ipc분류(섹션-서브클래스) , cpc분류(섹션-서브클래스) 
# 
# 인용특허는 섹선-서브클래스 단위로 정리되지 않아서 = IPC/CPC분류 를 사용하거나 , 너무 복잡하면 IPC/CPC(main)만 사용해야 할 것 같다.
# 

# #### 공동출원 네트워크 전처리 

# In[2]:


import pandas as pd 
import msoffcrypto
from io import BytesIO

with open('target.xlsx', 'rb') as file:
    decrypted = BytesIO()
    office_file = msoffcrypto.OfficeFile(file)
    office_file.load_key(password='12345')  # 비밀번호 입력
    office_file.decrypt(decrypted)
df = pd.read_excel(decrypted, sheet_name='2_융합특허 분석(15~23년)_공동출원인별 분리', header=1)
df = df.drop('Unnamed: 0', axis=1) 
df = df.loc[:385,:]
df = df.loc[:,~(df.columns.str.contains('인용 문헌번호'))]
df = df.drop(columns=['출원번호(WIPSON)'])
df = df.rename(columns={'No.':'No', '출원인명(최종)':'출원인명', '출원인 국적':'출원인국적', '인용 문헌 수':'인용문헌수', 'IPC/CPC':'ICPC'})
df.loc[df['출원기관수']>=2].reset_index(drop=True)
df.columns
print(pd.__version__)
df.head()


# In[3]:


def common_codes(row):
    ipc = set(map(str.strip, row['IPC분류(섹션-서브클래스)'].split(',')))
    cpc = set(map(str.strip, row['CPC분류(섹션-서브클래스)'].split(',')))
    common_codes = ipc&cpc 
    return ','.join(common_codes)


df['IPC/CPC'] = df.apply(common_codes, axis=1)
df = df.loc[:, ~df.columns.str.contains('IPC분류\\(섹션-서브클래스\\)|CPC분류\\(섹션-서브클래스\\)', regex=True)]
df.isna().sum() # 43개의 출원기관수 null 값 존재. 
df = df.dropna(axis=0).reset_index(drop=True)
df


# In[4]:


df['출원인국적'] = df['출원인국적'].apply(lambda x:x.split('|')[0].strip())
df['출원연도'] = df['출원연도'].astype(int)
df[['유형구분2','유형구분3','출원인 그룹','출원인국적']] = df[['유형구분2','유형구분3','출원인 그룹','출원인국적']].astype('category')

df
#df.loc[(df['출원연도']>=2015) & (df['출원연도']<=2020)].to_csv("target15.csv", index=False)
#df.loc[(df['출원연도']>=2021) & (df['출원연도']<=2024)].to_csv("target21.csv", index=False)

target = df.loc[df['출원기관수']>1]
target


# In[1]:


# target1 =pd.read_csv("./target1.csv")
# target1 



# #### 인용특허 네트워크 전처리

# In[212]:


import pandas as pd 
import msoffcrypto
from io import BytesIO

with open('network.xlsx', 'rb') as file:
    decrypted = BytesIO()
    office_file = msoffcrypto.OfficeFile(file)
    office_file.load_key(password='12345')  # 비밀번호 입력
    office_file.decrypt(decrypted)
df2 = pd.read_excel(decrypted, sheet_name='3_특허인용분석')
df2.isnull().sum() #  출원인 국적 , 출원인 구분 
target2 = df2.loc[df2['구분']=='인용특허']


# In[ ]:


target2['공통부분'] = target2['No.'].str.extract(r'(DP-\d{3})')
target2['공통부분']


# In[224]:


df1 = df1.rename(columns={'No.':'공통부분'})
df1.head(1)


# In[312]:


target3.loc[target3['유형구분2']=='공공'].shape



# In[269]:


#target3 = target2.merge(df1, on ='공통부분', how='left')
target4 = target3[['출원인 구분','유형구분1','유형구분2','유형구분3','Main CPC','Main IPC','인용문헌 번호','피인용 문헌 수\n']]
target4 = target4.rename(columns={'피인용 문헌 수\n':'피인용 문헌 수'})
target4.shape # 1746

target5 = target4.dropna(axis=0).reset_index(drop=True)
target5['유형구분1'].value_counts()   # 공공&민간 ,국방&비국방 

target6 = target5.loc[~(target5['유형구분2']=='공공&민간')]
target7 = target6.loc[~(target6['유형구분3']=='국방&비국방')].reset_index(drop=True)
target7['출원인 구분'].value_counts()



# In[211]:


pd.set_option('display.max_columns', None)
target2 = pd.read_csv("./target2.csv")
target2['인용문헌 번호'].nunique() # 1293 너무많아. 
target2['Main CPC'].nunique() #  671 너무많아.
target2['Main IPC'].nunique() #  521 너무많아.
target2['출원인 구분'].value_counts().tail(30)

target2.loc[target2['출원인 구분']=='민수기업 | 방산기업'] 
target2 = target2.loc[~(target2['유형구분2']=='공공&민간')]
target2 = target2.loc[~(target2['유형구분3']=='국방&비국방')]
target2['출원인 구분']


# In[278]:


target3


# #### 융합특허 네트워크 전처리 

# In[64]:


import pandas as pd 
import msoffcrypto
from io import BytesIO

with open('network.xlsx', 'rb') as file:
    decrypted = BytesIO()
    office_file = msoffcrypto.OfficeFile(file)
    office_file.load_key(password='12345')  # 비밀번호 입력
    office_file.decrypt(decrypted)
df3 = pd.read_excel(decrypted, sheet_name='2_융합특허 분석(14~23년)', header=1)
df3 = df3.drop('Unnamed: 0', axis=1)
df3.head(3).columns


# In[108]:


target3 = df3[['출원연도','출원인 구분','유형구분1','유형구분2','유형구분3','IPC분류(섹션-서브클래스)','CPC분류(섹션-서브클래스)']]
target_final = target3.dropna(axis=0, how='any').reset_index(drop=True)
target_final
target_final.to_csv("target3.csv",index=False)


# In[ ]:


target3 = pd.read_csv("./target3.csv")
target3['유형구분3'].value_counts()  # 공공&민간 / 국방&비국방 제외 후 분석 
target3['IPC분류(섹션-서브클래스)'].value_counts()  # 서브클래스가 겹치는게 있긴하네 


# 
