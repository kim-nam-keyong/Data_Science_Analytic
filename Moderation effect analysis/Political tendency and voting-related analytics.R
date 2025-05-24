#################### 데이터 전처리#################### 
# 패키지 로드
library(dplyr)
library(readxl)

df2 <- read_excel('KGSS.xlsx')

change_age <- function(x) {
  if (20 <= x & x <= 29) {
    return(1)
  } else if (30 <= x & x <= 39) {
    return(2)
  } else if (40 <= x & x <= 49) {
    return(3)
  } else if (50 <= x & x <= 59) {
    return(4)
  } else if (60 <= x) {
    return(5)
  } else {
    return(NA)
  }
}

change_income <- function(x) {
  if (0 <= x & x <= 4) {
    return(1)
  } else if (5 <= x & x <= 7) {
    return(2)
  } else if (8 <= x & x <= 11) {
    return(3)
  } else if (12 <= x & x <= 15) {
    return(4)
  } else if (16 <= x & x <= 21) {
    return(5)
  } else {
    return(NA)
  }
}

change_ED <- function(x) {
  if (0 <= x & x <= 2) {
    return(1)
  } else if (x == 3) {
    return(2)
  } else if (4 <= x & x <= 5) {
    return(3)
  } else if (6 <= x & x <= 7) {
    return(4)
  } else {
    return(NA)
  }
}

# 전처리 # AGE값이 19보다 큰행을 선택하고 코딩을 진행합니다,
target <- df2 %>% filter(AGE > 19) %>% mutate(
  AGE = sapply(AGE, change_age),
  INCOME = sapply(INCOME, change_income)
) %>% filter(!is.na(INCOME)) %>% mutate(
  INCOME = as.integer(INCOME),
  EDUC = sapply(EDUC, change_ED)
) %>% filter(!is.na(EDUC)) %>% mutate(
  EDUC = as.integer(EDUC)   # EDUC컬럼에 결측치 제거하고 , 정수형으로 변환.
)

# 독립변수 -1,-8 데이터 제거
test <- target %>% filter(
  FINPROS != -1 & FINPROS != -8 &
    ECOPROS != -1 & ECOPROS != -8 &
    SATECO != -1 & SATECO != -8 &
    SATFIN != -1 & SATFIN != -8
)  


#VOTE로 시작하는 컬럼명을 모두 돌며 , 1,2값을 가지면 result 컬럼에 1을 추가 없으면 0을 추가합니다.
target_col <- target %>% select(starts_with("VOTE"))
test <- test %>% mutate(result = apply(target_col, 1, function(row) {
  if (any(row %in% c(1, 2))) 1 else 0
})) 

# 사용할 변수만 설정해서 저장합니다.
test_final <- test %>% select(YEAR, SEX, AGE, EMPLY, EDUC, INCOME, REGION1, SATECO, ECOPROS, SATFIN, FINPROS, result)

# GDP 데이터 로드 및 전처리
# GDP 데이터프레임에서 , 컬럼명을 바꾸고 전국 데이터 지우고 매칭시킵니다.
# 전국데이터는 앞선 데이터프레임에 없기때문이죠
gdp <- read_excel("RGDP.xlsx")
gdp <- gdp %>% rename(YEAR = t, REGION = REGION1, REGION1 = REGION_NEW)
gdp2 <- gdp %>% filter(!is.na(REGION1))
gdp3 <- gdp2 %>% select(YEAR, REGION1, RGDP, RGDP_t_1, RGDP_t_2, RGDP_t_3) %>% distinct(YEAR, REGION1, .keep_all = TRUE)
gdp_all <- gdp %>% filter(REGION == '전국')


# 데이터 병합 GDP데이터를 YEAR, REGION1 컬럼에 맞춰서 합칩니다.
merged_df <- left_join(test_final, gdp3, by = c("YEAR", "REGION1"))


# 특정 연도 데이터 전처리
df_2011 <- merged_df %>% filter(YEAR == 2011) %>% mutate(target2 = RGDP_t_1 / (RGDP_t_2 + RGDP_t_3 / 2))
df_2012 <- merged_df %>% filter(YEAR == 2012) %>% mutate(target2 = RGDP_t_1 / (RGDP_t_2 + RGDP_t_3 / 2))

#merged_df %>% filter(YEAR == 2011) %>% mutate(target2 = RGDP_t_1 / (RGDP_t_2 + RGDP_t_3 / 2)): merged_df 데이터프레임에서 
#YEAR 값이 2011인 행을 선택하고, target2 컬럼을 계산하여 df_2011 데이터프레임에 저장합니다.


# 결과 출력
print(head(df_2011))
print(head(df_2012))

################################### 모델링 파트 ####################################################
library(ggplot2)
library(car)
library(pROC)
library(dplyr)
library(brms)
library(nnet)
library(lme4)
library(scales)
library(lmerTest)

# 데이터 load 경로는 본인에 맞게 설정하셔야 합니다.
df <- read.csv("./final_df.csv")
df_2011 <- read.csv("./df_2011.csv")
df_2012 <- read.csv("./df_2012.csv")
print(df_2011)
#스케일링, 1-5점 척도 데이터이고 GDP, GNI 값 자체가 크기 때문에 스케일링을 하고 안하고 차이를 판단합니다.
# ----------------------------------------------
#Min-Max 스케일링은 데이터를 지정된 범위로 변환합니다. 여기서는 -1과 1 사이로 변환합니다.
df$RGNI <- rescale(df$RGNI, to = c(-1, 1)) 
df$RGNI_t_1 <- rescale(df$RGNI_t_1, to = c(-1, 1)) 
df$RGNI_t_2 <- rescale(df$RGNI_t_2, to = c(-1, 1)) 
df$RGNI_t_3 <- rescale(df$RGNI_t_3, to = c(-1, 1))

# Standarscaler 평균0, 표준편차 1 
df$RGNI <- scale(df$RGNI) 
df$RGNI_t_1 <- scale(df$RGNI_t_1) 
df$RGNI_t_2 <- scale(df$RGNI_t_2) 
df$RGNI_t_3 <- scale(df$RGNI_t_3)

# 로그 변환을 적용하기 전에, 값이 0이거나 음수인 경우 1을 더하여 양수로 만듭니다.
df$RGNI <- log(df$RGNI + 1) 
df$RGNI_t_1 <- log(df$RGNI_t_1 + 1) 
df$RGNI_t_2 <- log(df$RGNI_t_2 + 1) 
df$RGNI_t_3 <- log(df$RGNI_t_3 + 1)

# -----------------------------------------------------

#Min-Max 스케일링은 데이터를 지정된 범위로 변환합니다. 여기서는 -1과 1 사이로 변환합니다.
df$GDP <- rescale(df$RGDP, to = c(-1, 1)) 
df$RGDP_t_1 <- rescale(df$RGDP_t_1, to = c(-1, 1)) 
df$RGDP_t_2 <- rescale(df$RGDP_t_2, to = c(-1, 1)) 
df$RGDP_t_3 <- rescale(df$RGDP_t_3, to = c(-1, 1))

# Standarscaler 평균0, 표준편차 1 
df$RGDP <- scale(df$RGDP) 
df$RGDP_t_1 <- scale(df$RGDP_t_1) 
df$RGDP_t_2 <- scale(df$RGDP_t_2) 
df$RGDP_t_3 <- scale(df$RGDP_t_3)
dfdf$RGDP_t_3 <- scale(df$RGDP_t_3)




# 로그 변환을 적용하기 전에, 값이 0이거나 음수인 경우 1을 더하여 양수로 만듭니다.
df$RGDP <- log(df$RGDP + 1) 
df$RGDP_t_1 <- log(df$RGDP_t_1 + 1) 
df$RGDP_t_2 <- log(df$RGDP_t_2 + 1) 
df$RGDP_t_3 <- log(df$RGNI_t_3 + 1)

print(df_2011)
# ------------------------------------------------------------
# /// RGDP 적용 
# 기본 모델 적용 
# 독립변수와 , 통제변수를 넣습니다.
model <- glm(result ~ ECOPROS + FINPROS + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + GDP , data = df)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model)
print(vif_values)

# 상호작용 조절효과 1 모형 모델 
# 독립변수에 조절변수와 상호작용을 적용시킵니다.
model <- glm(result ~ ECOPROS * RGDP_t_1 + FINPROS * RGDP_t_1 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + GDP, data = df,family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model, type='terms')
print(vif_values)


# 상호작용 조절효과 2  모형 모델 
model <- glm(result ~ ECOPROS * target2 + FINPROS * target2 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + GDP, data = df,family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model, type='terms')
print(vif_values)


# ------------------------------------------------------------
# /// RGNI 적용 
# 기본 모델 적용
model <- glm(result ~ ECOPROS + FINPROS + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGNI , data = df, family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model)
print(vif_values)

# 상호작용 조절효과 1 모형 모델 
model <- glm(result ~ ECOPROS * RGNI_t_1 + FINPROS * RGNI_t_1 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGNI, data = df,family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model, type='terms')
print(vif_values)


# 상호작용 조절효과 2  모형 모델 
model <- glm(result ~ ECOPROS * target2_N + FINPROS * target2_N + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGNI, data =df,family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model, type='terms')
print(vif_values)

#————————————————————————————



#  ECOPROS와 FINPROS 상호작용 플롯
ggplot(df, aes(x = ECOPROS, y = result, color = "ECOPROS")) +
  geom_point(aes(color = "ECOPROS")) +
  geom_smooth(aes(color = "ECOPROS"), method = "glm", se = TRUE, fill = "lightblue", level = 0.95) +
  geom_point(aes(x = FINPROS, color = "FINPROS")) +
  geom_smooth(aes(x = FINPROS, color = "FINPROS"), method = "lm", se = TRUE, fill = "lightcoral", level = 0.95) +
  labs(title = "ECOPROS and FINPROS",
       x = "Independent Variables",
       y = "Result",
       color = "Variables") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


#------------------------------------
# 2011 년 데이터와 2012년 데이터 구분해서 진행 

# 2011년 기본모델
model <- glm(result ~ ECOPROS + FINPROS +SATECO + SATFIN + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGDP , data = df_2011, family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model)
print(vif_values)

# 2012년 기본모델 
model <- glm(result ~ ECOPROS + FINPROS +SATECO + SATFIN + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGDP , data = df_2012, family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model)
print(vif_values)

#---------------------------------------
# 조절변수 1 설정 후 모델 변화
# 2011년 데이터 
model <- glm(result ~ ECOPROS * RGDP_t_1 + FINPROS * RGDP_t_1 + SATFIN * RGDP_t_1 + SATECO * RGDP_t_1 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGDP, data = df_2011,family=binomial)
summary(model)
logLik(model)
AIC(model)
BIC(model)
vif_values <- vif(model, type='terms')
print(vif_values)
# 2012년 데이터 
model <- glm(result ~ ECOPROS * RGDP_t_1 + FINPROS * RGDP_t_1 + SATFIN * RGDP_t_1 + SATECO * RGDP_t_1 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGDP, data = df_2012,family=binomial)
summary(model)



# 조절변수 2 설정 후 모델변화
# 2011년 데이터 
model <- glm(result ~ ECOPROS * target2 + FINPROS * target2 + SATECO * target2 + SATFIN * target2 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGDP, data =df_2011,family=binomial)
summary(model)# 모델요약
logLik(model) # 로그우도 측정 보통 음수로 나오고 음수의 절댓값이 작을수록 모델이 적합함.
AIC(model) # AIC 측정
BIC(model) # BIC측정
vif_values <- vif(model, type='terms') # VIF 측정
print(vif_values)

# 2012년 데이터 
model <- glm(result ~ ECOPROS * target2 + FINPROS * target2 + SATECO * target2 + SATFIN * target2 + SEX + AGE + EMPLY + INCOME + REGION1 + EDUC + RGDP, data =df_2012,family=binomial)
summary(model)# 모델요약
logLik(model) # 로그우도 측정 보통 음수로 나오고 음수의 절댓값이 작을수록 모델이 적합함.
AIC(model) # AIC 측정
BIC(model) # BIC측정
vif_values <- vif(model, type='terms') # VIF 측정
print(vif_values)

