rm(list=ls())
install.packages("nnet")
install.packages("lmerTest") 
install.packages("scales")
install.packages("lme4")
install.packages("brms") 
pre <- read.csv("./pretest.csv")
delay <- read.csv("./delay_ACC.csv")
immediate <- read.csv("./immediate_ACC.csv")
print(delay)


library(brms)
library(nnet)
library(lme4)
library(scales)
library(lmerTest)

#아래의 스케일러중 성능이 잘 나오는 걸 사용하면됩니다.
#pretest_spd : Pre_SPD , pretest_acc : Pre_ACC

#Min-Max 스케일링은 데이터를 지정된 범위로 변환합니다. 여기서는 -1과 1 사이로 변환합니다.
delay$Pre_SPD <- rescale(delay$Pre_SPD, to = c(-1, 1)) 
delay$Speed <- rescale(delay$Speed, to = c(-1, 1)) 
delay$Question <- rescale(delay$Question, to = c(-1, 1)) 
delay$ID <- rescale(delay$ID, to = c(-1, 1))
delay$Aptitude <- rescale(delay$Aptitude, to = c(-1, 1))

# 로그 변환을 적용하기 전에, 값이 0이거나 음수인 경우 1을 더하여 양수로 만듭니다.
delay$Pre_SPD <- log(delay$Pre_SPD + 1) 
delay$Speed <- log(delay$Speed + 1) 
delay$Question <- log(delay$Question + 1) 
delay$ID <- log(delay$ID + 1)

# Standarscaler 평균0, 표준편차 1 
delay$Pre <- scale(delay$Pre) 
delay$Speed <- scale(delay$Speed) 
delay$Question <- scale(delay$Question) 
delay$ID <- scale(delay$ID)

print(delay)


# 순서형 변수요인으로 변환
delay$Accuracy <- factor(delay$Accuracy, ordered = TRUE)
delay$Pre_ACC <- factor(delay$Pre_ACC, ordered = TRUE)
delay$Aptitude <- factor(delay$Aptitude, ordered = TRUE)

#참가자와 문항별로 달라질 수 있음을 고려하여 모델델.
#선형랜덤효과모형 : 속도 
model <- lmer(Speed ~ Group*Aptitude + Pre_SPD*Aptitude + (1 | Question) + (1 | ID), data = delay) #
summary(model)

#순서형 로지스틱 랜덤덤효과모형 : 정확도 
brms_model_a <- brm( Accuracy ~ Group + Pre_ACC + (1 | Question) + (1 | ID), data = delay, family = cumulative() , chains=2) 

# 순서형 로지스틱 조절효과모형 적합, 상호작용 항 추가 
brms_model_b <- brm( Accuracy ~ Group * Aptitude + Pre_ACC * Aptitude + (1 | Question) + (1 | ID), data = delay, family = cumulative(), chains = 2) 



# brms 모델의 요약 결과
brms_summary <- summary(brms_model)

# 각 계수의 추정치와 표준 오차를 추출
estimates <- brms_summary$fixed[, "Estimate"]
std_errors <- brms_summary$fixed[, "Est.Error"]

# Z-값 계산
z_values <- estimates / std_errors

# 계수 이름 추출 
coef_names <- rownames(brms_summary$fixed)

# p-값 계산 (양측 검정, Z-분포의 누적 분포 함수 사용)
p_values <- 2 * (1 - pnorm(abs(z_values)))

# 결과를 데이터 프레임으로 정리
results <- data.frame(
  Coefficient = coef_names,
  Estimate = estimates,
  Std_Error = std_errors,
  Z_value = z_values,
  P_value = p_values
)

print(results)
# 상호작용 효과 확인 
interaction_effects <- results[grep(":", results$Coefficient), ]
print(interaction_effects)

plot(brms_model)
qqnorm(resid(brms_model))
qqline(resid(brms_model))



# ggplot2 패키지 설치 및 로드
install.packages("ggplot2")
library(ggplot2)



# 데이터에 'Dataset' 열 추가
immediate$Dataset <- 'Immediate'
delay$Dataset <- 'Delay'

# 두 데이터 프레임을 결합
combined_data <- rbind(immediate, delay)

# Accuracy 변수를 순서형 요인으로 변환
combined_data$Accuracy <- factor(combined_data$Accuracy, ordered = TRUE)

library(ggplot2)
library(dplyr)


# 데이터에 'Dataset' 열 추가
immediate$Dataset <- 'Immediate'
delay$Dataset <- 'delay'

# 두 데이터 프레임을 결합
combined_data <- bind_rows(immediate, delay)
library(dplyr)
# 그래프 생성

p <- ggplot(combined_data, aes(x = Aptitude, y = Speed, color = Group)) + 
  geom_smooth(method = "lm", se = TRUE) + # 95% 신뢰구간 포함 
  facet_wrap(~ Dataset) + # Dataset별로 그래프 분리 
  labs( title = "Effect of Aptitude on Speed by Group", x = "Aptitude", y = "Speed" ) 
+ theme_minimal()

# 그래프 출력
print(p)



#선형혼합효과모형 : 속도 // 상호작용 aptitude 

model <- lmer(Speed ~ Group * aptitude + Pre_SPD * aptitude + (1 | Question) + (1 | ID), data = delay)
# 모델 요약
summary(model)
