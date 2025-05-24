rm(list=ls())
library(meta)
library(metafor)
library(dmetar)
library(dplyr)
install.packages('metafor')
install.packages("dmetar")
df <- read.csv("C:/Users/pbcho/Documents/Rdata/meta/target.csv")
df <- df %>%
  filter(!(저자 %in% c("Lee et al. (2024)", "Li et al. (2025)")))
df

# 고정효과모델 
# 컬럼 이름을 올바르게 지정하고, 데이터 타입 확인 후 실행
# 고정효과모델 
m_fixed <- metacont(
  n.e         = df$Total_EG,
  mean.e      = df$EG.MEAN,
  sd.e        = df$EG.SD,
  n.c         = df$Total_CG,
  mean.c      = df$CG.MEAN,
  sd.c        = df$CG.SD,
  studlab     = df$저자,
  data        = df,
  sm          = "SMD",
  common = TRUE,
  random = FALSE,
  level = 0.99
)

m_random <- metacont(
  n.e         = df$Total_EG,
  mean.e      = df$EG.MEAN,
  sd.e        = df$EG.SD,
  n.c         = df$Total_CG,
  mean.c      = df$CG.MEAN,
  sd.c        = df$CG.SD,
  studlab     = df$저자,
  data        = df,
  sm          = "SMD",
  common  = FALSE,
  random = TRUE,
  method.tau  = "REML",
  method.random.ci  = TRUE,
  level = 0.95
)
summary(m_fixed)
summary(m_random)

I2_fixed   <- m_fixed$I2
tau2_fixed <- m_fixed$tau^2
I2_random  <- m_random$I2
tau2_random<- m_random$tau^2

cat("Fixed-effect: I² =", round(I2_fixed,1), "%, τ² =", round(tau2_fixed,3), "\n")
cat("Random-effects: I² =", round(I2_random,1), "%, τ² =", round(tau2_random,3), "\n")



forest(
  m_random,
  xlab        = "Standardized Mean Difference",
  leftlabs    = c("Study","n_e","mean_e","sd_e","n_c","mean_c","sd_c"),
  label.e       = "Experimental",
  label.c       = "Control",
  col.square  = "skyblue",
  col.diamond = "darkblue",
  print.tau2  = TRUE,    # τ² 표시
  print.I2    = TRUE,    # I² 표시
  print.Q     = TRUE     # Q-test p-value 표시
)


# Leave-one-out 분석
inf <- metainf(m_random)
inf
# 민감도 포레스트 플롯
forest(
  inf,
  main = "Leave-One-Out Sensitivity Analysis",
  xlab = "SMD"
)

#egger's test 
metabias(
  m_random,
  method.bias = "linreg",
  pibias      = TRUE,    # p-value 출력
  plotit      = TRUE     # funnel plot+regression line 시각화
)
funnel(m_random, studlab = TRUE)


# trim-and-fill
tf <- trimfill(m_random)
forest(tf)

#begg's test
metabias(m_random, method.bias="rank")

# Egger 테스트 실행
egger_test <- metabias(
  m_random, 
  method.bias = "egger",  # Egger's Regression Test 사용
  plotit = TRUE           # Funnel Plot + 회귀선 시각화
)

funnel(
  m_random,
  xlab        = "Standardized Mean Difference",
  ylab        = "Standard Error",
  #studlab     = TRUE,          # 연구명 표시
  random = TRUE,          # 랜덤효과 summary 선
  common  = FALSE,         # 고정효과는 생략
  level       = 0.99          # 신뢰구간 수준
   # 95%·99% 구간선
)
legend(
  "topright",
  legend = c("99% pseudo‐CI","95% pseudo‐CI"),
  lty    = c(2,3),
  col    = c("black","gray")
)

