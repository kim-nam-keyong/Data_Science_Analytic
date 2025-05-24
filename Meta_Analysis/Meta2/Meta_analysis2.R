# 패키지 사용보다 계산 수식을 작성해서 분석.
rm(list = ls())
install.packages("meta") # meta_8.0-1 / R 4.4.2 version
library(meta)
data <- read.csv('meta_final.csv') # 데이터 로드 


# 효과 크기 및 기본 통계량 계산
data$success_rate_per_1000 <- (data$distress.N / data$total.N) * 1000
data$log_effect_size <- log(data$success_rate_per_1000)
data$variance <- 1 / data$total.N
data$std_error <- sqrt(data$variance)

z <- 2.576  # 99% 신뢰 구간을 위한 Z 값
data$CI_lower <- exp(data$log_effect_size - z * data$std_error)
data$CI_upper <- exp(data$log_effect_size + z * data$std_error)


# Q-통계량 및 I² 계산
weights <- 1 / data$variance
mean_effect <- sum(weights * data$log_effect_size) / sum(weights)
q_stat <- sum(weights * (data$log_effect_size - mean_effect)^2)
df <- nrow(data) - 1
i2 <- max(0, (q_stat - df) / q_stat) * 100


# 랜덤 효과 모델 계산  
tau2 <- max(0, (q_stat - df) / (sum(weights) - sum(weights^2) / sum(weights)))
random_weights <- 1 / (data$variance + tau2)
random_mean <- sum(random_weights * data$log_effect_size) / sum(random_weights)
print(exp(random_mean)) #랜덤효과의 전체효과는 280.72 랜덤효과 채택


# Leave-one-out 분석
leave_one_out <- function(data) {
  results <- c()
  for (i in 1:nrow(data)) {
    temp_data <- data[-i, ]
    temp_weights <- 1 / temp_data$variance
    temp_mean <- sum(temp_weights * temp_data$log_effect_size) / sum(temp_weights)
    results <- c(results, exp(temp_mean))
  }
  return(results)
}

sensitivity_results <- leave_one_out(data)
print(sensitivity_results) # 4번째 연구를 제외했을떄 전체효과 279.69 / 랜덤효과 모델이 더 좋음.

random_se <- sqrt(1 / sum(random_weights))
ci_lower <- exp(random_mean - 2.576 * random_se)  # 99% CI
ci_upper <- exp(random_mean + 2.576 * random_se)

# 결과 출력
cat("이질성 통계량:\n")
cat(sprintf("Q-statistic: %.2f\n", q_stat))
cat(sprintf("I²: %.2f%%\n", i2))
cat(sprintf("τ²: %.4f\n", tau2))
cat(sprintf("\n전체 효과 크기 (99%% CI): %.2f [%.2f, %.2f]\n", exp(random_mean), ci_lower, ci_upper))



# 메타분석 객체 생성
meta_analysis <- metagen(
  TE = data$log_effect_size,
  seTE = data$std_error,
  studlab = data$First.author.year,
  sm = "OR",
  random = TRUE,
  method.tau = "DL"
)
print(ci_lower)
meta_analysis$studlab <- data$First.author.year
meta_analysis$total.N <- data$total.N
meta_analysis$distress.N <- data$distress.N
xlim_range <- range(c(data$CI_lower, data$CI_upper))
par(mar = c(50, 10, 4, 2) + 10)
# 포레스트 플롯 생성
forest(meta_analysis, 
       xlab = "Effect Size Success", 
       leftcols = c("studlab", "total.N", "distress.N"), 
       leftlabs = c("Study", "Total N", "Distress N"), 
       rightcols = c("effect.ci", "w.random"), 
       rightlabs = c("Success per 1000 (99% CI)", "Weight (random)"), 
       print.tau2 = TRUE, 
       print.tau2.lab = "τ²",  
       print.I2 = TRUE, 
       print.tau2.lab = expression(tau^2), 
       print.Q = TRUE,
       print.I2.lab = expression(I^2),
       print.Q.lab = expression(chi^2),
       print.Q.lab = "Q", 
       col.diamond = "green", 
       col.diamond.lines = "blue", 
       col.predict = "red", 
       col.predict.lines = "red", 
       col.square = "blue", 
       col.square.lines = "blue", 
       xlim = xlim_range,# xlim 설정
       fontsize=10,
       spacing=1.2,
       squaresize=0.8,
       smlab ="",
       test.overall = TRUE,
       layout="meta",
       margin=c(100,10,10,10),
       colgap.forest.left = "1cm",
       plotwidth = "5cm",
       )


print(data$CI_lower)

# 고정효과와 , 랜덤효과를 비교한 그래프 케이스입니다.
# 큰 다이아몬드는 랜덤효과의 신뢰구간 옆에가 고정효과의 신뢰구간
