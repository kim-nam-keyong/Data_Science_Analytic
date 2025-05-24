rm(list=ls())

library(lightgbm)
library(treeshap)
library(shapviz)
library(ranger)
library(iml)
library(shapr)
library(e1071)
library(spmoran)
library(spatialreg)
library(sp)
library(spdep)
library(randomForest)
library(gbm)
library(neuralnet)
library(ggplot2)
library(caret)
library(Metrics)
library(spatialreg)
install.packages("gstat")
install.packages("sf")
library(sf)
library(gstat)



################ 회귀 분석#####################################
# 데이터 로드
df <- read.csv("./Rdata/machin_SAC/target.csv")
colSums(is.na(df))
# 변수 정의
independent <- c("BdgHeight","BCR","FAR","BdgYear","PopDensity",
                 "GreenAreaR","WaterAreaR","Price","ElderlyPop",
                 "ChildPopRa","WomanPopRa")
dependent <- c("AnnualEUI", "SummerEUI", "WinterEUI")

total <- c("BdgHeight","BCR","FAR","BdgYear","PopDensity",
           "GreenAreaR","WaterAreaR","Price","ElderlyPop",
           "ChildPopRa","WomanPopRa","AnnualEUI", "SummerEUI", "WinterEUI")



# 독립 변수와 종속 변수의 왜도 계산
skewness_values <- sapply(df[total], skewness)

# 결과 출력
print(skewness_values)

# 심한 왜도가 있는 변수 필터링 (|skewness| >= 1)
high_skew_vars <- names(skewness_values[abs(skewness_values) >= 1])
cat("로그 변환이 필요한 변수:", high_skew_vars, "\n")

df[high_skew_vars] <- log(df[high_skew_vars] + 1) 

# Train-Test 분리 (8:2)
set.seed(42)
index <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[index, ]
test <- df[-index, ]

# RobustScaler 적용 함수 정의
robust_scale <- function(data) {
  median_vec <- apply(data, 2, median)
  iqr_vec <- apply(data, 2, function(x) quantile(x, 0.75) - quantile(x, 0.25))
  
  scaled_data <- sweep(data, 2, median_vec, "-")
  scaled_data <- sweep(scaled_data, 2, iqr_vec, "/")
  
  return(list(
    scaled_data = as.data.frame(scaled_data),
    median = median_vec,
    iqr = iqr_vec
  ))
}



# 독립 변수에 RobustScaler 적용
train_scaled <- robust_scale(train[independent])
test_scaled <- robust_scale(test[independent])

# 종속변수는 원본 유지
train_scaled_df <- cbind(train_scaled$scaled_data, train[dependent])
test_scaled_df <- cbind(test_scaled$scaled_data, test[dependent])

#   성능 지표 계산 함수
calculate_metrics <- function(model, test_data, dep_var) {
  # 모델 예측값 계산
  predicted <- predict(model, newdata = test_data)
  
  # 실제값과 잔차 계산
  actual <- test_data[[dep_var]]
  residuals <- actual - predicted
  
  # 오차 지표 계산
  mse <- mean(residuals^2)                 
  rmse <- sqrt(mse)                           
  mae <- mean(abs(residuals))                
  mape <- mean(abs(residuals / actual)) * 100
  r_squared <- summary(model)$r.squared      
  
  # 결과 반환
  return(data.frame(
    R2 = r_squared,
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    MAPE = mape
  ))
}
# 공간 가중치 생성.
train_indices <- as.integer(rownames(train_scaled_df))
coords <-cbind(df$lon[train_indices], df$lat[train_indices])
dist_matrix_train <- dnearneigh(coords, d1 = 0, d2 = 3, longlat=TRUE) 
listw_train <- nb2listw(dist_matrix_train, style = "W", zero.policy=TRUE)

# 훈련 데이터의 행 개수 확인
nrow(train_scaled_df)

# 공간 가중치 행렬의 크기 확인
length(listw_train$neighbours)


models_LM <- lapply(dependent, function(dep_var) {
  # 모델 생성 (훈련 데이터 사용)
  formula <- as.formula(paste(dep_var, "~", paste(independent, collapse = "+")))
  model <- lm(formula, data = train_scaled_df)
  
  # 성능 평가 (테스트 데이터 사용)
  metrics <- calculate_metrics(model, test_scaled_df, dep_var)
  
  
  # 테스트 결과 저장
  metrics <- cbind(metrics)
  
  # 결과 저장: 모델과 성능 지표 모두 반환
  list(
    model = model,
    metrics = cbind(Dep_Variable = dep_var, metrics)
  )
})

for(dep_var in dependent) {
  # 모델 적합
  fmla  <- as.formula(paste(dep_var, "~", paste(independent, collapse = "+")))
  model <- lm(fmla, data = train_scaled_df)
  
  # LM 검정 실행 → 결과를 sp_tests 에 저장
  sp_tests <- lm.RStests(
    model,
    listw       = listw_train,
    test        = c("RSerr","RSlag","adjRSerr","adjRSlag"),
    zero.policy = TRUE
  )
  
  # 검정 결과 출력 (여기서야 비로소 RSerr/RSlag 값이 보임)
  cat("====", dep_var, "LM Tests ====\n")
  print(sp_tests)
  
}
# 모델별 AIC확인인
formula <- as.formula(paste("WinterEUI", "~", paste(independent, collapse = "+")))
model <- lm(formula, data = train_scaled_df)
AIC(model)
# 모델별 결과 통합
names(models_LM) <- dependent  # 리스트 이름을 종속변수 이름으로 설정
result_df <- do.call(rbind, lapply(models_LM, function(x) x$metrics))

# 결과 출력
cat("\n=== OLS 모델 성능 및 공간적 테스트 결과 ===\n")
print(result_df, row.names = FALSE, digits = 4)

# 모든 모델의 summary 출력
lapply(models_LM, function(x) summary(x$model))


test_indices <- as.integer(rownames(test_scaled_df))



#### SLM 모델 생성 및 평가 


#  훈련/테스트 데이터 각각 별도의 row.names 부여
train_df <- df[train_indices, ]
test_df <- df[test_indices,]
train_df
#  훈련 데이터 스케일링
train_scaled <- robust_scale(train_df[independent])
train_df[independent] <- train_scaled$scaled_data

# 테스트 데이터 스케일링
test_scaled <- robust_scale(test_df[independent])
test_df[independent] <- test_scaled$scaled_data


row.names(train_df) <- paste0("train_", seq_len(nrow(train_df)))
row.names(test_df) <- paste0("test_", seq_len(nrow(test_df)))

# 훈련/테스트용 좌표 추출
coords_train <- cbind(train_df$lon, train_df$lat)
coords_test  <- cbind(test_df$lon, test_df$lat)

# 훈련/테스트용 nb/listw 생성
nb_train    <- dnearneigh(coords_train, d1=0, d2=3, longlat=TRUE, row.names = row.names(train_df))
listw_train <- nb2listw(nb_train, style="W", zero.policy=TRUE)

nb_test     <- dnearneigh(coords_test, d1=0, d2=3, longlat=TRUE, row.names = row.names(test_df))
listw_test  <- nb2listw(nb_test, style="W", zero.policy=TRUE)

row.names(train_df) == attr(listw_train, "region.id")
row.names(test_df) == attr(listw_test, "region.id")

# SLM 모델 적합
fmla <- as.formula(paste("SummerEUI", "~", paste(independent, collapse = "+")))
slm_mod <- lagsarlm(
  fmla,
  data        = train_df,
  listw       = listw_train,
  method      = "eigen",
  zero.policy = TRUE
)

preds <- predict(slm_mod, newdata = test_df, listw = listw_test)
summary(slm_mod)

calculate_metrics <- function(preds, actuals) {
  residuals <- actuals - preds
  mse  <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(residuals))
  mape <- mean(abs(residuals / actuals)) * 100
  ss_res <- sum(residuals^2)
  ss_tot <- sum((actuals - mean(actuals))^2)
  r2 <- 1 - ss_res / ss_tot
  data.frame(
    R2   = r2,
    MSE  = mse,
    RMSE = rmse,
    MAE  = mae,
    MAPE = mape
  )
}


metrics <- calculate_metrics(preds, test_df$SummerEUI)
print(metrics)

#########################################################################
coordinates(df) <- ~lon+lat  # sp 객체로 변환
v <- variogram(WinterEUI ~ 1, data = df)

fit <- fit.variogram(v, vgm(psill=0.3, model="Exp", range=0.8, nugget=0.05))

plot(v, model=fit, main="Semivariogram of Winter", xlab="Distance (1 KM)", ylab="Semivariance")




##############################################################
# 데이터 시각화 비교 
##############################################################
install.packages("ggforce")
library(ggforce)

train_df <- df[train_indices, ]
test_df <- df[test_indices,]
train_df
# 1. 훈련 데이터 스케일링
train_scaled <- robust_scale(train_df[independent])
train_df[independent] <- train_scaled$scaled_data

# 2. 테스트 데이터 스케일링
test_scaled <- robust_scale(test_df[independent])
test_df[independent] <- test_scaled$scaled_data


calculate_metrics <- function(actuals, pred) {
  residuals <- actuals - pred
  mse  <- mean(residuals^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(residuals))
  mape <- mean(abs(residuals / actuals)) * 100
  ss_res <- sum(residuals^2)
  ss_tot <- sum((actuals - mean(actuals))^2)
  r2 <- 1 - ss_res / ss_tot
  data.frame(
    R2   = r2,
    MSE  = mse,
    RMSE = rmse,
    MAE  = mae,
    MAPE = mape
  )


# 독립변수 행렬
X_train <- as.matrix(train_df[, independent])
X_test  <- as.matrix(test_df[, independent])

# 종속변수 벡터
y_train_Annual <- train_df$AnnualEUI
y_test_Annual  <- test_df$AnnualEUI

y_train_Summer <- train_df$SummerEUI
y_test_Summer  <- test_df$SummerEUI

y_train_Winter <- train_df$WinterEUI
y_test_Winter  <- test_df$WinterEUI



# AnnualEUI 모델
gbdt_Annual <- xgboost(
  data = X_train, label = y_train_Annual,
  nrounds = 50, objective = "reg:squarederror", verbose = 0
)

# SummerEUI 모델
gbdt_Summer <- xgboost(
  data = X_train, label = y_train_Summer,
  nrounds = 50, objective = "reg:squarederror", verbose = 0
)

# WinterEUI 모델
gbdt_Winter <- xgboost(
  data = X_train, label = y_train_Winter,
  nrounds = 50, objective = "reg:squarederror", verbose = 0
)

pred_Annual <- predict(gbdt_Annual, X_test)
pred_Summer <- predict(gbdt_Summer, X_test)
pred_Winter <- predict(gbdt_Winter, X_test)


metrics_Annual <- calculate_metrics(y_test_Annual, pred_Annual)
metrics_Summer <- calculate_metrics(y_test_Summer, pred_Summer)
metrics_Winter <- calculate_metrics(y_test_Winter, pred_Winter)

# 결과 출력
cat("\n=== AnnualEUI Test Metrics ===\n"); print(metrics_Annual)
cat("\n=== SummerEUI Test Metrics ===\n"); print(metrics_Summer)
cat("\n=== WinterEUI Test Metrics ===\n"); print(metrics_Winter)






# 4. SLM 모델 적합
fmla <- as.formula(paste("WinterEUI", "~", paste(independent, collapse = "+")))
slm_mod3 <- lagsarlm(
  fmla,
  data        = train_df,
  listw       = listw_train,
  method      = "eigen",
  zero.policy = TRUE
)

slm_preds_annual <- predict(slm_mod, newdata = test_df, listw = listw_test)
slm_preds_Annual <- as.numeric(slm_preds_annual)
slm_preds_Summer <- predict(slm_mod2, newdata = test_df, listw = listw_test)
slm_preds_Summer <- as.numeric(slm_preds_Summer)
slm_preds_Winter <- predict(slm_mod3, newdata = test_df, listw = listw_test)
slm_preds_Winter <- as.numeric(slm_preds_Winter)


library(ggplot2)
library(dplyr)
library(tidyr)

# 1) test_df에 두 모델 예측값을 붙이기
inv_log1p <- function(x) expm1(x)
slm_preds_Annual_bt  <- inv_log1p(slm_preds_Annual)
slm_preds_Summer_bt  <- inv_log1p(slm_preds_Summer)
slm_preds_Winter_bt  <- inv_log1p(slm_preds_Winter)

# GBDT 예측값 역변환
pred_Annual_bt  <- inv_log1p(pred_Annual)
pred_Summer_bt  <- inv_log1p(pred_Summer)
pred_Winter_bt  <- inv_log1p(pred_Winter)

plot_df <- test_df %>%
  select(all_of(independent)) %>%
  mutate(
    SLM_Annual  = slm_preds_Annual,
    SLM_Summer  = slm_preds_Summer,
    SLM_Winter  = slm_preds_Winter,
    GBDT_Annual = pred_Annual,
    GBDT_Summer = pred_Summer,
    GBDT_Winter = pred_Winter
  )
plot_df
#    long 포맷으로 변환
#    Model: SLM vs GBDT
#    Season: Annual / Summer / Winter
#    Value: 예측값
long_df <- plot_df %>%
  pivot_longer(
    cols = starts_with(c("SLM_","GBDT_")),
    names_to = c("Model","Season"),
    names_sep = "_",
    values_to = "Pred"
  )
df_melt <- long_df %>%
  pivot_longer(
    cols      = all_of(independent),   # independent에 저장된 변수명 벡터
    names_to  = "variable",            # 새 변수명 칼럼
    values_to = "value"                # 각 관측치의 변수값
  )
df_melt
# 변수별로 facet, 모델·계절별 color 구분
ggplot(df_melt, aes(x = value, y = Pred, color = Model, linetype = Model)) +
  
  geom_smooth(se = FALSE, size = 0.8) +
  facet_grid(Season ~ variable, scales = "free_x") +
  labs(
    x     = "독립변수 값",
    y     = "예측된 EUI",
    color = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0),    # 행 패싯 레이블 회전
    panel.spacing = unit(0.5, "lines")
  )


p <- ggplot(df_melt,
            aes(x = value, y = Pred,
                color    = Model,
                linetype = Model,
                group    = Model)) +
  geom_smooth(se = FALSE, size = 0.8) +
  labs(x = "독립변수 값",
       y = "예측된 EUI",
       color = "Model",
       linetype = "Model") +
  theme_minimal() +
  theme(legend.position = "top")

# 전체 페이지 수 계산 (facets = Season ~ variable, 3×3 패널씩)
total_pages <- n_pages(
  p + facet_grid_paginate(
    Season ~ variable,
    nrow   = 3,
    ncol   = 3,
    scales = "free_x",
    space  = "free_x",
    shrink = FALSE
  )
)

p + facet_grid_paginate(
    Season ~ variable,  # 행=Season, 열=variable
    nrow   = 3,         # 한 페이지에 3행
    ncol   = 3,         # 한 페이지에 3열
    page   = 4,      # 1, 2, … 차례대로
    scales = "free_x",  # 변수별 x축 범위 자유
    #space  = "free_x",  # 패널 폭도 변수별 범위 비례
    shrink = FALSE      # 통계 shrink가 아니라 원 본 데이터 범위 사용
  )

