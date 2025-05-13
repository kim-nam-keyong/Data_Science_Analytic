################################################################################
#  multi‐index HC‐SDR 시뮬레이션
################################################################################
gc()
# 0. 필수 라이브러리 로드
library(MASS)       
library(FNN)        
library(Matrix)     
library(copula)     
library(parallel)   
library(doParallel) 
library(foreach)   
library(dr)        
library(MAVE)       
library(numDeriv)
library(future) 
library(doFuture)
library(progressr)
library(copula)
library(doRNG)
rm(list = ls())    
install.packages(('doFuture'))
install.packages("doRNG")
install.packages("HellCor")
library(HellCor)

################################################################################
# 1. KNN 기반 Bhattacharyya 계수 추정 / 참고 논문에 방식과 유사하지만 , 논문에서의 값의 범위가 나오질 않음.
################################################################################
estimate_B <- function(xb, y) {
  n <- length(y)
  U <- cbind(rank(xb)/(n+1), rank(y)/(n+1))
  k = floor(sqrt(length(y)))
  # k=1으로 nearest neighbor
  d1 <- FNN::knn.dist(U, k=1)[,1]
  
  
  Bn <- (2*sqrt(n-1) / n) * sum(d1)
  return(Bn)
}
################################################################################
# 1.2 Copula 기반 Bhattacharyya 계수 추정
################################################################################
estimate_B <- function(xb, y) {
  n <- length(y)
  U <- cbind(rank(xb)/(n+1), rank(y)/(n+1))
  fit <- fitCopula(normalCopula(dim=2), data = U, method="mpl")
  rho <- coef(fit)
  Bn <- (2*(1 - rho^2)^(1/4)) / sqrt(4 - rho^2)
  return(Bn)
}
################################################################################
# 1.3 Copula 모멘트 기반 역변환 추정 Bhattacharyya 계수 추정
################################################################################
estimate_B <- function(xb, y) {
  n <- length(y)
  Ux <- rank(xb)/(n+1); Uy <- rank(y)/(n+1)
  tau <- cor(Ux, Uy, method="kendall")
  rho <- sin(pi * tau / 2)        # Gaussian copula 역변환 식
  Bn  <- (2*(1 - rho^2)^(1/4)) / sqrt(4 - rho^2)
  return(Bn)
}


################################################################################
# 2. Hellinger 상관 계산 
################################################################################
HCXY <- function(beta, X, Y,
                 clip = c(1e-5, 0.99)) {
  xb <- as.vector(X %*% beta)
  if (is.null(xb) || any(!is.finite(xb))) return(-Inf)
  Bn <- estimate_B(xb, Y)
  if (!is.finite(Bn) || Bn <= 0) return(-Inf)
  
  num <-2* sqrt(Bn^4 + sqrt(pmax(4 - 3 * Bn^4, 0)) - 2)
  den <- Bn^2
  Hn  <- if (den > 0) (num / den) else -Inf
  Hn  <- max(min(Hn, clip[2]), clip[1])
  return(Hn)
}


################################################################################
# 3. 보조 함수
################################################################################
normalize <- function(v) {
  if (!is.numeric(v)) return(v)
  nrm <- sqrt(sum(v^2))
  if (!is.finite(nrm) || nrm < 1e-10) return(v)
  v / nrm
}

robust_scale <- function(x) {
  med <- median(x, na.rm = TRUE)
  s   <- mad(x, center = med, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(s) || s < 1e-10) return(rep(0, length(x)))
  r <- (x - med) / s
  r[!is.finite(r)] <- 0
  r
}

matpower <- function(M, pwr) {
  eig <- eigen(M, symmetric = TRUE)
  vals <- pmax(eig$values, 1e-10)^pwr
  V    <- eig$vectors
  V %*% diag(vals) %*% t(V)
}

proj_mat <- function(B) {
  qrB <- qr(B)
  Q   <- qr.Q(qrB, complete = FALSE)
  tcrossprod(Q)
}

# spectrum norm 
diff_measure <- function(B1,B2){
  P1 <-proj_mat(B1); P2 <- proj_mat(B2)
  M <- P1 - P2
  vals <- eigen(M, symmetric=TRUE, only.values=TRUE)$values
  max(abs(vals))
}
# frobenius norm   
#diff_measure <- function(B1, B2) {
#  P1 <- proj_mat(B1); P2 <- proj_mat(B2)
#  if (is.null(P1) || is.null(P2)) return(Inf)
#  sqrt(sum((P1 - P2)^2))
#}



################################################################################
# 4. 최적화 엔진 (전역 Adam + 국소 Nelder-Mead)
################################################################################
# θ = as.vector(W) 길이 p·d 벡터
f_multi <- function(theta, X, Y, p, d) {
  W <- matrix(theta, nrow = p, ncol = d)    # p×d 행렬로 복원
  Q <- qr.Q(qr(W))                           # 직교 기저
  # HCXY는 single-index 헬링거 상관 함수
  sum(sapply(1:d, function(k) HCXY(Q[,k], X, Y)))
}

compute_gradient_multi <- function(theta, X, Y, p, d, eps = 1e-6) {
  m    <- length(theta)         # m = p*d
  grad <- numeric(m)
  for (i in seq_len(m)) {
    e      <- numeric(m); e[i]  <- eps
    f1     <- f_multi(theta + e, X, Y, p, d)
    f2     <- f_multi(theta - e, X, Y, p, d)
    grad[i] <- if (is.finite(f1) && is.finite(f2)) (f1 - f2)/(2 * eps) else 0
  }
  grad
}



adam_optimize_multi <- function(init, X, Y, p, d,
                                max_iter = 2000, alpha = 1e-3,
                                beta1 = 0.9, beta2 = 0.999,
                                eps = 1e-8, tol = 1e-7) {
  theta      <- init
  m          <- numeric(length(theta))
  v          <- m
  best_theta <- theta
  best_val   <- f_multi(theta, X, Y, p, d)
  prev_val   <- best_val
  
  for (t in 1:max_iter) {
    g     <- compute_gradient_multi(theta, X, Y, p, d, eps=1e-6)
    m     <- beta1 * m + (1 - beta1) * g
    v     <- beta2 * v + (1 - beta2) * (g^2)
    m_hat <- m / (1 - beta1^t)
    v_hat <- v / (1 - beta2^t)
    step  <- alpha * m_hat / (sqrt(v_hat) + eps)
    theta <- normalize(theta + step)
    val   <- f_multi(theta, X, Y, p, d)
    if (is.finite(val) && val > best_val + tol) {
      best_val   <- val
      best_theta <- theta
    }
    if (!is.finite(val) || abs(val - prev_val) < tol) break
    prev_val <- val
  }
  list(par = best_theta, value = best_val)
}



global_opt_multi <- function(starts, X, Y, p, d,
                             max_iter, alpha, beta1, beta2, tol) {
  best_val <- -Inf; best_par <- NULL
  for (init in starts) {
    res <- adam_optimize_multi(init, X, Y, p, d,
                               max_iter, alpha, beta1, beta2, tol=tol)
    if (is.finite(res$value) && res$value > best_val) {
      best_val <- res$value
      best_par <- res$par
    }
  }
  list(par = best_par, value = best_val)
}



local_opt_multi <- function(start, X, Y, p, d, max_iter, tol) {
  nm <- 
    optim(par=start,
          fn=function(th) f_multi(th, X, Y, p, d),
          method="Nelder-Mead",
          control=list(fnscale=-1, maxit=max_iter, reltol=tol))
  
  if (!is.null(nm) && nm$convergence==0) {
    th <- normalize(nm$par)
    return(list(par=th, value=f_multi(th, X, Y, p, d)))
  }
  NULL
}



################################################################################
# 5. multi_hcsdr: multi-index HC-SDR 최적화
################################################################################
multi_hcsdr <- function(X, Y, d=2, n_rand=100,
                        max_iter_g=800, max_iter_l=700,
                        alpha=0.005, beta1=0.9, beta2=0.999,
                        tol_g=1e-4, tol_l=1e-6,
                        init_dirs=NULL) {
  n <- nrow(X); p <- ncol(X)
  # 1) 시작점 목록: init_dirs cols + 랜덤
  starts <- list()
  if (!is.null(init_dirs) && ncol(init_dirs)>=d) {
    starts[[1]] <- as.vector(init_dirs[,1:d])
  }
  for (i in seq_len(n_rand)) {
    starts[[length(starts)+1]] <- as.vector(matrix(rnorm(p*d), p, d))
  }
  # 2) 전역 탐색
  glob <- global_opt_multi(starts, X, Y, p, d,
                           max_iter_g, alpha, beta1, beta2, tol_g)
  # 3) 국소 탐색
  loc  <- local_opt_multi(glob$par, X, Y, p, d,
                          max_iter_l, tol_l)
  best <- if (!is.null(loc) && loc$value > glob$value) loc else glob
  
  # 4) 결과 조립
  W_est <- matrix(best$par, nrow=p, ncol=d)
  Q     <- qr.Q(qr(W_est))
  Sig_sqrt <- matpower(cov(apply(X,2,robust_scale)),0.5)
  B_orig   <- Sig_sqrt %*% Q
  B_norm   <- apply(B_orig,2,normalize)
  list(B_original = B_norm, W_est = Q)
}


################################################################################
# 6. HCSI: 시뮬레이션 단위 함수
################################################################################
HCSI <- function(n, model,hp) {
  p    <- 10
  SDRs <- c("SIR","SAVE","MAVE")
  out  <- setNames(numeric(length(SDRs)*2),
                   c(t(outer(SDRs, c("","-multiHC"), paste0))))
  # 데이터 생성 및 기준준방향벡터 설정
  X <- matrix(rnorm(n*p), n, p)
  beta_setup <- switch(model,
                       {
                         b1 <- normalize(c(1,0,0,0,0,0,0,0,0,0))
                         b2 <- normalize(c(0,1,0,0,0,0,0,0,0,0))
                         list(b1, b2)
                       },
                       {
                         b1 <- normalize(c(1,-1,0,0,0, rep(0,p-5)))
                         b2 <- normalize(c(rep(0,p-2) , 1, -1))
                         list(b1, b2)
                       },
                       stop("Invalid model")
  )
  A_true <- apply(do.call(cbind, beta_setup), 2, normalize)
  Z1 <- X %*% A_true[,1]; Z2 <- X %*% A_true[,2]
  Z1 <- normalize(Z1)
  Z2 <- normalize(Z2)
  y  <- switch(model,
               Z1 + Z2 + 0.05*rnorm(n),
               Z1^2 + Z2^2 + 0.05*rnorm(n),
               stop("Invalid model"))
  d <- 2
  # SIR, SAVE, MAVE 비교
  for (m in SDRs) {
    dat <- data.frame(y = y, X)
    drout <- switch(m,
                    "SIR"  = dr::dr(y~., data=dat, method="sir")$evec[,1:d],
                    "SAVE" = dr::dr(y~., data=dat, method="save")$evec[,1:d],
                    "MAVE" = {
                      mr <- MAVE::mave(y~., data=dat, max.dim=d)
                      if (is.null(mr)) dr::dr(y~., data=dat, method="sir")$evec[,1:d]
                      else do.call(cbind, mr$dir[1:d])
                    },
                    stop("Unknown SDR"))
    drout <- apply(drout, 2, normalize)
    
    multi<-multi_hcsdr(X,y,d,
                       n_rand     = hp$n_rand,
                       max_iter_g = hp$max_iter_g,
                       max_iter_l = hp$max_iter_l,
                       alpha      = hp$alpha,
                       beta1      = hp$beta1,
                       beta2      = hp$beta2,
                       tol_g      = hp$tol_g,
                       tol_l      = hp$tol_l,
                       init_dirs  = drout)
    out[m]            <- diff_measure(A_true, drout)
    out[paste0(m,"-multiHC")]<-diff_measure(A_true,multi$B_original)
  }
  out
}
################################################################################
# 7.하이퍼파라미터터 시뮬레이션 및 결과 처리
################################################################################
tune_hc_sdr <- function(n = 200) {
  #–– 라이브러리 로드 및 백엔드 설정
  library(doFuture); library(future); library(progressr)
  library(foreach); library(dr); library(MAVE); library(FNN); library(dplyr)
  
  registerDoFuture()
  plan(multisession, workers = max(1, availableCores() - 4))
  
  handlers(global = TRUE)
  handlers("progress")   # CLI 진행바 활성화
  
  #–– 1) 하이퍼파라미터 그리드
  param_grid <- expand.grid(
    n_rand     = c(100,200),
    max_iter_g = c(1000,2000),
    max_iter_l = c(500,1000),
    alpha      = c(0.001,0.005),
    beta1      = c(0.8,0.9),
    beta2      = c(0.99,0.999),
    tol_g      = c(1e-5,1e-6),
    tol_l      = c(1e-7,1e-8),
    stringsAsFactors = FALSE
  )
  
  # 진행자 생성
  p <- progressor(along = seq_len(nrow(param_grid)))
  
  #–– 2) 평가 함수 정의
  evaluate_hp <- function(hp) {
    SDRs <- c("SIR","SAVE","MAVE")
    r1 <- HCSI(n, model = 1, hp)
    r2 <- HCSI(n, model = 2, hp)
    d1 <- r1[paste0(SDRs,"-multiHC")] - r1[SDRs]
    d2 <- r2[paste0(SDRs,"-multiHC")] - r2[SDRs]
    data.frame(
      delta1_SIR  = d1["SIR"],  delta2_SIR  = d2["SIR"],
      delta1_SAVE = d1["SAVE"], delta2_SAVE = d2["SAVE"],
      delta1_MAVE = d1["MAVE"], delta2_MAVE = d2["MAVE"],
      improve_SIR  = (d1["SIR"] < 0) & (d2["SIR"] < 0),
      improve_SAVE = (d1["SAVE"]< 0) & (d2["SAVE"]< 0),
      improve_MAVE = (d1["MAVE"]< 0) & (d2["MAVE"]< 0),
      stringsAsFactors = FALSE
    )
  }
  
  #–– 3) 병렬 그리드 서치 (진행바 표시)
  all_res <- foreach(i = seq_len(nrow(param_grid)),
                     .combine = function(...) cbind(param_grid[i,], rbind(...)),
                     .packages = c("dr","MAVE","FNN")) %dorng% {
                       p()  # 진행 업데이트
                       evaluate_hp(as.list(param_grid[i, ]))
                     }
  
  #–– 4) 방법별 최적 하이퍼파라미터 추출
  best_list <- list()
  for (m in c("SIR","SAVE","MAVE")) {
    best_list[[m]] <- all_res %>%
      filter(!!sym(paste0("improve_",m))) %>%
      mutate(total_delta = abs(!!sym(paste0("delta1_",m))) +
               abs(!!sym(paste0("delta2_",m)))) %>%
      arrange(total_delta) %>%
      slice(1)
  }
  
  invisible(list(all = all_res, best = best_list))
}



res <- tune_hc_sdr(n=200)
# SIR만 개선된 최적 하이퍼파라미터
res$best$SIR  
# SAVE만 개선된 최적
res$best$SAVE
# MAVE만 개선된 최적
res$best$MAVE


## n = 100,200  두 경우에서 최적의 파라미터 조합 찾기. 
ns <- c(100, 200)
best_list <- lapply(ns, function(n_obs) {
  cat(">> Tuning for n =", n_obs, "\n")
  best_hp <- tune_hc_sdr(n = n_obs)
  if (is.null(best_hp)) return(NULL)
  cbind(n = n_obs, best_hp)
})
best_df <- do.call(rbind, best_list)

################################################################################
# 8. 병렬 시뮬레이션 및 결과 처리
################################################################################
run_simulations <- function(n_sim = 500,
                            hp = list(
                              n_rand     = 300,
                              max_iter_g = 2000,
                              max_iter_l = 2000,
                              alpha      = 0.005,
                              beta1      = 0.9,
                              beta2      = 0.999,
                              tol_g      = 1e-5,
                              tol_l      = 1e-6
                            )) {
  ## 0) 백엔드 & 핸들러 설정
  
  registerDoFuture()
  plan(multisession, workers = max(1, availableCores() - 3))
  
  
  handlers("txtprogressbar")   # 퍼센트, ETA 표시
  handlers(global = TRUE)
  
  
  # HCSI, multi_hcsdr 등은 전역에 정의되어 있어야 합니다
  
  results <- list()
  keys <- expand.grid(n = c(100,200), m = 1:2)
  
  # 전체 단계 수 = sum(keys)*(n_sim)
  total_steps <- nrow(keys) * n_sim
  
  with_progress({
    p_global <- progressor(steps = total_steps)
    
    for (row in seq_len(nrow(keys))) {
      n <- keys$n[row]
      m <- keys$m[row]
      key <- sprintf("SI.%d.%d", n, m)
      message("Running ", key)
      
      ## 1) 각 key별 병렬 foreach
      results[[key]] <- foreach(i = seq_len(n_sim),
                                .combine = rbind,
                                .packages = c("dr","MAVE","FNN")) %dorng% {
                                  p_global()                    # 1 step 진행 신호
                                  HCSI(n, m, hp)               # HCSI에 최적화된 hp 전달
                                }
    }
  })
  
  return(results)
}

process_results <- function(results) {
  summary_stats <- list()
  
  for (name in names(results)) {
    
    means <- colMeans(results[[name]])
    sds <- apply(results[[name]], 2, sd)
    
    # Format and print results
    result_table <- data.frame(
      Method = names(means),
      Mean = means,
      SD = sds
    )
    
    #print(result_table)
    summary_stats[[name]] <- result_table
  }
  
  return(summary_stats)
}
# 실행
optimal_hp <- list(
  n_rand     = 200,
  max_iter_g = 2000,
  max_iter_l = 2000,
  alpha      = 0.001,
  beta1      = 0.8,
  beta2      = 0.99,
  tol_g      = 1e-6,
  tol_l      = 1e-7
)
set.seed(123)
sim_results  <- run_simulations(n_sim = 10, optimal_hp) # 시뮬레이션 횟수 설정. 
process_results(sim_results)



