rm(list=ls())
install.packages("tidyr")
install.packages("purrr")# tidyr 패키지 설치
library(purrr)
library(tidyr)             # tidyr 패키지 로드
library(intergraph)
library(parallel)
library(network)
library(ergm)
library(sna)
library(readr)
library(igraph)
library(RColorBrewer)
library(intergraph)
library(dplyr)
library(sna)
library(intergraph)

target <- read_csv("C:/Users/pbcho/Documents/Rdata/network/네트워크 결과/target1_공동출원속성.csv")
cor <- read_csv("C:/Users/pbcho/Documents/Rdata/network/네트워크 결과/cos_df_공동출원유사도행렬.csv")

library(tidyverse)
library(igraph)
library(tidyverse)
library(igraph)
library(igraph)
library(igraph)
library(dplyr)

#공동출원인인
############################################################################
threshold <- 0
adj_matrix <- as.matrix(cor)>threshold
adj_matrix
net15 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", diag = FALSE)
V(net15)$출원인명 <- target$출원인명
V(net15)$출원인그룹 <- target$`출원인그룹`
V(net15)$유형구분2 <- target$유형구분2
V(net15)$유형구분3 <- target$유형구분3
V(net15)$출원인국적 <- target$출원인국적
V(net15)$인용문헌수 <- target$인용문헌수


#### 중심성 분석석
centrality_analysis <- function(net, net_name) {
  degree_centrality <- degree(net)
  closeness_centrality <- closeness(net)
  betweenness_centrality <- betweenness(net)
  eigenvector_centrality <- evcent(net)$vector  # 위세 중심성 계산
  
  cat(paste(net_name, "Network Centrality:\n"))
  
  cat("Degree Centrality:\n")
  print(summary(degree_centrality))
  
  cat("Closeness Centrality:\n")
  print(summary(closeness_centrality))
  
  cat("Betweenness Centrality:\n")
  print(summary(betweenness_centrality))
  
  cat("Eigenvector Centrality:\n")
  print(summary(eigenvector_centrality))  # 위세 중심성 출력
  
  return(list(
    degree = degree_centrality,
    closeness = closeness_centrality,
    betweenness = betweenness_centrality,
    eigenvector = eigenvector_centrality  # 위세 중심성 반환
  ))
}
print_top_10_centralities <- function(centrality, title) {
  top_10 <- sort(centrality, decreasing = TRUE)[1:6]
  cat(title, "Top 5 Nodes:\n")
  print(top_10)
}
centrality_analysis(net15,'easy')
test <- centrality_analysis(net15, 'net15')
print_top_10_centralities(test$degree, "Degree ")
print_top_10_centralities(test$closeness, "closeness Centrality")
print_top_10_centralities(test$betweenness, "betweenness Centrality")
print_top_10_centralities(test$eigenvector, "eigenvector centrality")

#### 커뮤니티 탐지 및 출원인명 네트워크집단 출력 함수
print_community_companies <- function(net, net_name) {
  community_detection <- cluster_fast_greedy(net)
  membership <- membership(community_detection)
  company_communities <- data.frame(Company = V(net)$출원인명, Community = membership)
  
  cat(paste(net_name, "Network Communities:\n"))
  for (i in 1:max(membership)) {
    cat(paste("Community", i, ":\n"))
    print(company_communities$Company[company_communities$Community == i])
    cat("\n")
  }
}
print_community_companies(net15, "공동출원인")


empty_net <- structural_holes_analysis(net15, "공동출원인")
empty_net
#######시각화
plot_network <- function(net, net_name, top_10_names) {
  # 고유한 출원인 그룹별 색상 매핑
  unique_groups <- unique(V(net)$출원인그룹)  # 고유한 출원인 그룹 추출
  group_colors <- setNames(rainbow(length(unique_groups)), unique_groups)  # 그룹별 색상 매핑
  
  # 중복된 노드 및 엣지 제거
  net <- simplify(net)  # 중복된 엣지와 루프 제거
  
  # 노드 색상 설정: 출원인 그룹별 색상, Top 10 노드는 색상 강조
  #vertex_color <- ifelse(V(net)$출원인명 %in% top_10_names, "red", group_colors[V(net)$출원인그룹])
  vertex_color <- group_colors[V(net)$출원인그룹]
  
  # 노드 크기 설정: Top 10 노드는 크기를 키움
  vertex_size <- ifelse(V(net)$출원인명 %in% top_10_names, 10, 5)
  
  # 레이블 설정: 모든 노드의 이름 표시
  vertex_label <- V(net)$출원인명
  vertex_label_cex <- ifelse(V(net)$출원인명 %in% top_10_names, 1.7, 1.0)  # Top 10은 크기 1.5, 나머지는 1.0
  vertex_label_font <- ifelse(V(net)$출원인명 %in% top_10_names, 2, 1)
  
  # 레이아웃 설정 (간격 조정)
  layout <- layout_with_fr(net)  # 기본 프루흐터만 레이아웃
  layout <- layout.norm(layout, xmin = -2, xmax = 2, ymin = -2, ymax = 2)  # 간격 조정
  
  # 네트워크 시각화
  plot(net, layout = layout, 
       vertex.size = vertex_size, 
       vertex.label = vertex_label,  # 모든 노드의 이름 표시
       vertex.label.cex = vertex_label_cex,
       vertex_label_font = vertex_label_font,# top 10 노드의 텍스트를 굵게 
       vertex.color = vertex_color,# 그룹별 색상 적용
       vertex.label.dist = -1.2,
       edge.color = "gray", 
       edge.width = 1.5, 
       main = paste(net_name, "네트워크"))
  
  # 범례 추가 (출원인 그룹별 색상)
  legend("bottomleft", legend = names(group_colors), col = group_colors, pch = 18, 
         title = "출원인 그룹", cex = 0.6)
}

top_10_indices <- order(degree(net15), decreasing = TRUE)[1:5]
top_10_name <- V(net15)$출원인명[top_10_indices]
top_10_name
plot_network(net15, "공동출원인", top_10_name)

# 융합특허 네트워크분석
########################################################
cor <- read_csv("C:/Users/pbcho/Documents/Rdata/network/네트워크 결과/matrix_21_1공동출현_비국방.csv")
nodes <- colnames(cor)
threshold <- 0
adj_matrix <- as.matrix(cor)>threshold 

net_18 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
V(net_18)$코드명 <- colnames(cor)

#######시각화
plot_network <- function(net, net_name, top_10_names) {
  # 고유한 출원인 그룹별 색상 매핑 (모든 노드 동일 색상으로 설정)
  vertex_color <- "skyblue"  # 모든 노드를 동일한 색상으로 설정
  low_degree_nodes <- V(net)[degree(net) < 10]  # Degree가 10 미만인 노드
  net <- delete_vertices(net, low_degree_nodes)
  # 중복된 노드 및 엣지 제거
  net <- simplify(net)  # 중복된 엣지와 루프 제거
  
  # Degree 계산 및 정규화
  degree_values <- degree(net)  # 각 노드의 Degree 계산
  min_size <- 5  # 노드 크기의 최소값
  max_size <- 15  # 노드 크기의 최대값
  vertex_size <- ((degree_values - min(degree_values)) / (max(degree_values) - min(degree_values))) * (max_size - min_size) + min_size
  
  # 레이블 설정: 모든 노드의 이름 표시
  vertex_label <- V(net)$코드명
  vertex_label_cex <- ifelse(V(net)$코드명 %in% top_10_names, 1.5, 0.9)  # Top 10은 크기 1.7, 나머지는 0.9
  vertex_label_font <- ifelse(V(net)$코드명 %in% top_10_names, 2, 1)  # Top 10 노드는 굵게 표시
  
  # 레이아웃 설정 (간격 조정)
  layout <- layout_with_kk(net)  # Kamada-Kawai 레이아웃
  layout <- layout.norm(layout, xmin = -5, xmax = 5, ymin = -5, ymax = 5)  # 간격 조정
  
  # 네트워크 시각화
  plot(net, layout = layout, 
       vertex.size = vertex_size,  # Degree 기반 정규화된 노드 크기
       vertex.label = vertex_label,  # 모든 노드의 이름 표시
       vertex.label.cex = vertex_label_cex,
       vertex.label.font = vertex_label_font,  # Top 10 노드의 텍스트를 굵게
       vertex.label.dist = -1.0,
       vertex.label.degree = -pi/2,
       vertex.color = vertex_color,  # 모든 노드 동일 색상
       edge.color = "gray", 
       edge.width = 1.5, 
       main = paste(net_name, "네트워크"))
  
  # 범례 제거 (모든 노드가 동일한 색상이므로 범례 불필요)
}

top_10_indices <- order(degree(net_18), decreasing = TRUE)[1:10]
top_10_name <- V(net_18)$코드명[top_10_indices]
top_10_name
plot_network(net_18, "3구간 공동출현 국방", top_10_name)

#### 중심성 분석석
centrality_analysis <- function(net, net_name) {
  degree_centrality <- degree(net)
  closeness_centrality <- closeness(net)
  betweenness_centrality <- betweenness(net)
  eigenvector_centrality <- evcent(net)$vector  # 위세 중심성 계산
  
  cat(paste(net_name, "Network Centrality:\n"))
  
  cat("Degree Centrality:\n")
  print(summary(degree_centrality))
  
  cat("Closeness Centrality:\n")
  print(summary(closeness_centrality))
  
  cat("Betweenness Centrality:\n")
  print(summary(betweenness_centrality))
  
  cat("Eigenvector Centrality:\n")
  print(summary(eigenvector_centrality))  # 위세 중심성 출력
  
  return(list(
    degree = degree_centrality,
    closeness = closeness_centrality,
    betweenness = betweenness_centrality,
    eigenvector = eigenvector_centrality  # 위세 중심성 반환
  ))
}
print_top_10_centralities <- function(centrality, title) {
  top_10 <- sort(centrality, decreasing = TRUE)[1:10]
  cat(title, "Top 10 Nodes:\n")
  print(top_10)
}
centrality_analysis(net_18,'easy')
test <- centrality_analysis(net_18, 'net18')
# Degree 중심성 상위 10개 출력
print_top_10_centralities(test$degree, "Degree")
# Closeness 중심성 상위 10개 출력
print_top_10_centralities(test$closeness, "Closeness" )
# Betweenness 중심성 상위 10개 출력
print_top_10_centralities(test$betweenness, "Betweenness" )
# Eigenvector 중심성 상위 10개 출력
print_top_10_centralities(test$eigenvector, "Eigenvector" )
# 중심성 값과 노드 이름 매핑
sorted_indices <- order(test$degree, decreasing = TRUE)[1:10]
top_10_name <- V(net_18)$출원인명[sorted_indices]
top_10_name
# 결과 출력: 노드 이름과 값 함께 보기
data.frame(Node = top_10_names, Degree_Centrality = top_10_values)

#### 커뮤니티 탐지 및 출원인명 네트워크집단 출력 함수
print_community_companies <- function(net, net_name) {
  community_detection <- cluster_fast_greedy(net)
  membership <- membership(community_detection)
  company_communities <- data.frame(Company = V(net)$코드명, Community = membership)
  
  cat(paste(net_name, "Network Communities:\n"))
  for (i in 1:max(membership)) {
    cat(paste("Community", i, ":\n"))
    print(company_communities$Company[company_communities$Community == i])
    cat("\n")
  }
}
print_community_companies(net_18, "융합특허")






########################
# 융합, 인용 출원인 네트워크 

cor <- read_csv("C:/Users/pbcho/Documents/Rdata/network/네트워크 결과/matrix_2_21_인용융합.csv")
target <- read_csv("C:/Users/pbcho/Documents/Rdata/network/네트워크 결과/target21_1_인용융합.csv")
nodes <- colnames(cor)

threshold <- 1
adj_matrix <- as.matrix(cor)>threshold

net_18 <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
V(net_18)$출원인명 <- target$출원인명
V(net_18)$출원인그룹 <- target$`출원인구분`
V(net_18)$유형구분2 <- target$유형구분2
V(net_18)$유형구분3 <- target$유형구분3
#V(net_18)$출원인국적 <- target$출원인국적

library(RColorBrewer)
#######시각화
plot_network <- function(net, net_name, top_10_names, all_groups) {
  # 고유한 출원인 그룹별 색상 매핑 (일관된 색상 매핑 생성)
  all_groups <- c('기타','민수기업','방산기업','국외','기타정부기관','대학','정부출연연구기관(국방)')
  group_colors <- setNames(brewer.pal(n = length(all_groups), name = "Set3"), all_groups)
  
  # 현재 네트워크의 출원인 그룹
  current_groups <- unique(V(net)$출원인그룹)
  
  # Degree가 2 미만인 노드 제거
  low_degree_nodes <- V(net)[degree(net) < 3]
  net <- delete_vertices(net, low_degree_nodes)
  
  # 중복된 노드 및 엣지 제거
  net <- simplify(net)  # 중복된 엣지와 루프 제거
  
  # Degree 계산 및 정규화
  degree_values <- degree(net)  # 각 노드의 Degree 계산
  min_size <- 5  # 노드 크기의 최소값
  max_size <- 20  # 노드 크기의 최대값
  vertex_size <- ((degree_values - min(degree_values)) / (max(degree_values) - min(degree_values))) * (max_size - min_size) + min_size
  
  # 레이블 설정: 모든 노드의 이름 표시
  vertex_label <- V(net)$출원인명
  vertex_label_cex <- ifelse(V(net)$출원인명 %in% top_10_names, 1, 0.55)  # Top 10은 크기 0.7, 나머지는 0.5
  vertex_label_font <- ifelse(V(net)$출원인명 %in% top_10_names, 1.5, 1)  # Top 10 노드는 굵게 표시
  
  # 레이아웃 설정 (간격 조정)
  layout <- layout_with_fr(net, niter = 1000, area = vcount(net)^2)
  layout <- layout.norm(layout, xmin = -10, xmax = 10, ymin = -10, ymax = 10)
  
  # 엣지 가중치 반영: 엣지 두께 설정
  edge_weights <- E(net)$weight  # 엣지의 가중치 값
  edge_width <- edge_weights / max(edge_weights) * 1.5  # 가중치를 1~5 범위로 정규화
  
  # 네트워크 시각화
  plot(net, layout = layout, 
       vertex.size = vertex_size,  # Degree 기반 정규화된 노드 크기
       vertex.label = vertex_label,  # 모든 노드의 이름 표시
       vertex.label.cex = vertex_label_cex,
       vertex.label.font = vertex_label_font,  # Top 10 노드의 텍스트를 굵게
       vertex.label.dist = -1.2,
       vertex.color = group_colors[V(net)$출원인그룹],  # 일관된 색상 매핑 사용
       vertex.label.degree = -pi/2,
       edge.color = "gray", 
       edge.width = edge_width, 
       main = paste(net_name, "네트워크"))
  
  # 범례 추가
  legend('topleft',
         legend = all_groups,  # 전체 그룹을 범례에 표시
         col = group_colors[all_groups],  # 일관된 색상 매핑 사용
         pch = 19,
         pt.cex = 1.2,
         title = "출원인그룹",
         cex = 0.9,
         bty = "n",
         inset = c(0.08,0.08)
  )
}

top_10_indices <- order(degree(net_18), decreasing = TRUE)[1:10]
top_10_name <- V(net_18)$출원인명[top_10_indices]
top_10_name
plot_network(net_18, "3구간 융합－인용", top_10_name, all_groups)

#### 중심성 분석석
centrality_analysis <- function(net, net_name) {
  # 중앙성 값 계산
  degree_centrality <- degree(net)
  closeness_centrality <- closeness(net)
  betweenness_centrality <- betweenness(net)
  eigenvector_centrality <- evcent(net)$vector  # 위세 중심성 계산
  
  # vertex attribute "출원인명"을 이름으로 부여
  names(degree_centrality) <- V(net)$출원인명
  names(closeness_centrality) <- V(net)$출원인명
  names(betweenness_centrality) <- V(net)$출원인명
  names(eigenvector_centrality) <- V(net)$출원인명
  
  cat(paste(net_name, "Network Centrality:\n"))
  
  cat("Degree Centrality:\n")
  print(summary(degree_centrality))
  
  cat("Closeness Centrality:\n")
  print(summary(closeness_centrality))
  
  cat("Betweenness Centrality:\n")
  print(summary(betweenness_centrality))
  
  cat("Eigenvector Centrality:\n")
  print(summary(eigenvector_centrality))
  
  return(list(
    degree = degree_centrality,
    closeness = closeness_centrality,
    betweenness = betweenness_centrality,
    eigenvector = eigenvector_centrality
  ))
}

# top 10 중앙성 값을 출력하는 함수. 이미 이름이 매겨진 벡터가 전달됩니다.
print_top_10_centralities <- function(centrality, title) {
  top_10 <- sort(centrality, decreasing = TRUE)[1:10]
  cat(title, "Top 10 Nodes:\n")
  print(top_10)
}

# 예시: 네트워크 분석 및 출력
centrality_results <- centrality_analysis(net_18, "Example Network")
print_top_10_centralities(centrality_results$degree, "Degree")
print_top_10_centralities(centrality_results$closeness, "Closeness" )
# Betweenness 중심성 상위 10개 출력
print_top_10_centralities(centrality_results$betweenness, "Betweenness" )
# Eigenvector 중심성 상위 10개 출력
print_top_10_centralities(centrality_results$eigenvector, "Eigenvector" )




