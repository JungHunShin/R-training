# SJH

# [문항1] MASS 패키지에 있는 Animals 데이터 셋에 대해 R의 기본 함수를 이용하여 body컬럼을 대상으로 다음의 기술통계량을 구하시오. 코드와 답을 기재하세요.
install.packages('MASS')
library(MASS)
# (1) Animals 데이터 셋 구조 보기
str(Animals)

# (2) 요약통계량
summary(Animals)

# (3) 평균
mean(Animals$body)
mean(Animals$brain)

# (4) 표준편차
sd(Animals$body)
sd(Animals$brain)

# (5) Animals 데이터 셋의 빈도수 구하기
table(Animals)

# [문항2]  2. iris데이터를 이용하여 5겹 3회 반복하는 교차검정 데이터를 샘플링 하시오.
install.packages('cvTools')
library(cvTools)
cvFolds(nrow(iris), K=5, R=3)


# [문항3]  3. descriptive.csv 파일내 데이터를 data변수에 담고 다음의 기술통계량을 구하시오.
data <- read.csv('descriptive.csv')

# 1) Cost 컬럼 내 데이터에서 1이상 11이하의 데이터만 추출하여 평균을 구하시오.
data <- subset(data, data$cost >= 1 & data$cost <= 11)
data

# 2) 1)번에서 추출한 데이터를 내림차순으로 정렬한 후 첫 10줄의 데이터를 보이시오
head(data[order(data$cost,decreasing = T),], n=10)


# 3) 1)번에서 추출한 데이터에서 3사분위수를 구하시오.
quantile(data$cost,3/4)

# 4) Cost2 컬럼을 생성하여 1이상 4미만의 데이터는 1로, 4이상 8미만의 데이터는 2로 8이상의 데이터는 3으로 범주화하여 저장하고 첫 10줄의 데이터를 보이시오.
data$cost2[data$cost >=1 & data$cost < 4] <- 1
data$cost2[data$cost >=4 & data$cost < 8] <- 2
data$cost2[data$cost >=8 ] <- 3
head(data,n=10)

# 5) 4)번에서 생성한 Cost2 컬럼내 데이터를 이용하여 파이 그래프로 시각화 하시오.
pie(table(data$cost2))

# [문항4]  4. twomethod2.csv 내 데이터 셋을 이용하여 교육방법(method)에 따른 시험성적(score)에 차이가 있는지 검정하시오.
data <- read.csv('twomethod2.csv')
data
# 1) 결측치를 제거하여 데이터 전처리 하시오
result <- subset(data,!is.na(score),c(method,score))

# 2) 가설 설정
# 귀무가설 : 교육방법(method)에 따른 시험성적(score)에 차이가 없다
# 대립가설 : 교육방법(method)에 따른 시험성적(score)에 차이가 있다

# 3) 적합한 검정 방법 선택
a <- subset(result, method == 1)
b <- subset(result, method == 2)
a1 <- a$score
b1 <- b$score
var.test(a1,b1)
# p-value(0.9951) > 0.05 두집단 간 분포의 모양이 동질


# 4) 가설 검정
t.test(a1, b1, altr = "two.sided", 
       conf.int = TRUE, conf.level = 0.95)
# p-value(6.57e-07) < 0.05 귀무가설을 기각하고 대립가설을 채택

t.test(a1, b1, alter = "greater", 
       conf.int = TRUE, conf.level = 0.95)
# p-value(1) > 0.05 오른쪽 검정으로 귀무가설 채택하고 대립가설을 기각

t.test(a1, b1, alter = "less", 
       conf.int = TRUE, conf.level = 0.95)
# p-value(3.285e-07) < 0.05 왼쪽 검정으로 귀무가설을 기각

# 5) 검정 결과
# 교육방법(method)에 따른 시험성적(score)에 차이가 있다.