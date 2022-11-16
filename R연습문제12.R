# SJH
library(ggplot2)
library(gmodels)
# 1. 직업 유형에 따른 응답 정도에 차이가 있는가를 단계별로 검정하시오 (동질성 검정)
# 1) 파일 가져오기
data <- read.csv("Response.csv")

# 2) 코딩변경 - 리코딩
data$job1[data$job == 1] <- "학생"
data$job1[data$job == 2] <- "직장인"
data$job1[data$job == 3] <- "주부"
data$response1[data$response == 1] <- "무응답"
data$response1[data$response == 2] <- "낮음"
data$response1[data$response == 3] <- "높음"

# 3) 교차 분할표 작성
x <- data$job1
y <- data$response1
table(x, y)


# 4) 동질성 검정
chisq.test(x,y)

# 5) 검정결과 해석
# p-value(0.0001189)<0.05 귀무가설을 기각한다.
# 직업 유형에 따른 응답 정도에 차이가 있다.

# 2. 나이(age)와 직위(position)간의 관련성을 단계별로 분석하시오 (독립성 검정)
# 1) 파일 가져오기
data <- read.csv("cleanData.csv")

# 2) 코딩 변경(변수 리코딩)
X <- data$position #행: 직위 변수 이용
Y <- data$age3 #열: 나이 리코딩 변수 이용

# 3) 산점도를 이용한 변수간의 관련성 보기
plot(formula=Y~X, data=data)
plot(X,Y)

# 4) 독립성 검정
CrossTable(X, Y, chisq = TRUE)

# 5) 검정 결과 해석
# p-value(1.548058e-57)<0.05 귀무가설을 기각한다
# 나이(age)와 직위(position)간의 관련성이 있다

# 3. 교육수준(education)과 흡연율(smoking)간의 관련성을 분석하기 위한 연구가설을 수립하고, 
# 단계별로 가설을 검정하시오. (독립성 검정)
# 귀무가설(H0): 교육수준(education)과 흡연율(smoking)간의 관련성이 없다
# 연구가설(H1): 교육수준(education)과 흡연율(smoking)간의 관련성이 있따
# 1) 파일 가져오기
data <- read.csv("smoke.csv")
data
# 2) 코딩변경
data$eudcation1[data$education == 1] <- "대졸"
data$eudcation1[data$education == 2] <- "고종"
data$eudcation1[data$education == 3] <- "중졸"
data$smoking1[data$smoking == 1] <- "과자흡연"
data$smoking1[data$smoking == 2] <- "보통흡연"
data$smoking1[data$smoking == 3] <- "비흡연"

# 3) 교차분할표 작성
x <- data$eudcation1
y <- data$smoking1
table(x, y)


# 4) 검정 결과 해석
chisq.test(x,y)

# p-value(0.0008183)<0.05 귀무가설을 기각한다.
# 직업 유형에 따른 응답 정도에 차이가 있다
