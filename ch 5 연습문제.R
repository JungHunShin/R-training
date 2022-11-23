# SJH
# 1. iris데이터 셋을 대상으로 다음 조건에 맞게 시각화 하시오
# 1) 1번 컬럼을 x축으로 하고 3번 컬럼을 y축으로 한다.
# 2) 5번 컬럼으로 색상지정한다.
# 3) 차트 제목을 “iris 데이터 산포도”로 추가한다.
# 4) 다음 조건에 맞추어 작성한 차트를 파일에 저장한다.
# - 작업 디렉토리: “C:/Rwork/output”
# - 파일명: “iris.jpg”
# - 크기: 폭(720픽셀), 높이(480픽셀)
setwd("C:/Rwork/output")
jpeg(filename ="iris.jpg",width=720,height=480,units="px",res=500)
plot(iris[,1],iris[,3],col = iris[,5],main = "iris 데이터 산포도")

# 2. iris3 데이터 셋을 대상으로 다음 조건에 맞게 산점도를 그리시오
library(scatterplot3d)
# 1) iris3 데이터 셋의 컬럼명을 확인한다.
dimnames(iris3)

# 2) iris3 데이터 셋의 구조를 확인한다.
str(iris3)

# 3) 꽃의 종별로 산점도 그래프를 그린다.
iris_setosa = iris3[,,1]
iris_versicolor = iris3[,,2]
iris_virginica = iris3[,,3]
# 3단계: 3차원 틀(Frame)생성하기
iris3
d3 <- scatterplot3d(iris3[,1,], 
                    iris3[,2,],
                    iris3[,3,],
                    type = 'n')
# 4단계: 3차원 산점도 시각화
d3$points3d(iris_setosa[,1],
            iris_setosa[,2],
            iris_setosa[,3], 
            bg = 'orange', pch = 21)
d3$points3d(iris_versicolor[,1], 
            iris_versicolor[,2],
            iris_versicolor[,3],
            bg = 'blue', pch = 23)
d3$points3d(iris_virginica[,1],
            iris_virginica[,2],
            iris_virginica[,3], 
            bg = 'green', pch = 25)
