###  Ch07.t-검정

###  2020/05/02 keonwoo park

## 01.일표본 t-검정

## 아이스크림회사에서 판매하는 아이스크림 중 파인트의 무게는 320g
## 320g이 아니다 라는 소비자들의 불만이 있어서 320g이 맞는지를 확인하려고 한다.
## 귀무가설 H0: u = 320g 파인트의 무게는 320g이다.
## 대립가설 H1: u != 320g 파인트의 무게는 320g이 아니다.

ice<- read.csv("Ch0701.OST.csv",
               header = T,
               na.strings = '.')
str(ice)
ice <- round(ice,2) #소수점 두자리로 반올림

## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describe(ice)
# 평균 295.44, 표준편차 20.04
attach(ice)

# 03.그래프그리기(박스그래프, 히스토그램)
opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

par(mfrow=c(1,2)) #화면 분할 행, 열로 화면을 쪼갠다.
boxplot(weight)
hist(weight,
     breaks=10,
     col='red',
     xlab='무게',
     ylab='개수',
     ylim = c(0,25),
     main = '아이스크림무게에 대한 정규분포')

par(opar) #분할 복구


## 04.통계분석
# two--sided test : alternative = c("two.sided) 양측검정
# right-sided test: alternative = c("greater) 단측검정 큰값
# left- sided test: alternative = c('less') 단측검정 작은값

options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(ice,
       alternative= c("two.sided"), #양측검정
       mu = 320.0, #평균 mu 값 입력(귀무가설)검정값
       conf.level = 0.95) # 신뢰도 설정


## t 검정 검정통계량 t0 = -12.252
## 표본평균-모평균/(표준편차)
# t검정에서는 표본표준편차를 사용 s/(루트n)

# 295.44-320/( 20.04/(100)^0.5))
# -12.25
# 기각역 => 95퍼신뢰구간 하한-> 평균-1.96*표준오차


# 05.통계결과정규분포그래프

mu=320
se=2 # 표본이므로 표준편차sd 대신 표준오차 se사용
inter = qt(p=0.025, df=99) # 95퍼 신뢰구간 df=자유도 n-1
data <- rnorm(1000, mu, se)
data <- sort(data) 
plot(data,
     dnorm(data, mu, se),
     type = 'l',
     main = '아이스크림무게에 대한 정규분포',
     xlim=c(290,330))
abline(v=mu, col='green', lty=5) #수직라인 초록색평균
abline(v=mu+inter*se,col='blue',lty=5) #신뢰구간 상한
abline(v=mu-inter*se,col='blue',lty=5) #신뢰구간 하한
abline(v=295.44, col='red', lty=5) #표본의 평균

detach(ice)
