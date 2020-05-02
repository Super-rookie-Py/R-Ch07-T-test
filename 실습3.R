###  Ch07.t-검정

###  2020/05/02 keonwoo park

## 실습3

# K제약회사에서는 새로운 진통제를 개발하였다.
# 새로운 진통제의 지속효과가 5시간 이상이라고 말할 수 있는가

painkiller <-read.csv('Ch0704.painkiller.csv',
                  header = TRUE,
                  na.strings = '.')

painkiller

library(psych)
describe(painkiller)

opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

par(mfrow=c(1,2)) #화면 분할 행, 열로 화면을 쪼갠다.
boxplot(painkiller$time)
hist(painkiller$time,
     breaks=10,
     col='red',
     xlab='점수',
     ylab='개수',
     main = '만족도점수')

par(opar) #분할 복구

options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(painkiller$time,
       alternative= c("greater"), #양측검정
       mu = 5, #평균 mu 값 입력(귀무가설)검정값
       conf.level = 0.95) # 신뢰도 설정

# p-value 0.9898 -> 귀무가설이 참이다.

mu=5
se=0.0411 # 표본이므로 표준편차sd 대신 표준오차 se사용
inter = qt(p=0.05, df=39, lower.tail = F) # 95퍼 신뢰구간 df=자유도 n-1
data <- rnorm(1000, mu, se)
data <- sort(data) 
plot(data,
     dnorm(data, mu, se),
     type = 'l',
     main = '5시간보다 지속되는가',
     xlim=c(4.95,5.1))
abline(v=mu, col='green', lty=5) #수직라인 초록색평균
abline(v=mu+inter*se,col='blue',lty=5) #신뢰구간 상한
abline(v=mu-inter*se,col='blue',lty=5) #신뢰구간 하한
abline(v=5.0675, col='red', lty=5) #표본의 평균
