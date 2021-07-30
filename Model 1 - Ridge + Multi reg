library(dplyr)

# 원본 데이터
Selling_origin = read.csv(file = 'SELNG.csv', sep = '|')
View(Selling)

# 필요한 열 전체 추출
Selling = Selling_origin %>% dplyr::select(c(1:6, 19:56))

# 매출액/매출 건수를 가맹점 수로 나누어 하나의 행을 하나의 업소로 취급
for (i in c(7:44)){
  Selling[, i] = Selling[, i] / Selling$MCT_CNT
}

# 매출액이 MAX가 되는 나이, 요일, 시간 테이블 추출
Sales_amt_age = Selling %>% dplyr::select(AMT_10, AMT_20, AMT_30, AMT_40, AMT_50, AMT_60)
Sales_amt_days = Selling %>% dplyr::select(AMT_SUN, AMT_MON, AMT_TUE, AMT_WED, AMT_THU, AMT_FRI, AMT_SAT)
Sales_amt_times = Selling %>% dplyr::select(AMT_T06, AMT_T11, AMT_T14, AMT_T17, AMT_T21, AMT_T24)
Sales_cnt_age = Selling %>% dplyr::select(CNT_10, CNT_20, CNT_30, CNT_40, CNT_50, CNT_60)
Sales_cnt_age = Selling %>% dplyr::select(CNT_SUM, CNT_MON, CNT_TUE, CNT_WED, CNT_THU, CNT_FRI, CNT_SAT)
Sales_cnt_age = Selling %>% dplyr::select(CNT_T06, CNT_T11, CNT_T14, CNT_T17, CNT_T21, CNT_T24)

# 나이, 요일, 시간 별로 MAX(index값) 테이블 생성
## AGE
which_max_age_amt = as.list(0)

for (i in 1:nrow(Sales_amt_age)) {
  row_list = c(Sales_amt_age[i, 1], Sales_amt_age[i, 2], Sales_amt_age[i, 3], Sales_amt_age[i, 4],
               Sales_amt_age[i, 5], Sales_amt_age[i, 6])
  which_max_age_amt[[i]] = which.max(row_list)

}

## 블록코드, 업종코드, 총매출액과 cbind
age_amt_max = cbind(which_max_age_amt_df_t, Max_amt_age_df)
days_amt_max = cbind(which_max_days_amt_df_t, Max_amt_days_df)
times_amt_max = cbind(which_max_times_amt_df_t, Max_amt_times_df)
View(age_amt_max)

### 열 이름 변경
names(age_amt_max)[1] = 'Age_index'
names(days_amt_max)[1] = 'Days_index'
names(times_amt_max)[1] = 'Times_index'
names(age_amt_max)[2] = 'Age_amt'
names(days_amt_max)[2] = 'Days_amt'
names(times_amt_max)[2] = 'Times_amt'

# 사용할 열만 추출 후 열 이름 rename
AMT_MAX = cbind(Selling$BLCK_CD, Selling$KSIC_CD, Selling$TA_YM, age_amt_max, days_amt_max, times_amt_max, Selling$AMT)

rename = dplyr::rename
AMT_MAX = rename(AMT_MAX, 'BLCK_CD'= 'Selling$BLCK_CD', 'KSIC_CD' = 'Selling$KSIC_CD',
                 'TA_YM' = 'Selling$TA_YM', 'AMT' = 'Selling$AMT')

## 상가업소의 블록코드와 지번 주소 불러오기
CNFM = read.csv(file = 'C://Users//bigdata15//Desktop//복사본 CNFM_PRMISN.csv')
region = CNFM %>% dplyr::select(BLCK_CD, OLD_ADRES)
region$OLD_ADRES = substr(region$OLD_ADRES, 7, 9)
region_split = split(region, region$OLD_ADRES) # 5개 지역구와 1개의 null 집합 확인

### 강남구와 강북구만 남겨두고 제거
region = region[!(region$OLD_ADRES == ''),]
region = region[!(region$OLD_ADRES == '노원구'),]
region = region[!(region$OLD_ADRES == '성북구'),]
region = region[!(region$OLD_ADRES == '송파구'),]
region_split = split(region, region$OLD_ADRES)

## 블록코드 기준 innerjoin
query_innerjoin = "
SELECT *
FROM 'AMT_MAX'
INNER JOIN 'region' ON 'region'.BLCK_CD = 'AMT_AMX'.BLCK_CD
"

# 1차 split - 지역별
Gangnam_amt = AMT_FIN[AMT_FIN$OLD_ADRES == '강남구',]
Gangbuk_amt = AMT_FIN[AMT_FIN$OLD_ADRES == '강북구',]

# 업종 기준 2차 split
amt_sectors_gn = split(Gangnam_amt, Gangnam_amt$KSIC_CD)
amt_sectors_gb = split(Gangbuk_amt, Gangbuk_amt$KSIC_CD)

# 이상치 처리===========
## 이상치 확인
boxplot(`55`$AMT)
boxplot(`55`$AMT)$stats
`55`$AMT = ifelse(`55`$AMT < 100 | `55`$AMT > 9978125, NA, `55`$AMT)

# 결측치 제거
`55` = na.omit(`55`)
sum(is.na(`55`$AMT))
nrow(`55`)
#============================

# `55_BC`로 채택 후 표준화
`55` = transform(`55`, z.BC = scale(BC))
head(`55`)

hist(`55`$z.BC, breaks = 100)
#==========

# train, test set split =============
set.seed(100) # 난수 생성

index = sample(1:nrow(`55`), 0.7 * nrow(`55`))
nrow(`55`)

train = `55`[index,] # Create the training data # 70%
test = `55`[-index,] # Create the test data # 30%

# 각 요소 개수 확인========
## 빈도수 높은 변수로 더미변수의 기준 채택
hist(`55`$Age_index) # 2 채택
hist(`55`$Days_index) # 7 채택
hist(`55`$Times_index) # 1 채택

# encoding ===========
### train
train = transform(train,
                  Age_index_1 = ifelse(Age_index == '1', 1, 0),
                  Age_index_3 = ifelse(Age_index == '3', 1, 0),
                  Age_index_4 = ifelse(Age_index == '4', 1, 0),
                  Age_index_5 = ifelse(Age_index == '5', 1, 0),
                  Age_index_6 = ifelse(Age_index == '6', 1, 0),
                  Days_index_1 = ifelse(Days_index == '1', 1, 0),
                  Days_index_2 = ifelse(Days_index == '2', 1, 0),
                  Days_index_3 = ifelse(Days_index == '3', 1, 0),
                  Days_index_4 = ifelse(Days_index == '4', 1, 0),
                  Days_index_5 = ifelse(Days_index == '5', 1, 0),
                  Days_index_6 = ifelse(Days_index == '6', 1, 0),
                  Times_index_2 = ifelse(Times_index == '2', 1, 0),
                  Times_index_3 = ifelse(Times_index == '3', 1, 0)
                  )

# 모델링
## 람다값 설정
lambdas = 10 ^ seq(2, -3, by = -.1)

# parameter 수, alpha값 (ridge는 0), 분포 모형, 람다값 설정
ridge_reg = glmnet(x_train, y_train, nlambda = 5, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg)

# hyperparameter lambda value tuning (자동)====
cv_ridge = cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
optimal_lambda = cv_ridge$lambda.min
optimal_lambda # 최적값 = 0.2511886 # 표준화 + 정규화 : 0.01258925
