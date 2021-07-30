# 필요한 열 추출
Selling = Selling_origin %>% dplyr::select(c(1:6, 57:63))
View(Selling)

# 재방문 수 연령대 전체 sum
RCNT = Selling %>% dplyr::select(RCNT_10, RCNT_20, RCNT_30, RCNT_40, RCNT_50, RCNT_60)
RNCT = apply(RCNT, 1, sum)
RCNT = as.data.frame(RCNT)

# 전체 매출 건수 추출
CNT = Selling %>% dplyr::select(CNT)

# 지역코드/업종코드-대분류와 매핑
## 상가업소 테이블의 블록코드와 지번주소 불러오기
CNFN = read.csv(file = 'C://Users//bigdata15//Desktop//복사본 CNFM_PRMISN.csv')
region = CNFM %>% dplyr::select(BLCK_CD, OLD_ADRES)

# 행정동만 추출
region$OLD_ADRES = substr(region$OLD_ADRES, 11, 13)

# 행정동별 데이터 split
region_split = split(region, region$OLD_ADRES) # 53개 지역구와 1개의 null 집합 확인

# 행정동명이 비어있는 데이터 삭제
region = region[!(region$OLD_ADRES == ''),]
region_split = split(region, region$OLD_ADRES)

# 최신(2019년 12월) 자료로 선택
`201912` = AMT_FIN[AMT_FIN$TA_YM == '201912', ]

# 업종별 split
amt_sector = split(`201912`, `201912`$KSIC_CD)

# 최종 테이블 생성 (예제: 45번 업종)
## 가맹점수, 평균영업개월수, 매출건수 행정동별 계산
MCT_CNT = aggregate(MCT_CNT~OLD_ADRES, dat_45, sum)
MCT_SALES = aggregate(MCT_SALES~OLD_ADRES, dat_45, mean)
RCNT = aggregate(RCNT~OLD_ADRES, dat_45, sum)
CNT = aggregate(CNT~OLD_ADRES, dat_45, sum)

## cbind
dat_45 = cbind(MCT_CNT, MCT_SALES, RCNT, CNT)

## 행정동 중복 column 제거
dat_45 = dat_45[, c(-3, -5, -7)]

## 재방문건수를 재방문율로 변경
dat_45[4] = dat_45[4] / dat_45[5] ## RCNT/CNT
dat_45[5] = NULL # CNT 열 삭제

## 행정동_업종코드 라벨 생성
for (i in 1;nrow(dat_45)) {
  dat_45[i, 5] = paste(dat_45[i, 1], '_45')
  
}

## 행정동별 분포 확인
ggplot(data = dat_45) + 
  geom_density(mapping = aes(x = MCT_SALES, colour = OLD_ADRES))

summary(dat_45)

## 표준화
dat_45[2:3] = scale(dat_45[2:3])

## rbind
dat = rbind(dat_45, dat_46, dat_47, dat_55, dat_56, dat_59, dat_68, dat_69,
            dat_71, dat_73, dat_74, dat_75, dat_85, dat_86, dat_90, dat_91, dat_95, dat_96)
