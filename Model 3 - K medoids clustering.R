## 지역 및 업종 코드 테이블
Block_table = BLCK_DIM[, -c(2, 11, 13)]
Adm_table = ADSTRD_DIM[, c(1:9)]

Block_Sector_table = SELNG[, c('BLCK_CD', 'TA_YM', 'KSIC_CD')]
month = split(Block_Sector_table, Block_Sector_table$TA_YM)
Block_Sector_table = month$'201912'

## 업종 테이블 
Sector_table = SVC_INDUTY_MAPNG_NEW[, c('SVC_INDUTY_CD_NM', 'INDUTY_CD')]
names(Sector_table) = c('업종명', 'KSIC_CD')
Sector_table = merge(Block_Sector_table, Sector_table, by = 'BLCK_CD')
Sector_table = merge(Sector_table, Block_table, by = 'BLCK_CD')
Sector_table = merge(Sector_table, Adm_table, by = 'ADSTRD_CD')
Sector_table = Sector_table %>% dplyr::select(BLCK_CD, ADSTRD_NM, KSIC_CD, 업종명)
Sector_table = Sector_table[order(Sector_table$BLCK_CD), ]

### 상주인구 테이블 정제
Resident_table / RESDNG_POPLTN[, c(1:12)]
Resident_table$Gender_max = apply(Resident_table[, c(5:6)], 1, which.max)
Resident_table$Age_max = apply(Resident_table[, c(7:12)], 1, which.max)
Resident_table = within(Resident_table, {
  Gender = character(0)
  Gender[Gender_max == 1] = 'Male'
  Gender[Gender_max == 2] = 'Female'
  Gender = factor(Gender, level = c('Male', 'Female'))
  
  Age = character(0)
  Age[Age_max == 1] = '10대'
  Age[Age_max == 2] = '20대'
  Age[Age_max == 3] = '30대'
  Age[Age_max == 4] = '40대'
  Age[Age_max == 5] = '50대'
  Age[Age_max == 6] = '60대'
  Age = factor(Age, level = c('10대', '20대', '30대', '40대', '50대', '60대'))
  })

### 결측치 처리 - 해당 행정동의 평균으로 대체
Population[!complete.cases(Population),]

Population[Population$ADSTRD_CD == 11680580, ]
Population[860, '가구수'] = mean(Population[Population$ADSTRD_CD == 11680580, ]$가구수)
Population[860, '평균가구소득'] = 0
Population[860, '평균가구소득'] = mean(Population[Population$ADSTRD_CD == 11680580, ]$평균가구소득)

## 교통정보 테이블 - X, Y 좌표를 블록코드, 행정동 단위로 변경

### 버스 유무 테이블
Bus_table = STTN_INFO[, c('STTN_ID', 'XCNTS_VALUE', 'YDNTS_VALUE')]
Bus_table[!complete.cases(Bus_table), ]
Bus_table = Bus_table[complete.cases(Bus_table), ] # 결측치 제거
rownames(Bus_table) = NULL

Bus = Block_table[, c('BLCK_CD', 'ADSTRD_CD')]
Bus$Bus = NA

for (i in (1:nrow(Bus))) {
  Block_bus = Bus_table[Bus_table$XCNTS_VALUE >= Block_table[i, 'XCNTS_MIN_VALUE'] &
                          Bus_table$XCNTS_VALUE <= Block_table[i, 'XCNTS_MAX_VALUE'] &
                          Bus_table$YDNTS_VALUE >= Block_table[i, 'YDNTS_MIN_VALUE'] &
                          Bus_table$YDNTS_VALUE <= Block_table[i, 'YDNTS_MAX_VALUE'], ]
  
  Bus[i, 'Bus'] = ifelse(nrow(Block_bus) == 0, 0, 1)
  
}
