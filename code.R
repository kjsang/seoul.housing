#####################
###### 크롤링 #######
#####################

# 0. 패키지 설치 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, rvest, magrittr
)

# 1.1.링크 저장 및 만들기 --------------------------------------

"https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%84%9C%EC%9A%B8%EC%8B%9C%20%22%EC%B2%AD%EB%85%84%22%20%EC%A3%BC%EA%B1%B0%20%EC%A7%80%EC%9B%90&sort=1&photo=3&field=0&pd=3&ds=2019.01.01&de=2021.12.31&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from20190101to20211231,a:all&start=" -> url
# 검색어와 기간설정 후 검색한 뒤 링크 통째로 따기

PAGE <- seq(from=1,to=1111,by=10) # (to에 검색결과창 맨 마지막 페이지 번호 입력)
naver_url_list <- c()
for (page_i in PAGE) {
  naver_url <- paste0(url,page_i)
  naver_url_list <- c(naver_url_list, naver_url)
  
}
naver_url_list

# 1.3. url 에서 관련기사 url 추출 ----------------------------------
news_url <- c()

for (page_i in PAGE) {
  naver_url <- paste0(url, page_i)
  html <- read_html(naver_url)
  temp <- unique(
    html_nodes(html, '#main_pack') %>% # id= 는 #
      html_nodes(css = '.group_news') %>% # class= 는 css= 붙이고 . 찍어주기
      html_nodes(css = '.news_info') %>%
      html_nodes('a') %>%
      html_attr('href')
  )
  news_url <- c(news_url, temp)
}
    
news_url %>% 
  as_tibble() %>% 
  filter(value %>% str_detect("news.naver.com")) -> NEWS

NEWS %>% 
  rename(url = value) %>% 
  mutate(id =row_number(),
         title = "",
         content = "",
         date = "",
         press = "") %>% 
  select(id, date, title, content, url) -> NEWS_prep

for (i in 1:dim(NEWS_prep)[1]) {
  html <- read_html(as.character(NEWS_prep$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html, '#articleTitle')), from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html, '#articleBodyContents')), from = 'utf-8')
  temp_news_date <- repair_encoding(html_text(html_nodes(html, css = ".t11")), from = 'utf-8')
  temp_news_press <- repair_encoding(html_text(html_nodes(html, css = ".copyright")), from = 'utf-8')
  if (length(temp_news_title) > 0) {
    NEWS_prep$title[i]   <- temp_news_title
    NEWS_prep$content[i] <- temp_news_content
    NEWS_prep$date[i] <- temp_news_date
    NEWS_prep$press[i] <- temp_news_press
  }
}
NEWS_prep -> NEWS
NEWS %>%  select(press)
NEWS %>% 
  write_excel_csv("news.csv")


#####################
###### 본분석 #######
#####################


# 0. 패키지 로드 -------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext, tidyr,
  KoNLP, 
  lubridate, scales, 
  tidylo, rvest,
  tm, furrr, topicmodels,
  ggpmisc, # 시계열 시각화
  ggraph, # 네트워크 분석
  widyr, # 단어쌍 
  tidygraph
)


# 0.1. 사전 불러오기: NIAdic --------------------------------

useNIADic() # 사전 불러오기: NIAdic 사용

read_csv("news.csv") -> data # 데이터 불러오기: 전체 언론기사 중 지면기사 수집

data %>% # 총 3,148 건 데이터 중
  mutate(date = date %>% 
           str_replace_all("(오전|오후) [0-9]{1,2}:[0-9]{1,2}", "") %>% # 날짜에 붙은 시간 제거
           lubridate::as_date(), # 날짜변수를 날짜로 정제
         press = press %>%  # 신문사를
           str_replace_all("\\..*|\n| |(.*| )ⓒ", ""),
         ref = paste0("(", press, ", ", date, ")")) -> data_prep_ver1 # 기본 필터링

data_prep_ver1 %>% 
  count(date) %>% 
  as.data.frame()

data_prep_ver1 %>% # 총 64 건
  mutate(content = content %>%
           str_replace_all("\n\t\n\t\n\n\n\n// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "") %>%
           str_replace_all("\n|\t|\\(\\)|\\{\\}", "") %>% 
           str_replace_all("\\[(동아일보|서울신문)\\]", "")) -> data_prep_ver2
data_prep_ver2 %>% write_excel_csv("data_prep_ver2.csv")


# 2.2. 데이터 정제: 기사제목 기준 필터링 ----------------------------

# 기사제목만 확인
data_prep_ver2 %>% 
  select(id, title) %>% 
  as.data.frame()

data_prep_ver2 %>% 
  filter(!title %>% str_detect("금융허브|시민사회|GTX|우울증|보호종료아동|하나은행|금융경쟁력|이건희|오늘의|기본소득|박영선")) %>% 
  filter(!title %>% str_detect("조직개편|공황증세|김어준|꿈나래통장|시흥|보호종료아동|안심소득|이재명|클러스터|마음건강|거짓말|메타버스|금융투자|나무심기")) -> data_prep_ver3

data_prep_ver3 %>% write_excel_csv("data_prep_ver3.csv") # 중간 저장

data_prep_ver3 %>% # 584 건 중 내용분석을 통해 걸러내기
  filter(!title %>% str_detect("알림")) -> data_prep_ver4


# 2.4. 데이터 정제: 본문 태그 필터링 ------------------------------

data_prep_ver4 %>% 
  mutate(
    content = content %>% 
      str_replace_all("☞.*", "") %>%
      str_replace_all("▶.*", "") %>%
      str_replace_all("★.*", "") %>%
      str_replace_all("이 시각 인기뉴스.*", "") %>% 
      str_replace_all("\\<.*\\>", "") %>%
      str_replace_all("◆.*", "") %>% 
      str_replace_all("※.*", "") %>% 
      str_replace_all("(Copyright|ⓒ).*", "") %>% # 저작권 표기 제거
      str_replace_all("김태희 몰디브.*", "") %>% # 광고 태그
      str_replace_all("■ 장준하.*", "") %>% 
      str_replace_all("\\[(한겨레|내일신문)\\]", "") %>% 
      str_replace_all("경향신문|세계일보|공식 SNS|게티이미지뱅크|연합뉴스", "") %>% 
      str_replace_all("오늘의 핫뉴스|모바일 경향.*|GoodNews paper.*", "") %>%
      str_replace_all("맛있는 정보! 신선한 뉴스!.*", "") %>% 
      str_replace_all("한국일보는 투기 제보를 기다리고 있습니다.*|■ MB, ‘노무현 4주기’에 1박2일 골프…논란 확산.*", "") %>% 
      str_replace_all("\\[.*\\]", "") %>% 
      str_replace_all("[a-zA-Z0-9]{1,}@.*", "") %>% 
      str_replace_all("[·가-힣]{2,10}(| )(기자|특파원|차장|논설위원|정치부장|디지털부문장|대기자)", "")) -> data_prep_ver5

data_prep_ver5 %>% # 내용이 적은 기사 제외 (포토 등)
  filter(!content %>% str_length() <= 200) -> data_prep_ver5
data_prep_ver5 %>% write_excel_csv("data_prep_ver5.csv")

par(family = "AppleGothic") # 시각화를 위한 설정 1: 기본 글꼴 지정
theme_set(theme_gray(base_family = 'AppleGothic')) # 시각화를 위한 설정 2: plot용 글꼴 지정

# 3.1. 신문사별 기사 수 --------------------------------------

data_prep_ver5 %>% # 544 건 -> 최종 데이터셋
  count(press) %>% 
  arrange(desc(n)) -> data_prep_신문사별
data_prep_신문사별

# 3.2. 날짜별 기사 수: 월별 ---------------------------------------
data_prep_ver5 %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = lubridate::quarter(date),
    yq = paste0(year, ": ", quarter, "Q"),
    quarterly = lubridate::yq(yq))  -> data_prep_ver6

data_prep_ver6 %>% 
  count(quarterly) %>% 
  filter(n >= 30)

data_prep_ver6 %>%
  count(quarterly) %>%
  ggplot(aes(x = quarterly, y = n))


# 오픈문항 처리 ---------------------------------------------

# 0. 패키지 로드 -------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, magrittr,
  tidytext, tidyr,
  KoNLP, 
  lubridate, scales, 
  tidylo, rvest,
  tm, furrr, topicmodels,
  ggpmisc, # 시계열 시각화
  ggraph, # 네트워크 분석
  widyr, # 단어쌍 
  tidygraph
)

readxl::read_xlsx("open.xlsx") -> open
open %>% 
  mutate(id = 1:length(ID)) %>% 
  select(id, OPEN) -> open


# 기본 설정 -----------------------------------------------

par(family = "AppleGothic") # 시각화를 위한 설정 1: 기본 글꼴 지정
theme_set(theme_gray(base_family = 'AppleGothic')) # 시각화를 위한 설정 2: plot용 글꼴 지정
useNIADic() # 사전 불러오기: NIAdic 사용

소득분위 

open %>% 
  unnest_tokens(input = OPEN,
                output = words,
                token = extractNoun,
                drop = F
  ) %>% 
  select(-OPEN) -> open_word
open_word %>% 
  filter(words %>% str_detect("")) %>% 
  count(words) %>% 
  arrange(desc(n))

open_word %>% 
  anti_join(open_word %>% 
              count(words) %>% 
              filter(!n >= 2) %>% # 빈도수 5 이상의 단어만 추출
              select(words),
            by = "words") %>% # 96,580 단어
    filter(words %>% str_length() >= 2) -> open_word_prep
open_word_prep %>% 
  mutate(words = words %>% 
           str_replace_all("1인가구", "1인가구") %>% 
           str_replace_all("기피", "기피") %>% 
           str_replace_all("정책", "정책")%>% 
           str_replace_all("임대아파트", "임대아파트")%>% 
           str_replace_all("받을사람", "정책대상")%>% 
           str_replace_all("청약", "청약")%>% 
           str_replace_all("까다", "까다롭다")%>% 
           str_replace_all("오래", "오래걸린다")%>% 
           str_replace_all("짧", "짧다")%>% 
           str_replace_all("소득기준|소득분", "소득기준")%>% 
           str_replace_all("저소득", "저소득층")%>% 
           str_replace_all("중간소득계층", "중위소득층")) -> open_word_prep2

open_word_prep2 %>% 
  filter(!words %>% str_detect("좋겠")) -> open_word_prep3

open_word_prep2 %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  as.data.frame()

open_word_prep3 %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  mutate(words = reorder(words, n)) %>%
  slice_max(n, n = 30,  with_ties = F) %>% 
  ggplot(aes(x = fct_reorder(words, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.5) +
  xlab("") + ylab("")
