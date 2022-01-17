#####################
###### 크롤링 #######
#####################

# 0. 패키지 설치 ----------------------------------------------

if (!require("pacman")) (install.packages("pacman"))
pacman::p_load(
  tidyverse, rvest, magrittr
)

# 1.1.링크 저장 및 만들기 --------------------------------------

"https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%84%9C%EC%9A%B8%EC%8B%9C%20%EC%B2%AD%EB%85%84%20%EC%A3%BC%EA%B1%B0%EC%A7%80%EC%9B%90%EC%A0%95%EC%B1%85&sort=1&photo=3&field=0&pd=3&ds=2021.01.01&de=2021.12.31&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from20210101to20211231,a:all&start=" -> url
# 검색어와 기간설정 후 검색한 뒤 링크 통째로 따기

PAGE <- seq(from=1,to=61,by=10) # (to에 검색결과창 맨 마지막 페이지 번호 입력)
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
NEWS %>% 
  select(press)
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

data_prep_ver1

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
  
