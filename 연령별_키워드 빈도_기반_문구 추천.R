library(readxl)
library(dplyr)
library(stringr)
library(KoNLP)
library(tidyr)
library(tidytext)
library(ggplot2)

# 데이터셋 불러오기
scrap <- read_excel("readers_db/08_scrap.xlsx")
user <- read_excel("readers_db/01_user.xlsx")
user_book <- read_excel("readers_db/04_user_book.xlsx")
book <- read_excel("readers_db/05_book.xlsx")

# 중복된 book_id와 id를 title로 변경
scrap <- scrap %>%
  inner_join(book %>% distinct(id, title), by = c("book_id" = "id")) %>%
  select(-book_id) %>%
  rename(book_title = title)

# book_id를 기준으로 도서의 평점이 4 이상인 도서만 필터링
filtered_books <- user_book %>% filter(rate >= 4)

# 중복 id를 기준으로 유저의 나이, content를 필터링하여 age_content 변수에 저장
age_content <- scrap %>% 
  inner_join(user, by = "user_id") %>% 
  select(age = birth_year, content, book_title, page) %>% 
  mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(age) + 1)

# 결측치 제거 및 20세 미만 값 제거
age_content <- na.omit(age_content)
age_content <- age_content %>% filter(age > 20)

# 전체 데이터의 40%만 랜덤 추출
set.seed(123)
sample_age_content <- age_content %>% sample_frac(0.4)

# 한글 아닌 글자 및 모든 특수문자와 이모티콘 공백으로 변경
sample_age_content$content <- str_replace_all(sample_age_content$content, 
                                            "[^[:alpha:]ㄱ-ㅎㅏ-ㅣ가-힣]", " ")
sample_age_content$content <- str_squish(sample_age_content$content)

# 영어와 러시아어를 제거하는 함수 정의
remove_english_and_russian <- function(text) {
  str_replace_all(text, "[a-zA-Zа-яА-Я]", "")
}

# 'ㅡ'와 단일한 자음과 모음 및 한자를 제거하는 함수 정의
remove_dash <- function(text) {
  pattern <- "[-ㅋㅎㅠㅡㅜ\u4E00-\u9FFF]"
  str_replace_all(text, pattern, "")
}

# 조사 제거하는 함수 정의
remove_particles <- function(text) {
  str_replace_all(text, "\\s[은는이가을를과와\\s]", " ")
}

# "content" 열에서 불필요한 문자 제거
sample_age_content$content <- sapply(sample_age_content$content, 
                                     remove_english_and_russian)
sample_age_content$content <- sapply(sample_age_content$content, remove_dash)
sample_age_content$content <- sapply(sample_age_content$content, remove_particles)
sample_age_content$content <- str_squish(sample_age_content$content)

# 명사 추출 후 토큰화 
noun_tokenizer <- function(x) {
  words <- unlist(extractNoun(x))
  words <- setdiff(words, c("교보", "교보에서","을","를",
                            "은","는","이","가")) 
  paste(words, collapse = " ")
}

# "content"에 대해서 명사를 추출하고 "nouns" 열을 생성
sample_age_content <- sample_age_content %>%
  mutate(nouns = sapply(content, noun_tokenizer))

# "nouns" 열의 내용을 기준으로 토큰화
output <- sample_age_content %>%
  unnest_tokens(word, nouns)

# 결과 확인
print(output$word)

# 토큰화된 명사들의 빈도수
word_frequency <- output %>%
  count(word, sort = TRUE) %>% # 단어 빈도 구해 내림차순 정렬
  filter(nchar(word) > 1) # 두 글자 이상만 남기기

# 결과 확인
print(word_frequency)

# 연대 변수 추가
output <- output %>%
  mutate(age_group = case_when(
    age >= 20 & age < 30 ~ "20대",
    age >= 30 & age < 40 ~ "30대",
    age >= 40 & age < 50 ~ "40대",
    age >= 50 ~ "50대 이상",
  ))

# 단어 빈도수 연령별로 정렬
word_frequency <- output %>%
  count(age_group, word, sort = TRUE) %>%
  filter(nchar(word) > 1)

# 연령별 Tf-idf 계산
word_tfidf <- word_frequency %>%
  bind_tf_idf(word, age_group, n)

# Tf-idf를 기준으로 내림차순 정렬
sorted_word_tfidf <- word_tfidf %>%
  arrange(desc(tf_idf))

# 연령대별 Tf-idf 확인
sorted_word_tfidf[sorted_word_tfidf$age_group=='20대',]
sorted_word_tfidf[sorted_word_tfidf$age_group=='30대',]
sorted_word_tfidf[sorted_word_tfidf$age_group=='40대',]
sorted_word_tfidf[sorted_word_tfidf$age_group=='50대 이상',]

# 상위 10개의 tf-idf 값을 가진 단어들만 추출
top_words <- sorted_word_tfidf %>%
  group_by(age_group) %>%
  top_n(10, wt = tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, age_group))

# 시각화
ggplot(top_words, aes(x = reorder_within(word, tf_idf, age_group),
                      y = tf_idf,
                      fill = age_group)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ age_group, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL)

# 각 연령대별로 가장 높은 Tff 값을 가진 단어 선택
top_tfidf_words <- sorted_word_tfidf %>%
  group_by(age_group) %>%
  top_n(1, wt = tf_idf) %>%
  ungroup() %>%
  select(age_group, word) %>%
  arrange(age_group)

# 각 연령대에서 상위 단어를 포함하는 content, book_title, page 선택하는 함수
get_top_word_examples <- function(age_group, top_word, df, n = 3) {
  results <- df %>%
    filter(age_group == age_group & grepl(top_word, content)) %>%
    sample_n(min(n, nrow(.))) %>%
    select(book_title, content, page)
  colnames(results) <- c("추천 도서", "추천 스크랩 문구", "페이지")
  return(results)
}

# 각 연령대별 결과 추출
results_20s <- get_top_word_examples("20대", 
                                     top_tfidf_words$word[top_tfidf_words$age_group == "20대"], 
                                     sample_age_content)

results_30s <- get_top_word_examples("30대", 
                                     top_tfidf_words$word[top_tfidf_words$age_group == "30대"], 
                                     sample_age_content)

results_40s <- get_top_word_examples("40대", 
                                     top_tfidf_words$word[top_tfidf_words$age_group == "40대"], 
                                     sample_age_content)

results_50s <- get_top_word_examples("50대 이상", 
                                     top_tfidf_words$word[top_tfidf_words$age_group == "50대 이상"], 
                                     sample_age_content)

# 연령별 top 키워드 추천 문구, 도서, 페이지 추천 (3쌍씩)
print(results_20s)
print(results_30s)
print(results_40s)
print(results_50s)
