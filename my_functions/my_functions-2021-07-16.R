# 지금까지 만든 모든 함수를 정리한 것
## xaringan용은 뒤에 xaringan을 붙여 구분함

# 평균 t 검정 함수
## 동분산 여부를 Levene 검정을 통해 검정한 후 그에 따른 t-검정 결과를 출력함.
## xft: 요인 변수
## yct: 척도 변수
## simple은 왼쪽 컬럼에 요인에 대한 설명을 제외하고 각주의 수를 4개에서 2개로 줄인 것임.
## my.ctables.t.simple
## my.ctables.t.simple.xaringan
## my.ctables.t
### t 결과를 한줄 올림 (2021.7.24.)
### cell_name을 지정할 때는 마지막에 "소계"를 넣어야 한다는 설명 추가 (2021.7.24.)
### cell_name <- c("학생 수", "평균", "준오편차", "검정", "소계")를 넣어야 한다는 설명 추가 (2021.7.24.)
## my.ctables.t.xaringan
### t 결과를 한줄 올림 (2021.7.24.)
### cell_name을 지정할 때는 마지막에 "소계"를 넣어야 한다는 설명 추가 (2021.7.24.)
### cell_name <- c("학생 수", "평균", "준오편차", "검정", "소계")를 넣어야 한다는 설명 추가 (2021.7.24.)


# ANOVA 검정 함수(Simple)
## Scheffe의 사후 검정을 포함함.
## xft: 요인 변수
## yct: 척도 변수
## simple은 왼쪽 컬럼에 요인에 대한 설명을 제외하고 각주의 수를 4개에서 2개로 줄인 것임.
## my.ctables.anova.simple
### 2021.7.16. F -> FALSE: 두 번째 주를 다음 줄로 구분
## my.ctables.anova.simple.xaringan
### 2021.7.16. F -> FALSE: 두 번째 주를 다음 줄로 구분
## my.ctables.anova
## 2021.4.7. spacing=space 옵션 추가
## my.ctables.anova.xaringan
## 2021.4.7. spacing=space 옵션 추가


# 독립성 검정 함수
## Crosstable 형태
## xft의 요인 수가 2개 이상일 경우 (2개이면 에러가 나서 이를 고치기 위해 chi2 결과를 한줄 올림 (2021.7.20.))
## my.ctables.p.before - 프린트 옵션 전
## my.ctables.p.before.xaringan - 프린트 옵션 전
## xft: 요인 변수
## yft: 요인 변수
## my.ctables.p - 프린트 옵션 추가
## my.ctables.p.xaringan - 프린트 옵션 추가

# 독립성 검정 함수 - 표로부터 자료를 읽고 분석
## Crosstable 형태
## xft의 요인 수가 3개 이상일 경우 (2개이면 에러)
## xft의 요인 수가 2개 이상일 경우 (2개이면 에러가 나서 이를 고치기 위해 chi2 결과를 한줄 올림 (2021.7.20.))
## my.ctables.p.from.table.before - 프린트 옵션 전
## my.ctables.p.from.table.before.xaringan - 프린트 옵션 전
## ct: 표
## my.ctables.p.from.table - 프린트 옵션 추가
## my.ctables.p.from.table.xaringan - 프린트 옵션 추가


# 적합도 검정 함수 (gf: Goodness of Fit)
## my.ctables.gf
## my.ctables.gf.xaringan
## xft: 요인 변수

# 적합도 검정 함수 - 표로부터 자료를 읽고 분석
## my.ctables.gf.from.table - 프린트 옵션 추가
## my.ctables.gf.from.table.xaringan - 프린트 옵션 추가
## ct: 표

# Bar 그래프
## my.barplot
## my.barplot.xaringan
## xft: 요인 변수


# lm 결과 출력 함수
## my.ft
## my.ft.xaringan
## coeff: 계수 추정치
## pvalue: p-value,
## my.ft.p : 2021. 7. 26. p-value 포함
## coeff: 계수 추정치
## pvalue: p-value,



### 그래프-histogram-  1변수 함수
## graph_hist1
## graph_hist1.xaringan
## data: 척도 변수



# 등록률: 보도자료 형태
## my.register
## df: 자료 데이터프레임
## group = "권역" # 대학, 권역
## category = "정원내" # 정원내, 정원외, 전체
## xvar = c("모집", "등록") # 모집, 지원, 등록
## year = c("2020", "2021")
## criteria = -5.0
## spacing = 0.5
## na.rm = FALSE
## 이는 아래 세 가지 형태를 모두 합쳐서 만든 것임
### 등록률: 권역별
### my.register.area
### 등록률: 대학별
### my.register.id
### 등록률: 설립형태별
### my.register.private


# 평균 t 검정 함수

my.ctables.t.simple <- function(xft = xft, yct = yct, na.rm = TRUE, caption = NA) {
  # xft: factor variable, yct: continuous numeric variable
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[2]]))
  }

  if (is.na(caption)) {
    caption = label(data[[2]])
  }
  
  m1 <- t.test(yct~xft, data = data, var.equal = TRUE)
  m2 <- t.test(yct~xft, data = data, var.equal = FALSE)
  m3 <- levene.test(data$yct, data$xft, location = "mean") # SPSS와 동일한 결과

  ft1 <- data %>%
  group_by(xft) %>%
  summarise(count = n(), mean = mean(yct), std.dev = sd(yct))

  ft2 <- data %>%
  mutate_at(vars(xft), funs(as.character(.))) %>%
  summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))

  large_mean <- ifelse (ft1$mean[[1]] > ft1$mean[[2]], 1, 2)

  ft <- rbind(ft1, ft2)

  if (m3$p.value > 0.05) {
    equal.variance = TRUE
  } else {
    equal.variance = FALSE
  }

  if (equal.variance) {
    t <- m1$statistic
    p <- m1$p.value
    levene <- 'e'
  } else {
    t <- m2$statistic
    p <- m2$p.value
    levene <- 'u'
  }

  col_stats <- tibble(test = c(t, p, NA))

  ft <- cbind(ft, col_stats)
ft
  
  ft <- ft %>%
    flextable (
      col_keys = c("xft", "count", "mean", "std.dev", "test")
    ) 
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, xft = label(data$xft))

    asterisk <- ""

  if (p <= 0.01) {
    asterisk <- "***" } else if(p <= 0.05) {
    asterisk <- "**" } else if(p <= 0.10){
    asterisk <- "*"
  }

  ft <- ft %>%
    colformat_double(j = 2, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 3, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 3) 

  ft <- ft %>%
    compose(
      i = large_mean, j = 3, value = as_paragraph(as_b(mean), as_sup(asterisk))
    ) %>%
    compose(
      i = 1, j = 5, value = as_paragraph(as_chunk(test, digits = 1), as_sup(levene))
    ) %>%
    compose(
      i = 2, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) %>%
    footnote(i = 1, j = c(3,5),
              value = as_paragraph(
              c(" * < 0.10, ** < 0.05, *** < 0.01",
                " The number in the parenthesis is the p-value in a two-sample t test for the difference of means. The superscript 'e' and 'u' denote a t-test comparing the mean under the assumption of the equal and unequal variance of two groups based on Levene's test, respectively.")
                ),
              ref_symbols = c("1)", "2)"),
              part = "header", inline = FALSE)
  ft <- autofit(ft)
  return(ft)
}


# 평균 t 검정 함수(xaringan)

my.ctables.t.simple.xaringan <- function(xft = xft, yct = yct, na.rm = TRUE, caption = NA) {
  # xft: factor variable, yct: continuous numeric variable
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[2]]))
  }
  
  if (is.na(caption)) {
    caption = label(data[[2]])
  }
  
  m1 <- t.test(yct~xft, data = data, var.equal = TRUE)
  m2 <- t.test(yct~xft, data = data, var.equal = FALSE)
  m3 <- levene.test(data$yct, data$xft, location = "mean") # SPSS와 동일한 결과
  
  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))
  
  large_mean <- ifelse (ft1$mean[[1]] > ft1$mean[[2]], 1, 2)
  
  ft <- rbind(ft1, ft2)
  
  if (m3$p.value > 0.05) {
    equal.variance = TRUE
  } else {
    equal.variance = FALSE
  }
  
  if (equal.variance) {
    t <- m1$statistic
    p <- m1$p.value
    levene <- 'e'
  } else {
    t <- m2$statistic
    p <- m2$p.value
    levene <- 'u'
  }
  
  col_stats <- tibble(test = c(t, p, NA))
  
  ft <- cbind(ft, col_stats)
  ft
  
  ft <- ft %>%
    flextable (
      col_keys = c("xft", "count", "mean", "std.dev", "test")
    ) 
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, xft = label(data$xft))
  
  asterisk <- ""
  
  if (p <= 0.01) {
    asterisk <- "\\*\\*\\*" } else if(p <= 0.05) {
      asterisk <- "\\*\\*" } else if(p <= 0.10){
        asterisk <- "\\*"
      }
  
  ft <- ft %>%
    colformat_double(j = 2, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 3, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 3) 
  
  ft <- ft %>%
    compose(
      i = large_mean, j = 3, value = as_paragraph(as_b(mean), as_sup(asterisk))
    ) %>%
    compose(
      i = 1, j = 5, value = as_paragraph(as_chunk(test, digits = 1), as_sup(levene))
    ) %>%
    compose(
      i = 2, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) %>%
    footnote(i = 1, j = c(3,5),
             value = as_paragraph(
               c(" \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01",
                 " The number in the parenthesis is the p-value in a two-sample t test for the difference of means. The superscript 'e' and 'u' denote a t-test comparing the mean under the assumption of the equal and unequal variance of two groups based on Levene's test, respectively.")
             ),
             ref_symbols = c("1)", "2)"),
             part = "header", inline = FALSE)
  ft <- autofit(ft)
  return(ft)
}


# 평균 t 검정 함수

my.ctables.t <- function(xft = xft, yct = yct,
                         xft_name = NA, yct_name = NA, 
                         xft_level = NA, yct_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, 
                         header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE,
                         spacing = NA) {
  # xft: factor variable, yct: continuous numeric variable
  # cell_name을 지정할 때는 마지막에 "소계"를 넣어야 함
  # cell_name <- c("학생 수", "평균", "준오편차", "검정", "소계")

  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[2]]))
  }

  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yct_name)) {
    yct_name <- label(yct)
  }

  # yct의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yct_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(yct_level[[1]])) { 
    if (is.factor(yct)) {
       yct_level <- levels(yct)
    } else {
      yct_level <- sort(unique(yct))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Mean", "Std.Dev", "Test statistics", "Total")  
  }
    
  if (is.na(caption[[1]])) {
#    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yct_name)
  }

  first_note <- "1)  * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The superscript 'e' and 'u' denote a t-test comparing the mean under the assumption of the equal and unequal variance of two groups based on Levene's test, respectively."
  third_note <- "3) The number in the parenthesis is the p-value in a two-sample t test for the difference of means."
  forth_note <- "4) The larger column mean is printed in boldface."

  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }

  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))

  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))

  ft2[1, 1] <- cell_name[5] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈

  ft <- rbind(ft1, ft2)

  nxf <- 2 # factor 수가 2개인 경우에 대한 검정
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  
  # t test
  m1 <- t.test(yct~xft, data = data, var.equal = TRUE)
  m2 <- t.test(yct~xft, data = data, var.equal = FALSE)
  m3 <- levene.test(data$yct, data$xft, location = "mean") # SPSS와 동일한 결과

  if (m3$p.value > 0.05) {
    equal.variance = TRUE
  } else {
    equal.variance = FALSE
  }

  if (equal.variance) {
    t <- m1$statistic
    p <- m1$p.value
    df <- m1$parameter
    levene <- 'e'
    digits_df <- 0
  } else {
    t <- m2$statistic
    p <- m2$p.value
    df <- m2$parameter
    levene <- 'u'
    digits_df <- 1
  }

  asterisk <- ""
  if (p <= 0.01) {
    asterisk <- "***" } else if(p <= 0.05) {
    asterisk <- "**" } else if(p <= 0.10){
    asterisk <- "*"
  }

#  col_stats <- tibble(test = c(NA, t, p))
  col_stats <- tibble(test = c(t, p, NA)) # 2021.7.24. t 결과 표시를 한 줄 올림
  
  ft <- cbind(ft, col_stats)

  ft <- ft %>%
    flextable (
      col_keys = c("rcell", "xft", "count", "mean", "std.dev", "test")
    ) 

  if (!is.na(ans)) {
    ft <- color(ft, i = ans, j = 2:5, color = "red")
  }

  ft <- ft %>%
    colformat_double(j = 3, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 6, digits = 3) 

  ft <- ft %>%
    compose(
#      i = 1, j = 6, value = as_paragraph("t", as_sup(levene), as_sub("("), as_sub(as_chunk(df, digits = digits_df)), as_sub(")"), " = ")
      i = 1, j = 6, value = as_paragraph("t", as_sup(levene), as_sub("("), as_sub(as_chunk(df, digits = digits_df)), as_sub(")"), " = ", as_chunk(test, digits = 1), as_sup(asterisk)) # 2021.7.24. t 결과 표시를 한 줄 올림
    ) %>%
# 2021.7.24. t 결과를 한줄 올리기 위해 아래는 주석 처리    
#    compose(
#      i = 2, j = 6, value = as_paragraph(as_chunk(test, digits = 1), as_sup(asterisk))
#    ) %>%
    compose(
#      i = 3, j = 6, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
      i = 2, j = 6, value = as_paragraph("(", as_chunk(test, digits = 3), ")") # 2021.7.24. t 결과 표시를 한 줄 올림
    ) 
    
  ft <- bold(ft, i = which.max(ft1$mean), ~mean , bold = TRUE)
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:4) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[j]  
  }

# 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, xft = " ") # vname을 xft로 수정
  ft <- set_header_labels(ft, test = cell_name[4]) 

  header1 <- c("", yct_name, "")
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 3, 1))
  }

  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft <- set_caption(ft, caption = caption)

  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 6) %>%
      add_footer_row(values = third_note, colwidths = 6) %>%
      add_footer_row(values = second_note, colwidths = 6) %>%
      add_footer_row(values = first_note, colwidths = 6) 
  }  
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  if (!is.na(spacing)) {
    ft <- line_spacing(ft, space = spacing, part = "header") 
    ft <- line_spacing(ft, space = spacing, part = "body") 
    ft <- line_spacing(ft, space = spacing, part = "footer") 
  }
    
      ft <- autofit(ft)
  
  return(ft)
}

# 평균 t 검정 함수 (xaringan)

my.ctables.t.xaringan <- function(xft = xft, yct = yct,
                         xft_name = NA, yct_name = NA, 
                         xft_level = NA, yct_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, 
                         header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE,
                         spacing = NA) {
  # xft: factor variable, yct: continuous numeric variable
  # cell_name을 지정할 때는 마지막에 "소계"를 넣어야 함
  # cell_name <- c("학생 수", "평균", "준오편차", "검정", "소계")
  
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[2]]))
  }
  
  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yct_name)) {
    yct_name <- label(yct)
  }
  
  # yct의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yct_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(yct_level[[1]])) { 
    if (is.factor(yct)) {
      yct_level <- levels(yct)
    } else {
      yct_level <- sort(unique(yct))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Mean", "Std.Dev", "Test statistics", "Total")  
  }
  
  if (is.na(caption[[1]])) {
    #    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yct_name)
  }
  
  first_note <- "1)  \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The superscript 'e' and 'u' denote a t-test comparing the mean under the assumption of the equal and unequal variance of two groups based on Levene's test, respectively."
  third_note <- "3) The number in the parenthesis is the p-value in a two-sample t test for the difference of means."
  forth_note <- "4) The larger column mean is printed in boldface."
  
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft2[1, 1] <- cell_name[5] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  
  ft <- rbind(ft1, ft2)
  
  nxf <- 2 # factor 수가 2개인 경우에 대한 검정
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  
  # t test
  m1 <- t.test(yct~xft, data = data, var.equal = TRUE)
  m2 <- t.test(yct~xft, data = data, var.equal = FALSE)
  m3 <- levene.test(data$yct, data$xft, location = "mean") # SPSS와 동일한 결과
  
  if (m3$p.value > 0.05) {
    equal.variance = TRUE
  } else {
    equal.variance = FALSE
  }
  
  if (equal.variance) {
    t <- m1$statistic
    p <- m1$p.value
    df <- m1$parameter
    levene <- 'e'
    digits_df <- 0
  } else {
    t <- m2$statistic
    p <- m2$p.value
    df <- m2$parameter
    levene <- 'u'
    digits_df <- 1
  }
  
  asterisk <- ""
  if (p <= 0.01) {
    asterisk <- "\\*\\*\\*" } else if(p <= 0.05) {
      asterisk <- "\\*\\*" } else if(p <= 0.10){
        asterisk <- "\\*"
      }
  
#  col_stats <- tibble(test = c(NA, t, p)) 
  col_stats <- tibble(test = c(t, p, NA)) # 2021.7.24. t 결과 표시를 한 줄 올림
  
  ft <- cbind(ft, col_stats)
  
  ft <- ft %>%
    flextable (
      col_keys = c("rcell", "xft", "count", "mean", "std.dev", "test")
    ) 
  
  if (!is.na(ans)) {
    ft <- color(ft, i = ans, j = 2:5, color = "red")
  }
  
  ft <- ft %>%
    colformat_double(j = 3, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 6, digits = 3) 
  
  ft <- ft %>%
    compose(
      #      i = 1, j = 6, value = as_paragraph("t", as_sup(levene), as_sub("("), as_sub(as_chunk(df, digits = digits_df)), as_sub(")"), " = ")
      i = 1, j = 6, value = as_paragraph("t", as_sup(levene), as_sub("("), as_sub(as_chunk(df, digits = digits_df)), as_sub(")"), " = ", as_chunk(test, digits = 1), as_sup(asterisk)) # 2021.7.24. t 결과 표시를 한 줄 올림
    ) %>%
    # 2021.7.24. t 결과를 한줄 올리기 위해 아래는 주석 처리    
    #    compose(
    #      i = 2, j = 6, value = as_paragraph(as_chunk(test, digits = 1), as_sup(asterisk))
    #    ) %>%
    compose(
      #      i = 3, j = 6, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
      i = 2, j = 6, value = as_paragraph("(", as_chunk(test, digits = 3), ")") # 2021.7.24. t 결과 표시를 한 줄 올림
    ) 
  
  ft <- bold(ft, i = which.max(ft1$mean), ~mean , bold = TRUE)
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:4) {
    ft$header$content$content$data[[j+2]]$txt <- cell_name[j]  
  }
  
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, xft = " ") # vname을 xft로 수정
  ft <- set_header_labels(ft, test = cell_name[4]) 
  
  header1 <- c("", yct_name, "")
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 3, 1))
  }
  
  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft <- set_caption(ft, caption = caption)
  
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 6) %>%
      add_footer_row(values = third_note, colwidths = 6) %>%
      add_footer_row(values = second_note, colwidths = 6) %>%
      add_footer_row(values = first_note, colwidths = 6) 
  }  
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  if (!is.na(spacing)) {
    ft <- line_spacing(ft, space = spacing, part = "header") 
    ft <- line_spacing(ft, space = spacing, part = "body") 
    ft <- line_spacing(ft, space = spacing, part = "footer") 
  }
  
  ft <- autofit(ft)
  
  return(ft)
}



# ANOVA 검정 함수(Simple)
## 2021.7.16. F -> FALSE: 두 번째 주를 다음 줄로 구분

my.ctables.anova.simple <- function(xft = xft, yct = yct, na.rm = TRUE, ans = NA, caption = NA) {
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[2]]))
  }

  if (is.na(caption)) {
    caption = label(data[[2]])
  }
  ifelse (label(data$xft) == "", xft_label <- "xft", xft_label <- label(data$xft))
  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))

  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))

  ft <- rbind(ft1, ft2)

  # ANOVA
  
  m <- aov(yct~xft, data = data)
  sm <- summary(m)

  nxf <- length(unique(xft)) # nlevels(xft)는 factor에만 적용되기 때문에 character에도 적용할 수 있도록 수정

  F <- sm[[1]]$`F value`[1]
  p <- sm[[1]]$`Pr(>F)`[1]

  if (nxf == 2) {
    col_stats <- tibble(test = c(F, p))
  } else {
    col_stats <- tibble(test = c(F, p, rep(NA, nxf - 1)))
  }

  ft <- cbind(ft, col_stats)

  ft <- ft %>%
    flextable (
      col_keys = c("xft", "count", "mean", "std.dev", "test")
    )
  
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, xft = xft_label)

  if (!is.na(ans)) {
    ft <- color(ft, i = ans, j = 1:4, color = "red")
  }

  ft <- bold(ft, i = which.max(ft1$mean), ~mean , bold = TRUE)

  ft <- ft %>%
    colformat_double(j = 2, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 3, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 3) 

  ft <- ft %>%
    compose(
      i = 2, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 
  ft <- ft %>%
    compose(
      i = 1, j = 5, value = as_paragraph(as_chunk(test, digits = 1), as_sup("f"))
    )

   # Schffe test
  # means는 행 제목의 알파벳 오름차순으로, groups는 score_total의 내림차순으로 정렬!!!
  # rownames는 label이 있을 때 1, 2, 3, 4가 아니라 label명으로 됨.
  # 따라서 factor를 integer로 바꿔서 rownames가 레이블이 아니라 1, 2, 4, 4로 되게 만든 후 rownames를 사용해야 함.
  # 이를 위해 xft를 integer로 변환한 후 scheffe test를 다시 한번 실시
  # factor가 아닌 character에서 xft가 NA로 되는 오류 발생
  # 따라서 다시 한번 수정
  # 문자의 경우에는 위와 같이 문자 알파벳 순서로 정렬되게 만들고
  # factor의 경우에는 원래의 순서로 되는 df를 만들고, scheffe 결과에서 그룹을 left_join으로 연결함
  # data <- tibble(xft = xft, yct = yct)
  # data <- data %>%
  # mutate(xft = as.integer(xft))
  # m <- aov(yct~xft, data = data)
  # sm <- summary(m)
  scheffe <- scheffe.test(m, "xft", group=TRUE, console = FALSE) # colsole = FALSE -> scheffe 결과를 출력하지 않도록 함
  scheffe_groups <- scheffe$groups
  if (is.factor(xft)) {
    xft_l <- tibble(xft_level = levels(xft))
    xft_g <- tibble(xft_level = rownames(scheffe_groups), xft_group = scheffe_groups$groups)
    xft_lg <- left_join(xft_l, xft_g, by = "xft_level")
    groups <- xft_lg$xft_group
  } else {
    groups <- scheffe_groups[order(rownames(scheffe_groups)),]$groups # mean순으로 되어 있는 것을 1, 2, 3, 4순으로 정렬하여 a, ab, b 등을 저장  
  }
  # 2021.2.20 1:4 -> 1:nxf로 수정
    if (p <= 0.05) {
    for (nn in 1:nxf) {
      ft <- ft %>%
         compose(
         i = nn, j = 3, value = as_paragraph(mean, as_sup(groups[nn]))
         ) 
    }

  ft <- ft %>%
  footnote(i = 1, j = c(3, 5),
            value = as_paragraph(
              c(" Superscripts 'a', 'b', and 'c' denote subgroups in descending order according to Scheffe’s post hoc test. If mean statistics share the same superscript, then subgroups are homogeneous such that there are no significant mean differences. Otherwise, there are significant differences.",
                "The superscript 'f' denotes a F-test comparing the mean of more than two groups.")
              ),
            ref_symbols = c("1)", "2)"),
            part = "header", inline = FALSE) # 2021.7.16. F -> FALSE
    }
  ft <- autofit(ft)
  return(ft)
}


# ANOVA 검정 함수(Simple) (xaringan)
## 2021.7.16. F -> FALSE: 두 번째 주를 다음 줄로 구분

my.ctables.anova.simple.xaringan <- function(xft = xft, yct = yct, na.rm = TRUE, ans = NA, caption = NA) {
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[2]]))
  }
  
  if (is.na(caption)) {
    caption = label(data[[2]])
  }
  ifelse (label(data$xft) == "", xft_label <- "xft", xft_label <- label(data$xft))
  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft <- rbind(ft1, ft2)
  
  # ANOVA
  
  m <- aov(yct~xft, data = data)
  sm <- summary(m)
  
  nxf <- length(unique(xft)) # nlevels(xft)는 factor에만 적용되기 때문에 character에도 적용할 수 있도록 수정
  
  F <- sm[[1]]$`F value`[1]
  p <- sm[[1]]$`Pr(>F)`[1]
  
  if (nxf == 2) {
    col_stats <- tibble(test = c(F, p))
  } else {
    col_stats <- tibble(test = c(F, p, rep(NA, nxf - 1)))
  }
  
  ft <- cbind(ft, col_stats)
  
  ft <- ft %>%
    flextable (
      col_keys = c("xft", "count", "mean", "std.dev", "test")
    )
  
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, xft = xft_label)
  
  if (!is.na(ans)) {
    ft <- color(ft, i = ans, j = 1:4, color = "red")
  }
  
  ft <- bold(ft, i = which.max(ft1$mean), ~mean , bold = TRUE)
  
  ft <- ft %>%
    colformat_double(j = 2, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 3, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 3) 
  
  ft <- ft %>%
    compose(
      i = 2, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 
  ft <- ft %>%
    compose(
      i = 1, j = 5, value = as_paragraph(as_chunk(test, digits = 1), as_sup("f"))
    )
  
  # Schffe test
  # means는 행 제목의 알파벳 오름차순으로, groups는 score_total의 내림차순으로 정렬!!!
  # rownames는 label이 있을 때 1, 2, 3, 4가 아니라 label명으로 됨.
  # 따라서 factor를 integer로 바꿔서 rownames가 레이블이 아니라 1, 2, 4, 4로 되게 만든 후 rownames를 사용해야 함.
  # 이를 위해 xft를 integer로 변환한 후 scheffe test를 다시 한번 실시
  # factor가 아닌 character에서 xft가 NA로 되는 오류 발생
  # 따라서 다시 한번 수정
  # 문자의 경우에는 위와 같이 문자 알파벳 순서로 정렬되게 만들고
  # factor의 경우에는 원래의 순서로 되는 df를 만들고, scheffe 결과에서 그룹을 left_join으로 연결함
  # data <- tibble(xft = xft, yct = yct)
  # data <- data %>%
  # mutate(xft = as.integer(xft))
  # m <- aov(yct~xft, data = data)
  # sm <- summary(m)
  scheffe <- scheffe.test(m, "xft", group=TRUE, console = FALSE) # colsole = FALSE -> scheffe 결과를 출력하지 않도록 함
  scheffe_groups <- scheffe$groups
  if (is.factor(xft)) {
    xft_l <- tibble(xft_level = levels(xft))
    xft_g <- tibble(xft_level = rownames(scheffe_groups), xft_group = scheffe_groups$groups)
    xft_lg <- left_join(xft_l, xft_g, by = "xft_level")
    groups <- xft_lg$xft_group
  } else {
    groups <- scheffe_groups[order(rownames(scheffe_groups)),]$groups # mean순으로 되어 있는 것을 1, 2, 3, 4순으로 정렬하여 a, ab, b 등을 저장  
  }
  # 2021.2.20 1:4 -> 1:nxf로 수정
  if (p <= 0.05) {
    for (nn in 1:nxf) {
      ft <- ft %>%
        compose(
          i = nn, j = 3, value = as_paragraph(mean, as_sup(groups[nn]))
        ) 
    }
    
    ft <- ft %>%
      footnote(i = 1, j = c(3, 5),
               value = as_paragraph(
                 c(" Superscripts 'a', 'b', and 'c' denote subgroups in descending order according to Scheffe’s post hoc test. If mean statistics share the same superscript, then subgroups are homogeneous such that there are no significant mean differences. Otherwise, there are significant differences.",
                   "The superscript 'f' denotes a F-test comparing the mean of more than two groups.")
               ),
               ref_symbols = c("1)", "2)"),
               part = "header", FALSE) # 2021.7.16. F -> FALSE
  }
  ft <- autofit(ft)
  return(ft)
}

# ANOVA 검정 함수
my.ctables.anova <- function(xft = xft, yct = yct,
                         xft_name = NA, yct_name = NA, 
                         xft_level = NA, yct_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE, spacing = space) {
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[2]]))
  }

  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yct_name)) {
    yct_name <- label(yct)
  }

#  ifelse (label(data$xft) == "", xft_label <- "xft", xft_label <- label(data$xft))

  # yct의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yct_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장 (2021.2.18. 수정)
  if (is.na(xft_level[[1]])) { 
    if (is.factor(xft)) {
       xft_level <- levels(xft)
    } else {
      xft_level <- sort(unique(xft))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Mean", "Std.Dev", "Test statistics", "Total")  
  }
    
  if (is.na(caption[[1]])) {
#    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yct_name)
  }

  first_note <- "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) Superscripts 'a', 'b', and 'c' denote subgroups in descending order according to Scheffe’s post hoc test. If mean statistics share the same superscript, then subgroups are homogeneous such that there are no significant mean differences. Otherwise, there are significant differences."
  third_note <- "3) The number in the parenthesis is the p-value in a F-test comparing the mean of more than two groups."
  forth_note <- "4) The largest column mean is printed in boldface."

  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    third_note <- note[4]
  }


  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))

  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))

  ft2[1, 1] <- cell_name[5] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈

    
  ft <- rbind(ft1, ft2)

  nxf <- length(unique(xft)) # nlevels(xft)는 factor에만 적용되기 때문에 character에도 적용할 수 있도록 수정
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)

  # ANOVA
  
  m <- aov(yct~xft, data = data)
  sm <- summary(m)


  F <- sm[[1]]$`F value`[1]
  p <- sm[[1]]$`Pr(>F)`[1]
  df1 <- sm[[1]]$Df[1]
  df2 <- sm[[1]]$Df[2]
  
  asterisk <- ""
  if (p <= 0.01) {
    asterisk <- "***" } else if(p <= 0.05) {
    asterisk <- "**" } else if(p <= 0.10){
    asterisk <- "*"
  }

  if (nxf == 2) {
    col_stats <- tibble(test = c(NA, F, p))
  } else {
    col_stats <- tibble(test = c(NA, F, p, rep(NA, nxf - 2)))
  }

  # Schffe test
  # means는 행 제목의 알파벳 오름차순으로, groups는 score_total의 내림차순으로 정렬!!!
  # rownames는 label이 있을 때 1, 2, 3, 4가 아니라 label명으로 됨.
  # 따라서 factor를 integer로 바꿔서 rownames가 레이블이 아니라 1, 2, 4, 4로 되게 만든 후 rownames를 사용해야 함.
  # 이를 위해 xft를 integer로 변환한 후 scheffe test를 다시 한번 실시
  # factor가 아닌 character에서 xft가 NA로 되는 오류 발생
  # 따라서 다시 한번 수정
  # 문자의 경우에는 위와 같이 문자 알파벳 순서로 정렬되게 만들고
  # factor의 경우에는 원래의 순서로 되는 df를 만들고, scheffe 결과에서 그룹을 left_join으로 연결함
  # data <- tibble(xft = xft, yct = yct)
  # data <- data %>%
  # mutate(xft = as.integer(xft))
  # m <- aov(yct~xft, data = data)
  # sm <- summary(m)
  scheffe <- scheffe.test(m, "xft", group=TRUE, console = FALSE) # colsole = FALSE -> scheffe 결과를 출력하지 않도록 함
  scheffe_groups <- scheffe$groups
  
  ft <- cbind(ft, col_stats)

  ft <- ft %>%
    flextable (
      col_keys = c("rcell", "xft", "count", "mean", "std.dev", "test")
    )
  
  # 2021.2.18.수정
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    for (i in 1:nxf) {
      ft$body$content$content$data[[nxf+1+i]]$txt <- xft_level[i]
    }
  }
  
  if (!is.na(ans)) {
    ft <- color(ft, i = ans, j = 2:5, color = "red")
  }
  
  ft <- ft %>%
    colformat_double(j = 3, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 6, digits = 3) 

      
  ft <- ft %>%
    compose(
      i = 1, j = 6, value = as_paragraph("F", as_sub("("), as_sub(as.character(df1)), as_sub(", "), as_sub(as.character(df2)), as_sub(")"), " = ")
    )

  ft <- ft %>%
    compose(
      i = 2, j = 6, value = as_paragraph(as_chunk(test, digits = 1), as_sup(asterisk))
    )
  ft <- ft %>%
    compose(
      i = 3, j = 6, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 

  ft <- bold(ft, i = which.max(ft1$mean), ~mean , bold = TRUE)

# Scheffe 검정 결과에 따라 a, ab, b를 평균에 표시
  if (is.factor(xft)) {
    xft_l <- tibble(xft_level = levels(xft))
    xft_g <- tibble(xft_level = rownames(scheffe_groups), xft_group = scheffe_groups$groups)
    xft_lg <- left_join(xft_l, xft_g, by = "xft_level")
    groups <- xft_lg$xft_group
  } else {
    groups <- scheffe_groups[order(rownames(scheffe_groups)),]$groups # mean순으로 되어 있는 것을 1, 2, 3, 4순으로 정렬하여 a, ab, b 등을 저장  
  }
  # 2021.2.20 1:4 -> 1:nxf로 수정
  if (p <= 0.05) {
    for (i0 in 1:nxf) {
      ft <- ft %>%
         compose(
         i = i0, j = 4, value = as_paragraph(mean, as_sup(groups[i0]))
         ) 
    }
  }

  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:4) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[j]  
  }

# 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, xft = " ") # vname을 xft로 수정
  ft <- set_header_labels(ft, test = cell_name[4]) 

  header1 <- c("", yct_name, "")
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 3, 1))
  }

  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft <- set_caption(ft, caption = caption)

  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 6) %>%
      add_footer_row(values = third_note, colwidths = 6) %>%
      add_footer_row(values = second_note, colwidths = 6) %>%
      add_footer_row(values = first_note, colwidths = 6) 
  }  
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  if (!is.na(spacing)) {
    ft <- line_spacing(ft, space = spacing, part = "header") 
    ft <- line_spacing(ft, space = spacing, part = "body") 
    ft <- line_spacing(ft, space = spacing, part = "footer") 
  }
  ft <- autofit(ft)
  return(ft)
}


# ANOVA 검정 함수 (xaringan)
## 2021.7.16. F -> FALSE

my.ctables.anova.xaringan <- function(xft = xft, yct = yct,
                             xft_name = NA, yct_name = NA, 
                             xft_level = NA, yct_level = NA,
                             cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE, spacing = space) {
  data <- tibble(xft = xft, yct = yct)
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[2]]))
  }
  
  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yct_name)) {
    yct_name <- label(yct)
  }
  
  #  ifelse (label(data$xft) == "", xft_label <- "xft", xft_label <- label(data$xft))
  
  # yct의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yct_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장 (2021.2.18. 수정)
  if (is.na(xft_level[[1]])) { 
    if (is.factor(xft)) {
      xft_level <- levels(xft)
    } else {
      xft_level <- sort(unique(xft))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Mean", "Std.Dev", "Test statistics", "Total")  
  }
  
  if (is.na(caption[[1]])) {
    #    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yct_name)
  }
  
  first_note <- "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) Superscripts 'a', 'b', and 'c' denote subgroups in descending order according to Scheffe’s post hoc test. If mean statistics share the same superscript, then subgroups are homogeneous such that there are no significant mean differences. Otherwise, there are significant differences."
  third_note <- "3) The number in the parenthesis is the p-value in a F-test comparing the mean of more than two groups."
  forth_note <- "4) The largest column mean is printed in boldface."
  
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    third_note <- note[4]
  }
  
  
  ft1 <- data %>%
    group_by(xft) %>%
    summarise(count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft2 <- data %>%
    mutate_at(vars(xft), funs(as.character(.))) %>%
    summarise(xft = "Total", count = n(), mean = mean(yct), std.dev = sd(yct))
  
  ft2[1, 1] <- cell_name[5] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  
  
  ft <- rbind(ft1, ft2)
  
  nxf <- length(unique(xft)) # nlevels(xft)는 factor에만 적용되기 때문에 character에도 적용할 수 있도록 수정
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  
  # ANOVA
  
  m <- aov(yct~xft, data = data)
  sm <- summary(m)
  
  
  F <- sm[[1]]$`F value`[1]
  p <- sm[[1]]$`Pr(>F)`[1]
  df1 <- sm[[1]]$Df[1]
  df2 <- sm[[1]]$Df[2]
  
  asterisk <- ""
  if (p <= 0.01) {
    asterisk <- "\\*\\*\\*" } else if(p <= 0.05) {
      asterisk <- "\\*\\*" } else if(p <= 0.10){
        asterisk <- "\\*"
      }
  
  if (nxf == 2) {
    col_stats <- tibble(test = c(NA, F, p))
  } else {
    col_stats <- tibble(test = c(NA, F, p, rep(NA, nxf - 2)))
  }
  
  # Schffe test
  # means는 행 제목의 알파벳 오름차순으로, groups는 score_total의 내림차순으로 정렬!!!
  # rownames는 label이 있을 때 1, 2, 3, 4가 아니라 label명으로 됨.
  # 따라서 factor를 integer로 바꿔서 rownames가 레이블이 아니라 1, 2, 4, 4로 되게 만든 후 rownames를 사용해야 함.
  # 이를 위해 xft를 integer로 변환한 후 scheffe test를 다시 한번 실시
  # factor가 아닌 character에서 xft가 NA로 되는 오류 발생
  # 따라서 다시 한번 수정
  # 문자의 경우에는 위와 같이 문자 알파벳 순서로 정렬되게 만들고
  # factor의 경우에는 원래의 순서로 되는 df를 만들고, scheffe 결과에서 그룹을 left_join으로 연결함
  # data <- tibble(xft = xft, yct = yct)
  # data <- data %>%
  # mutate(xft = as.integer(xft))
  # m <- aov(yct~xft, data = data)
  # sm <- summary(m)
  scheffe <- scheffe.test(m, "xft", group=TRUE, console = FALSE) # colsole = FALSE -> scheffe 결과를 출력하지 않도록 함
  scheffe_groups <- scheffe$groups
  
  ft <- cbind(ft, col_stats)
  
  ft <- ft %>%
    flextable (
      col_keys = c("rcell", "xft", "count", "mean", "std.dev", "test")
    )
  
  # 2021.2.18.수정
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    for (i in 1:nxf) {
      ft$body$content$content$data[[nxf+1+i]]$txt <- xft_level[i]
    }
  }
  
  if (!is.na(ans)) {
    ft <- color(ft, i = ans, j = 2:5, color = "red")
  }
  
  ft <- ft %>%
    colformat_double(j = 3, digits = 0) 
  ft <- ft %>%
    colformat_double(j = 4, digits = 1) 
  ft <- ft %>%
    colformat_double(j = 5, digits = 2) 
  ft <- ft %>%
    colformat_double(j = 6, digits = 3) 
  
  
  ft <- ft %>%
    compose(
      i = 1, j = 6, value = as_paragraph("F", as_sub("("), as_sub(as.character(df1)), as_sub(", "), as_sub(as.character(df2)), as_sub(")"), " = ")
    )
  
  ft <- ft %>%
    compose(
      i = 2, j = 6, value = as_paragraph(as_chunk(test, digits = 1), as_sup(asterisk))
    )
  ft <- ft %>%
    compose(
      i = 3, j = 6, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 
  
  ft <- bold(ft, i = which.max(ft1$mean), ~mean , bold = TRUE)
  
  # Scheffe 검정 결과에 따라 a, ab, b를 평균에 표시
  if (is.factor(xft)) {
    xft_l <- tibble(xft_level = levels(xft))
    xft_g <- tibble(xft_level = rownames(scheffe_groups), xft_group = scheffe_groups$groups)
    xft_lg <- left_join(xft_l, xft_g, by = "xft_level")
    groups <- xft_lg$xft_group
  } else {
    groups <- scheffe_groups[order(rownames(scheffe_groups)),]$groups # mean순으로 되어 있는 것을 1, 2, 3, 4순으로 정렬하여 a, ab, b 등을 저장  
  }
  # 2021.2.20 1:4 -> 1:nxf로 수정
  if (p <= 0.05) {
    for (i0 in 1:nxf) {
      ft <- ft %>%
        compose(
          i = i0, j = 4, value = as_paragraph(mean, as_sup(groups[i0]))
        ) 
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:4) {
    ft$header$content$content$data[[j+2]]$txt <- cell_name[j]  
  }
  
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, xft = " ") # vname을 xft로 수정
  ft <- set_header_labels(ft, test = cell_name[4]) 
  
  header1 <- c("", yct_name, "")
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 3, 1))
  }
  
  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft <- set_caption(ft, caption = caption)
  
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 6) %>%
      add_footer_row(values = third_note, colwidths = 6) %>%
      add_footer_row(values = second_note, colwidths = 6) %>%
      add_footer_row(values = first_note, colwidths = 6) 
  }  
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  if (!is.na(spacing)) {
    ft <- line_spacing(ft, space = spacing, part = "header") 
    ft <- line_spacing(ft, space = spacing, part = "body") 
    ft <- line_spacing(ft, space = spacing, part = "footer") 
  }
  ft <- autofit(ft)
  return(ft)
}

# 독립성 검정 함수 - 프린트 옵션 전
my.ctables.p.before <- function(xft = xft, yft = yft, colpct = TRUE, rowpct = FALSE, 
                         xft_name = NA, yft_name = NA, 
                         xft_level = NA, yft_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE) {
  data <- tibble(xft = xft, yft = yft) 
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[1]]), !is.na(data[[2]]))
  }

  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yft_name)) {
    yft_name <- label(yft)
  }

  # yft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yft_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(yft_level[[1]])) { 
    if (is.factor(yft)) {
       yft_level <- levels(yft)
    } else {
      yft_level <- sort(unique(yft))
    }
  }

  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
    
  if (is.na(caption[[1]])) {
#    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yft_name)
  }

  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript *'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }

  ct <- table(xft, yft)
  ct.p <- prop.table(ct, margin = margin2)

  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
#  digit1 <- rep(0, nxf+1)
#  digit2 <- rep(3, nxf+1)

  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴

  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]

  colnames(ft) <- cnames
 
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)

  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter

  chi2_asterisk <- ""

  if (chi2_p <= 0.01) {
    chi2_asterisk <- "***" } else if(chi2_p <= 0.05) {
    chi2_asterisk <- "**" } else if(chi2_p <= 0.10){
    chi2_asterisk <- "*"
  }

  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))

  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)

  chisq.z <- as.data.frame(chisq$stdres)
  chisq.z <- chisq.z %>%
               pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
               select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1

  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "***"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "**"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "*"
    }
  }
  
  ft <- cbind(ft, col_stats)

  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }

  ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  

  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨

  if (!is.na(ans)) {
#    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
        ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    } else {
        ft <- color(ft, i = 1:(nxf+1), j = (2*ans+1):(2*ans+2), color = "red")
    }
  }

  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf+3, 2), digits = 0) 
  ft <- colformat_double(ft, j = 2*nyf+5, digits = 3) 
  
  
  # chi가 표시되지 않아 code로 만듦
  ft <- ft %>%
    compose(
      i = 1, j = 2*nyf+5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
      
    ) 

  ft <- ft %>%
    compose(
      i = 2, j = 2*nyf+5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
      
    ) 

  ft <- ft %>%
    compose(
      i = 3, j = 2*nyf+5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 

  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      if (chisq.bon.p[i0,j0] <= 0.05) {
        prop <- ct.p.sum_tibble[[i0,j0+1]]
        ft <- ft %>%
          compose(
            i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
           ) 
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf+2)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  ft <- set_header_labels(ft, test = cell_name[4]) 

  header1 <- c("", yft_name, "")
  header2 <- c("", yft_level, cell_name[3], "")

  ft <- add_header_row(ft, values = header2, colwidths = c(2, rep(2, nyf+1), 1))
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 2*nyf+2, 1))
  }
  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = third_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = second_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = first_note, colwidths = 2*nyf+5) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}


# 독립성 검정 함수 - 프린트 옵션 전 (xaringan)
my.ctables.p.before.xaringan <- function(xft = xft, yft = yft, colpct = TRUE, rowpct = FALSE, 
                                xft_name = NA, yft_name = NA, 
                                xft_level = NA, yft_level = NA,
                                cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE) {
  data <- tibble(xft = xft, yft = yft) 
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]), !is.na(data[[2]]))
  }
  
  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yft_name)) {
    yft_name <- label(yft)
  }
  
  # yft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yft_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(yft_level[[1]])) { 
    if (is.factor(yft)) {
      yft_level <- levels(yft)
    } else {
      yft_level <- sort(unique(yft))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    #    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yft_name)
  }
  
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript \\*'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ct <- table(xft, yft)
  ct.p <- prop.table(ct, margin = margin2)
  
  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
  #  digit1 <- rep(0, nxf+1)
  #  digit2 <- rep(3, nxf+1)
  
  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴
  
  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]
  
  colnames(ft) <- cnames
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "\\*\\*\\*" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "\\*\\*" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "\\*"
      }
  
  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
  
  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)
  
  chisq.z <- as.data.frame(chisq$stdres)
  chisq.z <- chisq.z %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
    select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1
  
  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "\\*\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "\\*"
    }
  }
  
  ft <- cbind(ft, col_stats)
  
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }
  
  ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  
  
  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
  
  if (!is.na(ans)) {
    #    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
      ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    } else {
      ft <- color(ft, i = 1:(nxf+1), j = (2*ans+1):(2*ans+2), color = "red")
    }
  }
  
  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf+3, 2), digits = 0) 
  ft <- colformat_double(ft, j = 2*nyf+5, digits = 3) 
  
  
  # chi가 표시되지 않아 code로 만듦
  ft <- ft %>%
    compose(
      i = 1, j = 2*nyf+5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
      
    ) 
  
  ft <- ft %>%
    compose(
      i = 2, j = 2*nyf+5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
      
    ) 
  
  ft <- ft %>%
    compose(
      i = 3, j = 2*nyf+5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 
  
  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      if (chisq.bon.p[i0,j0] <= 0.05) {
        prop <- ct.p.sum_tibble[[i0,j0+1]]
        ft <- ft %>%
          compose(
            i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
          ) 
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf+2)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  ft <- set_header_labels(ft, test = cell_name[4]) 
  
  header1 <- c("", yft_name, "")
  header2 <- c("", yft_level, cell_name[3], "")
  
  ft <- add_header_row(ft, values = header2, colwidths = c(2, rep(2, nyf+1), 1))
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 2*nyf+2, 1))
  }
  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = third_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = second_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = first_note, colwidths = 2*nyf+5) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}



# 독립성 검정 함수 (프린트 옵션 추가)

my.ctables.p <- function(xft = xft, yft = yft, colpct = TRUE, rowpct = FALSE, 
                         xft_name = NA, yft_name = NA, xft_level = NA, yft_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, 
                         note = NA, print_note = TRUE, print_stat = TRUE, print_xft = FALSE, 
                         print_Bonferroni = TRUE, print_row_total = TRUE, print_col_total = FALSE, spacing = NA) {
  data <- tibble(xft = xft, yft = yft) 
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[1]]), !is.na(data[[2]]))
  }

  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yft_name)) {
    yft_name <- label(yft)
  }

  # yft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yft_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(yft_level[[1]])) { 
    if (is.factor(yft)) {
       yft_level <- levels(yft)
    } else {
      yft_level <- sort(unique(yft))
    }
  }

  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
    
  if (is.na(caption[[1]])) {
#    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yft_name)
  }

  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript *'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }

  ct <- table(xft, yft)
  ct.p <- prop.table(ct, margin = margin2)

  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
#  digit1 <- rep(0, nxf+1)
#  digit2 <- rep(3, nxf+1)

  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴

  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]

  colnames(ft) <- cnames
 
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)

  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter

  chi2_asterisk <- ""

  if (chi2_p <= 0.01) {
    chi2_asterisk <- "***" } else if(chi2_p <= 0.05) {
    chi2_asterisk <- "**" } else if(chi2_p <= 0.10){
    chi2_asterisk <- "*"
  }

  #  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
    col_stats <- tibble(test = c(chi2, chi2_p, rep(NA, nxf - 1)))  # 2021.7.20. chi2 결과 표시를 한 줄 올림
  
  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)

  chisq.z <- as.data.frame(chisq$stdres)
  chisq.z <- chisq.z %>%
               pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
               select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1

  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "***"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "**"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "*"
    }
  }
  if (print_col_total){
    nyf1 <- nyf + 1
  } else {
    nyf1 <- nyf
  }
  ft <- ft[, c(1:(2*nyf1+2))] # column total 제외

  
  if (print_stat) {  
    ft <- cbind(ft, col_stats)
  }
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }
  if (print_row_total){
    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  }

    if (print_row_total){
    nxf1 <- nxf + 1
  } else {
    nxf1 <- nxf
  }

  ft <- ft[1:nxf1,]

  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨

  if (!is.na(ans[[1]])) {
#    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
        ft <- color(ft, i = ans, j = 2:(2*nyf1+2), color = "red")
    } else {
        if (length(ans) == 1) {
          ft <- color(ft, i = 1:(nxf1), j = (2*ans+1):(2*ans+2), color = "red") 
        } else {
          for (i0 in 1:length(ans)) {
            ft <- color(ft, i = i0, j = (2*ans[i0]+1):(2*ans[i0]+2), color = "red")
          }
        }
    }
  }

  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf1+1, 2), digits = 0) 
  if (print_stat) {
    ft <- colformat_double(ft, j = 2*nyf1+3, digits = 3) 
  }
  
  # chi가 표시되지 않아 code로 만듦
  if (print_stat) {
    ft <- ft %>%
      compose(
#        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ", as_chunk(chi2, digits = 1), as_sup(chi2_asterisk)) # 2021.7.20. chi2 결과를 한줄 올림
      ) 
# 2021.7.20. chi2 결과를 한줄 올리기 위해 아래는 주석 처리
#    ft <- ft %>%
#      compose(
#        i = 2, j = 2*nyf1+3, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
#      ) 
    ft <- ft %>%
      compose(
#        i = 3, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
        i = 2, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")") # 2021.7.20. chi2 결과를 한줄 올림
      ) 
  }
  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  if (print_Bonferroni) {
    for (i0 in 1:nxf) {
      for (j0 in 1:nyf) {
        if (chisq.bon.p[i0,j0] <= 0.05) {
          prop <- ct.p.sum_tibble[[i0,j0+1]]
          ft <- ft %>%
            compose(
              i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
             ) 
        }
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf1)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  
  if (print_stat) {
    ft <- set_header_labels(ft, test = cell_name[4]) 
  }
  
  if (print_stat) {
    header1 <- c("", yft_name, "")
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3], "")
    } else {
      header2 <- c("", yft_level, "")
    }
    colwidths1 <- c(2, 2*nyf1, 1)
    colwidths2 <- c(2, rep(2, nyf1), 1)
    footer_widths <- 2*nyf1+3
  } else {
    header1 <- c("", yft_name)
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3])
    } else {
      header2 <- c("", yft_level)
    }
    colwidths1 <- c(2, 2*nyf1)
    colwidths2 <- c(2, rep(2, nyf1))
    footer_widths <- 2*nyf1+2
  }
  
  ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
  }

  ft <- merge_at(ft, i = 1:(nxf1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  if (print_note) {
#  if (0) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = footer_widths) %>%
      add_footer_row(values = third_note, colwidths = footer_widths) %>%
      add_footer_row(values = second_note, colwidths = footer_widths) %>%
      add_footer_row(values = first_note, colwidths = footer_widths) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  if (!is.na(spacing)) {
    ft <- line_spacing(ft, space = spacing, part = "header") 
    ft <- line_spacing(ft, space = spacing, part = "body") 
    ft <- line_spacing(ft, space = spacing, part = "footer") 
  }
  ft <- autofit(ft)
  return(ft)
}


# 독립성 검정 함수 (프린트 옵션 추가) (xaringan)

my.ctables.p.xaringan <- function(xft = xft, yft = yft, colpct = TRUE, rowpct = FALSE, 
                         xft_name = NA, yft_name = NA, xft_level = NA, yft_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, 
                         note = NA, print_note = TRUE, print_stat = TRUE, print_xft = FALSE, 
                         print_Bonferroni = TRUE, print_row_total = TRUE, print_col_total = FALSE, spacing = NA) {
  data <- tibble(xft = xft, yft = yft) 
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]), !is.na(data[[2]]))
  }
  
  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  if (is.na(yft_name)) {
    yft_name <- label(yft)
  }
  
  # yft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yft_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(yft_level[[1]])) { 
    if (is.factor(yft)) {
      yft_level <- levels(yft)
    } else {
      yft_level <- sort(unique(yft))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    #    caption = label(data[[2]])
    caption = paste0(xft_name, " vs. ", yft_name)
  }
  
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript \\*'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ct <- table(xft, yft)
  ct.p <- prop.table(ct, margin = margin2)
  
  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
  #  digit1 <- rep(0, nxf+1)
  #  digit2 <- rep(3, nxf+1)
  
  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴
  
  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]
  
  colnames(ft) <- cnames
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "\\*\\*\\*" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "\\*\\*" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "\\*"
      }
  
#  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
  col_stats <- tibble(test = c(chi2, chi2_p, rep(NA, nxf - 1)))  # 2021.7.20. chi2 결과 표시를 한 줄 올림
  
  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)
  
  chisq.z <- as.data.frame(chisq$stdres)
  chisq.z <- chisq.z %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
    select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1
  
  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "\\*\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "\\*"
    }
  }
  if (print_col_total){
    nyf1 <- nyf + 1
  } else {
    nyf1 <- nyf
  }
  ft <- ft[, c(1:(2*nyf1+2))] # column total 제외
  
  
  if (print_stat) {  
    ft <- cbind(ft, col_stats)
  }
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }
  if (print_row_total){
    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  }
  
  if (print_row_total){
    nxf1 <- nxf + 1
  } else {
    nxf1 <- nxf
  }
  
  ft <- ft[1:nxf1,]
  
  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
  
  if (!is.na(ans[[1]])) {
    #    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
      ft <- color(ft, i = ans, j = 2:(2*nyf1+2), color = "red")
    } else {
      if (length(ans) == 1) {
        ft <- color(ft, i = 1:(nxf1), j = (2*ans+1):(2*ans+2), color = "red") 
      } else {
        for (i0 in 1:length(ans)) {
          ft <- color(ft, i = i0, j = (2*ans[i0]+1):(2*ans[i0]+2), color = "red")
        }
      }
    }
  }
  
  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf1+1, 2), digits = 0) 
  if (print_stat) {
    ft <- colformat_double(ft, j = 2*nyf1+3, digits = 3) 
  }
  
  # chi가 표시되지 않아 code로 만듦
  if (print_stat) {
    ft <- ft %>%
      compose(
#        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ", as_chunk(chi2, digits = 1), as_sup(chi2_asterisk)) # 2021.7.20. chi2 결과를 한줄 올림
      ) 
# 2021.7.20. chi2 결과를 한줄 올리기 위해 아래는 주석 처리
#    ft <- ft %>%
#      compose(
#        i = 2, j = 2*nyf1+3, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
#      ) 
    ft <- ft %>%
      compose(
#        i = 3, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
        i = 2, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")") # 2021.7.20. chi2 결과를 한줄 올림
      ) 
  }
  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  if (print_Bonferroni) {
    for (i0 in 1:nxf) {
      for (j0 in 1:nyf) {
        if (chisq.bon.p[i0,j0] <= 0.05) {
          prop <- ct.p.sum_tibble[[i0,j0+1]]
          ft <- ft %>%
            compose(
              i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
            ) 
        }
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf1)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  
  if (print_stat) {
    ft <- set_header_labels(ft, test = cell_name[4]) 
  }
  
  if (print_stat) {
    header1 <- c("", yft_name, "")
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3], "")
    } else {
      header2 <- c("", yft_level, "")
    }
    colwidths1 <- c(2, 2*nyf1, 1)
    colwidths2 <- c(2, rep(2, nyf1), 1)
    footer_widths <- 2*nyf1+3
  } else {
    header1 <- c("", yft_name)
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3])
    } else {
      header2 <- c("", yft_level)
    }
    colwidths1 <- c(2, 2*nyf1)
    colwidths2 <- c(2, rep(2, nyf1))
    footer_widths <- 2*nyf1+2
  }
  
  ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
  }
  
  ft <- merge_at(ft, i = 1:(nxf1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  #  if (print_note) {
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = footer_widths) %>%
      add_footer_row(values = third_note, colwidths = footer_widths) %>%
      add_footer_row(values = second_note, colwidths = footer_widths) %>%
      add_footer_row(values = first_note, colwidths = footer_widths) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  if (!is.na(spacing)) {
    ft <- line_spacing(ft, space = spacing, part = "header") 
    ft <- line_spacing(ft, space = spacing, part = "body") 
    ft <- line_spacing(ft, space = spacing, part = "footer") 
  }
  ft <- autofit(ft)
  return(ft)
}




# 독립성 검정 함수 - 표 자료 이용 - 프린트 옵션 전

my.ctables.p.from.table.before <- function(ct, colpct = TRUE, rowpct = FALSE, 
                         xft_name = NA, yft_name = NA, 
                         xft_level = NA, yft_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE) {
  dm <- dimnames(ct)
  
  if (is.na(xft_name)) {
    xft_name <- names(dm[1])
  }
  if (is.na(yft_name)) {
    yft_name <- names(dm[2])
  }

  if (is.na(xft_level[[1]])) { 
    xft_level <- dm[[1]]
  }

  if (is.na(yft_level[[1]])) { 
    yft_level <- dm[[2]]
  }

  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
    
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name, " vs. ", yft_name)
  }

  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript *'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }

  ct.p <- prop.table(ct, margin = margin2)

  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
#  digit1 <- rep(0, nxf+1)
#  digit2 <- rep(3, nxf+1)

  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
 
  ct.sum_tibble <- as_tibble(ct.sum)
  colnames(ct.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  colnames(ct.p.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
 
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴

  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]

  colnames(ft) <- cnames
 
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)

  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter

  chi2_asterisk <- ""

  if (chi2_p <= 0.01) {
    chi2_asterisk <- "***" } else if(chi2_p <= 0.05) {
    chi2_asterisk <- "**" } else if(chi2_p <= 0.10){
    chi2_asterisk <- "*"
  }

  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))

  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)

  chisq.z <- as.data.frame(chisq$stdres)
  colnames(chisq.z) <- c("xft", "yft", "Freq")  # table로 읽어들일 때 수정한 부분
  chisq.z <- chisq.z %>%
               pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
               select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1

  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "***"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "**"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "*"
    }
  }
  
  ft <- cbind(ft, col_stats)

  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }

  ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  

  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨

  if (!is.na(ans)) {
#    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
        ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    } else {
        ft <- color(ft, i = 1:(nxf+1), j = (2*ans+1):(2*ans+2), color = "red")
    }
  }
  
  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf+3, 2), digits = 0) 
  ft <- colformat_double(ft, j = 2*nyf+5, digits = 3) 

  # chi가 표시되지 않아 code로 만듦
  ft <- ft %>%
    compose(
      i = 1, j = 2*nyf+5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
      
    ) 

  ft <- ft %>%
    compose(
      i = 2, j = 2*nyf+5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
      
    ) 

  ft <- ft %>%
    compose(
      i = 3, j = 2*nyf+5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 

  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      if (chisq.bon.p[i0,j0] <= 0.05) {
        prop <- ct.p.sum_tibble[[i0,j0+1]]
        ft <- ft %>%
          compose(
            i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
           ) 
      }
    }
  }

  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf+2)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }

  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  ft <- set_header_labels(ft, test = cell_name[4]) 

  header1 <- c("", yft_name, "")
  header2 <- c("", yft_level, cell_name[3], "")
  
  ft <- add_header_row(ft, values = header2, colwidths = c(2, rep(2, nyf+1), 1))
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 2*nyf+2, 1))
  }
  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = third_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = second_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = first_note, colwidths = 2*nyf+5) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}


# 독립성 검정 함수 - 표 자료 이용 - 프린트 옵션 전

my.ctables.p.from.table.before.xaringan <- function(ct, colpct = TRUE, rowpct = FALSE, 
                                           xft_name = NA, yft_name = NA, 
                                           xft_level = NA, yft_level = NA,
                                           cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_xft = TRUE) {
  dm <- dimnames(ct)
  
  if (is.na(xft_name)) {
    xft_name <- names(dm[1])
  }
  if (is.na(yft_name)) {
    yft_name <- names(dm[2])
  }
  
  if (is.na(xft_level[[1]])) { 
    xft_level <- dm[[1]]
  }
  
  if (is.na(yft_level[[1]])) { 
    yft_level <- dm[[2]]
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name, " vs. ", yft_name)
  }
  
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript \\*'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ct.p <- prop.table(ct, margin = margin2)
  
  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
  #  digit1 <- rep(0, nxf+1)
  #  digit2 <- rep(3, nxf+1)
  
  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  colnames(ct.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  colnames(ct.p.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴
  
  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]
  
  colnames(ft) <- cnames
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "\\*\\*\\*" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "\\*\\*" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "\\*"
      }
  
  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
  
  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)
  
  chisq.z <- as.data.frame(chisq$stdres)
  colnames(chisq.z) <- c("xft", "yft", "Freq")  # table로 읽어들일 때 수정한 부분
  chisq.z <- chisq.z %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
    select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1
  
  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "\\*\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "\\*"
    }
  }
  
  ft <- cbind(ft, col_stats)
  
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }
  
  ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  
  
  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
  
  if (!is.na(ans)) {
    #    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
      ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    } else {
      ft <- color(ft, i = 1:(nxf+1), j = (2*ans+1):(2*ans+2), color = "red")
    }
  }
  
  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf+3, 2), digits = 0) 
  ft <- colformat_double(ft, j = 2*nyf+5, digits = 3) 
  
  # chi가 표시되지 않아 code로 만듦
  ft <- ft %>%
    compose(
      i = 1, j = 2*nyf+5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
      
    ) 
  
  ft <- ft %>%
    compose(
      i = 2, j = 2*nyf+5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
      
    ) 
  
  ft <- ft %>%
    compose(
      i = 3, j = 2*nyf+5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
    ) 
  
  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      if (chisq.bon.p[i0,j0] <= 0.05) {
        prop <- ct.p.sum_tibble[[i0,j0+1]]
        ft <- ft %>%
          compose(
            i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
          ) 
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf+2)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  ft <- set_header_labels(ft, test = cell_name[4]) 
  
  header1 <- c("", yft_name, "")
  header2 <- c("", yft_level, cell_name[3], "")
  
  ft <- add_header_row(ft, values = header2, colwidths = c(2, rep(2, nyf+1), 1))
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = c(2, 2*nyf+2, 1))
  }
  ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = third_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = second_note, colwidths = 2*nyf+5) %>%
      add_footer_row(values = first_note, colwidths = 2*nyf+5) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}



# 독립성 검정 함수 - 표 자료 이용 - 프린트 옵션 추가

#my.ctables.p.from.table.before <- function(ct, colpct = TRUE, rowpct = FALSE, 
#                         xft_name = NA, yft_name = NA, 
#                         xft_level = NA, yft_level = NA,
#                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption #= caption, note = note, print_note = TRUE, print_xft = TRUE) {


my.ctables.p.from.table <- function(ct = ct, colpct = TRUE, rowpct = FALSE, 
                                    xft_name = NA, yft_name = NA, 
                                    xft_level = NA, yft_level = NA,
                                    cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_stat = TRUE, print_xft = TRUE, print_Bonferroni = TRUE, print_row_total = TRUE, print_col_total = FALSE) {
  
  dm <- dimnames(ct)
  
  if (is.na(xft_name)) {
    xft_name <- names(dm[1])
  }
  if (is.na(yft_name)) {
    yft_name <- names(dm[2])
  }
  
  if (is.na(xft_level[[1]])) { 
    xft_level <- dm[[1]]
  }
  
  if (is.na(yft_level[[1]])) { 
    yft_level <- dm[[2]]
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name, " vs. ", yft_name)
  }
  
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript \\*'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ct.p <- prop.table(ct, margin = margin2)
  
  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
  #  digit1 <- rep(0, nxf+1)
  #  digit2 <- rep(3, nxf+1)
  
  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  colnames(ct.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  colnames(ct.p.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴
  
  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]
  
  colnames(ft) <- cnames
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "\\*\\*\\*" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "\\*\\*" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "\\*"
      }
  
#  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
  col_stats <- tibble(test = c(chi2, chi2_p, rep(NA, nxf - 1)))  # 2021.7.20. chi2 결과 표시를 한 줄 올림
  
  
  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)
  # 아래는 표로 읽어들이기 때문에 명령어가 다름
  chisq.z <- as.data.frame(chisq$stdres)
  colnames(chisq.z) <- c("xft", "yft", "Freq")  # table로 읽어들일 때 수정한 부분
  chisq.z <- chisq.z %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
    select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1
  
  
  
  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "\\*\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "\\*\\*"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "\\*"
    }
  }
  if (print_col_total){
    nyf1 <- nyf + 1
  } else {
    nyf1 <- nyf
  }
  ft <- ft[, c(1:(2*nyf1+2))] # column total 제외
  
  
  if (print_stat) {  
    ft <- cbind(ft, col_stats)
  }
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }
  if (print_row_total){
    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  }
  
  if (print_row_total){
    nxf1 <- nxf + 1
  } else {
    nxf1 <- nxf
  }
  
  ft <- ft[1:nxf1,]
  
  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
  
  if (!is.na(ans[[1]])) {
    #    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
      ft <- color(ft, i = ans, j = 2:(2*nyf1+2), color = "red")
    } else {
      if (length(ans) == 1) {
        ft <- color(ft, i = 1:(nxf1), j = (2*ans+1):(2*ans+2), color = "red") 
      } else {
        for (i0 in 1:length(ans)) {
          ft <- color(ft, i = i0, j = (2*ans[i0]+1):(2*ans[i0]+2), color = "red")
        }
      }
    }
  }
  
  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf1+1, 2), digits = 0) 
  if (print_stat) {
    ft <- colformat_double(ft, j = 2*nyf1+3, digits = 3) 
  }
  
  # chi가 표시되지 않아 code로 만듦
  if (print_stat) {
    ft <- ft %>%
      compose(
        #        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ", as_chunk(chi2, digits = 1), as_sup(chi2_asterisk)) # 2021.7.20. chi2 결과를 한줄 올림
      ) 
    # 2021.7.20. chi2 결과를 한줄 올리기 위해 아래는 주석 처리
    #    ft <- ft %>%
    #      compose(
    #        i = 2, j = 2*nyf1+3, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
    #      ) 
    ft <- ft %>%
      compose(
        #        i = 3, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
        i = 2, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")") # 2021.7.20. chi2 결과를 한줄 올림
      ) 
  }
  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  if (print_Bonferroni) {
    for (i0 in 1:nxf) {
      for (j0 in 1:nyf) {
        if (chisq.bon.p[i0,j0] <= 0.05) {
          prop <- ct.p.sum_tibble[[i0,j0+1]]
          ft <- ft %>%
            compose(
              i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
            ) 
        }
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf1)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  
  if (print_stat) {
    ft <- set_header_labels(ft, test = cell_name[4]) 
  }
  
  if (print_stat) {
    header1 <- c("", yft_name, "")
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3], "")
    } else {
      header2 <- c("", yft_level, "")
    }
    colwidths1 <- c(2, 2*nyf1, 1)
    colwidths2 <- c(2, rep(2, nyf1), 1)
    footer_widths <- 2*nyf1+3
  } else {
    header1 <- c("", yft_name)
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3])
    } else {
      header2 <- c("", yft_level)
    }
    colwidths1 <- c(2, 2*nyf1)
    colwidths2 <- c(2, rep(2, nyf1))
    footer_widths <- 2*nyf1+2
  }
  
  ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
  }
  
  ft <- merge_at(ft, i = 1:(nxf1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  #  if (print_note) {
  if (0) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = footer_widths) %>%
      add_footer_row(values = third_note, colwidths = footer_widths) %>%
      add_footer_row(values = second_note, colwidths = footer_widths) %>%
      add_footer_row(values = first_note, colwidths = footer_widths) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}




# 독립성 검정 함수 - 표 자료 이용 - 프린트 옵션 추가

#my.ctables.p.from.table.before.xaringan <- function(ct, colpct = TRUE, rowpct = FALSE, 
#                         xft_name = NA, yft_name = NA, 
#                         xft_level = NA, yft_level = NA,
#                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption #= caption, note = note, print_note = TRUE, print_xft = TRUE) {


my.ctables.p.from.table.xaringan <- function(ct = ct, colpct = TRUE, rowpct = FALSE, 
                                    xft_name = NA, yft_name = NA, 
                                    xft_level = NA, yft_level = NA,
                                    cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_stat = TRUE, print_xft = TRUE, print_Bonferroni = TRUE, print_row_total = TRUE, print_col_total = FALSE) {
  
  dm <- dimnames(ct)
  
  if (is.na(xft_name)) {
    xft_name <- names(dm[1])
  }
  if (is.na(yft_name)) {
    yft_name <- names(dm[2])
  }
  
  if (is.na(xft_level[[1]])) { 
    xft_level <- dm[[1]]
  }
  
  if (is.na(yft_level[[1]])) { 
    yft_level <- dm[[2]]
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name, " vs. ", yft_name)
  }
  
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  if (colpct == TRUE) {
    margin1 <- 1
    margin2 <- 2
  } else if (rowpct == TRUE) {
    margin1 <- 2
    margin2 <- 1
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript *'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ct.p <- prop.table(ct, margin = margin2)
  
  nxf <- dim(ct)[1] # Total 제외
  nyf <- dim(ct)[2] # Total 제외
  
  #  digit1 <- rep(0, nxf+1)
  #  digit2 <- rep(3, nxf+1)
  
  ct.sum <- addmargins(ct, margin = c(1,2))
  ct.sum_2 <- addmargins(ct, margin = margin2)
  ct.p.sum_2 <- prop.table(ct.sum_2, margin = margin2)
  ct.p.sum <- addmargins(ct.p.sum_2, margin = margin1)
  
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum)
  colnames(ct.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.sum_tibble <- ct.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ct.p.sum_tibble <- as_tibble(ct.p.sum)
  colnames(ct.p.sum_tibble) <- c("xft", "yft", "n")  # table로 읽어들일 때 수정한 부분
  ct.p.sum_tibble <- ct.p.sum_tibble %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = n)
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  sel_index <- rbind(c(2:(nyf+2)), c((nyf+3):(2*(nyf+1)+1)))
  sel_index <- as.vector(sel_index)
  
  ft <- ft[, c(1, sel_index)]
  
  rcell <- rep(xft_name, nxf+1)
  ft <- cbind(rcell, ft)
  sel_index <- sel_index + 1 # rcell을 추가하여 인덱스 번호도 1씩 증가시킴
  
  cnames <- c("rcell", "vname", paste0("count", 1:nyf), "count_t", paste0("prop", 1:nyf), "prop_t")
  cnames <- cnames[c(1, 2, sel_index)]
  
  colnames(ft) <- cnames
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "***" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "**" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "*"
      }
  
#  col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
  col_stats <- tibble(test = c(chi2, chi2_p, rep(NA, nxf - 1)))  # 2021.7.20. chi2 결과 표시를 한 줄 올림
  
  
  # Bonferroni Test
  # Chisqure 검정의 z matrix를 data frame으로 변환 후 Bonnferroni p-value를 계산
  # 이는 chisq.posthoc.test에서 계산한 아래 코드의 chisq.ph.p와 동일함
  # chisq.ph <- chisq.posthoc.test(ct, method = "bonferroni")
  # chisq.ph.z <- chisq.ph %>%
  #   filter(Value == "Residuals") %>%
  #   select(-Dimension, -Value)
  # chisq.ph.p <- chisq.ph %>%
  #   filter(Value == "p values") %>%
  #   select(-Dimension, -Value)
  # 아래는 표로 읽어들이기 때문에 명령어가 다름
  chisq.z <- as.data.frame(chisq$stdres)
  colnames(chisq.z) <- c("xft", "yft", "Freq")  # table로 읽어들일 때 수정한 부분
  chisq.z <- chisq.z %>%
    pivot_wider(id_cols = xft, names_from = yft, values_from = Freq) %>%
    select(-xft)
  chisq.bon.p <- map_df(chisq.z, function(x) x <- 2*(1-pnorm(abs(x)))*nxf*nyf)
  chisq.bon.p[chisq.bon.p >= 1] <- 1
  
  
  
  # 유의수준에 따라 *를 저장  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (chisq.bon.p[i,j] <= 0.01) asterisk[i,j] <- "***"
      else if (chisq.bon.p[i,j] <= 0.05) asterisk[i,j] <- "**"
      else if (chisq.bon.p[i,j] <= 0.10) asterisk[i,j] <- "*"
    }
  }
  if (print_col_total){
    nyf1 <- nyf + 1
  } else {
    nyf1 <- nyf
  }
  ft <- ft[, c(1:(2*nyf1+2))] # column total 제외
  
  
  if (print_stat) {  
    ft <- cbind(ft, col_stats)
  }
  # xft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 xft_level을 함수 밖에서 정의 요망
  # NA 여부를 판단하기 위해서는 [[1]]을 이용하여 scalar로 해야 함
  if (!is.na(xft_level[[1]])) { 
    ft[1:nxf, 2] <- xft_level
  }
  if (print_row_total){
    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
  }
  
  if (print_row_total){
    nxf1 <- nxf + 1
  } else {
    nxf1 <- nxf
  }
  
  ft <- ft[1:nxf1,]
  
  ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
  
  if (!is.na(ans[[1]])) {
    #    ft <- color(ft, i = ans, j = 2:(2*nyf+4), color = "red")
    if (colpct) {
      ft <- color(ft, i = ans, j = 2:(2*nyf1+2), color = "red")
    } else {
      if (length(ans) == 1) {
        ft <- color(ft, i = 1:(nxf1), j = (2*ans+1):(2*ans+2), color = "red") 
      } else {
        for (i0 in 1:length(ans)) {
          ft <- color(ft, i = i0, j = (2*ans[i0]+1):(2*ans[i0]+2), color = "red")
        }
      }
    }
  }
  
  ft <- colformat_double(ft, digits = 1) 
  ft <- colformat_double(ft, j = seq(3, 2*nyf1+1, 2), digits = 0) 
  if (print_stat) {
    ft <- colformat_double(ft, j = 2*nyf1+3, digits = 3) 
  }
  
  # chi가 표시되지 않아 code로 만듦
  if (print_stat) {
    ft <- ft %>%
      compose(
        #        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
        i = 1, j = 2*nyf1+3, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ", as_chunk(chi2, digits = 1), as_sup(chi2_asterisk)) # 2021.7.20. chi2 결과를 한줄 올림
      ) 
    # 2021.7.20. chi2 결과를 한줄 올리기 위해 아래는 주석 처리
    #    ft <- ft %>%
    #      compose(
    #        i = 2, j = 2*nyf1+3, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
    #      ) 
    ft <- ft %>%
      compose(
        #        i = 3, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
        i = 2, j = 2*nyf1+3, value = as_paragraph("(", as_chunk(test, digits = 3), ")") # 2021.7.20. chi2 결과를 한줄 올림
      ) 
  }
  # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
  if (colpct) {
    for (j0 in 1:nyf) {
      ft <- bold(ft, i = which.max(ct.p[, j0]), j = 2*j0+2, bold = TRUE)
    }
  } else if (rowpct) {
    for (i0 in 1:nxf) {
      ft <- bold(ft, i = i0, j = 2*which.max(ct.p[i0, ])+2, bold = TRUE)
    }
  }
  # Bonferroni 검정 결과에 따라 *를 비율에 표시
  if (print_Bonferroni) {
    for (i0 in 1:nxf) {
      for (j0 in 1:nyf) {
        if (chisq.bon.p[i0,j0] <= 0.05) {
          prop <- ct.p.sum_tibble[[i0,j0+1]]
          ft <- ft %>%
            compose(
              i = i0, j = 2*j0+2, value = as_paragraph(sprintf("%0.1f", prop), as_sup(asterisk[i0,j0]))            
            ) 
        }
      }
    }
  }
  
  # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
  for (j in 1:(2*nyf1)) {
    if (j %% 2 == 1) {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[1]  
    } else {
      ft$header$content$content$data[[j+2]]$txt <- cell_name[2]
    }
  }
  # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
  ft <- set_caption(ft, caption = caption)
  ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
  ft <- set_header_labels(ft, vname = " ") 
  
  if (print_stat) {
    ft <- set_header_labels(ft, test = cell_name[4]) 
  }
  
  if (print_stat) {
    header1 <- c("", yft_name, "")
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3], "")
    } else {
      header2 <- c("", yft_level, "")
    }
    colwidths1 <- c(2, 2*nyf1, 1)
    colwidths2 <- c(2, rep(2, nyf1), 1)
    footer_widths <- 2*nyf1+3
  } else {
    header1 <- c("", yft_name)
    if (print_col_total) {
      header2 <- c("", yft_level, cell_name[3])
    } else {
      header2 <- c("", yft_level)
    }
    colwidths1 <- c(2, 2*nyf1)
    colwidths2 <- c(2, rep(2, nyf1))
    footer_widths <- 2*nyf1+2
  }
  
  ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
  if (header){
    ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
  }
  
  ft <- merge_at(ft, i = 1:(nxf1), j = 1)
  ft <- theme_booktabs(ft)
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  #  if (print_note) {
  if (0) {
    ft <- ft %>%
      add_footer_row(values = forth_note, colwidths = footer_widths) %>%
      add_footer_row(values = third_note, colwidths = footer_widths) %>%
      add_footer_row(values = second_note, colwidths = footer_widths) %>%
      add_footer_row(values = first_note, colwidths = footer_widths) 
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}




# 적합도 검정

my.ctables.gf <- function(xft = xft, colpct = TRUE, rowpct = FALSE, 
                         xft_name = NA, xft_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_stat = TRUE, print_xft = FALSE) {

 data <- tibble(xft = xft) 
  if (na.rm == TRUE) {
    data <- data %>% 
    filter(!is.na(data[[1]]))
  }

  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }

 
   # yft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yft_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(xft_level[[1]])) { 
    if (is.factor(xft)) {
       xft_level <- levels(xft)
    } else {
      xft_level <- sort(unique(xft))
    }
  }

  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
    
  if (is.na(caption[[1]])) {
#    caption = label(data[[2]])
    caption = paste0(xft_name)
  }

  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript *'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }

  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
    
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name)
  }
  # colpct를 가정하고 작업한 후, rowpct일 때 나중에 transpose 처리.
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare goodness of fit test."
  if (colpct == TRUE) {
    third_note <- "3) The highest column proportion is printed in boldface."
  } else {
    third_note <- "3) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  ct <- data %>%
    table() %>%
    apply(1, function (x) round(x, 0))
  ct.p <- ct %>%
    prop.table()*100 # 퍼센트로 변환

  # 2021.2.18. zero.rm 추가 요망
  if (!is.na(xft_level[[1]])) { 
    nxf <- length(xft_level)
  } else {
    nxf <- length(unique(xft)) # nlevels(xft)는 factor에만 적용되기 때문에 character에도 적용할 수 있도록 수정
  }
  
  ct_p <- rbind(ct, ct.p)
  ct_p.sum <- addmargins(ct_p, margin=2)
  ct_p.sum <- t(ct_p.sum)
  ft <- as_tibble(ct_p.sum)

  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)

  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter

  chi2_asterisk <- ""

  if (chi2_p <= 0.01) {
    chi2_asterisk <- "***" } else if(chi2_p <= 0.05) {
    chi2_asterisk <- "**" } else if(chi2_p <= 0.10){
    chi2_asterisk <- "*"
  }

  if (colpct) {
#    if (xft_name == "") {
    if (xft_name == "" | is.na(xft_name)) {
      xft_name <- "xft" # 뒤에서 left_join을 하기 위해 데이터가 있어야 함
    }
    rcell <- rep(xft_name, nxf+1)
    vname <- c(as.character(xft_level), "Total") ### 2021.2.14
    ft <- cbind(rcell, vname, ft)
    cnames <- c("rcell", "vname", "count", "prop")
    colnames(ft) <- cnames
    col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
    if (!is.na(xft_level[[1]])) { 
      ft[1:nxf, 2] <- as.character(xft_level)
    }

    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
if (print_stat) {
    ft <- cbind(ft, col_stats)
}
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨

    if (!is.na(ans)) {
      ft <- color(ft, i = ans, j = 2:4, color = "red")
    }

    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = 3, digits = 0) 

    # chi가 표시되지 않아 code로 만듦
if (print_stat) {
      ft <- ft %>%
      compose(
        i = 1, j = 5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
      ) 
    ft <- ft %>%
      compose(
        i = 2, j = 5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
      ) 

    ft <- ft %>%
      compose(
        i = 3, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
      ) 
}
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = which.max(ct.p), j = 4, bold = TRUE)
    
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    ft$header$content$content$data[[3]]$txt <- cell_name[1]  
    ft$header$content$content$data[[4]]$txt <- cell_name[2]  

    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
    ft <- set_header_labels(ft, vname = " ") 
if (print_stat) {
    ft <- set_header_labels(ft, test = cell_name[4]) 
}
if (print_stat) {
    footer_widths <- 5
} else {
    footer_widths <- 4
}
    ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
     }
  } else {
#    if (xft_name == "") {
    if (xft_name == "" | is.na(xft_name)) {
      xft_name <- "xft" # 뒤에서 left_join을 하기 위해 데이터가 있어야 함
    }
    rcell <- rep(xft_name, nxf+1)
ft <- as_tibble(ct_p.sum)    
    vname <- c(as.character(xft_level), "Total")
    ft <- cbind(rcell, vname, ft)
    
    ft.n <- ft %>%
      select(rcell, vname, ct) %>%
      pivot_wider(id_cols = rcell, names_from = vname, values_from = ct)
    ft.prop <- ft %>%
      select(rcell, vname, ct.p) %>%
      pivot_wider(id_cols = rcell, names_from = vname, values_from = ct.p)
    ft <- left_join(ft.n, ft.prop, by = "rcell") 
    sel_index <- rbind(c(2:(nxf+2)), c((nxf+3):(2*(nxf+1)+1)))
    sel_index <- as.vector(sel_index)
  
    ft <- ft[, c(1, sel_index)]

    cnames <- c("rcell", paste0("count", 1:nxf), "count_t", paste0("prop", 1:nxf), "prop_t")
    cnames <- cnames[c(1, sel_index)]

    colnames(ft) <- cnames
    col_stats <- tibble(test = chi2)
if (print_stat) {
    ft <- cbind(ft, col_stats)
}    
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
    if (!is.na(ans)) {
      ft <- color(ft, i = 1, j = (2*ans):(2*ans+1), color = "red")
    }
    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = seq(2, 2*nxf+3, 2), digits = 0) 

    # chi가 표시되지 않아 code로 만듦
if (print_stat) {
  ft <- ft %>%
      compose(
        i = 1, j = 2*nxf+4, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk), " (", as_chunk(chi2_p, digits = 3), ")")
      ) 
}
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = 1, j = 2*which.max(ct.p)+1, bold = TRUE)
    
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    for (j in 1:(2*nxf+2)) {
      if (j %% 2 == 1) {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[1]  
      } else {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[2]
      }
    }

    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
if (print_stat) {    
    ft <- set_header_labels(ft, test = cell_name[4]) 
}
if (print_stat) {
    header1 <- c("", xft_name, "")
    header2 <- c("", as.character(xft_level), cell_name[3], "")
    colwidths1 <- c(1, 2*nxf+2, 1)
    colwidths2 <- c(1, rep(2, nxf+1), 1)
    footer_widths <- 2*nxf+4
} else {
    header1 <- c("", xft_name)
    header2 <- c("", as.character(xft_level), cell_name[3])
    colwidths1 <- c(1, 2*nxf+2)
    colwidths2 <- c(1, rep(2, nxf+1))
    footer_widths <- 2*nxf+3
}

    ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
    if (header){
      ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
    }
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
    }
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}


# 적합도 검정

my.ctables.gf.xaringan <- function(xft = xft, colpct = TRUE, rowpct = FALSE, 
                          xft_name = NA, xft_level = NA,
                          cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = NA, note = NA, print_note = TRUE, print_stat = TRUE, print_xft = FALSE) {
  
  data <- tibble(xft = xft) 
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]))
  }
  
  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  
  
  # yft의 셀 제목을 지정. 단 factor가 아닌 경우에는 알파벳 순으로 했으니 그렇지 않을 경우 yft_level을 함수 밖에서 정의 요망
  # xft의 셀 제목은 뒤에서 테이블을 모두 만든 후에 다시 저장
  if (is.na(xft_level[[1]])) { 
    if (is.factor(xft)) {
      xft_level <- levels(xft)
    } else {
      xft_level <- sort(unique(xft))
    }
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    #    caption = label(data[[2]])
    caption = paste0(xft_name)
  }
  
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare independence test."
  third_note <- "3) The proportion with superscript \\*'s denotes that the corresponding group is significantly different from other groups according to the Bonferroni post hoc test."
  if (colpct == TRUE) {
    forth_note <- "4) The highest column proportion is printed in boldface."
  } else {
    forth_note <- "4) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name)
  }
  # colpct를 가정하고 작업한 후, rowpct일 때 나중에 transpose 처리.
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare goodness of fit test."
  if (colpct == TRUE) {
    third_note <- "3) The highest column proportion is printed in boldface."
  } else {
    third_note <- "3) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  ct <- data %>%
    table() %>%
    apply(1, function (x) round(x, 0))
  ct.p <- ct %>%
    prop.table()*100 # 퍼센트로 변환
  
  # 2021.2.18. zero.rm 추가 요망
  if (!is.na(xft_level[[1]])) { 
    nxf <- length(xft_level)
  } else {
    nxf <- length(unique(xft)) # nlevels(xft)는 factor에만 적용되기 때문에 character에도 적용할 수 있도록 수정
  }
  
  ct_p <- rbind(ct, ct.p)
  ct_p.sum <- addmargins(ct_p, margin=2)
  ct_p.sum <- t(ct_p.sum)
  ft <- as_tibble(ct_p.sum)
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "\\*\\*\\*" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "\\*\\*" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "\\*"
      }
  
  if (colpct) {
    #    if (xft_name == "") {
    if (xft_name == "" | is.na(xft_name)) {
      xft_name <- "xft" # 뒤에서 left_join을 하기 위해 데이터가 있어야 함
    }
    rcell <- rep(xft_name, nxf+1)
    vname <- c(as.character(xft_level), "Total") ### 2021.2.14
    ft <- cbind(rcell, vname, ft)
    cnames <- c("rcell", "vname", "count", "prop")
    colnames(ft) <- cnames
    col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
    if (!is.na(xft_level[[1]])) { 
      ft[1:nxf, 2] <- as.character(xft_level)
    }
    
    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
    if (print_stat) {
      ft <- cbind(ft, col_stats)
    }
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
    
    if (!is.na(ans)) {
      ft <- color(ft, i = ans, j = 2:4, color = "red")
    }
    
    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = 3, digits = 0) 
    
    # chi가 표시되지 않아 code로 만듦
    if (print_stat) {
      ft <- ft %>%
        compose(
          i = 1, j = 5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
        ) 
      ft <- ft %>%
        compose(
          i = 2, j = 5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
        ) 
      
      ft <- ft %>%
        compose(
          i = 3, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
        ) 
    }
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = which.max(ct.p), j = 4, bold = TRUE)
    
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    ft$header$content$content$data[[3]]$txt <- cell_name[1]  
    ft$header$content$content$data[[4]]$txt <- cell_name[2]  
    
    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
    ft <- set_header_labels(ft, vname = " ") 
    if (print_stat) {
      ft <- set_header_labels(ft, test = cell_name[4]) 
    }
    if (print_stat) {
      footer_widths <- 5
    } else {
      footer_widths <- 4
    }
    ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
    }
  } else {
    #    if (xft_name == "") {
    if (xft_name == "" | is.na(xft_name)) {
      xft_name <- "xft" # 뒤에서 left_join을 하기 위해 데이터가 있어야 함
    }
    rcell <- rep(xft_name, nxf+1)
    ft <- as_tibble(ct_p.sum)    
    vname <- c(as.character(xft_level), "Total")
    ft <- cbind(rcell, vname, ft)
    
    ft.n <- ft %>%
      select(rcell, vname, ct) %>%
      pivot_wider(id_cols = rcell, names_from = vname, values_from = ct)
    ft.prop <- ft %>%
      select(rcell, vname, ct.p) %>%
      pivot_wider(id_cols = rcell, names_from = vname, values_from = ct.p)
    ft <- left_join(ft.n, ft.prop, by = "rcell") 
    sel_index <- rbind(c(2:(nxf+2)), c((nxf+3):(2*(nxf+1)+1)))
    sel_index <- as.vector(sel_index)
    
    ft <- ft[, c(1, sel_index)]
    
    cnames <- c("rcell", paste0("count", 1:nxf), "count_t", paste0("prop", 1:nxf), "prop_t")
    cnames <- cnames[c(1, sel_index)]
    
    colnames(ft) <- cnames
    col_stats <- tibble(test = chi2)
    if (print_stat) {
      ft <- cbind(ft, col_stats)
    }    
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
    if (!is.na(ans)) {
      ft <- color(ft, i = 1, j = (2*ans):(2*ans+1), color = "red")
    }
    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = seq(2, 2*nxf+3, 2), digits = 0) 
    
    # chi가 표시되지 않아 code로 만듦
    if (print_stat) {
      ft <- ft %>%
        compose(
          i = 1, j = 2*nxf+4, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk), " (", as_chunk(chi2_p, digits = 3), ")")
        ) 
    }
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = 1, j = 2*which.max(ct.p)+1, bold = TRUE)
    
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    for (j in 1:(2*nxf+2)) {
      if (j %% 2 == 1) {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[1]  
      } else {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[2]
      }
    }
    
    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
    if (print_stat) {    
      ft <- set_header_labels(ft, test = cell_name[4]) 
    }
    if (print_stat) {
      header1 <- c("", xft_name, "")
      header2 <- c("", as.character(xft_level), cell_name[3], "")
      colwidths1 <- c(1, 2*nxf+2, 1)
      colwidths2 <- c(1, rep(2, nxf+1), 1)
      footer_widths <- 2*nxf+4
    } else {
      header1 <- c("", xft_name)
      header2 <- c("", as.character(xft_level), cell_name[3])
      colwidths1 <- c(1, 2*nxf+2)
      colwidths2 <- c(1, rep(2, nxf+1))
      footer_widths <- 2*nxf+3
    }
    
    ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
    if (header){
      ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
    }
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
    }
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}


# 적합도 검정 - 표 이용

my.ctables.gf.from.table <- function(ct = ct, colpct = TRUE, rowpct = FALSE, 
                         xft_name = NA, xft_level = NA,
                         cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = caption, note = note, print_note = TRUE, print_stat = FALSE, print_xft = FALSE) {
  dm <- dimnames(ct)
  if (is.na(xft_name)) {
    xft_name <- names(dm[1])
  }

  if (is.na(xft_level[[1]])) { 
    xft_level <- dm[[1]]
  }

  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
    
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name)
  }
  # colpct를 가정하고 작업한 후, rowpct일 때 나중에 transpose 처리.
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  first_note <-  "1) * < 0.10, ** < 0.05, *** < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare goodness of fit test."
  if (colpct == TRUE) {
    third_note <- "3) The highest column proportion is printed in boldface."
  } else {
    third_note <- "3) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }

  ct.p <- prop.table(ct, margin = 2)

  nxf <- dim(ct)[1] # Total 제외

  ct.sum <- addmargins(ct, margin = 1)
  ct.p.sum <- addmargins(ct.p, margin = 1)
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환

  ct.sum_tibble <- as_tibble(ct.sum) %>%
    select(c(1, 3)) # 2번째 열이 불필요하게 생기기 때문에 이를 제외시킴
  
  colnames(ct.sum_tibble) <- c("xft", "n")  # table로 읽어들일 때 수정한 부분

  ct.p.sum_tibble <- as_tibble(ct.p.sum) %>%
    select(c(1, 3)) # 2번째 열이 불필요하게 생기기 때문에 이를 제외시킴
  colnames(ct.p.sum_tibble) <- c("xft", "prop")  # table로 읽어들일 때 수정한 부분

  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)

  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter

  chi2_asterisk <- ""

  if (chi2_p <= 0.01) {
    chi2_asterisk <- "***" } else if(chi2_p <= 0.05) {
    chi2_asterisk <- "**" } else if(chi2_p <= 0.10){
    chi2_asterisk <- "*"
  }

  if (colpct) {
    rcell <- rep(xft_name, nxf+1)
    ft <- cbind(rcell, ft)
    cnames <- c("rcell", "vname", "count", "prop")
    colnames(ft) <- cnames
    col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
    if (!is.na(xft_level[[1]])) { 
      ft[1:nxf, 2] <- xft_level
    }

    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
if (print_stat) {
    ft <- cbind(ft, col_stats)
}
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨

    if (!is.na(ans)) {
      ft <- color(ft, i = ans, j = 2:4, color = "red")
    }

    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = 3, digits = 0) 

    # chi가 표시되지 않아 code로 만듦
if (print_stat) {
      ft <- ft %>%
      compose(
        i = 1, j = 5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
      ) 
    ft <- ft %>%
      compose(
        i = 2, j = 5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
      ) 

    ft <- ft %>%
      compose(
        i = 3, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
      ) 
}
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = which.max(ct.p[, 1]), j = 4, bold = TRUE)
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    ft$header$content$content$data[[3]]$txt <- cell_name[1]  
    ft$header$content$content$data[[4]]$txt <- cell_name[2]  

    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
    ft <- set_header_labels(ft, vname = " ") 
if (print_stat) {
    ft <- set_header_labels(ft, test = cell_name[4]) 
}
if (print_stat) {
    footer_widths <- 5
} else {
    footer_widths <- 4
}
    ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
     }
  } else {
    rcell <- rep(xft_name, nxf+1)
    ft <- cbind(rcell, ft)
    ft.n <- ft %>%
      select(rcell, xft, n) %>%
      pivot_wider(id_cols = rcell, names_from = xft, values_from = n)
    ft.prop <- ft %>%
      select(rcell, xft, prop) %>%
      pivot_wider(id_cols = rcell, names_from = xft, values_from = prop)
    ft <- left_join(ft.n, ft.prop, by = "rcell") 
    sel_index <- rbind(c(2:(nxf+2)), c((nxf+3):(2*(nxf+1)+1)))
    sel_index <- as.vector(sel_index)
  
    ft <- ft[, c(1, sel_index)]

    cnames <- c("rcell", paste0("count", 1:nxf), "count_t", paste0("prop", 1:nxf), "prop_t")
    cnames <- cnames[c(1, sel_index)]

    colnames(ft) <- cnames
    col_stats <- tibble(test = chi2)
if (print_stat) {
    ft <- cbind(ft, col_stats)
}    
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
    if (!is.na(ans)) {
      ft <- color(ft, i = 1, j = (2*ans):(2*ans+1), color = "red")
    }
    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = seq(2, 2*nxf+3, 2), digits = 0) 

    # chi가 표시되지 않아 code로 만듦
if (print_stat) {
  ft <- ft %>%
      compose(
        i = 1, j = 2*nxf+4, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk), " (", as_chunk(chi2_p, digits = 3), ")")
      ) 
}
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = 1, j = 2*which.max(ct.p[, 1])+1, bold = TRUE)
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    for (j in 1:(2*nxf+2)) {
      if (j %% 2 == 1) {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[1]  
      } else {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[2]
      }
    }

    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
if (print_stat) {    
    ft <- set_header_labels(ft, test = cell_name[4]) 
}
if (print_stat) {
    header1 <- c("", xft_name, "")
    header2 <- c("", xft_level, cell_name[3], "")
    colwidths1 <- c(1, 2*nxf+2, 1)
    colwidths2 <- c(1, rep(2, nxf+1), 1)
    footer_widths <- 2*nxf+4
} else {
    header1 <- c("", xft_name)
    header2 <- c("", xft_level, cell_name[3])
    colwidths1 <- c(1, 2*nxf+2)
    colwidths2 <- c(1, rep(2, nxf+1))
    footer_widths <- 2*nxf+3
}

    ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
    if (header){
      ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
    }
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
    }
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}

# 적합도 검정 - 표 이용

my.ctables.gf.from.table.xaringan <- function(ct = ct, colpct = TRUE, rowpct = FALSE, 
                                     xft_name = NA, xft_level = NA,
                                     cell_name = NA, na.rm = TRUE, ans = NA, header = FALSE, caption = caption, note = note, print_note = TRUE, print_stat = FALSE, print_xft = FALSE) {
  dm <- dimnames(ct)
  if (is.na(xft_name)) {
    xft_name <- names(dm[1])
  }
  
  if (is.na(xft_level[[1]])) { 
    xft_level <- dm[[1]]
  }
  
  if (is.na(cell_name[[1]])) {
    cell_name <- c("Number", "Prop", "Total", "Test statistics")  
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0(xft_name)
  }
  # colpct를 가정하고 작업한 후, rowpct일 때 나중에 transpose 처리.
  if (colpct == FALSE & rowpct == FALSE) {
    colpct <- TRUE
  }
  first_note <-  "1) \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  second_note <- "2) The number in the parenthesis is the p-value in a Chisquare goodness of fit test."
  if (colpct == TRUE) {
    third_note <- "3) The highest column proportion is printed in boldface."
  } else {
    third_note <- "3) The highest row proportion is printed in boldface."
  }    
  # override 
  if (!is.na(note[[1]])) {
    first_note <- note[1]
    second_note <- note[2]
    third_note <- note[3]
    forth_note <- note[4]
  }
  
  ct.p <- prop.table(ct, margin = 2)
  
  nxf <- dim(ct)[1] # Total 제외
  
  ct.sum <- addmargins(ct, margin = 1)
  ct.p.sum <- addmargins(ct.p, margin = 1)
  ct.p.sum <- ct.p.sum * 100 # 퍼센트로 변환
  
  ct.sum_tibble <- as_tibble(ct.sum) %>%
    select(c(1, 3)) # 2번째 열이 불필요하게 생기기 때문에 이를 제외시킴
  
  colnames(ct.sum_tibble) <- c("xft", "n")  # table로 읽어들일 때 수정한 부분
  
  ct.p.sum_tibble <- as_tibble(ct.p.sum) %>%
    select(c(1, 3)) # 2번째 열이 불필요하게 생기기 때문에 이를 제외시킴
  colnames(ct.p.sum_tibble) <- c("xft", "prop")  # table로 읽어들일 때 수정한 부분
  
  ft <- left_join(ct.sum_tibble, ct.p.sum_tibble, by = "xft")
  
  # Chisquare test
  chisq <- chisq.test(ct, correct=TRUE) # Yates' Continuity correction (only for 2   x 2)
  
  chi2 <- chisq$statistic
  chi2_p <- chisq$p.value
  chi2_df <- chisq$parameter
  
  chi2_asterisk <- ""
  
  if (chi2_p <= 0.01) {
    chi2_asterisk <- "\\*\\*\\*" } else if(chi2_p <= 0.05) {
      chi2_asterisk <- "\\*\\*" } else if(chi2_p <= 0.10){
        chi2_asterisk <- "\\*"
      }
  
  if (colpct) {
    rcell <- rep(xft_name, nxf+1)
    ft <- cbind(rcell, ft)
    cnames <- c("rcell", "vname", "count", "prop")
    colnames(ft) <- cnames
    col_stats <- tibble(test = c(NA, chi2, chi2_p, rep(NA, nxf - 2)))
    if (!is.na(xft_level[[1]])) { 
      ft[1:nxf, 2] <- xft_level
    }
    
    ft[nxf+1, 2] <- cell_name[3] # 마지막 행을 sum에서 Total 또는 지정한 명칭으로 바꿈
    if (print_stat) {
      ft <- cbind(ft, col_stats)
    }
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
    
    if (!is.na(ans)) {
      ft <- color(ft, i = ans, j = 2:4, color = "red")
    }
    
    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = 3, digits = 0) 
    
    # chi가 표시되지 않아 code로 만듦
    if (print_stat) {
      ft <- ft %>%
        compose(
          i = 1, j = 5, value = as_paragraph("\U03C7", as_sup("2"),as_sub("("), as_sub(as.character(chi2_df)), as_sub(")"), " = ")
        ) 
      ft <- ft %>%
        compose(
          i = 2, j = 5, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk))
        ) 
      
      ft <- ft %>%
        compose(
          i = 3, j = 5, value = as_paragraph("(", as_chunk(test, digits = 3), ")")
        ) 
    }
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = which.max(ct.p[, 1]), j = 4, bold = TRUE)
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    ft$header$content$content$data[[3]]$txt <- cell_name[1]  
    ft$header$content$content$data[[4]]$txt <- cell_name[2]  
    
    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
    ft <- set_header_labels(ft, vname = " ") 
    if (print_stat) {
      ft <- set_header_labels(ft, test = cell_name[4]) 
    }
    if (print_stat) {
      footer_widths <- 5
    } else {
      footer_widths <- 4
    }
    ft <- merge_at(ft, i = 1:(nxf+1), j = 1)
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
    }
  } else {
    rcell <- rep(xft_name, nxf+1)
    ft <- cbind(rcell, ft)
    ft.n <- ft %>%
      select(rcell, xft, n) %>%
      pivot_wider(id_cols = rcell, names_from = xft, values_from = n)
    ft.prop <- ft %>%
      select(rcell, xft, prop) %>%
      pivot_wider(id_cols = rcell, names_from = xft, values_from = prop)
    ft <- left_join(ft.n, ft.prop, by = "rcell") 
    sel_index <- rbind(c(2:(nxf+2)), c((nxf+3):(2*(nxf+1)+1)))
    sel_index <- as.vector(sel_index)
    
    ft <- ft[, c(1, sel_index)]
    
    cnames <- c("rcell", paste0("count", 1:nxf), "count_t", paste0("prop", 1:nxf), "prop_t")
    cnames <- cnames[c(1, sel_index)]
    
    colnames(ft) <- cnames
    col_stats <- tibble(test = chi2)
    if (print_stat) {
      ft <- cbind(ft, col_stats)
    }    
    ft <- flextable(ft) # col_keys를 지정하지 않아 모든 변수가 flextable로 저장됨
    if (!is.na(ans)) {
      ft <- color(ft, i = 1, j = (2*ans):(2*ans+1), color = "red")
    }
    ft <- colformat_double(ft, digits = 1) 
    ft <- colformat_double(ft, j = seq(2, 2*nxf+3, 2), digits = 0) 
    
    # chi가 표시되지 않아 code로 만듦
    if (print_stat) {
      ft <- ft %>%
        compose(
          i = 1, j = 2*nxf+4, value = as_paragraph(as_chunk(chi2, digits = 1), as_sup(chi2_asterisk), " (", as_chunk(chi2_p, digits = 3), ")")
        ) 
    }
    # 열 또는 행 기준으로 가장 높은 비율을 진하게 표시
    ft <- bold(ft, i = 1, j = 2*which.max(ct.p[, 1])+1, bold = TRUE)
    # ctable의 행 제목을 불러 들인 후 셀 제목을 바꿈
    for (j in 1:(2*nxf+2)) {
      if (j %% 2 == 1) {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[1]  
      } else {
        ft$header$content$content$data[[j+1]]$txt <- cell_name[2]
      }
    }
    
    # 표의 셀 제목 중 처음 두개를 공백 문자 하나씩으로 처리
    ft <- set_caption(ft, caption = caption)
    ft <- set_header_labels(ft, rcell = " ")  # 최소한 공백 문자 1개라도 넣어야 열이 없어지지 않음
    if (print_stat) {    
      ft <- set_header_labels(ft, test = cell_name[4]) 
    }
    if (print_stat) {
      header1 <- c("", xft_name, "")
      header2 <- c("", xft_level, cell_name[3], "")
      colwidths1 <- c(1, 2*nxf+2, 1)
      colwidths2 <- c(1, rep(2, nxf+1), 1)
      footer_widths <- 2*nxf+4
    } else {
      header1 <- c("", xft_name)
      header2 <- c("", xft_level, cell_name[3])
      colwidths1 <- c(1, 2*nxf+2)
      colwidths2 <- c(1, rep(2, nxf+1))
      footer_widths <- 2*nxf+3
    }
    
    ft <- add_header_row(ft, values = header2, colwidths = colwidths2)
    if (header){
      ft <- add_header_row(ft, values = header1, colwidths = colwidths1)
    }
    ft <- theme_booktabs(ft)
    ft <- align(ft, align = "center", part = "header")
    ft <- align(ft, align = "center", part = "body")
    if (print_note) {
      ft <- ft %>%
        add_footer_row(values = third_note, colwidths = footer_widths) %>%
        add_footer_row(values = second_note, colwidths = footer_widths) %>%
        add_footer_row(values = first_note, colwidths = footer_widths) 
    }
  }
  if (print_xft == FALSE){
    ft <- merge_h_range(ft, j1 = 2, j2 = 1, part = "body")
  }
  ft <- autofit(ft)
  return(ft)
}




# my.barplot

my.barplot <- function(xft = xft, xft_name = NA, xft_level = NA, yaxis_label = NA, na.rm = TRUE, zero.rm = FALSE, ans = NA, ans_level = NA,
                       caption = NA, flip = FALSE, graph_name = FALSE) {  
  # as_factor: 데이터 순서를 고려하여 level을 정함.
  # as.factor:  데이터의 오름차순으로 level을 정함.
  # 따라서 as.factor가 적절함.

  xft <- as.factor(xft)

  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  
  if (!is.na(xft_level[[1]])) { 
    levels(xft) <- xft_level
  }
  
  if (is.na(yaxis_label)) { 
    yaxis_label <- "빈도"
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0("The Distribution of ", xft_name)
  }
  
  if (flip == TRUE) {
    vjust1 = 0.5
    vjust2 = 0.5
    hjust1 = -0.5
    hjust2 = -1.0
    width.level = 40
  } else {
    vjust1 = -1.5
    vjust2 = -0.5
    hjust1 = 0.5
    hjust2 = 0.5
    width.level = 16
  }
  
  data <- tibble(xft = xft) 

  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]))
  }
  
  dfg <- data %>%
    group_by(xft) %>%
    summarise(count = n()) %>%
    mutate(prop = count/sum(count))
  
  nxf <- nrow(dfg)
  
  if (is.na(ans_level[[1]])) {
    ans_level <- c("Correct", "Incorrect")
  }
  
  if (!is.na(ans)) {
    ans_f <- factor(c(1:nxf) == ans, levels = c("TRUE", "FALSE"))
    levels(ans_f) <- ans_level
  } else {
    ans_f = FALSE  # ans가 없어도 에러가 나지 않도록 만들기 위한 것!
  }
  
  gg <- dfg %>%
    ggplot(aes(x=fct_relabel(xft, str_wrap, width = width.level), y = count, fill = ans_f)) + 
    geom_bar(color = "blue", width = 0.4, stat = "identity") +
    geom_text(aes(label = count), stat = "identity", vjust = vjust1, hjust = hjust1, color = "blue") +
    geom_text(aes(label=paste0("(", scales::percent(prop, 0.1), ")")), stat = "identity", vjust = vjust2, hjust = hjust2, size = 3, color = "blue") +
    ggtitle(str_wrap(caption, 120)) +
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue")) +  # 글씨체, 글씨 모양, 가운데 정렬, 크기, 색상
    theme(axis.title = element_text(family = "serif", size = 15)) +
    theme(axis.text.x = element_text(family = "serif", size = 14)) +
    theme(axis.text.y = element_text(family = "serif", size = 14)) +
    xlab(label = NULL) +
    ylab(label = yaxis_label) +
    labs(fill = NULL) +
    ylim(0, max(dfg$count)*1.1) +
    scale_x_discrete(drop = zero.rm)
  if (flip == TRUE) {
    gg <- gg + coord_flip()
  }
  if (is.na(ans)) {
    gg <- gg + theme(legend.position = "none")
  }
  print(gg)
  if (!is.na(graph_name)) {
    ggsave(paste(graph_name, ".jpg", sep = ""), dpi = 300)
  }
}

# my.barplot

my.barplot.xaringan <- function(xft = xft, xft_name = NA, xft_level = NA, yaxis_label = NA, na.rm = TRUE, zero.rm = FALSE, ans = NA, ans_level = NA,
                       caption = NA, flip = FALSE, graph_name = FALSE) {  
  # as_factor: 데이터 순서를 고려하여 level을 정함.
  # as.factor:  데이터의 오름차순으로 level을 정함.
  # 따라서 as.factor가 적절함.
  
  xft <- as.factor(xft)
  
  if (is.na(xft_name)) {
    xft_name <- label(xft)
  }
  
  if (!is.na(xft_level[[1]])) { 
    levels(xft) <- xft_level
  }
  
  if (is.na(yaxis_label)) { 
    yaxis_label <- "빈도"
  }
  
  if (is.na(caption[[1]])) {
    caption = paste0("The Distribution of ", xft_name)
  }
  
  if (flip == TRUE) {
    vjust1 = 0.5
    vjust2 = 0.5
    hjust1 = -0.5
    hjust2 = -1.0
    width.level = 40
  } else {
    vjust1 = -1.5
    vjust2 = -0.5
    hjust1 = 0.5
    hjust2 = 0.5
    width.level = 16
  }
  
  data <- tibble(xft = xft) 
  
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]))
  }
  
  dfg <- data %>%
    group_by(xft) %>%
    summarise(count = n()) %>%
    mutate(prop = count/sum(count))
  
  nxf <- nrow(dfg)
  
  if (is.na(ans_level[[1]])) {
    ans_level <- c("Correct", "Incorrect")
  }
  
  if (!is.na(ans)) {
    ans_f <- factor(c(1:nxf) == ans, levels = c("TRUE", "FALSE"))
    levels(ans_f) <- ans_level
  } else {
    ans_f = FALSE  # ans가 없어도 에러가 나지 않도록 만들기 위한 것!
  }
  
  gg <- dfg %>%
    ggplot(aes(x=fct_relabel(xft, str_wrap, width = width.level), y = count, fill = ans_f)) + 
    geom_bar(color = "blue", width = 0.4, stat = "identity") +
    geom_text(aes(label = count), stat = "identity", vjust = vjust1, hjust = hjust1, color = "blue") +
    geom_text(aes(label=paste0("(", scales::percent(prop, 0.1), ")")), stat = "identity", vjust = vjust2, hjust = hjust2, size = 3, color = "blue") +
    ggtitle(str_wrap(caption, 120)) +
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 20, color = "darkblue")) +  # 글씨체, 글씨 모양, 가운데 정렬, 크기, 색상
    theme(axis.title = element_text(family = "serif", size = 15)) +
    theme(axis.text.x = element_text(family = "serif", size = 14)) +
    theme(axis.text.y = element_text(family = "serif", size = 14)) +
    xlab(label = NULL) +
    ylab(label = yaxis_label) +
    labs(fill = NULL) +
    ylim(0, max(dfg$count)*1.1) +
    scale_x_discrete(drop = zero.rm)
  if (flip == TRUE) {
    gg <- gg + coord_flip()
  }
  if (is.na(ans)) {
    gg <- gg + theme(legend.position = "none")
  }
  print(gg)
  if (!is.na(graph_name)) {
    ggsave(paste(graph_name, ".jpg", sep = ""), dpi = 300)
  }
}



# lm 결과 출력 함수

my.ft <- function(coeff = coeff, pvalue = pvalue,
                  xft_name = NA, yft_name = NA, 
                  caption = NA, print_note = TRUE, print_stat = TRUE, selection = selection, color_row = NA, bold_row = NA) {
  if (!is.na(selection[[1]])) {
    coeff <- coeff[selection,]
    pvalue <- pvalue[selection,]
    xft_name <- xft_name[selection]
  }
  
  nxf <- nrow(coeff)
  nyf <- ncol(coeff)
  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (pvalue[i,j] <= 0.01) asterisk[i,j] <- "***"
      else if (pvalue[i,j] <= 0.05) asterisk[i,j] <- "**"
      else if (pvalue[i,j] <= 0.10) asterisk[i,j] <- "*"
    }
  }
  note <-  "Note: * < 0.10, ** < 0.05, *** < 0.01"
  
  ft <- cbind(xft_name, coeff)
  names(ft) <- c("Coefficients", yft_name)
  ft <- flextable(ft) %>%
    colformat_double(digits = 1) 
  
  if (!is.na(caption)) {
    ft <- set_caption(ft, caption = caption)
  }
  
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      if (pvalue[i0,j0] <= 0.05) {
        coeff_ij <- coeff[[i0,j0]]
        ft <- ft %>%
          compose(
            i = i0, j = j0+1, value = as_paragraph(sprintf("%0.1f", coeff_ij), as_sup(asterisk[i0,j0]))            
          ) 
      }
    }
  }
  ft <- theme_booktabs(ft) 
  
  ft <- ft %>%  
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body")
  
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = note, colwidths = nyf + 1)
  }
  if (!is.na(color_row[[1]])) {
    ft <- color(ft, i = color_row, color = "red")
  }
  if (!is.na(bold_row[[1]])) {
    ft <- bold(ft, i = bold_row)
  }
  ft <- autofit(ft)
  return(ft)
}


# lm 결과 출력 함수 (p-value 포함)
my.ft.p <- function(coeff = coeff, pvalue = pvalue,
                    xft_name = NA, yft_name = NA, 
                    caption = NA, print_note = TRUE, print_stat = TRUE, selection = selection, color_row = NA, bold_row = NA) {
  if (!is.na(selection[[1]])) {
    coeff <- coeff[selection,]
    pvalue <- pvalue[selection,]
    xft_name <- xft_name[selection]
  }
  
  nxf <- nrow(coeff)
  nyf <- ncol(coeff)
  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (pvalue[i,j] <= 0.01) asterisk[i,j] <- "***"
      else if (pvalue[i,j] <= 0.05) asterisk[i,j] <- "**"
      else if (pvalue[i,j] <= 0.10) asterisk[i,j] <- "*"
    }
  }
  note <-  "Note: * < 0.10, ** < 0.05, *** < 0.01"
  
  ft <- cbind(xft_name, coeff)
  names(ft) <- c("Coefficients", yft_name)
  ft <- flextable(ft) %>%
    colformat_double(digits = 1) 
  
  if (!is.na(caption)) {
    ft <- set_caption(ft, caption = caption)
  }
  
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      pvalue_ij <- pvalue[[i0,j0]] # 2021.7.26
      coeff_ij <- coeff[[i0,j0]]
      if (pvalue[i0,j0] <= 0.05) {
        ft <- ft %>%
          compose(
            #            i = i0, j = j0+1, value = as_paragraph(sprintf("%0.1f", coeff_ij), as_sup(asterisk[i0,j0]))          
            i = i0, j = j0+1, value = as_paragraph(sprintf("%0.3f", coeff_ij), as_sup(asterisk[i0,j0]), "(", sprintf("%0.3f", pvalue_ij), ")")            # 2021. 7.26.
          ) 
      } else {
        ft <- ft %>%
          compose(
            #            i = i0, j = j0+1, value = as_paragraph(sprintf("%0.1f", coeff_ij), as_sup(asterisk[i0,j0]))          
            i = i0, j = j0+1, value = as_paragraph(sprintf("%0.3f", coeff_ij), "(", sprintf("%0.3f", pvalue_ij), ")")            # 2021. 7.26.
          ) 
      }
    }
  }
  ft <- theme_booktabs(ft) 
  
  ft <- ft %>%  
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body")
  
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = note, colwidths = nyf + 1)
  }
  if (!is.na(color_row[[1]])) {
    ft <- color(ft, i = color_row, color = "red")
  }
  if (!is.na(bold_row[[1]])) {
    ft <- bold(ft, i = bold_row)
  }
  ft <- autofit(ft)
  return(ft)
}



# lm 결과 출력 함수

my.ft.xaringan <- function(coeff = coeff, pvalue = pvalue,
                  xft_name = NA, yft_name = NA, 
                  caption = NA, print_note = TRUE, print_stat = TRUE, selection = selection, color_row = NA, bold_row = NA) {
  if (!is.na(selection[[1]])) {
    coeff <- coeff[selection,]
    pvalue <- pvalue[selection,]
    xft_name <- xft_name[selection]
  }
  
  nxf <- nrow(coeff)
  nyf <- ncol(coeff)
  
  asterisk <- matrix(rep("", nxf*nyf), ncol = nyf)
  for (i in 1:nxf) {
    for (j in 1:nyf) {
      if (pvalue[i,j] <= 0.01) asterisk[i,j] <- "\\*\\*\\*"
      else if (pvalue[i,j] <= 0.05) asterisk[i,j] <- "\\*\\*"
      else if (pvalue[i,j] <= 0.10) asterisk[i,j] <- "\\*"
    }
  }
  note <-  "Note: \\* < 0.10, \\*\\* < 0.05, \\*\\*\\* < 0.01"
  
  ft <- cbind(xft_name, coeff)
  names(ft) <- c("Coefficients", yft_name)
  ft <- flextable(ft) %>%
    colformat_double(digits = 1) 
  
  if (!is.na(caption)) {
    ft <- set_caption(ft, caption = caption)
  }
  
  for (i0 in 1:nxf) {
    for (j0 in 1:nyf) {
      if (pvalue[i0,j0] <= 0.05) {
        coeff_ij <- coeff[[i0,j0]]
        ft <- ft %>%
          compose(
            i = i0, j = j0+1, value = as_paragraph(sprintf("%0.1f", coeff_ij), as_sup(asterisk[i0,j0]))            
          ) 
      }
    }
  }
  ft <- theme_booktabs(ft) 
  
  ft <- ft %>%  
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body")
  
  if (print_note) {
    ft <- ft %>%
      add_footer_row(values = note, colwidths = nyf + 1)
  }
  if (!is.na(color_row[[1]])) {
    ft <- color(ft, i = color_row, color = "red")
  }
  if (!is.na(bold_row[[1]])) {
    ft <- bold(ft, i = bold_row)
  }
  ft <- autofit(ft)
  return(ft)
}





### 그래프-histogram 1변수 함수

graph_hist1 <- function(data, na.rm = TRUE, flip = FALSE, caption = NA, graph.save.folder = NA, binwidth = 1) {
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]))
  }
  if (is.na(caption)) {
    caption = label(data[[1]])
  }
  gg <- data %>%
    ggplot(aes(x=data[[1]])) + 
    geom_histogram(fill = "lightblue", color = "blue", binwidth = binwidth) +
    stat_bin(geom = "text", aes(label = ..count..), vjust = -1.5, color = "blue", binwidth = binwidth) +
    stat_bin(geom = "text", aes(label = paste0("(", scales::percent(..count../sum(..count..), 0.1), ")")), vjust = -0.5, size = 3, color = "blue", binwidth = binwidth) +
    ggtitle(str_wrap(label(data[[1]]), 120)) +
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +  # 글씨체, 글씨 모양, 가운데 정렬, 크기, 색상을 설정합니다.
    xlab(label = NULL) +
    ylab(label = "빈도") +
    scale_x_continuous(breaks = round(seq(min(data[[1]]), max(data[[1]]), by = binwidth),0)) + # x축의 tick을 1 단위로 증가
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # y축을 아래는 0%, 위는 10% 확장, #  scale_y_continuous(expand = c(0, 0, 0, 30))는 y축 위를 30pt? 확장. c(0,30)은 잘 안되는데 잘 모르겠음. 
  if (flip == TRUE) {
    gg <- gg + coord_flip()
  }
  print(gg)
  if (!is.na(graph.save.folder)) {
    ggsave(paste(".\\", graph.save.folder, "\\", names(data[1]), ".jpg", sep = ""), dpi = 300)
  }
}


### 그래프-histogram 1변수 함수

graph_hist1.xaringan <- function(data, na.rm = TRUE, flip = FALSE, caption = NA, graph.save.folder = NA, binwidth = 1) {
  if (na.rm == TRUE) {
    data <- data %>% 
      filter(!is.na(data[[1]]))
  }
  if (is.na(caption)) {
    caption = label(data[[1]])
  }
  gg <- data %>%
    ggplot(aes(x=data[[1]])) + 
    geom_histogram(fill = "lightblue", color = "blue", binwidth = binwidth) +
    stat_bin(geom = "text", aes(label = ..count..), vjust = -1.5, color = "blue", binwidth = binwidth) +
    stat_bin(geom = "text", aes(label = paste0("(", scales::percent(..count../sum(..count..), 0.1), ")")), vjust = -0.5, size = 3, color = "blue", binwidth = binwidth) +
    ggtitle(str_wrap(label(data[[1]]), 120)) +
    theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue")) +  # 글씨체, 글씨 모양, 가운데 정렬, 크기, 색상을 설정합니다.
    xlab(label = NULL) +
    ylab(label = "빈도") +
    scale_x_continuous(breaks = round(seq(min(data[[1]]), max(data[[1]]), by = binwidth),0)) + # x축의 tick을 1 단위로 증가
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # y축을 아래는 0%, 위는 10% 확장, #  scale_y_continuous(expand = c(0, 0, 0, 30))는 y축 위를 30pt? 확장. c(0,30)은 잘 안되는데 잘 모르겠음. 
  if (flip == TRUE) {
    gg <- gg + coord_flip()
  }
  print(gg)
  if (!is.na(graph.save.folder)) {
    ggsave(paste(".\\", graph.save.folder, "\\", names(data[1]), ".jpg", sep = ""), dpi = 300)
  }
}


# 등록률: 권역, 대학
my.register <- function(df = df, group = group, category = category, xvar = xvar, year = year, criteria = criteria, spacing = spacing, na.rm = na.rm) {
  df <- df[c("대학", "대학명", "권역", "설립유형", 
             paste0(category, xvar[1], "_", year[1], "학년도"), 
             paste0(category, xvar[2], "_", year[1], "학년도"), 
             paste0(category, xvar[1], "_", year[2], "학년도"), 
             paste0(category, xvar[2], "_", year[2], "학년도"))
  ]  
  colnames(df) <- c("대학", "대학명", "권역", "설립유형", "N1", "R1", "N2", "R2")  
  xg <- group
  if (xg == "대학") {
    df <- df %>%
      select(대학명, 권역, 설립유형, N1, R1, N2, R2)
    colnames(df) <- c("xg", "권역", "설립유형", "N1", "R1", "N2", "R2")  
    df1 <- df %>%
      group_by(xg) %>%
      summarise(
        권역 = 권역,
        설립유형 = 설립유형,
        n = n(),
        N1 = sum(N1, na.rm = na.rm),
        R1 = sum(R1, na.rm = na.rm),
        N2 = sum(N2, na.rm = na.rm),
        R2 = sum(R2, na.rm = na.rm)) 
    if (xvar[2] == "등록") {
      df1 <- df1 %>%
        mutate(P1 = R1/N1*100, P2 = R2/N2*100) 
    } else {
      df1 <- df1 %>%
        mutate(P1 = R1/N1, P2 = R2/N2) 
    }
    df2 <- df %>%
      summarise(xg = "전체",
                권역 = "",
                설립유형 = "",
                n = n(),
                N1 = sum(N1, na.rm = na.rm),
                R1 = sum(R1, na.rm = na.rm),
                N2 = sum(N2, na.rm = na.rm),
                R2 = sum(R2, na.rm = na.rm)) 
    if (xvar[2] == "등록") {
      df2 <- df2 %>%
        mutate(P1 = R1/N1*100, P2 = R2/N2*100) 
    } else {
      df2 <- df2 %>%
        mutate(P1 = R1/N1, P2 = R2/N2) 
    }
    df <- rbind(df1, df2) %>%
      select(xg, 권역, 설립유형, n, N1, R1, P1, N2, R2, P2) %>%
      mutate(dP = P2 - P1)
    column_red = df$dP < criteria
    header <- c(group, "권역", "설립유형", "대학수", paste0(year[1], "학년도"), paste0(year[2], "학년도"), paste0(xvar[2], "률 변화"))
    if (xvar[2] == "지원") {
      header <- c(group, "권역", "설립유형", "대학수", paste0(year[1], "학년도"), paste0(year[2], "학년도"), "경쟁률 변화")
    }
    ft <- df %>%
      flextable() 
    ft$header$content$content$data[[1]]$txt <- group  
    ft$header$content$content$data[[2]]$txt <- "권역" 
    ft$header$content$content$data[[3]]$txt <- "설립유형"  
    ft$header$content$content$data[[4]]$txt <- "대학수"  
    ft$header$content$content$data[[5]]$txt <- xvar[1]  
    ft$header$content$content$data[[6]]$txt <- xvar[2]  
    ft$header$content$content$data[[7]]$txt <- paste0(xvar[2], "률")  
    ft$header$content$content$data[[8]]$txt <- xvar[1]  
    ft$header$content$content$data[[9]]$txt <- xvar[2]  
    ft$header$content$content$data[[10]]$txt <- paste0(xvar[2], "률")  
    ft$header$content$content$data[[11]]$txt <- "변화"
    if (xvar[2] == "지원") {
      ft$header$content$content$data[[7]]$txt <- "경쟁률"  
      ft$header$content$content$data[[10]]$txt <- "경쟁률"  
    }
    ft <- ft %>%
      add_header_row(values = header, colwidths = c(1, 1, 1, 1, 3, 3, 1)) %>%
      merge_at(i = 1:2, j = 1, part = "header") %>%
      merge_at(i = 1:2, j = 2, part = "header") %>%
      merge_at(i = 1:2, j = 3, part = "header") %>%
      merge_at(i = 1:2, j = 4, part = "header") %>%
      merge_at(i = 1:2, j = 11, part = "header") %>%
      theme_booktabs() %>%
      align(align = "center", part = "header") %>%
      align(align = "center", j = 1:4, part = "body") %>%
      colformat_double(j = c(5, 6, 8, 9), digits = 0) %>%
      colformat_double(j = c(7, 10, 11), digits = 1) %>%
      line_spacing(space = spacing, part = "body") %>%
      color(i = which(column_red == TRUE), j = 1:11, color = "red") %>%
      bold(i = length(column_red), j = 1:11, bold = TRUE) %>%
      width(j = 1, 2.0) %>%
      width(j = 3, 1.2)
  } else if (xg == "권역") {
    df <- df %>%
      select(권역, N1, R1, N2, R2)
    colnames(df) <- c("xg", "N1", "R1", "N2", "R2")  
    df1 <- df %>%
      group_by(xg) %>%
      summarise(
        n = n(),
        N1 = sum(N1, na.rm = na.rm),
        R1 = sum(R1, na.rm = na.rm),
        N2 = sum(N2, na.rm = na.rm),
        R2 = sum(R2, na.rm = na.rm)) 
    if (xvar[2] == "등록") {
      df1 <- df1 %>%
        mutate(P1 = R1/N1*100, P2 = R2/N2*100) 
    } else {
      df1 <- df1 %>%
        mutate(P1 = R1/N1, P2 = R2/N2) 
    }
    df2 <- df %>%
      summarise(xg = "전체",
                n = n(),
                N1 = sum(N1, na.rm = na.rm),
                R1 = sum(R1, na.rm = na.rm),
                N2 = sum(N2, na.rm = na.rm),
                R2 = sum(R2, na.rm = na.rm)) 
    if (xvar[2] == "등록") {
      df2 <- df2 %>%
        mutate(P1 = R1/N1*100, P2 = R2/N2*100) 
    } else {
      df2 <- df2 %>%
        mutate(P1 = R1/N1, P2 = R2/N2) 
    }
    df <- rbind(df1, df2) %>%
      select(xg, n, N1, R1, P1, N2, R2, P2) %>%
      mutate(dP = P2 - P1)
    area <- tibble(
      xg = c("서울", "인천", "경기", "강원", "대전", "세종", "충남", "충북", "광주", "전남", 
             "전북", "부산", "대구", "울산", "경남", "경북", "제주", "전체")
    )
    df <- left_join(area, df, by = "xg")
    column_red = df$dP < criteria
    header <- c(group, "대학수", paste0(year[1], "학년도"), paste0(year[2], "학년도"), paste0(xvar[2], "률 변화"))
    if (xvar[2] == "지원") {
      header <- c(group, "대학수", paste0(year[1], "학년도"), paste0(year[2], "학년도"), "경쟁률 변화")
    }
    ft <- df %>%
      flextable() 
    ft$header$content$content$data[[1]]$txt <- group  
    ft$header$content$content$data[[2]]$txt <- "대학수"  
    ft$header$content$content$data[[3]]$txt <- xvar[1]  
    ft$header$content$content$data[[4]]$txt <- xvar[2]  
    ft$header$content$content$data[[5]]$txt <- paste0(xvar[2], "률")  
    ft$header$content$content$data[[6]]$txt <- xvar[1]  
    ft$header$content$content$data[[7]]$txt <- xvar[2]  
    ft$header$content$content$data[[8]]$txt <- paste0(xvar[2], "률")  
    ft$header$content$content$data[[9]]$txt <- "변화"
    if (xvar[2] == "지원") {
      ft$header$content$content$data[[5]]$txt <- "경쟁률"  
      ft$header$content$content$data[[8]]$txt <- "경쟁률"  
    }
    ft <- ft %>%
      add_header_row(values = header, colwidths = c(1, 1, 3, 3, 1)) %>%
      merge_at(i = 1:2, j = 1, part = "header") %>%
      merge_at(i = 1:2, j = 2, part = "header") %>%
      merge_at(i = 1:2, j = 9, part = "header") %>%
      theme_booktabs() %>%
      align(align = "center", part = "header") %>%
      align(align = "center", j = 1:2, part = "body") %>%
      colformat_double(j = c(2, 3, 4, 6, 7), digits = 0) %>%
      colformat_double(j = c(5, 8, 9), digits = 1) %>%
      line_spacing(space = spacing, part = "body") %>%
      color(i = which(column_red == TRUE), j = 1:9, color = "red") %>%
      bold(i = length(column_red), j = 1:9, bold = TRUE)
  } else if (xg == "설립유형") {
    df <- df %>%
      select(설립유형, N1, R1, N2, R2)
    colnames(df) <- c("xg", "N1", "R1", "N2", "R2")  
    df1 <- df %>%
      group_by(xg) %>%
      summarise(
        n = n(),
        N1 = sum(N1, na.rm = na.rm),
        R1 = sum(R1, na.rm = na.rm),
        N2 = sum(N2, na.rm = na.rm),
        R2 = sum(R2, na.rm = na.rm)) 
    if (xvar[2] == "등록") {
      df1 <- df1 %>%
        mutate(P1 = R1/N1*100, P2 = R2/N2*100) 
    } else {
      df1 <- df1 %>%
        mutate(P1 = R1/N1, P2 = R2/N2) 
    }
    df2 <- df %>%
      summarise(xg = "전체",
                n = n(),
                N1 = sum(N1, na.rm = na.rm),
                R1 = sum(R1, na.rm = na.rm),
                N2 = sum(N2, na.rm = na.rm),
                R2 = sum(R2, na.rm = na.rm)) 
    if (xvar[2] == "등록") {
      df2 <- df2 %>%
        mutate(P1 = R1/N1*100, P2 = R2/N2*100) 
    } else {
      df2 <- df2 %>%
        mutate(P1 = R1/N1, P2 = R2/N2) 
    }
    df <- rbind(df1, df2) %>%
      select(xg, n, N1, R1, P1, N2, R2, P2) %>%
      mutate(dP = P2 - P1)
    type <- tibble(
      xg = c("국공립", "사립", "전체")
    )
    df <- left_join(type, df, by = "xg")
    column_red = df$dP < criteria
    header <- c(group, "대학수", paste0(year[1], "학년도"), paste0(year[2], "학년도"), paste0(xvar[2], "률 변화"))
    if (xvar[2] == "지원") {
      header <- c(group, "대학수", paste0(year[1], "학년도"), paste0(year[2], "학년도"), "경쟁률 변화")
    }
    ft <- df %>%
      flextable() 
    ft$header$content$content$data[[1]]$txt <- group  
    ft$header$content$content$data[[2]]$txt <- "대학수"  
    ft$header$content$content$data[[3]]$txt <- xvar[1]  
    ft$header$content$content$data[[4]]$txt <- xvar[2]  
    ft$header$content$content$data[[5]]$txt <- paste0(xvar[2], "률")  
    ft$header$content$content$data[[6]]$txt <- xvar[1]  
    ft$header$content$content$data[[7]]$txt <- xvar[2]  
    ft$header$content$content$data[[8]]$txt <- paste0(xvar[2], "률")  
    ft$header$content$content$data[[9]]$txt <- "변화"
    if (xvar[2] == "지원") {
      ft$header$content$content$data[[4]]$txt <- "경쟁률"  
      ft$header$content$content$data[[7]]$txt <- "경쟁률"  
    }
    ft <- ft %>%
      add_header_row(values = header, colwidths = c(1, 1, 3, 3, 1)) %>%
      merge_at(i = 1:2, j = 1, part = "header") %>%
      merge_at(i = 1:2, j = 2, part = "header") %>%
      merge_at(i = 1:2, j = 9, part = "header") %>%
      theme_booktabs() %>%
      align(align = "center", part = "header") %>%
      align(align = "center", j = 1, part = "body") %>%
      colformat_double(j = c(2, 3, 4, 6, 7), digits = 0) %>%
      colformat_double(j = c(5, 8, 9), digits = 1) %>%
      line_spacing(space = spacing, part = "body") %>%
      color(i = which(column_red == TRUE), j = 1:9, color = "red") %>%
      bold(i = length(column_red), j = 1:9, bold = TRUE)
  }
  return(ft)
}


# 등록률: 권역별
my.register.area <- function(dfr= dfr, year1 = year1, year2 = year2, criteria = criteria, na.rm = TRUE, caption = NA, spacing = NA) {
  dfr_y1 <- dfr %>%
    filter(year == year1) %>%
    group_by(area) %>%
    summarise(N1sum_y1 = sum(N1), R1sum_y1 = sum(R1)) %>%
    mutate(R1RW_y1 = R1sum_y1/N1sum_y1 * 100)
  ft1 <- dfr_y1 %>%
    mutate_at(vars(area), funs(as.character(.))) %>%
    summarise(area = "전체", N1sum_y1 = sum(N1sum_y1), R1sum_y1 = sum(R1sum_y1), R1RW_y1 = sum(R1sum_y1)/sum(N1sum_y1)*100)
  ft1 <- rbind(dfr_y1, ft1)
  
  dfr_y2 <- dfr %>%
    filter(year == year2) %>%
    group_by(area) %>%
    summarise(N1sum_y2 = sum(N1), R1sum_y2 = sum(R1)) %>%
    mutate(R1RW_y2 = R1sum_y2/N1sum_y2 * 100)
  ft2 <- dfr_y2 %>%
    mutate_at(vars(area), funs(as.character(.))) %>%
    summarise(area = "전체", N1sum_y2 = sum(N1sum_y2), R1sum_y2 = sum(R1sum_y2), R1RW_y2 = sum(R1sum_y2)/sum(N1sum_y2)*100)
  ft2 <- rbind(dfr_y2, ft2)
  
  ft <- full_join(ft1, ft2) %>%
    mutate(R1W_change = R1RW_y2 - R1RW_y1)
  
  column_red = ft$R1W_change < criteria
  
  ft <- ft %>%
    flextable() 
  
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft$header$content$content$data[[1]]$txt <- "권역"  
  ft$header$content$content$data[[2]]$txt <- "모집"  
  ft$header$content$content$data[[3]]$txt <- "등록"  
  ft$header$content$content$data[[4]]$txt <- "등록률"  
  ft$header$content$content$data[[5]]$txt <- "모집"  
  ft$header$content$content$data[[6]]$txt <- "등록"  
  ft$header$content$content$data[[7]]$txt <- "등록률"  
  ft$header$content$content$data[[8]]$txt <- "변화"  
  
  ft <- ft %>%
    colformat_double(j = c(2, 3, 5, 6), digits = 0) 
  ft <- ft %>%
    colformat_double(j = c(4, 7, 8), digits = 1) 
  
  header1 <- c("", paste0(year1, "학년도"), paste0(year2, "학년도"), "등록률")
  ft <- add_header_row(ft, values = header1, colwidths = c(1, 3, 3, 1))
  
  ft <- line_spacing(ft, space = spacing, part = "header") 
  ft <- line_spacing(ft, space = spacing, part = "body") 
  ft <- line_spacing(ft, space = spacing, part = "footer") 
  
  ft <- color(ft, i = which(column_red == TRUE), j = 1:8, color = "red")
  ft <- bold(ft, i = length(column_red), j = 1:8, bold = TRUE)
  
  ft <- autofit(ft)
  ft <- width(ft, j = ~area, width = 1.8)
  
  return(ft)
}


# 등록률: 대학별
my.register.id <- function(dfr= dfr, year1 = year1, year2 = year2, criteria = criteria, na.rm = TRUE, caption = NA, spacing = NA) {
  dfr_y1 <- dfr %>%
    filter(year == year1) %>%
    group_by(id) %>%
    summarise(N1sum_y1 = sum(N1), R1sum_y1 = sum(R1)) %>%
    mutate(R1RW_y1 = R1sum_y1/N1sum_y1 * 100)
  ft1 <- dfr_y1 %>%
    mutate_at(vars(id), funs(as.character(.))) %>%
    summarise(id = "전체", N1sum_y1 = sum(N1sum_y1), R1sum_y1 = sum(R1sum_y1), R1RW_y1 = sum(R1sum_y1)/sum(N1sum_y1)*100)
  ft1 <- rbind(dfr_y1, ft1)
  
  dfr_y2 <- dfr %>%
    filter(year == year2) %>%
    group_by(id) %>%
    summarise(N1sum_y2 = sum(N1), R1sum_y2 = sum(R1)) %>%
    mutate(R1RW_y2 = R1sum_y2/N1sum_y2 * 100)
  ft2 <- dfr_y2 %>%
    mutate_at(vars(id), funs(as.character(.))) %>%
    summarise(id = "전체", N1sum_y2 = sum(N1sum_y2), R1sum_y2 = sum(R1sum_y2), R1RW_y2 = sum(R1sum_y2)/sum(N1sum_y2)*100)
  ft2 <- rbind(dfr_y2, ft2)
  
  ft <- full_join(ft1, ft2) %>%
    mutate(R1W_change = R1RW_y2 - R1RW_y1)
  
  column_red = ft$R1W_change < criteria
  
  ft <- ft %>%
    flextable() 
  
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft$header$content$content$data[[1]]$txt <- "대학"  
  ft$header$content$content$data[[2]]$txt <- "모집"  
  ft$header$content$content$data[[3]]$txt <- "등록"  
  ft$header$content$content$data[[4]]$txt <- "등록률"  
  ft$header$content$content$data[[5]]$txt <- "모집"  
  ft$header$content$content$data[[6]]$txt <- "등록"  
  ft$header$content$content$data[[7]]$txt <- "등록률"  
  ft$header$content$content$data[[8]]$txt <- "변화"  
  
  ft <- ft %>%
    colformat_double(j = c(2, 3, 5, 6), digits = 0) 
  ft <- ft %>%
    colformat_double(j = c(4, 7, 8), digits = 1) 
  
  header1 <- c("", paste0(year1, "학년도"), paste0(year2, "학년도"), "등록률")
  ft <- add_header_row(ft, values = header1, colwidths = c(1, 3, 3, 1))
  
  ft <- line_spacing(ft, space = spacing, part = "header") 
  ft <- line_spacing(ft, space = spacing, part = "body") 
  ft <- line_spacing(ft, space = spacing, part = "footer") 
  
  ft <- color(ft, i = which(column_red == TRUE), j = 1:8, color = "red")
  ft <- bold(ft, i = length(column_red), j = 1:8, bold = TRUE)
  
  ft <- autofit(ft)
  ft <- width(ft, j = ~id, width = 2.2)
  
  return(ft)
}


# 등록률: 설립형태별
my.register.private <- function(dfr= dfr, year1 = year1, year2 = year2, criteria = criteria, na.rm = TRUE, caption = NA, spacing = NA) {
  dfr_y1 <- dfr %>%
    filter(year == year1) %>%
    group_by(private) %>%
    summarise(N1sum_y1 = sum(N1), R1sum_y1 = sum(R1)) %>%
    mutate(R1RW_y1 = R1sum_y1/N1sum_y1 * 100)
  ft1 <- dfr_y1 %>%
    mutate_at(vars(private), funs(as.character(.))) %>%
    summarise(private = "전체", N1sum_y1 = sum(N1sum_y1), R1sum_y1 = sum(R1sum_y1), R1RW_y1 = sum(R1sum_y1)/sum(N1sum_y1)*100)
  ft1 <- rbind(dfr_y1, ft1)
  
  dfr_y2 <- dfr %>%
    filter(year == year2) %>%
    group_by(private) %>%
    summarise(N1sum_y2 = sum(N1), R1sum_y2 = sum(R1)) %>%
    mutate(R1RW_y2 = R1sum_y2/N1sum_y2 * 100)
  ft2 <- dfr_y2 %>%
    mutate_at(vars(private), funs(as.character(.))) %>%
    summarise(private = "전체", N1sum_y2 = sum(N1sum_y2), R1sum_y2 = sum(R1sum_y2), R1RW_y2 = sum(R1sum_y2)/sum(N1sum_y2)*100)
  ft2 <- rbind(dfr_y2, ft2)
  
  ft <- full_join(ft1, ft2) %>%
    mutate(R1W_change = R1RW_y2 - R1RW_y1)
  
  column_red = ft$R1W_change < criteria
  
  ft <- ft %>%
    flextable() 
  
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "center", part = "body")
  
  ft$header$content$content$data[[1]]$txt <- "권역"  
  ft$header$content$content$data[[2]]$txt <- "모집"  
  ft$header$content$content$data[[3]]$txt <- "등록"  
  ft$header$content$content$data[[4]]$txt <- "등록률"  
  ft$header$content$content$data[[5]]$txt <- "모집"  
  ft$header$content$content$data[[6]]$txt <- "등록"  
  ft$header$content$content$data[[7]]$txt <- "등록률"  
  ft$header$content$content$data[[8]]$txt <- "변화"  
  
  ft <- ft %>%
    colformat_double(j = c(2, 3, 5, 6), digits = 0) 
  ft <- ft %>%
    colformat_double(j = c(4, 7, 8), digits = 1) 
  
  header1 <- c("", paste0(year1, "학년도"), paste0(year2, "학년도"), "등록률")
  ft <- add_header_row(ft, values = header1, colwidths = c(1, 3, 3, 1))
  
  ft <- line_spacing(ft, space = spacing, part = "header") 
  ft <- line_spacing(ft, space = spacing, part = "body") 
  ft <- line_spacing(ft, space = spacing, part = "footer") 
  
  ft <- color(ft, i = which(column_red == TRUE), j = 1:8, color = "red")
  ft <- bold(ft, i = length(column_red), j = 1:8, bold = TRUE)
  
  ft <- autofit(ft)
  ft <- width(ft, j = ~private, width = 1.8)
  
  return(ft)
}

