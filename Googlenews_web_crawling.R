#########################################
# GOOGLE NEWS CRAWLER - v1.1
#########################################
# load library and user function ------------------------------------------
#install.packages(c("httr","xml2","stringr","lubridate","gridExtra"))

library(httr)
library(xml2)
library(stringr)
library(lubridate)
library(gridExtra)
library(grid)
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# control keyword and number of posts -------------------------------------
# 검색어

keyWord=read.csv(paste0(getwd(),"/","data","/","keyword.csv"))
keyword=keyWord
result=list()


# 검색 뉴스 수 (너무 많을 경우, 블락당할 수 있음)
numOfPost=1
result=list()

for (i in 1:nrow(keyWord)){
# google news crawler main part -------------------------------------------
# 키워드 변환
 keyWordUTF8 <- URLencode(iconv(keyWord[i,],to="UTF-8"))




# 크롤링 페이지 분할
startPosts    <- seq(1,numOfPost,100)
startPosts[1] <- startPosts[1] - 1
if (numOfPost%%100==0) {
  endPosts      <- rep(100,numOfPost/100)
} else {
  endPosts      <- c(rep(100,floor(numOfPost/100)),numOfPost%%100)
}

# 결과셋 생성
#result      <- data.frame(stringsAsFactors=FALSE)

for (j in seq(length(startPosts))) {
  # 뉴스 가져오기
  url         <- paste0("https://www.google.co.kr/search?hl=ko&q=",keyWordUTF8,"&tbm=nws&start=",startPosts[j],"&num=",endPosts[j])
  doc         <- content(GET(url), "text")
  # doc         <- readLines(url,warn=FALSE)
  doc_table   <- str_split(doc,"<table><tr>")
  
  for(p in seq(endPosts[j])+1) {
    # 뉴스 정제
    doc_tab     <- str_split(doc_table[[1]][p],"</div>")
    doc_tab2    <- str_split(doc_tab[[1]][1],"</a></h3>")
    
    # 뉴스 제목
    title       <- unescape_html(doc_tab2[[1]][1])
    
    # 뉴스 기사
    article     <- unescape_html(gsub("&nbsp;..."," (중략)...",doc_tab[[1]][2]))
    
    # 뉴스 제공업체
    supplier    <- str_split(unescape_html(doc_tab2[[1]][2])," - ")[[1]][1]
    
    # 뉴스 URL
    news_url    <- URLdecode(str_sub(doc_tab2[[1]][1]
                                     ,str_locate(doc_tab2[[1]][1],"\"/url")[2]+4
                                     ,str_locate(doc_tab2[[1]][1],"&")[1]-1)) 

    # 뉴스 게시 시간
    tmp_time    <- str_split(unescape_html(doc_tab2[[1]][2])," - ")[[1]][2]
    if(length(grep(" 전",tmp_time))==0){
      post_time <- as_date(tmp_time)
    } else {
      post_time <- lubridate::ymd_hms(Sys.time())
      num       <- as.integer(gsub("[^0-9]","",tmp_time))
      unit      <- gsub("([0-9]*)| 전","",tmp_time)
      if(unit=="일"){
        lubridate::day(post_time) <- lubridate::day(post_time) - num
      } else if(unit=="시간"){
        lubridate::hour(post_time) <- lubridate::hour(post_time) - num
      } else if(unit=="분"){
        lubridate::minute(post_time) <- lubridate::minute(post_time) - num
      } else if(unit=="초"){
        lubridate::second(post_time) <- lubridate::second(post_time) - num
      }
      post_time <- as_date(post_time)
    }
    
    # 데이터프레임화
    res         <- data.frame(키워드=keyword[p,1]
                              ,제목=title
                              ,기사내용=article
                              ,신문사=supplier
                              ,URL=news_url
                              ,게시시간=post_time
                              ,stringsAsFactors=FALSE)
    
    result      <- rbind(result, res)
  }
  print(paste0("In Progressing! (",nrow(result),"/",nrow(keyWord),")"))
  
  # 빠르게 자주 사용하면 블락당함..
  Sys.sleep(3)
 }
}

# 완성 데이터 확인
View(result)

#pdf 파일로 저장

pdf(paste0(getwd(),"/","Google news","/",Sys.Date(),"_KBLife_news",".pdf"),width =29,family="Korea1deb", paper="special"
    ,colormodel ="srgb")
grid.table(result)
dev.off()


#write.csv(data.frame(result), paste0(getwd(),"/","Google news","/",Sys.Date(),"_KBLife_news",".csv"),row.names=FALSE,fileEncoding="CP949")