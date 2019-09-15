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
# �˻���

keyWord=read.csv(paste0(getwd(),"/","data","/","keyword.csv"))
keyword=keyWord
result=list()


# �˻� ���� �� (�ʹ� ���� ���, �������� �� ����)
numOfPost=1
result=list()

for (i in 1:nrow(keyWord)){
# google news crawler main part -------------------------------------------
# Ű���� ��ȯ
 keyWordUTF8 <- URLencode(iconv(keyWord[i,],to="UTF-8"))




# ũ�Ѹ� ������ ����
startPosts    <- seq(1,numOfPost,100)
startPosts[1] <- startPosts[1] - 1
if (numOfPost%%100==0) {
  endPosts      <- rep(100,numOfPost/100)
} else {
  endPosts      <- c(rep(100,floor(numOfPost/100)),numOfPost%%100)
}

# ����� ����
#result      <- data.frame(stringsAsFactors=FALSE)

for (j in seq(length(startPosts))) {
  # ���� ��������
  url         <- paste0("https://www.google.co.kr/search?hl=ko&q=",keyWordUTF8,"&tbm=nws&start=",startPosts[j],"&num=",endPosts[j])
  doc         <- content(GET(url), "text")
  # doc         <- readLines(url,warn=FALSE)
  doc_table   <- str_split(doc,"<table><tr>")
  
  for(p in seq(endPosts[j])+1) {
    # ���� ����
    doc_tab     <- str_split(doc_table[[1]][p],"</div>")
    doc_tab2    <- str_split(doc_tab[[1]][1],"</a></h3>")
    
    # ���� ����
    title       <- unescape_html(doc_tab2[[1]][1])
    
    # ���� ���
    article     <- unescape_html(gsub("&nbsp;..."," (�߷�)...",doc_tab[[1]][2]))
    
    # ���� ������ü
    supplier    <- str_split(unescape_html(doc_tab2[[1]][2])," - ")[[1]][1]
    
    # ���� URL
    news_url    <- URLdecode(str_sub(doc_tab2[[1]][1]
                                     ,str_locate(doc_tab2[[1]][1],"\"/url")[2]+4
                                     ,str_locate(doc_tab2[[1]][1],"&")[1]-1)) 

    # ���� �Խ� �ð�
    tmp_time    <- str_split(unescape_html(doc_tab2[[1]][2])," - ")[[1]][2]
    if(length(grep(" ��",tmp_time))==0){
      post_time <- as_date(tmp_time)
    } else {
      post_time <- lubridate::ymd_hms(Sys.time())
      num       <- as.integer(gsub("[^0-9]","",tmp_time))
      unit      <- gsub("([0-9]*)| ��","",tmp_time)
      if(unit=="��"){
        lubridate::day(post_time) <- lubridate::day(post_time) - num
      } else if(unit=="�ð�"){
        lubridate::hour(post_time) <- lubridate::hour(post_time) - num
      } else if(unit=="��"){
        lubridate::minute(post_time) <- lubridate::minute(post_time) - num
      } else if(unit=="��"){
        lubridate::second(post_time) <- lubridate::second(post_time) - num
      }
      post_time <- as_date(post_time)
    }
    
    # ������������ȭ
    res         <- data.frame(Ű����=keyword[p,1]
                              ,����=title
                              ,��系��=article
                              ,�Ź���=supplier
                              ,URL=news_url
                              ,�Խýð�=post_time
                              ,stringsAsFactors=FALSE)
    
    result      <- rbind(result, res)
  }
  print(paste0("In Progressing! (",nrow(result),"/",nrow(keyWord),")"))
  
  # ������ ���� ����ϸ� ��������..
  Sys.sleep(3)
 }
}

# �ϼ� ������ Ȯ��
View(result)

#pdf ���Ϸ� ����

pdf(paste0(getwd(),"/","Google news","/",Sys.Date(),"_KBLife_news",".pdf"),width =29,family="Korea1deb", paper="special"
    ,colormodel ="srgb")
grid.table(result)
dev.off()


#write.csv(data.frame(result), paste0(getwd(),"/","Google news","/",Sys.Date(),"_KBLife_news",".csv"),row.names=FALSE,fileEncoding="CP949")