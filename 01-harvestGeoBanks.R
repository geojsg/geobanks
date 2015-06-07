## Loading Libraries
library(httr)
library(XML)
library(rvest)
library(selectr)

## Output directory
dir_output<-"."


## Establishing connection with the source webpage

cafile <- system.file("CurlSSL", "cacert.pem", package = "RCurl")
doc.html <- GET(
  "https://www.nbg.gov.ge/", 
  path="index.php", 
  query="m=404&lng=eng",
  config(cainfo = cafile)
)
h = htmlParse(doc.html)


## Extracting a clean list of Banks and store it in a CSV file
h %>% html_nodes("strong") %>% html_text() %>% gsub('\\\"',"",.) %>% 
regmatches(.,regexpr("JSC.*", .))%>% substring(.,4) %>%
gsub("^[[:space:]]","",.)%>% gsub("[[:space:]]$","",.) %>% toupper() %>%
gsub("[[:space:]]{2,}","",.)  -> banknames

write.csv(data.frame("Names"=banknames),paste(dir_output,"banknames.csv",sep="/"),row.names=F)


## Extracting URLs of banks' quarterly reports (Excel format only)
h %>% html_nodes(xpath="//a") %>% html_attr("href") %>%
regmatches(.,regexpr(".*/banks/.*(xls|XLS).*", .)) %>% 
gsub("\\.\\.","https://www.nbg.gov.ge",.)-> links


## Downloading banks' quarterly reports
d<-lapply(links, function(x) download.file(x,paste(dir_output,"Banks",regmatches(x,regexpr("[^/]*\\.[^/]*$", x)),sep="/"),method="libcurl", mode="wb"))
