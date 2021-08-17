require(rvest)
require(httr)
require(dplyr)
require(stringr)
require(tidytext)
require(tidyr)
require(tm)
require(wordcloud2)
require(textcat)
require(ggplot2)

#coloca a url geral da pagina que quer coletar


#colocou a sequencia que encontra o texto
x <- seq(1, 30, by=1)

#fez uma matrix para acomodar o coletado
ini = 30 
discursos<- matrix("aqui",nrow= length(x), ncol= 3) %>% 
  as_tibble()
#o que gravou no inspecionar (copy xpath), aqui faz o for para baixar o que quer
for(j in 0:15){
  if (j == 0){
    url_discursos <- paste("https://www.gov.br/planalto/pt-br/acompanhe-o-planalto/discursos?b_start:int=",j,sep='')
    
  }else{
    url_discursos <- paste("https://www.gov.br/planalto/pt-br/acompanhe-o-planalto/discursos?b_start:int=",ini,sep='')
    ini = ini + 30
  }
  discursos_temp <- matrix("aqui",nrow= length(x), ncol= 3) %>% 
    as_tibble()
  print(paste('página: ',j+1,sep = ""))
  for(i in 1:length(x)){
    acessar <- url_discursos %>%
      GET() %>% 
      read_html() %>% 
      html_nodes(xpath = str_c('//*[@id="content-core"]/article[',x[i],']')) 
    print(acessar)
    print(xml_attrs(xml_child(xml_child(xml_child(acessar[[1]], 1), 1), 1))[["href"]])
    url_temp <- xml_attrs(xml_child(xml_child(xml_child(acessar[[1]], 1), 1), 1))[["href"]]
    url_discursos_tab <- url_temp %>%
      GET()%>%
      read_html() %>%
      html_nodes(xpath = str_c('//*[@id="content-core"]')) %>% 
      html_text() 
    url_discursos_tab1 <- url_temp %>%
      GET()%>%
      read_html() %>%
      html_nodes(xpath = str_c('//*[@id="content"]/h1')) %>% 
      html_text() 
    url_discursos_tab2 <- url_temp %>%
      GET()%>%
      read_html() %>%
      html_nodes(xpath = str_c('//*[@id="plone-document-byline"]/span[1]/span[2]')) %>% 
      html_text() 
    
    discursos_temp[i,1] <-  url_discursos_tab2 
    discursos_temp[i,2] <-  url_discursos_tab1
    discursos_temp[i,3] <-  url_discursos_tab
  }
  if (j == 0){
    discursos <- data.frame(discursos_temp)
  }else{
    discursos<- rbind(discursos,discursos_temp)
  }
  
}
discursos_temp = discursos_temp[1:22,]
discursos<- rbind(discursos,discursos_temp)
discursos$V3 <- str_remove_all(discursos$V3,'^(\n                                 \n\n)')
discursos$V3 <- str_remove_all(discursos$V3,'(\n \n\n\n)$')


discursos$V3 <- str_trim(discursos$V3)
names(discursos) = c('DataHora','Titulo','Texto')
#discursos$V3 = str_replace_all(discursos$Texto,'^(.*?[0-9][0-9][0-9][0-9])', "")
discursos$V4 = NULL
#export(discursos, file = "discursos.txt")
library(xlsx)
#inserir caminho aonde deseja salvar o arquivo xlsx
caminho = "C:\"
write.xlsx(discursos, file = caminho)

#organizou a matrix com os textos
discursos <- discursos %>% 
  setNames(c("Descricao")) %>% 
  mutate(Ep = c(1:length(x)))
