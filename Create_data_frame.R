## step 3 - create data frame :)

df <- data.frame()
climate_related_phrases = c('green growth', 'green economy', 'sustainable growth', 
                            'inclusive growth', 'low-carbon growth', 'clean growth')


files <- list.files(path="raw_xmls", full.names=TRUE, recursive=FALSE)
for (file in files) {
  xml_content <-read_xml(file)
  namespaces <- xml_ns(xml_content)
  speech_tags <- xml_content %>%
    xml_find_all(".//speech", ns=namespaces)
  for (speech_tag in speech_tags){
    speech_tag_paragraphs <- speech_tag %>%
      xml_find_all(".//p", ns=namespaces) %>%
      xml_text()
    contain_matrix <-sapply(climate_related_phrases, function(pattern) {
      grepl(pattern, speech_tag_paragraphs, ignore.case = TRUE)
    })
    
    if (TRUE %in% contain_matrix) {
      speech_text <- paste(speech_tag_paragraphs, collapse = "\n")
      speakername <- xml_attr(speech_tag,"speakername")
      id <- xml_attr(speech_tag, "id")
      
      prelim.df <- data.frame(id = c(id), speakername = c(speakername), speech_text = c(speech_text))
      df <- rbind(df, prelim.df)
    }
  }
}

write.csv(df, "green_growth_debates.csv")



