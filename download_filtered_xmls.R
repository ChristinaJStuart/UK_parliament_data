library(tidyverse)
library(rvest)
library(xml2)

### Step 1) XML URL COLLECTION ###

# First get the contents of the html that lists the urls to the XML files
theyworkforyou_xml_list_html <- read_html("https://www.theyworkforyou.com/pwdata/scrapedxml/debates/")

# Get the links to those XMLs from the HTML
theyworkforyou_xml_list_html

anchor_elements <- theyworkforyou_xml_list_html %>%
  html_elements("a[href]")


href_values <- anchor_elements %>%
  html_attr("href") %>%
  as.array()

# We collect all the XML links that we need to download
links_to_download = c()
for (href_value in href_values) {
  if (grepl('debates', href_value)) {
    year_string = as.numeric(substring(href_value, 8, 11))
    if (year_string > 1999) {
      links_to_download <- c(links_to_download, href_value)
    }
  }
}
number_of_links <- length(links_to_download)

### Step 2) XML Download ###

# Create a directory for saving XMLs
dir_path <- "raw_xmls"
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
  message("Created directory: ", dir_path)
} else {
  message("Directory already exists: ", dir_path)
}

# Define phrases to look for in XMLs (they are only saved if contained)
climate_related_phrases = c('green growth', 'green economy', 'sustainable growth', 
                            'inclusive growth', 'low-carbon growth', 'clean growth')

# Iterate XML URLs and check if any of the climate related phrases are contained
#  in their speech paragraphs
base_url = 'https://www.theyworkforyou.com/pwdata/scrapedxml/debates/'
counter = 0
for (xml_name in links_to_download) {
  counter = counter + 1
  print(paste0('Processing XML ', counter, '/', number_of_links))
  # Try reading XML URL
  tryCatch(
    {xml_content <-read_xml(paste0(base_url, xml_name))},
    error=function(cond) {
      message(cond, 'error when reading: ', paste0(base_url, xml_name))
    }
  )
  
  # Extract paragraph texts from XML
  namespaces <- xml_ns(xml_content)
  paragraph_texts <- xml_content %>%
    xml_find_all(".//p", ns=namespaces) %>%
    xml_text()
  
  # Check if any of the paragraphs contain any of the phrases
  contain_matrix <-sapply(climate_related_phrases, function(pattern) {
    grepl(pattern, paragraph_texts, ignore.case = TRUE)
  })

  if (TRUE %in% contain_matrix) {
    print(paste0('Saving XML: ', xml_name))
    write_xml(xml_content, paste0(dir_path, '/', xml_name))
  }
}
