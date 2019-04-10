############## Endoscopy Clean-up functions##############


#' Cleans endoscopist column if present
#'
#' If an endoscopist column is part of the dataset once the extractor
#' function has been used this cleans the endoscopist column from the report.
#' It gets rid of titles
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor.
#'
#' @param EndoscopistColumn The endoscopy text column
#' @keywords Endoscopist extraction
#' @export
#' @return This returns a character vector
#' @importFrom stringr str_replace
#' @examples Myendo$Endoscopist<-EndoscEndoscopist(Myendo$Endoscopist)


EndoscEndoscopist <- function(EndoscopistColumn) {
  # Extraction of the Endoscopist
  EndoscopistColumn <- str_replace_all(EndoscopistColumn,"Mr|Professor|Prof|Dr|2nd.*|[^[:alnum:],]", "")
  # Put gaps between names
  EndoscopistColumn <- str_replace(EndoscopistColumn,"([a-z])([A-Z])", "\\1 \\2")
  #Trim the whitespace
  EndoscopistColumn <- trimws(EndoscopistColumn,which = c("both"))
  return(EndoscopistColumn)
}




#' Cleans instrument column if present
#'
#' This cleans the Instument column from the report assuming such a column exists
#' (where instrument usually refers to the endoscope number being used.)
#' It gets rid of common entries that are not needed.
#' It should be used after the Extractor.
#' @param EndoInstrument column of interest
#' @keywords Instrument
#' @return This returns a character vector
#' @importFrom stringr str_replace
#' @export
#' @examples Myendo$Instrument<-EndoscInstrument(Myendo$Instrument)

EndoscInstrument <- function(EndoInstrument) {
  # Extraction of the Instrument used:
  EndoInstrument<- trimws(toupper(str_replace_all(EndoInstrument,
                                                  "X.*[Ll][Oo][Aa][Nn] [Ss][Cc][Oo][Pp][Ee] \\(|
                                                  Loan Scope \\(specify serial no:|
                                                  Loan Scope \\(specify\\s*serial no|\\)|-.*|,.*|
                                                  :|FC |[Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee] |^,|
                                                  [Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no\\)\\s*|
                                                  [Ll][Oo][Aa][Nn]\\s+[Ss][Cc][Oo][Pp][Ee]\\(specify serial no:\\)\\s*", "")))
  EndoInstrument <- str_replace(EndoInstrument, "FC ", "FC")
  EndoInstrument <- str_replace(EndoInstrument,"^\\s*([1-9])", "A\\1")
  return(EndoInstrument)
}

############################# Extrapolating Endoscopy ################################


#' Cleans medication column if present
#'
#' This cleans medication column from the report assuming such a column exists.
#' It gets rid of common entries that are not needed. It also splits the
#' medication into fentanyl and midazolam numeric doses for use. 
#' It should be used after the Extractor
#' @param MedColumn column of interest as a string vector
#' @keywords Endoscopy medications
#' @return This returns a dataframe
#' @importFrom stringr str_extract str_replace
#' @export
#' @examples MyendoNew<-cbind(EndoscMeds(Myendo$Medications),Myendo)
#' 

EndoscMeds <- function(MedColumn) {
  # Extraction of the Medications: Extract the fentanyl if present
  
  dataframe<-data.frame(MedColumn,stringsAsFactors = FALSE)
  
  dataframe$Fent <-
    str_extract(dataframe$MedColumn, "[Ff]entanyl\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Fent <- as.numeric(str_replace_all(dataframe$Fent,"[Ff]entanyl|mcg", ""))
  
  # Extract the midazolam if present
  
  dataframe$Midaz <-
    str_extract(dataframe$MedColumn, "[Mm]idazolam\\s*(\\d*(\\.\\d+)?)\\s*mg")
  dataframe$Midaz <- as.numeric(str_replace_all(dataframe$Midaz,"[Mm]idazolam|mg", ""))
  
  
  # Extract the pethidine if present
  dataframe$Peth <-
    str_extract(dataframe$MedColumn, "[Pp]ethidine\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Peth <- as.numeric(str_replace_all(dataframe$Peth,"[Pp]ethidine|mcg", ""))
  
  
  # Extract the propofol if present
  dataframe$Prop <-
    str_extract(dataframe$MedColumn, "[Pp]ropofol\\s*(\\d*(\\.\\d+)?)\\s*mcg")
  dataframe$Prop <- as.numeric(str_replace_all(dataframe$Prop,"[Pp]ropofol|mcg ", ""))
  
  #Drop the first column to avoid repetition
  
  return(dataframe)
}


#' EndoscopyEvent 
#'
#' This extracts the endoscopic event. It looks for the event term and then looks in the event sentence as well as the one above to see if
#' the location is listed. It needs to be run AFTER the HistolTypeAndSite function as emr needs to be
#' added to the event. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param dataframe datafrane of interest
#' @param EventColumn1 The relevant endoscopt free text column describing the findings
#' @param Procedure Column saying which procedure was performed
#' @param Macroscopic Column describing all the macroscopic specimens
#' @param Histology Column with free text histology (usually microscopic histology)
#' @return This returns a character vector
#' @export
#' @examples # Myendo$EndoscopyEvent<-EndoscopyEvent(Myendo,"Findings",
#' #"ProcedurePerformed","MACROSCOPICALDESCRIPTION","HISTOLOGY")

EndoscopyEvent<-function(dataframe,EventColumn1,Procedure,Macroscopic,Histology){
  
  
  dataframe<-data.frame(dataframe,stringsAsFactors = FALSE)
  
  # Extract the events from the 
  output<-EntityPairs_TwoSentence(dataframe[,EventColumn1],EventList(),LocationList())
  
  MyHistolEvents<-HistolTypeAndSite(dataframe[,Histology],dataframe[,Macroscopic],dataframe[,Procedure])
  output<-unlist(lapply(output, function(x) paste(x,collapse=";")))
  
  #Add emr only if this is seen in the histopath
  #Remove EMR from events if not seen in histopath
  
  #If emr is in the histology and in the event then leave it
  output<-ifelse(grepl("emr",MyHistolEvents,ignore.case = TRUE)&grepl("(oesophagus|goj):emr",output,ignore.case = TRUE),output,
                 #If emr is in the histology but not in the event then dont add it (sometimes EMR written in the request as past therapy
                 #but  it hasn't actually been done)
                 ifelse(grepl("emr",MyHistolEvents,ignore.case = TRUE)&!grepl("emr",output,ignore.case = TRUE),gsub("[A-Za-z]+:emr","",output),
                        #If emr is not in the histology but is in the event then remove from the event, otherwise leave as output
                        ifelse(!grepl("emr|nodul",MyHistolEvents,ignore.case = TRUE)&grepl("emr",output,ignore.case = TRUE),gsub("[A-Za-z]+:emr","",output),output)))
  
  
  
  d<-lapply(output, function(x) strsplit(x,";"))
  t<-lapply(d,function(x) unlist(x))
  out<-lapply(t,function(x) unique(x))
  output<-unlist(lapply(out, function(x) paste(x,collapse=";")))
  #output<-unlist(output)
  #Need to know if emr done here so can add it
  return(output)
}



############################# Extrapolating Histology ################################


#' Extract the number of biopsies taken from histology report
#'
#' This extracts the number of biopsies taken from the pathology report.
#' This is usually from the Macroscopic description column.
#' It collects everything from the regex [0-9]{1,2}.{0,3}
#' to whatever the string boundary is (z).
#'
#' @param inputString The input text to process
#' @param regString The keyword to remove and to stop at in the regex
#' @importFrom stringr str_match_all str_replace_all
#' @keywords Biopsy number
#' @export
#' @examples
#' qq<-HistolNumbOfBx(Mypath$Macroscopicdescription,'specimen')


HistolNumbOfBx <- function(inputString, regString) {
  inputString <- DictionaryInPlaceReplace(inputString,WordsToNumbers())
  mylist <-
    #I need to collapse the unlist
    stringr::str_match_all(inputString, 
                           paste(unlist(lapply(strsplit(regString,"\\|",fixed=FALSE),
                                               function(x){paste("[0-9]{1,2}.{0,3}",x, sep = "")})),collapse="|"))
  NumbOfBx <-
    vapply(mylist, function(p)
      sum(as.numeric(stringr::str_replace_all(p,regString,""))),numeric(1))
  return(NumbOfBx)
}


#' Determine the largest biopsy size from the histology report
#'
#' This extracts the biopsy size from the report. If there are multiple
#' biopsies it will extract the overall size of each one (size is calculated
#' usually in cubic mm from the three dimensions provided). This will result
#' in row duplication.
#'
#' This is usually from the Macroscopic description column.
#' @param MacroColumn Macdescrip
#' @importFrom stringr  str_match str_replace
#' @keywords biopsy size
#' @export
#' @examples rr<-HistolBxSize(Mypath$Macroscopicdescription)

HistolBxSize <- function(MacroColumn) {
  # What's the average biopsy size this month?
  BxSize <- str_extract(MacroColumn, "the largest.*?mm")
  BxSize <- str_replace(BxSize,"the largest measuring |mm|less than", "")
  strBxSize <- "([0-9]+).*?([0-9])+.*?([0-9])"
  BxSize <-
    as.numeric(str_match(BxSize, strBxSize)[, 2]) *
    as.numeric(str_match(BxSize, strBxSize)[, 3]) *
    as.numeric(str_match(BxSize, strBxSize)[, 4])
  return(BxSize)
}




#' HistolTypeAndSite 
#'
#' This needs some blurb to be written. Used in the OPCS4 coding
#' @keywords Find and replace
#' @param inputString1 The first column to look in 
#' @param inputString2 The second column to look in 
#' @param procedureString The column with the procedure in it
#' @importFrom stringr str_remove_all str_replace_all
#' @export
#' @return a list with two columns, one is the type and site and the other
#' is the index to be used for OPCS4 coding later if needed.
#' @examples  Myendo$PathSite<-HistolTypeAndSite(Myendo$Original.x,
#' Myendo$mscroscopicaldescription,Myendo$procedureperformed) #This example needs correction


HistolTypeAndSite<-function(inputString1,inputString2,procedureString){
  
  output<-ifelse(EntityPairs_OneSentence(inputString1,HistolType(),LocationList())=="NA:NA", 
                 EntityPairs_OneSentence(inputString2,HistolType(),LocationList()),
                 EntityPairs_OneSentence(inputString1,HistolType(),LocationList()))
  
  #If there is only a colon (punctuation) and then empty then assume it is a biopsy
  output<-str_replace_all(output, ":,|:$", ":biopsy,")
  
  #Make sure only unique values represented:
  output<-lapply(output, function(x) paste(unlist(unique(unlist(strsplit(x,",")))),collapse=","))
  
  output<-ifelse(grepl("Gastroscopy",procedureString),
                 str_remove_all(output, paste0('(',tolower( paste0(unlist(LocationListLower(),use.names=F),collapse="|")),')',':biopsy')),
                 ifelse(grepl("Colonoscopy|Flexi",procedureString),
                        str_remove_all(output, paste0('(',tolower(paste0(unlist(LocationListUpper(),use.names=F),collapse="|")),')',':biopsy')),output))
  
  output<-unlist(output)
  
  biopsyIndexresults<-ExtrapolatefromDictionary(output,BiopsyIndex())
  
  output<-list(HistolTypeAndSite=output,BiopsyIndex=biopsyIndexresults)
  return(output)
}

#' Primary Diagnosis from Endoscopy
#'
#' This function extracts the primary diagnosis from the endoscopy free text. It will also add
#' pathology diagnosis to the final primary code
#' 
#'
#' @param dataframe the dataframe
#' @param Procedure the Procedure column
#' @param PathSite the PathSite column
#' @param FINDINGS the FINDINGS column
#' @param ENDOSCOPICDIAGNOSIS the ENDOSCOPICDIAGNOSIS column
#' @param EndoscopyEvent the EndoscopyEvent column
#' @keywords OPCS-4 codes extraction
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @importFrom stringi stri_split_lines
#' @export
#' @examples # Need to run the HistolTypeSite and EndoscopyEvent functions first here
#' # SelfOGD_Dunn$OPCS4w<-ExtrapolateOPCS4Prep(SelfOGD_Dunn,"PROCEDUREPERFORMED",
#' # "PathSite","EndoscopyEvent")

dev_ExtrapolateEndoscopicICD10 <- function(dataframe, Procedure,PathSite,FINDINGS,ENDOSCOPICDIAGNOSIS,EndoscopyEvent) { 
  
  
  #Just get the uppers for now:
  dataframe<-SelfOGD_Dunn[grepl("Gastroscopy",dataframe$Procedure),]
  
  #Merge the findings in with the myDx:
  dataframe$FINDINGSmyDx<-paste(dataframe$FINDINGS,dataframe$ENDOSCOPICDIAGNOSIS)
  
  #Split into a string
  dataframe$FINDINGSmyDx<-stri_split_lines(dataframe$FINDINGSmyDx,omit_empty = TRUE)
  
  #Copy over to a new column to be sampled
  dataframe$FindingsAfterProcessing<-dataframe$FINDINGSmyDx
  
  
  #Use case when on nested list to generate ICD-10 codes and then remove from the list.
  
  #If it matches the grep then add the diagnosis, and then chop into list and remove that string
  
  
  dataframe<-dataframe %>%
    mutate(
      PrimaryDiagnosisCode = map(
        FINDINGSmyDx, ~ case_when(
          grepl("mitotic|emr|tumour", unlist(tolower(.x)),ignore.case=TRUE) ~ "C159  -  Malignant neoplasm oesophagus, unspecified - Oesophagus - unspecified",
          grepl("dysplasia", unlist(tolower(.x)),ignore.case=TRUE) ~ "K229  -  Disease of oesophagus - unspecified",
          grepl("stricture", unlist(tolower(.x)),ignore.case=TRUE) ~  "K222  -  Oesophageal obstruction",
          grepl("barrett", unlist(tolower(.x)),ignore.case=TRUE) ~  "K227  -  Barrett's oesophagus",
          grepl("(\\.|^)(?=[^\\.]*inlet)(?=[^\\.]*patch)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K228  -  Other specified diseases of oesophagus",
          grepl("hiatus",tolower(.x), unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K449  -  Diaphragmatic hernia without obstruction or gangrene",
          grepl("oesophagitis",tolower(.x), unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K20  -  Oesophagitis",
          grepl("(?=[^\\.]*(duodenal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K26  -  Duodenal ulcer",
          grepl("(?=[^\\.]*(oesophageal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)", unlist(tolower(.x)),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          TRUE ~ "")
      )
    )
  
  dataframe$FINDINGSmyDx<-lapply(dataframe$FINDINGSmyDx, function(x) x[!grepl("OESOPH.*","", x,ignore.case=TRUE,perl=TRUE)])
  
  dataframe<-dataframe %>%  FindingsAfterProcessing = map(
    FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
  )
  
  
  
  
  ######### Go through it again for the second codes:
  
  dataframe<-dataframe %>%
    mutate(
      OverallDiagnosisCode = map(
        FindingsAfterProcessing, ~ case_when(
          grepl("mitotic|emr|tumour", tolower(.x)) ~ "C159  -  Malignant neoplasm oesophagus, unspecified - Oesophagus - unspecified",
          grepl("dysplasia", tolower(.x)) ~ "K229  -  Disease of oesophagus - unspecified",
          grepl("stricture",tolower(.x),ignore.case=TRUE) ~  "K222  -  Oesophageal obstruction",
          grepl("barrett",tolower(.x),ignore.case=TRUE) ~  "K227  -  Barrett's oesophagus",
          grepl("(\\.|^)(?=[^\\.]*inlet)(?=[^\\.]*patch)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K228  -  Other specified diseases of oesophagus",
          grepl("hiatus",tolower(.x),ignore.case=TRUE) ~  "K449  -  Diaphragmatic hernia without obstruction or gangrene",
          grepl("oesophagitis",tolower(.x),ignore.case=TRUE) ~  "K20  -  Oesophagitis",
          grepl("(?=[^\\.]*(duodenal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K26  -  Duodenal ulcer",
          grepl("(?=[^\\.]*(oesophageal|oesophagus))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          grepl("gastritis",tolower(.x),ignore.case=TRUE) ~  "K297  -  Gastritis - unspecified",
          grepl("duodenitis",tolower(.x),ignore.case=TRUE) ~  "K298  -  Duodenitis",
          grepl("(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K317  -  Polyp of stomach and duodenum",
          grepl("candid",tolower(.x),ignore.case=TRUE) ~  "B387  -  Candidiasis of other sites",
          TRUE ~ "")
      ),
      FindingsAfterProcessing = map(
        FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr|tumour|dysplasia|hiatus|stricture|barrett|inlet patch|
                                             hiatus|esophagitis|duodenitis|gastritis)|(?:(?=[^\\.]*(duodenum|oesophagus|gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$))|
                                             (?:(?=[^\\.]*(stomach))(?=[^\\.]*polyp)[^\\.]*(\\.|$))|(candid)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
      )
        )
  
  
  dataframe$OverallDiagnosisCode<-lapply(dataframe$OverallDiagnosisCode,function(x) (unique(x)))
  dataframe$OverallDiagnosisCode<-lapply(dataframe$OverallDiagnosisCode, function(x) x[x != "" & x != "\n"])
  
  
  #Now go through the OverallCodes and determine the 
  
  
  ######### Go through it again for the third codes: 
  
  dataframe<-dataframe %>%
    mutate(
      ThirdDiagnosisCode = map(
        FindingsAfterProcessing, ~ case_when(
          grepl("mitotic|emr|tumour", tolower(.x)) ~ "C159  -  Malignant neoplasm oesophagus, unspecified - Oesophagus - unspecified",
          grepl("dysplasia", tolower(.x)) ~ "K229  -  Disease of oesophagus - unspecified",
          grepl("stricture",tolower(.x),ignore.case=TRUE) ~  "K222  -  Oesophageal obstruction",
          grepl("barrett",tolower(.x),ignore.case=TRUE) ~  "K227  -  Barrett's oesophagus",
          grepl("(\\.|^)(?=[^\\.]*inlet)(?=[^\\.]*patch)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K228  -  Other specified diseases of oesophagus",
          grepl("hiatus",tolower(.x),ignore.case=TRUE) ~  "K449  -  Diaphragmatic hernia without obstruction or gangrene",
          grepl("oesophagitis",tolower(.x),ignore.case=TRUE) ~  "K20  -  Oesophagitis",
          grepl("(?=[^\\.]*(duodenal))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K26  -  Duodenal ulcer",
          grepl("(?=[^\\.]*(oesophageal|oesophagus))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K221  -  Oesophageal ulcer",
          grepl("(?=[^\\.]*(gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K25  -  Gastric ulcer",
          grepl("gastritis",tolower(.x),ignore.case=TRUE) ~  "K297  -  Gastritis - unspecified",
          grepl("duodenitis",tolower(.x),ignore.case=TRUE) ~  "K298  -  Duodenitis",
          grepl("(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$)",tolower(.x),ignore.case=TRUE,perl=TRUE) ~  "K317  -  Polyp of stomach and duodenum",
          grepl("candid",tolower(.x),ignore.case=TRUE) ~  "B387  -  Candidiasis of other sites",
          TRUE ~ "")
      ),
      FindingsAfterProcessing = map(
        FindingsAfterProcessing, ~ .x[!grepl("(mitotic|emr|tumour|dysplasia|hiatus|stricture|barrett|inlet patch|
                                             hiatus|esophagitis|duodenitis|gastritis)|(?:(?=[^\\.]*(duodenum|oesophagus|gastric|stomach|pylor))(?=[^\\.]*ulcer)[^\\.]*(\\.|$))|
                                             (?:(?=[^\\.]*(gastric))(?=[^\\.]*polyp)[^\\.]*(\\.|$))|(candid)", tolower(.x),ignore.case=TRUE,perl=TRUE)]
      )
    )
  
  #To Do: Also assess the findings column
  
  
  
  return(primDxVector)
}








