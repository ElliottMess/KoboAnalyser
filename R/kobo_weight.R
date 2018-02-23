#' @name kobo_weight
#' @rdname kobo_weight
#' @title  Weight the data

#' @description  Automatically weight the data according to the information of 0-config.R
#' @param mainDir
#' @author Elliott Messeiller
#'
#' @examples
#' kobo_weight()
#'
#' @export kobo_weight
#' @examples
#' \dontrun{
#' kobo_weight()
#' }
#'
#'

kobo_weight<- function(mainDir='') {
  if (mainDir==''){
    mainDir <- getwd()
  }

  source(paste0(mainDir,"/code/0-config.R"), local=TRUE)

  sampling <- read_excel(path.to.form, sheet = "sampling_frame")
  data$weight <- ""

  if(usedweight=='sampling_frame'){
    if(usedsampling=='2_stages'){

      stratas <- sampling

      stratified <- unique(sampling$strata)

      name_col_sample <- names(sampling)
      normal_col <- c("id_sampl",	"Survey",	"pop",	"psu",	"SumDist",	"proba",	"survey_buffer")

      if (sum(normal_col %in% name_col_sample)==length(normal_col)){


          if (length(stratified)>1){
            normal_col <- c("X__1","id_sampl",	"Survey",	"pop",	"psu",	"SumDist",	"proba",	"survey_buffer")
            stratas <- unique(stratas[setdiff(names(stratas),normal_col)])


          }

          if (length(stratified)==1){
            normal_col <- c("X__1","id_sampl",	"Survey",	"strata",	"pop",	"psu",	"SumDist",	"proba",	"survey_buffer")
            stratas <- unique(stratas[setdiff(names(stratas),normal_col)])

          }
          if (length(unique(stratas$strata %in% dico$name))==1){
            strat_row_n <- match(stratas[1,"strata"],dico$name)

            fullname_strata <- as.character(dico[strat_row_n,"fullname"])
            fullname_strata <- data.frame(strsplit(fullname_strata,"\\."))
            fullname_strata <- data.frame(fullname_strata[-nrow(fullname_strata),])
            fullname_strata <- as.character(fullname_strata[nrow(fullname_strata),])

            names(stratas)[names(stratas)=="strata"] <- fullname_strata
            names(sampling)[names(sampling)=="strata"] <- fullname_strata


          }

          col_stratas <- data.frame(colnames(stratas), stringsAsFactors = FALSE)
          for (j in 1:nrow(col_stratas)){
            split_temp<-as.character(col_stratas[j,])
            names(stratas)[names(stratas)==split_temp] <- as.character(dico[dico$name==split_temp,c("fullname"),])
            names(sampling)[names(sampling)==split_temp] <- as.character(dico[dico$name==split_temp,c("fullname"),])

          }

          n_col_stratas <-ncol(stratas)
          names_stratas <- names(stratas)
          sampling$actual_sample <- ""
          sampling$weight <- ""
          tot_pop <- sum(sampling$pop)
          if (n_col_stratas>2){
            cat("Sorry, only two stratas supported")
          }
          if(n_col_stratas==1){
            strata1 <- names_stratas[1]
            for (j in 1:nrow(sampling)){
              v_strata1 <- as.character(stratas[j,1])
              sampling[j,"actual_sample"]<- as.numeric(sum(data[,strata1]==v_strata1))
            }
          }
          if (n_col_stratas==2){
            strata1 <- names_stratas[1]
            strata2 <- names_stratas[2]
            for (j in 1:nrow(sampling)){
              v_strata1 <- as.character(stratas[j,1])
              v_strata2 <- as.character(stratas[j,2])
              sampling[j,"actual_sample"]<- as.numeric(sum(data[,strata1]==v_strata1 & data[,strata2]==v_strata2))
            }
          }

          sampling$actual_sample <- as.numeric(sampling$actual_sample)
          tot_sample <- sum(sampling$actual_sample)

          for (i in 1:nrow(sampling)){
            sampling[i,"weight"] <- (sampling[i,"pop"]/tot_pop)/(sampling[i,"actual_sample"]/tot_sample)
          }

          if (n_col_stratas==1){
            for (i in 1:nrow(data)){
              v_strata1 <- as.character(data[i,strata1])
              result <- as.character( sampling[sampling[strata1]==v_strata1, c("weight"),])
              data[i,"weight"] <- result
            }
            data$weight <- as.numeric(data$weight)
            surveydesign <- svydesign(ids=~1,
                                strata= data[[strata1]],
                                weights= ~weight,
                                data=data)
            pastedesign <- paste0("svydesign(ids=~1,
                                strata= data[[strata1]],
                                weights= ~weight,
                                data=data)")

          }

          if (n_col_stratas==2){
            for (i in 1:nrow(data)){
              v_strata1 <- as.character(data[i,strata1])
              v_strata2 <- as.character(data[i,strata2])
              result <- as.character( sampling[sampling[strata1]==v_strata1 & sampling[strata2]==v_strata2, c("weight"),])
              data[i,"weight"] <- result
            }
            data$weight <- as.numeric(data$weight)
            samplingdesign <- svydesign(ids=~1,
                                strata= ~data[[strata1]]+~data[[strata2]],
                                weights = ~weight,
                                data=data)
            pastedesign <- paste0("svydesign(ids=~1,
                                strata= ~data[[strata1]]+~data[[strata2]],
                                weights = ~weight,
                                data=data)")
          }

          weight2dico <- data.frame(matrix("weight",ncol = 13))
          names(weight2dico) <- c("type", "name",  "fullname", "label", "disaggregation","correlate","listname","qlevel", "qgroup", "labelchoice","ordinal","weight","formpart")
          dico<- rbind(dico,weight2dico)

          # Rewritting dico file
          write.csv(dico, paste0("data/dico_",form,".csv"), row.names=FALSE, na = "")

          # Coherce data to a clean dataframe
          data <- data.frame(data)

          #Rewrite data with weights
          wb <- loadWorkbook(path.to.data)
          sheets <- getSheets(wb)
          removeSheet(wb, sheetName="cleaned_data")
          new_sheet <- createSheet(wb, sheetName="cleaned_data")
          addDataFrame(data, new_sheet, row.names = FALSE)
          saveWorkbook(wb, path.to.data)

          #Fetching the directory
          #Path to file
          configfile<-paste(mainDir,"/code/0-config.R",sep="")
          #Writting file
          sink(configfile,append=TRUE)

          cat("\n")
          cat(paste('data <- read_excel("',path.to.data,'", sheet="cleaned_data")',sep=""))

          cat("\n")
          cat(paste('strata1 <- "',strata1,'"',sep=""))
          cat("\n")

          if (n_col_stratas>1){
            cat(paste('strata2 <- "',strata2,'"',sep=""))
            cat("\n")

          }

          cat(paste('design <- ',pastedesign,sep=""))
          cat("\n")


          sink()
      }
      if(usedsampling=="cluster_sampling"){


      }
      if(usedsampling=="simple_random"){

      }


    }
    else{
      cat("You didn't use the R Sampling tool or your sampling frame is not valid. ")
      cat("Copy paste the input of https://oliviercecchi.shinyapps.io/R_sampling_tool_v2/ in the sampling_frame sheet ")

    }
  }
  if(usedweight=="custom"){

  }
}

NULL
