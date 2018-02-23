#' @name kobo_histo
#' @rdname kobo_histo
#' @title  Generate histograme for all integer questions
#'
#' @description  Automatically generate histogrammes for each of the integer questions in the dataset. ggplot2 is used.
#'
#' @param data kobodatset to use
#' @param dico ( generated from kobo_dico)
#'
#' @author Edouard Legoupil
#'
#' @examples
#' kobo_histo()
#'
#' @export kobo_histo
#'
#' @examples
#' \dontrun{
#' kobo_histo()
#' }
#'
#'

kobo_histo <- function() {
  source("code/0-config.R")
  dico <- read.csv(path.to.dico,sep = ",")


  mainDir <- "out"
  subDir <- "histo"
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("histo directory exists in out directory and is a directory.\n")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("histo directory exists in your out directory.\n")
    # you will probably want to handle this separately
  } else {
    cat("histo directory does not exist in your out directory - creating now!\n ")
    dir.create(file.path(mainDir, subDir))
  }


  selectdf <- dico[dico$type=="integer", c("fullname","listname","label","name","qrepeat","type")]


  ### Verify that those variable are actually in the original dataframe
  check <- as.data.frame(names(data))
  names(check)[1] <- "fullname"
  check$id <- row.names(check)
  selectdf <- join(x=selectdf, y=check, by="fullname",  type="left")
  selectdf <- selectdf[!is.na(selectdf$id), ]

  if (nrow(selectdf)==0){
    cat("There's no integer variables. \n")
  } else{

  selectinteger <- as.character(selectdf[, c("fullname")])
  data.integer <- data [selectinteger  ]

  selectfacet <- as.character(selectdf[selectdf$disaggregation!="" , c("fullname")])
  selectfacet <- selectfacet[!is.na(selectfacet)]


  ## force to data frame
  data.integer <- as.data.frame(data.integer)
  data.integer  <- kobo_label(data.integer, dico)
  wrapper <- function(x, ...)
  {
    paste(strwrap(x, ...), collapse = "\n")
  }


        for (i in 1:nrow(selectdf) ) {
        #  for (i in 1:2 ) {
          # i <- 10
          variablename <- names(data.integer)[i]
          title <- attributes(data.integer)$variable.labels[i]

          totalanswer <- nrow(data.integer)

          count_replied <- (sum(!is.na(data.integer[,i ])))

          percentresponse <- paste(round((count_replied/totalanswer*100),digits=2),"%",sep="")


          ## Ensure that the variable is recognised as integer
          data.integer[ , i] <- as.integer(data.integer[ , i])
          #  regular histogram
          theme_set(theme_gray(base_size = 18))

          plot <- ggplot(data=data.integer, aes(data.integer[ , i])) +
            geom_histogram(fill="#2a87c8", binwidth = 5) +
            ggtitle(wrapper(title,width=75),
                    subtitle = wrapper(paste0("Response rate to this question is ",percentresponse," of the total."),width=75))+
            labs(x="", y="Count")+
            #scale_x_discrete() +
            scale_x_continuous(breaks= pretty_breaks()) +
            theme(plot.title=element_text(face="bold", size=20),
                  plot.background = element_rect(fill = "transparent",colour = NA))
          cat(paste0(i, " - Generated histogramme for question: ", title , "\n"))
          ggsave(plot, filename=paste("out/histo/histo_",variablename,".png",sep=""), width=10, height=10,units="in", dpi=300)

          }

        for (i in 1:nrow(selectdf) ) {
         # for (i in 1:2 ) {
            # i <- 67
            variablename <- names(data.integer)[i]
            title <- attributes(data.integer)$variable.labels[i]

            ## Ensure that the variable is recognised as integer
            data.integer[ , i] <- as.integer(data.integer[ , i])
            #str(data.integer[ , i])

            totalanswer <- nrow(data.integer)

            count_replied <- (sum(!is.na(data.integer[,i ])))

            percentresponse <- paste(round((count_replied/totalanswer*100),digits=2),"%",sep="")

            theme_set(theme_gray(base_size = 18))


            # trendline on histogram by adding geom_density
            ggplot(data=data.integer, aes(data.integer[ , i])) +
              geom_histogram(aes(y =..density..), fill="#2a87c8", alpha = .6, binwidth = 5) +
              geom_density(col=2) +
              ggtitle(wrapper(title,width=65),
                      subtitle = wrapper(paste0("Response rate to this question is ",percentresponse," of the total."),width=65))+
              labs(x="", y="Frequency")+
              theme(plot.title=element_text(face="bold", size=20),
                    plot.background = element_rect(fill = "transparent",colour = NA))
            ggsave(filename=paste("out/histo/histodensity_",variablename,".png",sep=""), width=10, height=10,units="in", dpi=300)

            cat(paste0(i, "- Generated density graphs for question: ", title , "\n"))
          }
  }
  cat(" \n")
  cat(" \n")
  cat(" ###########################################################\n")
  cat(" # The histograms for integer questions were generated!    #\n")
  cat(" # You can find them in the folder 'out/bar_one'!          #\n")
  cat(" ###########################################################\n")


}
NULL
