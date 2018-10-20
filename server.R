library(shiny)
library(plotly)
library(jsonlite)
library(RCurl)
library(janitor)
library(tidyverse)

server <- function(input, output) {

  readUrl <- function(url) {
    out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully

      message("This is the 'try' part")

      fromJSON(url) 
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error=function(cond) {
      message(paste("Something happend", url))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("Processed URL:", url))
      #message("Some other message at the end")
    })    
    return(out)
  }
  
  delete_empty_rows <- function(dataframe){
    # find all empty rows with "", this function is not for NaN or Null values
    row <- nrow(dataframe)
    empty_rows <- c()
    for (i in 1:row){
      if (all(dataframe[i,] == "")){
        empty_rows <- append(empty_rows,i)
      }
    }
    dataframe <- dataframe[-empty_rows,]  # remove empty rows

    #change column names
    cols <- ncol(dataframe)
    for (i in 1:cols){
      colnames(dataframe)[i] <- as.character(dataframe[1,][[i]])  # take only values without levels...
    }
    dataframe <- dataframe[-c(1),]
    rownames(dataframe) <- NULL
    return(dataframe)
  }

  fetch_data <- function(datastore){
    for (resource in dane_o_przestepczosciach_1999_2017$data$relationships$resources$data$id){
      # take data under particular case link
      temporary_data <- readUrl(paste(base_url,resource,sep=''))
      if (!is.null(temporary_data)){
        title <- gsub(" ", "_", temporary_data$data$attributes$title)
        #datastore[[title]] <- list()
        datastore[[title]] <- temporary_data
        # store CSV data per case
        if (temporary_data$data$attributes$format == "csv"){
          # version with CSV file storing
          #file_name <- strsplit(temporary_data$data$attributes$file_url, "/")[[1]][7]
          #download.file(url=temporary_data$data$attributes$file_url,
          #              destfile=file_name)
          #datastore$title[["CSV"]] <- delete_empty_rows(remove_empty(read.csv(file_name, sep = ";"),
          #                                         which = c("rows", "cols")))


          # version with online access
          connection <- getURL(temporary_data$data$attributes$file_url)
          datastore[[title]][["CSV"]] <- delete_empty_rows(remove_empty(read.csv(textConnection(connection), sep = ";"),
                                                                        which = c("rows", "cols")))

        } else {
          print(paste("Data stored as:", temporary_data$data$attributes$format))
        }
      }
    }
    return(datastore)
  }

  scatter_plot <- function(obszar=NULL){
		if (is.null(obszar)){
			frag_data <- data[which(data$`Jednostka organizacyjna Policji` != 
																as.character(data$`Jednostka organizacyjna Policji`[1])),]
			frag_data$`Postepowania wszczete` <- as.numeric(gsub(" ", "", as.character(frag_data$`Postepowania wszczete`)))
			frag_data$Rok <- as.numeric(gsub(" ", "", as.character(frag_data$Rok)))
			# do nazw kolumn musze odwolywac sie z '~' znakiem, inaczej nie dziala... taka implementacja plot_ly 
			# (problem z rozmiarami heatmapy)
			plot_ly(frag_data, x = ~Rok, y = ~`Postepowania wszczete`, type="scatter", 
							text = paste("Jednostka:", obszar),
							mode = "markers", color = ~`Jednostka organizacyjna Policji`, size = ~`Postepowania wszczete`)
		} else{
			# scatter_plot - podajesz nazwe obszaru ktory chcesz uwzglednic w wyswietlaniu
			# rysuje wykres kropkowany, interaktywny
			frag_data <- data[which(data$`Jednostka organizacyjna Policji` == obszar),]
			frag_data$`Postepowania wszczete` <- as.numeric(gsub(" ", "", as.character(frag_data$`Postepowania wszczete`)))
			frag_data$Rok <- as.numeric(gsub(" ", "", as.character(frag_data$Rok)))
			# do nazw kolumn musze odwolywac sie z '~' znakiem, inaczej nie dziala... taka implementacja plot_ly 
			# (problem z rozmiarami heatmapy)
			plot_ly(frag_data, x = ~Rok, y = ~`Postepowania wszczete`, type="scatter", 
							text = paste("Jednostka:", obszar),
							mode = "markers", color = ~`Postepowania wszczete`, size = ~`Postepowania wszczete`) #%>%
			#add_trace(y = ~`Postepowania wszczete`, mode = 'lines')
		}
	}
	
  # Download Data
  options(timeout = 200000)
	base_url <- "https://api.dane.gov.pl/resources/"
	url <- "https://api.dane.gov.pl/datasets/1012"
	dane_o_przestepczosciach_1999_2017 <- fromJSON(url)

	data_for_each_criminal <- list()
	data_for_each_criminal <- fetch_data(data_for_each_criminal)
	data <- data_for_each_criminal$`Postępowania_wszczęte_zgwałcenie_w_latach_1999-2017`$CSV

  output$plot <- renderPlotly({
  	#plot_ly(annual, x=~Year, y=~J.D,type = "scatter", mode="lines")
		scatter_plot("KWP Bydgoszcz")
  })

  output$plot2 <- renderPlotly({
		mouse_event <- event_data("plotly_hover")
		year <- mouse_event[3]
		#monthly_subset <- monthly[monthly$Year==year$x,]
		#plot_ly(monthly_subset, x=~Month, y=~Deviation, type = "scatter", mode="lines + points")
		scatter_plot("KWP Bydgoszcz")
  })
}
