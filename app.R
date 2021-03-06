library(magrittr)
library(rhandsontable)
library(shiny)


source("Bokat.R")
source("splitte.R")


options(encoding = "UTF-8")


cache <- list(df = NULL, split = NULL)
n <- 0


shinyApp(
	shinyUI(
		fluidPage(
			theme = "style.css",
			rHandsontableOutput("players", width = 198),
			rHandsontableOutput("teams", width = 297),
			br(),
			actionButton("back", "prev"),
			textOutput("caption", inline = TRUE),
			actionButton("forward", "next"),
			actionButton("random", "random")
		)
	),
	
	shinyServer(function(input, output, session) {
		# http://stackoverflow.com/a/39617229/17216
		values <- reactiveValues()

		players <- reactive({
			#cat("PLAYERS\n")
			if (!is.null(input$players)) {
				#cat("df <- hot_to_r(input$players)\n")
				df <- hot_to_r(input$players)
				
			} else {
				if(is.null(values[["df"]])) {
					#cat("df <- getBokat...\n")
					df <- getBokat(92452290844323)

					# parse number of guests & extract names
					guests <- character(0)
					
					if(ncol(df) > 3) {
						df %<>% mutate(guest = as.integer(guest))
						
						for(i in 1:nrow(df)) if(!is.na(df$guest[i])) {
							names <- str_match_all(df$comment[i], "[A-Z][a-z]+")[[1]][, 1]
							guests <- c(guests, names, rep("?", df$guest[i] - length(names)))
						}
					}
					
					df <- rbind(
						df %>% select(status, name),
						data.frame(status = rep("yes", length(guests)), name = guests)
					) %>% 
						mutate(playing = status == "yes") %>% 
						select(playing, name) %>% 
						arrange(name)
				} else {
					#cat("df <- values[[\"df\"]]\n")
					df <- values[["df"]]
				}
			}
			
			values[["df"]] <- df

			df
		})
			
		# balanced split (ordinals of players out in one team; those not playing are ignored)
		splits <- reactive({
			df <- players()
			
			if(!identical(df, cache$df)) {
				cache <<- list(
					df = df,
					split = split(df %>% filter(playing))
				)
			}
			
			cache$split
		})

		# render output
		output$players <- renderRHandsontable({
			df <- players()

			if(!is.null(df)) df %>% 
				rhandsontable(colHeaders = c("", "name"), rowHeaders = NULL) %>% 
				hot_col("", halign = "htCenter") %>% 
				hot_col("name", readOnly = TRUE) %>% 
				hot_rows(rowHeights = 24)
		})
		
		output$teams <- renderRHandsontable({
			df <- players()
			ss <- splits()
			
			set.seed(input$random)
			x <- ss[[(input$forward - input$back + round(length(ss) * runif(1))) %% length(ss) + 1]]
			
			if(!is.null(df) & length(x) > 0) {
				df %<>% filter(playing)

				length(x) <- nrow(df) - length(x)

				cbind(
					df[x, ] %>% select(team1 = name),
					df[setdiff(1:nrow(df), x), ] %>% select(team2 = name)
				) %>% 
					rhandsontable(colHeaders = c("black", "white"), readOnly = TRUE, rowHeaders = NULL) %>% 
					hot_rows(rowHeights = 24)
			}
		})
		
		output$caption <- renderText({
			ss <- splits()

			set.seed(input$random)

			paste(
				(input$forward - input$back + round(length(ss) * runif(1))) %% length(ss) + 1,
				"out of",
				length(ss),
				"\t"
			)
		})
	})
)
