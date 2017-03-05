library(dplyr)
library(stringr)
library(XML)

options(stringsAsFactors = FALSE)


# fetches data from an event page on Bokat
getBokat <- function(eventId) {
	# http://stackoverflow.com/a/30947988/17216
	getImgSrc <- function(node, ...) {
		ifelse(
			xmlName(node) == "td" && !is.null(node[["img"]]),
			xmlGetAttr(node[["img"]], "src"),
			xmlValue(node)
		)
	}

	df <- readHTMLTable(
		doc = paste0("http://www.bokat.se/eventInfoOpen.jsp?eventId=", eventId),
		elFun = getImgSrc,
		which = 11,
		encoding = "UTF-8"
	)
	
	colnames(df) <- c("status", "name", rep("guest", ncol(df) > 3), "comment")

	# get status (even rows)
	df[c(TRUE, FALSE), ]$status <- str_match(
		df[c(FALSE, TRUE), ]$status,
		"/images/(.+)\\.png"
	)[, 2]
	
	# remove even rows & timestamps
	df[c(TRUE, FALSE), ] %>% mutate(name = str_replace(name, "\\s*\\(.*\\)", ""))
}
