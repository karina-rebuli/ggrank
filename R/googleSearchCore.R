#' Core function
#'
#' @param search.words Character value with words to be used on searching. Mandatory.
#' @param n.pages Integer value with number of search results pages to look at. It should be set between 1 and 30. Default \code{5}.
#' @param log.pages Character value with path to save search results pages. If NULL, they're not saved. Default \code{NULL}.
#' @param ua User agent.
#' @param result.type Character value with which searching result type should be read, only organic ('organic' or 'o'),
#' only cpc ('cpc' or 'c') or both ('both' or 'b'). Default \code{"organic"}.
#'
#' @return A data.frame with founded url, page number, position and, if it should look for target urls, target-url.
#'
#'
.googleSearchCore <- function( search.words, n.pages, ua, log.pages, result.type ){

  #
  # Pages
  #
  pages <- lapply( 1:n.pages, function( pg ){

    # Get pages
    httr::GET( url = utils::URLencode( paste0( "https://www.google.com.br/search?q="
                                               , paste( search.words, collapse = "+" )
                                               , "&start=", (pg-1)*10 ) )
               , httr::user_agent( ua ) )

  })

  # Log pages
  if( !is.null(log.pages) ){
    for( i in 1:length(pages) ){

      fileLog <- paste0( "logGoogleRanking_"
                         , gsub( search.words, pattern = " ", replacement = "-"), "_"
                         , gsub( gsub( x = Sys.time(), pattern = " ", replacement = "_", fixed = TRUE )
                                 , pattern = ":", replacement = "-" )
                         ,"_pg_", i
                         , ".html" )
      writeLines( httr::content( pages[[ i ]], "text" ), ifelse( log.pages == "", fileLog, file.path( log.pages, fileLog ) ) )

    }
  }


  #
  # Make return
  #
  res <- NULL


  #
  # Organic results - <h3 class='r'...
  #
  if( result.type %in% c("both", "b", "organic", "o" ) ){

    # Read data
    organic <- lapply( 1:length( pages ), function(p){

      parts <- strsplit( x = httr::content( pages[[ p ]], "text" )
                         , split = "<h3 class=\"r\">", fixed = TRUE )[[1]]
      found.url <- lapply( parts[-1], function( string ){
        strsplit( strsplit( string, split = "<a href=\"", fixed = TRUE )[[1]][2]
                  , split = "\"", fixed = TRUE )[[1]][1]
      })
      found.url <- do.call( rbind, found.url )

      # Return without empty lines
      ifelse( nrow( found.url ) > 0
              , return( data.frame( found.url = as.character( found.url[ !is.na( found.url[, 1] ) ] )
                                    , pg = p, result.type = "organic" ) )
              , return( NULL ) )
    })

    # Result log
    if( any( sapply(organic, class) != "NULL" ) ){
      res <- do.call( rbind, organic )
      res <- data.frame( res, position = 1:nrow(res) )
    }

  }


  #
  # CPC results - <li class='ads-ad'...
  #
  if( result.type %in% c("both", "b", "cpc", "c" ) ){

    # Read data
    cpc <- lapply( 1:length( pages ), function(p){

      parts <- strsplit( x = httr::content( pages[[ p ]], "text" )
                         , split = "<li class=\"ads-ad\"", fixed = TRUE )[[1]]

      found.url <- lapply( parts[-1], function( string ){

        main.url <- strsplit( strsplit( string, split = "<a href=\"", fixed = TRUE )[[1]][2]
                              , split = "\"", fixed = TRUE )[[1]][1]
        preconnected.url <- strsplit( strsplit( string, split = "data-preconnect-urls=\"", fixed = TRUE )[[1]][2]
                              , split = "\"", fixed = TRUE )[[1]][1]

        if( length( grep( main.url, pattern = "/aclk?", fixed = TRUE ) ) == 0 ){
          return( main.url )
        }else{
          return( preconnected.url )
        }

      })
      found.url <- do.call( rbind, found.url )

      # Return without empty lines
      ifelse( !is.null( found.url )
              , return( data.frame( found.url = as.character( found.url[ !is.na( found.url[, 1] ) ] )
                                    , pg = p, result.type = "cpc" ) )
              , return( NULL ) )
    })

    # Result log
    if( any( sapply(cpc, class) != "NULL" ) ){
      cpc <- do.call( rbind, cpc )
      cpc <- data.frame( cpc, position = 1:nrow(cpc) )
      res <- rbind(res, cpc)
    }

  }


  #
  # Explicit return
  #
  res

}
