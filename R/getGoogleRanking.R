#'
#' Get Google search rank positions of specific url's.
#'
#' @param search.words Character value with words to be used on searching. Mandatory.
#' @param target.url Character vector with target domains to look for. If NULL, it'll return all entries. Otherwise,
#' it should be a character vector with length between 1 and 30. Deafult \code{NULL}.
#' @param n.pages Integer value with number of search results pages to look at. It should be set between 1 and
#' 30. Default \code{5}.
#' @param log.pages Character value with path to save search results pages. If NULL, they're not saved.
#' Default \code{NULL}.
#' @param n.entries Numeric, positive. If target.url is not NULL, how many entries of target-urls must be saved.
#' Default \code{1}.
#' @param result.type Character value with which searching result type should be read, only organic ('organic' or 'o'),
#' only cpc ('cpc' or 'c') or both ('both' or 'b'). Default \code{organic}.
#' @param all.results Logical value to be used with target.url. If FALSE, it'll resturn only info about target urls.
#' Otherwise, return all results. Default \code{FALSE}.
#'
#' @return A data.frame with founded url, page number, position and, if it should look for target urls, target-url.
#'
#' @examples
#' \dontrun{
#' getGoogleRanking( search.words = "r project", target.url = c("berkeley.edu", "r-project.org" ) )
#' getGoogleRanking( search.words = "r project", n.pages = 5, target.url = c("berkeley.edu", "r-project.org" ), result.type = "c" )
#' getGoogleRanking( search.words = "r project", n.pages = 3, all.results = TRUE )
#' }
#'
getGoogleRanking <- function( search.words, target.url = NULL, n.pages = 5, log.pages = NULL
                              , n.entries = 1, result.type = "organic", all.results = FALSE ){

  # ================================================== #
  #                                                    #
  # Check args                                         #
  #                                                    #
  # ================================================== #

  # @arg[ search.words ]
  # @class[ character ]
  # @default[]
  # @mandatory = TRUE
  # @limits[ 1, 30 ]
  # @about[ Character vector with words to be used on searching. ]
  if( missing(search.words) | !( class(search.words) %in% "character" ) | length(search.words) > 1 )
    stop( "Wrong search.words arg. It should be a character vector with length between 1 and 30." )
  if( length( unlist( strsplit(search.words, " ") ) ) > 50 )
    stop( "Wrong search.words arg. It should be a character vector with up to 50 words." )

  # @arg[ target.url ]
  # @class[ character ]
  # @default[ NULL ]
  # @mandatory = FALSE
  # @limits[ 1, 30 ]
  # @about[ Character vector with target domains to look for. If NULL, it'll return all entries. ]
  if( missing(target.url) ) target.url <- NULL
  if( !is.null(target.url) & ( !( class(target.url) %in% "character" ) | length(target.url) > 30 | length( target.url ) < 1 ) ){
    target.url <- NULL
    warning( "Invalid target.url arg. Using NULL (which returns all results).\nIt should be a character vector with length between 1 and 30." )
  }

  # @arg[ n.pages ]
  # @class[ integer, numeric ]
  # @default[ 5 ]
  # @mandatory = FALSE
  # @limits[ 1, 30 ]
  # @about[ Integer value with number of search results pages to look at. It should be set between 1 and 30. ]
  if( missing(n.pages) | is.na( as.integer( n.pages ) ) | n.pages < 1 | n.pages > 30 ){
    n.pages <- 5
    warning( "Using default value 5 for n.pages. Maximum allowed is 30.\nUse with caution. Google servers are very restrict to allow robots searching." )
  }
  n.pages <- as.integer(n.pages)

  # @arg[ log.pages ]
  # @class[ character ]
  # @default[ NULL ]
  # @mandatory = FALSE
  # @limits[ 1 ]
  # @about[ Character value with path to save search results pages. If NULL, they're not saved. ]
  # missing - ok
  if( missing(log.pages) ) log.pages <- NULL
  # NA - error
  # if( is.na( log.pages ) ) log.pages <- TRUE
  # logical - depends
  if( class(log.pages) == "logical" ){
    if( log.pages ){
      # TRUE - error
      stop( paste( "Wrong log.pages arg. It should be a character vector with directory path to save returned pages from Google searching."
                   ,"If NULL, pages are not saved." ) )
    }else{
      # FALSE - ok
      log.pages <- NULL
    }
  }
  # not NULL - depends
  if( !is.null(log.pages) ){
    # not character or wrong lenght - error
    if( !( class(log.pages) %in% c("character") ) | length(log.pages) > 1 ){
      stop( paste( "Wrong log.pages arg. It should be a character vector with directory path to save returned pages from Google searching."
                   , "If NULL, pages are not saved." ) )
    }else{
      # not existing directory (if can't creates, stop on error already)
      if( !file.exists(log.pages) ){
        if( !dir.create( log.pages, showWarnings = FALSE ) ){
          stop( paste( "No permission to write on log.pages path." ) )
        }
      }
      # checks writing permission
      tmp <- paste0( "t", as.integer( as.POSIXct(Sys.time()) ), ".tmp" )
      if( dir.create( file.path( log.pages, tmp ), showWarnings = FALSE ) ){
        file.remove( file.path( log.pages, tmp ) )
      }else{
        stop( paste( "No permission to write on log.pages path." ) )
      }
    }

  }

  # @arg[ n.entries ]
  # @class[ integer ]
  # @default[ 1 ]
  # @mandatory = FALSE
  # @limits[ 1 ]
  # @about[ Numeric, positive. If target.url is not NULL, how many entries of target-urls must be saved. ]
  # if( missing(n.entries) | is.na( as.integer( n.pages ) ) | n.pages < 1 ){
  if( missing(n.entries) ) n.entries <- NULL
  if( !is.null(n.entries) ){
    if( !( class(n.entries) %in% c("numeric", "integer") ) ){
      n.entries <- 1
      warning( "Using default value 1 for n.entries." )
    }else{
      if( any( n.entries < 1 ) | length( n.entries ) > 1 ){
        n.entries <- 1
        warning( "Using default value 1 for n.entries." )
      }
    }
    n.entries <- as.integer( n.entries )
  }

  # @arg[ result.type ]
  # @class[ character ]
  # @default[ "organic" ]
  # @mandatory = TRUE
  # @limits[ c( "organic", "cpc", "both" ) ]
  # @about[ Character value with which searching result type should be read, only organic ('organic' or 'o'), only cpc ('cpc' or 'c') or both ('both' or 'b'). ]
  if( missing(result.type) | length( result.type ) != 1 | !( result.type %in% c( "organic", "cpc", "both", "o", "c", "b" ) ) ){
    result.type <- "organic"
    warning( "Using default value 'organic' for result.type. Allowed values are 'organic', 'cpc' or 'both' (or respectively 'o', 'c' or 'b' for short)." )
  }

  # @arg[ all.results ]
  # @class[ logical ]
  # @default[ FALSE ]
  # @mandatory = FALSE
  # @limits[]
  # @about[ Logical value to be used with target.url. If FALSE, it'll resturn only info about target urls. Otherwise, return all results. ]
  if( missing(all.results) | !( class(all.results) %in% c("logical") )  ){
    all.results <- FALSE
    warning( "Using default value FALSE for all.results." )
  }


  # ================================================== #
  #                                                    #
  # Methods                                            #
  #                                                    #
  # ================================================== #

  # Query settings
  # --------------
  pg <- 0
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"


  # Search core
  # -----------
  res <- .googleSearchCore( search.words = search.words, n.pages = n.pages, ua = ua
                           , log.pages = log.pages, result.type = result.type )
  if( is.null(res) ){
    warning( "No results to show." )
    return(NULL)
  }


  # Position info
  # -------------
  res$found.url <- as.character( res$found.url )


  # Targets
  # -------
  if( !is.null(target.url) ){

    # For each target
    tmp <- lapply( target.url, function( tgt ){

      # Look for
      tgt <- as.character(tgt)
      i <- grep( res$found.url, pattern = tgt, fixed = TRUE )

      # Shape result
      if( length(i) > 0 ){
        data.frame( i = i, target = tgt )
      }else{
        data.frame( i = NA, target = tgt )
      }

    })

    # Add targets to result
    tmp <- do.call( rbind, tmp )
    res$target.url <- NA
    res$target.url[ tmp$i[ !is.na(tmp$i) ] ] <- as.character( tmp$target[ !is.na(tmp$i) ] )

    # If there are not found targets
    if( any( is.na(tmp$i) ) ){
      res <- rbind( res, data.frame( found.url = NA, pg = NA, result.type = NA
                                     , position = NA, target.url = tmp$target[ is.na(tmp$i) ] ) )
    }

    # If it should return only target urls info
    if( !all.results ) res <- res[ which( !is.na(res$target.url) ), ]

    # If it should cut founds by n.entries param
    if( !is.null(n.entries) ){
      res <- res[ which( !is.na(res$target.url) ), ] %>%
        dplyr::arrange( position ) %>%
        dplyr::group_by( target.url ) %>%
        dplyr::slice( 1:n.entries ) %>%
        dplyr::ungroup()
    }


  }

  # Explict return
  # --------------
  return(res)

}
