comment_break <- function(commentstring = Kommentar, breakpoint = 70){

  if( nchar( commentstring ) < breakpoint) return( commentstring)

  seqp <-  seq(1, nchar( commentstring), breakpoint)
  seqp.string <- lapply( seqp[ - 1], function( x ) max( which( unlist(gregexpr(" ", commentstring)) < x)))


  commentstring.sub <- list()
  for(i in 1 : length( seqp)){
    if( i == 1) commentstring.sub[[ 1 ]] <- substr(commentstring
                                                   , 1
                                                   , unlist(gregexpr(" ", commentstring))[ seqp.string[[ i ]] ] - 1)

    if( i > 1 & i != length( seqp)) commentstring.sub[[ i ]] <- substr(commentstring
                                                                       , unlist(gregexpr(" ", commentstring))[ seqp.string[[ i - 1 ]] ] + 1
                                                                       , unlist(gregexpr(" ", commentstring))[ seqp.string[[ i ]] ] - 1)

    if( i == length( seqp) ) commentstring.sub[[ i ]] <- substr(commentstring
                                                                , unlist(gregexpr(" ", commentstring))[ seqp.string[[ i - 1 ]] ] + 1
                                                                , nchar( commentstring))

  }
  commentstring.sub <- paste(commentstring.sub, collapse = "\n")
  return( commentstring.sub)
}

dauschlogo.draw <- function(image = dt_logo
                            , x = .01
                            , y = (heights[ length( heights )] /  sum( heights ))/ 2
                            , size = 1.5) {
  if( par("mfrow")[ 2 ] != 1) size <- size / sqrt( par("mfrow")[ 2 ] - .75)
  logo <- grid::rasterGrob(image = image
                           , x = grid::unit(x, "npc"), y = grid::unit(y, "npc")
                           , width = grid::unit(size, "cm"), height = grid::unit(size, "cm")
                           , just = c("left", "center")#, gp = gpar( )
  )
  grid::grid.draw(logo)
}
