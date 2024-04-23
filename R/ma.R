ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

ma.date <- function(x, time, diff.time.max = 2400, n = 5){

  diff.time <- diff(time)
  diff.time.seq <- which(diff.time > diff.time.max)
  x.ma <- x

  for(i in 1:length(diff.time.seq)){

    if(i == 1)  x.ma[  1:diff.time.seq[i] ] <- as.numeric( ma( x.ma[ 1:diff.time.seq[i] ],
                                                               ifelse(length( 1:diff.time.seq[i] ) > n, n, length(( 1:diff.time.seq[i] ))) ))

    if(i > 1 & i < length(diff.time.seq)){
      x.ma[  (diff.time.seq[ i-1 ] + 1) :diff.time.seq[i] ] <- as.numeric( ma( x.ma[ (diff.time.seq[ i-1 ] + 1) :diff.time.seq[i] ]
                                                                               , ifelse(length((diff.time.seq[ i-1 ] + 1) :diff.time.seq[i] ) > n, n, length((diff.time.seq[ i-1 ] + 1) :diff.time.seq[i] ))) )
    }

    if(i == length(diff.time.seq)) x.ma[  (diff.time.seq[ i ] + 1) : diff.time.seq[ i ] ] <- as.numeric( ma( x.ma[ (diff.time.seq[ i ] + 1) : diff.time.seq[ i ] ],
                                                                                                             ifelse( length((diff.time.seq[ i ] + 1) : diff.time.seq[ i ]) > n, n, length((diff.time.seq[ i ] + 1) : diff.time.seq[ i ]))) )
  }

  return(x.ma)
}
