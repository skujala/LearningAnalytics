dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
require(xlsx)
require(reshape2)
require(plyr)
require(knitr)
require(ggplot2)
require(ggthemes)
require(RColorBrewer)

tulokset.meta = list(
  "2013"   = list(
    file="../2013/Mekaniikka 2013 tulokset_analysis.xlsx", 
    scales.et = rep(24, 10), scales.lh = rep(5,10), scale.matlab = 5,
    colIndex = c(1:27, 36:39, 48)
  ),
  "2014"   = list(
    file="../2014/Arvostelu_20150105.xlsx", 
    scales.et = rep(c(4,2), 5), scales.lh = c(12, rep(c(3,6),4), 3), scale.matlab = 15,
    colIndex = c(1:29, 36:38, 40)
  ),
  "2015"   = list(
    file="../2015/Valikokeet/Arvostelu.xlsx",
    scales.et = rep(c(4,2), 5), scales.lh = rep(4, 10), scale.matlab = 15,
    colIndex = c(1:30, 55:57, 59, 63)
  ),
  "2016"   = list(
    file="../2016/ELEC-A3110_2016_tulokset.xlsx",
    scales.et = rep(3, 10), scales.lh = rep(4, 10), scale.matlab = 21,
    colIndex = c(1:29, 37:39, 41, 44)
  )
)

fix_colnames <- function(x) {
  x <- gsub("ID.number", "Opnro", x)
  x <- gsub("First.name", "Etunimi", x)
  x <- gsub("Surname", "Sukunimi",x)
  x <- gsub("Email.address", "Email", x)
  x <- gsub("email", "Email", x)
  x <- gsub("LH0", "LH", x)
  x <- gsub(".max", "", x)
  x <- gsub(".sum", ".total", x)
  x <- gsub(".korjattu", "", x)
  x <- gsub("^Matlab$", "Matlab.1", x)
  x <- gsub("Matlab([12])", "Matlab.\\1", x)
  x <- gsub("kurssipalaute", "Palautepiste", x)
  x <- gsub("palautepiste", "Palautepiste", x)
  x <- gsub("AS", "Arvosana", x)
  
  return(x)
}

differences_fcn <- function(column) {
  return (
    function(x) {
      d <- vapply(
        seq(2,10),
        function(idx) {
          x[,paste(column, idx, sep="")] - x[,paste(column, idx-1, sep="")]
        },
        FUN.VALUE = numeric(nrow(x))
      )
      d <- cbind(x[,paste(column, 1, sep="")], d)
      
      colnames(d) <- paste(paste(column, ".d", sep=""), 0:9, sep="")
    
      return(d)
    }
  )
}

lh_diff <- differences_fcn("LH")
et_diff <- differences_fcn("ET")

tulokset = lapply(
  tulokset.meta, function (x) {
    d <- read.xlsx(x$file, sheetIndex = 1, colIndex = x$colIndex)
    colnames(d) <- fix_colnames(colnames(d))
  
    d[is.na(d)] <- 0
    

    d[,c(paste("ET", 1:10, sep=""), paste("LH", 1:10, sep=""), "Matlab.1")] <- scale(
      d[,c(paste("ET", 1:10, sep=""), paste("LH", 1:10, sep=""), "Matlab.1")], 
      center=F, 
      scale=c(x$scales.et, x$scales.lh, x$scale.matlab)
    )
    
    d <- cbind(d, lh_diff(d))
    d <- cbind(d, et_diff(d))
    
    d$Vuosi <- unlist(strsplit(x$file, "/"))[2]
    d$Lapi <- "Ei"
    d$Lapi[d$Arvosana != 0] <- "Kyllä"
        
    return(d)
  }
)

# find common columns and form one huge dataframe
tulokset.all <- Reduce(rbind, lapply(tulokset, function(x) {x[,Reduce(intersect, lapply(tulokset, colnames))]}))
tulokset.all$Vuosi <- factor(tulokset.all$Vuosi)
tulokset.all$Lapi <- factor(tulokset.all$Lapi)

# Now we have all the data in one huge data frame. Lovely. Start analyzing

