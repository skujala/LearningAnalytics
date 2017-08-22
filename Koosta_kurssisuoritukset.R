#dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
#require(xlsx)
require(readxl)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggthemes)
require(RColorBrewer)
require(sqldf)

tulokset.meta = list(
  "2013"   = list(
    file="../2013/Mekaniikka 2013 tulokset_analysis.xlsx", 
    scales.et = rep(24, 10), scales.lh = rep(5,10), scale.matlab = 5,
    range = c(1:30, 48) # Do not use midterm exam maximum points
  ),
  "2014"   = list(
    file="../2014/Arvostelu_20150105.xlsx", 
    scales.et = rep(c(4,2), 5), scales.lh = c(12, rep(c(3,6),4), 3), scale.matlab = 15,
    range = c(1:32, 40) # Do not use midterm exam maximum points
  ),
  "2015"   = list(
    file="../2015/Valikokeet/Arvostelu.xlsx",
    scales.et = rep(c(4,2), 5), scales.lh = rep(4, 10), scale.matlab = 15,
    range = c(1:30, 34, 38, 42, 59, 63) # Do not use midterm exam maximum points
  ),
  "2016"   = list(
    file="../2016/ELEC-A3110_2016_tulokset.xlsx",
    scales.et = rep(3, 10), scales.lh = rep(4, 10), scale.matlab = 21,
    range = c(1:33, 41, 44)
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
      d <- sapply(
        seq(2,10),
        function(idx) {
          x[,paste(column, idx, sep="")] - x[,paste(column, idx-1, sep="")]
        }#,
        #FUN.VALUE = numeric(nrow(x))
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
    d <- read_excel(x$file, sheet = 1, range = cell_cols(x$range))
    colnames(d) <- fix_colnames(colnames(d))
  
    d[!is.na(d$VK1) & d$VK1 == 0,"VK1"] <- NA
    d[!is.na(d$VK2) & d$VK2 == 0,"VK2"] <- NA
    d[!is.na(d$VK3) & d$VK3 == 0,"VK3"] <- NA
    
    #d[is.na(d)] <- 0

    d[,c(paste("ET", 1:10, sep=""), paste("LH", 1:10, sep=""), "Matlab.1")] <- scale(
      d[,c(paste("ET", 1:10, sep=""), paste("LH", 1:10, sep=""), "Matlab.1")], 
      center=F, 
      scale=c(x$scales.et, x$scales.lh, x$scale.matlab)
    )
    
    #d <- cbind(d, lh_diff(d))
    #d <- cbind(d, et_diff(d))
    
    d$Vuosi <- as.numeric(unlist(strsplit(x$file, "/"))[2])

    return(d)
  }
)


# find common columns and form one huge dataframe
tulokset.all <- Reduce(rbind, lapply(tulokset, function(x) {x[,Reduce(intersect, lapply(tulokset, colnames))]}))
tulokset.all$Arvosana <- factor(tulokset.all$Arvosana)


# Compute activity ("did student attempt Quiz or Exercise" -- 0, 1, 2)
aktiivisuus <- cbind(sapply(
  1:10, 
  function(x, y) {
    rowSums(!is.na(y[,paste0(c("ET", "LH"), x)]))
  },
  tulokset.all
),
as.numeric(as.character(tulokset.all$Arvosana))
)

# anonymize the data. Only interested in numerical data + grade (a factor)

cols <- c(
  unlist(
    lapply(
      1:10,
      function(x) {paste0(c("ET", "LH"), x)}
      )
    ),
  "VK1", "VK2", "VK3", "Arvosana", "Vuosi"
)

analysis <- tulokset.all[,c(sapply(tulokset.all, function(x) {is.numeric(x) | is.factor(x)}))]
analysis <- analysis[,cols]

analysis <- cbind(
  analysis, 
  sapply(
    seq(2,20,by=2), 
    function(x) {
      cut(
        rowSums(
          analysis[,seq(1,x)], na.rm=T
        ), 
        breaks=c(0,0.3,0.6,Inf)*x, right=F, 
        labels=c("0-33%", "33-66%", "66-100%")
      )
    }
  )
)

colnames(analysis)[26:35] <- sprintf("%02d", 1:10)

analysis2 <- 
  analysis[, c("Arvosana", "VK1", sprintf("%02d", 1:10))] %>%
    gather(Viikko, Tehtyjä_tehtäviä, -Arvosana, -VK1)

lapaisyt <- sqldf('
  SELECT 
    Viikko, 
    "Tehtyjä_tehtäviä", 
    case VK when 1 then "Kyllä" else "Ei" end as "Osallistui_VK1", 
    count_true as "Läpäisseitä", 
    count_true / count_all * 100 as "Läpäisyprosentti"
  FROM (
    SELECT 
      cast(count(case "arvosana" when 0 then null else 1 end) as real) as count_true,
      cast(count(*) as real) as count_all, 
      "Tehtyjä_tehtäviä", 
      Viikko, 
      VK1,
      case ifnull(VK1, 0) when 0 then 0 else 1 end as VK
    FROM analysis2 
    GROUP BY 
      "Tehtyjä_tehtäviä",
      Viikko,
      VK
  )
')


# Now we have all the data in one huge data frame. Lovely. Start analyzing

