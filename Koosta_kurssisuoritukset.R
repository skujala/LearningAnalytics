dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
require(xlsx)
require(reshape2)
require(plyr)
require(knitr)
require(ggplot2)
require(ggthemes)
require(RColorBrewer)

tulokset.meta = list(
  "2013"="../2013/Mekaniikka 2013 tulokset.xlsx",
  "2014_1"="../2014/Arvostelu_20150105.xlsx",
  "2014_2"="../2014/Arvostelu_20150105_erilliset.xlsx",
  "2015"="../2015/Valikokeet/ELEC-A3110-arvosanat.xlsx",
  "2016"="../2016/ELEC-A3110_2016_tulokset.xlsx"
)




# # lue välikokeiden tulokset kaikki kerralla yhteen dataframeen
# palautteet.data <- do.call(
#   rbind,
#   c(
#     llply(
#       palautteet.meta, 
#       function(x) { 
#         tmp <- read.xlsx(
#           x$File, 
#           sheetIndex = 1, 
#           colIndex = which(complete.cases(x$Columns)) # Only import columns we need -- homogenize data
#         ); 
#         colnames(tmp) <- na.omit(x$Columns);          # Rename those columns we choose to use 
#         tmp[tmp == '0'] <- NA;                        # Stupid f*ing Webropol signifies NA with zeros...replace them with NA
#         
#         # Replace any remaining "Unable to respond" with NA's
#         tmp[tmp == 'E'] <- NA;
#         tmp[tmp == "Ei perusteita vastata"] <- NA;
#         
#         # Convert response time to POSIXct timestamp
#         tmp$Vastausaika <- strptime(tmp$Vastausaika, "%d.%m.%Y %H:%M:%S")
#         
#         # Homogenize factors 
#         levels(tmp$Yleisarvio) <- grade_factors
#         levels(tmp$Toteutustapa) <- likert_factors
#         levels(tmp$Oma_panos) <- likert_factors
#         levels(tmp$Ymmarrys) <- likert_factors
#         levels(tmp$Arvostelun_linjakkuus) <- likert_factors
#         
#         levels(tmp$Tyoaika) <- work_likert_factors
#         levels(tmp$Lasnaolo) <- presence_factors
#         
#         # Add column for year
#         tmp$Vuosi <- tmp$Vastausaika$year+1900
#         
#         for (col in cols_to_fix) {
#           tmp[,col] <- ordered(
#             tmp[,col], 
#             levels=c(
#               "Täysin eri mieltä",
#               "Jonkin verran eri mieltä",
#               "Ei samaa eikä eri mieltä",
#               "Jonkin verran samaa mieltä",
#               "Täysin samaa mieltä"
#               #"Ei perusteita vastata"
#             ) 
#           )
#           
#         }
#         
#         return(tmp) 
#       }    
#     ),
#     make.row.names=F
#   )
# )


melt_data <- function(c, x) {
  
  x.table <- table(x[,'Vuosi'], x[,c])
  x.percentage <- x.table / rowSums(x.table)
  
  x.melt <- merge(x.percentage, melt(x.table), by=c("Var1", "Var2"))
  colnames(x.melt) <- c("Vuosi", "Vastaus", "Prosenttia", "N")
  
  return(x.melt)
}

melt_data_likert <- function(c, x) {
  
  transform_table_to_likert <- function (y) {
    negative_factors <- names(likert_factors)[1:2]
    neutral_factor <- names(likert_factors)[3]
    
    offsets <- melt(-y[, negative_factors[1]] - y[, negative_factors[2]] - 0.5 * y[,neutral_factor])
    offsets$Year <- row.names(offsets)
    
    
    y <- melt(y)
    y$start <- ave(y$value, by=y$Var1, FUN = cumsum)
    y <- merge(y, offsets, by.x="Var1", by.y="Year", suffixes=c(".koko", ".offset"))
    
    y$Alku <- y$start + y$value.offset
    y$start <- NULL
    y$value.offset <- NULL
    
    colnames(y) <- c("Vuosi", "Vastaus", "Koko",  "Alku")
    
    return(y)
  }
  
  
  x.table <- table(x[,'Vuosi'], x[,c])
  x.percentage <- transform_table_to_likert(x.table / rowSums(x.table))
  
  return(x.percentage)
}

# Now we have all the data in one huge data frame. Lovely. Start analyzing

