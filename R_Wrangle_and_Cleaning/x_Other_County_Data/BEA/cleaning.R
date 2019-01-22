library(plyr)
library(stringr)

# INITIAL IMPORT
df.S = read.csv("Personal_Income_by_Industry_1969-2000-SIC/CA5_1969_2000_ALL.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df.N = read.csv("Personal_Income_by_Industry_2001-2014-NAICS/CA5N_2001_2014_ALL.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# df.S <- iconv(df.S, "ISO_8859-2", "UTF-8")
# df.N <- iconv(df.N, "ISO_8859-2", "UTF-8")

################################################
# KEEPING VARIABLES I WANT
################################################
#SIC Industry
df.S <- df.S[ which(df.S$Description=='Personal income'
                    | df.S$Description=='Population (persons) 2/'
                    | df.S$Description=='Per capita personal income (dollars)'
                    | df.S$Description=='  Earnings by place of work'
                    | df.S$Description=='  Farm earnings'                                          
                    | df.S$Description=='  Nonfarm earnings'
                    | df.S$Description=='      Agricultural services, forestry, and fishing'
                    | df.S$Description=='      Mining'
                    | df.S$Description=='      Construction'
                    | df.S$Description=='      Manufacturing'
                    | df.S$Description=='      Transportation and public utilities'
                    | df.S$Description=='      Wholesale trade'
                    | df.S$Description=='      Retail trade'
                    | df.S$Description=='      Finance, insurance, and real estate'
                    | df.S$Description=='      Services'
                    | df.S$Description=='    Government and government enterprises'
                    | df.S$Description=='      Federal, civilian'),] 

#NAICS Industry
df.N <- df.N[ which(df.N$Description=='Personal income'
                    | df.N$Description=='Population (persons) 2/'
                    | df.N$Description=='Per capita personal income (dollars)'
                    | df.N$Description=='Earnings by place of work'
                    | df.N$Description=='  Farm earnings'
                    | df.N$Description=='  Nonfarm earnings'
                    | df.N$Description=='      Forestry, fishing, and related activities'
                    | df.N$Description=='      Mining, quarrying, and oil and gas extraction'
                    | df.N$Description=='      Utilities'
                    | df.N$Description=='      Construction'
                    | df.N$Description=='      Manufacturing'
                    | df.N$Description=='      Wholesale trade'
                    | df.N$Description=='      Retail trade'
                    | df.N$Description=='      Transportation and warehousing'
                    | df.N$Description=='      Information'
                    | df.N$Description=='      Finance and insurance'
                    | df.N$Description=='      Real estate and rental and leasing'
                    | df.N$Description=='      Professional, scientific, and technical services'
                    | df.N$Description=='      Management of companies and enterprises'
                    | df.N$Description=='      Administrative and support and waste management and remediation services'
                    | df.N$Description=='      Educational services'
                    | df.N$Description=='      Health care and social assistance'
                    | df.N$Description=='      Arts, entertainment, and recreation'
                    | df.N$Description=='      Accommodation and food services'
                    | df.N$Description=='      Other services (except public administration)'
                    | df.N$Description=='    Government and government enterprises'
                    | df.N$Description=='      Federal, civilian'),]

################################################################################################
#COMBINE INDUSTRIES BY RENAMING VARIABLES TO BE THE SAME IN EACH. THEN I CAN JOIN THEM BASED ON THESE VARIABLE NAMES AND GeoFIPS
################################################################################################
df.S$Description[df.S$Description=='Personal income'] <- "i.income"
df.N$Description[df.N$Description=='Personal income'] <- "i.income"

df.S$Description[df.S$Description=='Population (persons) 2/'] <- "pop"
df.N$Description[df.N$Description=='Population (persons) 2/'] <- "pop"

df.S$Description[df.S$Description=='Per capita personal income (dollars)'] <- "i.capita"
df.N$Description[df.N$Description=='Per capita personal income (dollars)'] <- "i.capita"

df.S$Description[df.S$Description=='  Earnings by place of work'] <- "i.workplace"
df.N$Description[df.N$Description=='Earnings by place of work'] <- "i.workplace"

df.S$Description[df.S$Description=='  Farm earnings'] <- "i.farm"
df.N$Description[df.N$Description=='  Farm earnings'] <- "i.farm"

df.S$Description[df.S$Description=='  Nonfarm earnings'] <- "i.nonfarm"
df.N$Description[df.N$Description=='  Nonfarm earnings'] <- "i.nonfarm"

df.S$Description[df.S$Description=='      Agricultural services, forestry, and fishing'] <- "i.forestry"
df.N$Description[df.N$Description=='      Forestry, fishing, and related activities'] <- "i.forestry"

df.S$Description[df.S$Description=='      Mining'] <- "i.mining"
df.N$Description[df.N$Description=='      Mining, quarrying, and oil and gas extraction'] <- "i.mining"

df.S$Description[df.S$Description=='      Transportation and public utilities'] <- "i.util"
df.N$Description[df.N$Description=='      Utilities'] <- "i.util"

df.S$Description[df.S$Description=='      Construction'] <- "i.constr"
df.N$Description[df.N$Description=='      Construction'] <- "i.constr"

df.S$Description[df.S$Description=='      Manufacturing'] <- "i.manuf"
df.N$Description[df.N$Description=='      Manufacturing'] <- "i.manuf"

df.S$Description[df.S$Description=='      Wholesale trade'] <- "i.trade1"
df.N$Description[df.N$Description=='      Wholesale trade'] <- "i.trade1"
df.S$Description[df.S$Description=='      Retail trade'] <- "i.trade2"
df.N$Description[df.N$Description=='      Retail trade'] <- "i.trade2"
df.N$Description[df.N$Description=='      Transportation and warehousing'] <- "i.trade3"

df.S$Description[df.S$Description=='      Finance, insurance, and real estate'] <- "i.non.labor" #SIC
df.N$Description[df.N$Description=='      Finance and insurance'] <- "i.non.labor1" #NCIAS combining
df.N$Description[df.N$Description=='      Real estate and rental and leasing'] <- "i.non.labor2" #NCIAS combining

df.S$Description[df.S$Description=='      Services'] <- "i.services"
df.N$Description[df.N$Description=='      Professional, scientific, and technical services'] <- "i.services1"
df.N$Description[df.N$Description=='      Management of companies and enterprises'] <- "i.services2"
df.N$Description[df.N$Description=='      Administrative and support and waste management and remediation services'] <- "i.services3"
df.N$Description[df.N$Description=='      Educational services'] <- "i.services4"
df.N$Description[df.N$Description=='      Health care and social assistance'] <- "i.services5"
df.N$Description[df.N$Description=='      Arts, entertainment, and recreation'] <- "i.services6"
df.N$Description[df.N$Description=='      Accommodation and food services'] <- "i.services7"
df.N$Description[df.N$Description=='      Other services (except public administration)'] <- "i.services8"

df.S$Description[df.S$Description=='    Government and government enterprises'] <- "i.govt"
df.N$Description[df.N$Description=='    Government and government enterprises'] <- "i.govt"

df.S$Description[df.S$Description=='      Federal, civilian'] <- "i.govt.fed"
df.N$Description[df.N$Description=='      Federal, civilian'] <- "i.govt.fed"

#CONVERT VARIABLES TO NUMERIC
str(df.N)
df.S[,(8:39)] <- lapply(df.S[,(8:39)], as.numeric)
df.N[,(8:21)] <- lapply(df.N[,(8:21)], as.numeric)

# CONVERT LONG TO WIDE
df.S.w <- reshape(df.S, idvar="GeoFIPS",timevar="Description", direction="wide")
df.N.w <- reshape(df.N, idvar="GeoFIPS",timevar="Description", direction="wide")

# sample <- df.S.w[0:3,] # CREATE SAMPLE TO WORK WITH
# sample$X1969.i.trade <- sample$X1969.i.trade1+sample$X1969.i.trade2 # SAMPLE ADDITION OF COLUMNS

# SIC TRADE ADDITION
df.S.w$X1969.i.trade <- df.S.w$X1969.i.trade1+df.S.w$X1969.i.trade2
df.S.w$X1970.i.trade <- df.S.w$X1970.i.trade1+df.S.w$X1970.i.trade2
df.S.w$X1971.i.trade <- df.S.w$X1971.i.trade1+df.S.w$X1971.i.trade2
df.S.w$X1972.i.trade <- df.S.w$X1972.i.trade1+df.S.w$X1972.i.trade2
df.S.w$X1973.i.trade <- df.S.w$X1973.i.trade1+df.S.w$X1973.i.trade2
df.S.w$X1974.i.trade <- df.S.w$X1974.i.trade1+df.S.w$X1974.i.trade2
df.S.w$X1975.i.trade <- df.S.w$X1975.i.trade1+df.S.w$X1975.i.trade2
df.S.w$X1976.i.trade <- df.S.w$X1976.i.trade1+df.S.w$X1976.i.trade2
df.S.w$X1977.i.trade <- df.S.w$X1977.i.trade1+df.S.w$X1977.i.trade2
df.S.w$X1978.i.trade <- df.S.w$X1978.i.trade1+df.S.w$X1978.i.trade2
df.S.w$X1979.i.trade <- df.S.w$X1979.i.trade1+df.S.w$X1979.i.trade2
df.S.w$X1980.i.trade <- df.S.w$X1980.i.trade1+df.S.w$X1980.i.trade2
df.S.w$X1981.i.trade <- df.S.w$X1981.i.trade1+df.S.w$X1981.i.trade2
df.S.w$X1982.i.trade <- df.S.w$X1982.i.trade1+df.S.w$X1982.i.trade2
df.S.w$X1983.i.trade <- df.S.w$X1983.i.trade1+df.S.w$X1983.i.trade2
df.S.w$X1984.i.trade <- df.S.w$X1984.i.trade1+df.S.w$X1984.i.trade2
df.S.w$X1985.i.trade <- df.S.w$X1985.i.trade1+df.S.w$X1985.i.trade2
df.S.w$X1986.i.trade <- df.S.w$X1986.i.trade1+df.S.w$X1986.i.trade2
df.S.w$X1987.i.trade <- df.S.w$X1987.i.trade1+df.S.w$X1987.i.trade2
df.S.w$X1988.i.trade <- df.S.w$X1988.i.trade1+df.S.w$X1988.i.trade2
df.S.w$X1989.i.trade <- df.S.w$X1989.i.trade1+df.S.w$X1989.i.trade2
df.S.w$X1990.i.trade <- df.S.w$X1990.i.trade1+df.S.w$X1990.i.trade2
df.S.w$X1991.i.trade <- df.S.w$X1991.i.trade1+df.S.w$X1991.i.trade2
df.S.w$X1992.i.trade <- df.S.w$X1992.i.trade1+df.S.w$X1992.i.trade2
df.S.w$X1993.i.trade <- df.S.w$X1993.i.trade1+df.S.w$X1993.i.trade2
df.S.w$X1994.i.trade <- df.S.w$X1994.i.trade1+df.S.w$X1994.i.trade2
df.S.w$X1995.i.trade <- df.S.w$X1995.i.trade1+df.S.w$X1995.i.trade2
df.S.w$X1996.i.trade <- df.S.w$X1996.i.trade1+df.S.w$X1996.i.trade2
df.S.w$X1997.i.trade <- df.S.w$X1997.i.trade1+df.S.w$X1997.i.trade2
df.S.w$X1998.i.trade <- df.S.w$X1998.i.trade1+df.S.w$X1998.i.trade2
df.S.w$X1999.i.trade <- df.S.w$X1999.i.trade1+df.S.w$X1999.i.trade2
df.S.w$X2000.i.trade <- df.S.w$X2000.i.trade1+df.S.w$X2000.i.trade2

#NAICS TRADE, NON-LABOR, and SERVICES
df.N.w$X2001.i.trade <- df.N.w$X2001.i.trade1+df.N.w$X2001.i.trade2
df.N.w$X2002.i.trade <- df.N.w$X2002.i.trade1+df.N.w$X2002.i.trade2
df.N.w$X2003.i.trade <- df.N.w$X2003.i.trade1+df.N.w$X2003.i.trade2
df.N.w$X2004.i.trade <- df.N.w$X2004.i.trade1+df.N.w$X2004.i.trade2
df.N.w$X2005.i.trade <- df.N.w$X2005.i.trade1+df.N.w$X2005.i.trade2
df.N.w$X2006.i.trade <- df.N.w$X2006.i.trade1+df.N.w$X2006.i.trade2
df.N.w$X2007.i.trade <- df.N.w$X2007.i.trade1+df.N.w$X2007.i.trade2
df.N.w$X2008.i.trade <- df.N.w$X2008.i.trade1+df.N.w$X2008.i.trade2
df.N.w$X2009.i.trade <- df.N.w$X2009.i.trade1+df.N.w$X2009.i.trade2
df.N.w$X2010.i.trade <- df.N.w$X2010.i.trade1+df.N.w$X2010.i.trade2
df.N.w$X2011.i.trade <- df.N.w$X2011.i.trade1+df.N.w$X2011.i.trade2
df.N.w$X2012.i.trade <- df.N.w$X2012.i.trade1+df.N.w$X2012.i.trade2
df.N.w$X2013.i.trade <- df.N.w$X2013.i.trade1+df.N.w$X2013.i.trade2
df.N.w$X2014.i.trade <- df.N.w$X2014.i.trade1+df.N.w$X2014.i.trade2

df.N.w$X2001.i.non.labor <- df.N.w$X2001.i.non.labor1+df.N.w$X2001.i.non.labor2
df.N.w$X2002.i.non.labor <- df.N.w$X2002.i.non.labor1+df.N.w$X2002.i.non.labor2
df.N.w$X2003.i.non.labor <- df.N.w$X2003.i.non.labor1+df.N.w$X2003.i.non.labor2
df.N.w$X2004.i.non.labor <- df.N.w$X2004.i.non.labor1+df.N.w$X2004.i.non.labor2
df.N.w$X2005.i.non.labor <- df.N.w$X2005.i.non.labor1+df.N.w$X2005.i.non.labor2
df.N.w$X2006.i.non.labor <- df.N.w$X2006.i.non.labor1+df.N.w$X2006.i.non.labor2
df.N.w$X2007.i.non.labor <- df.N.w$X2007.i.non.labor1+df.N.w$X2007.i.non.labor2
df.N.w$X2008.i.non.labor <- df.N.w$X2008.i.non.labor1+df.N.w$X2008.i.non.labor2
df.N.w$X2009.i.non.labor <- df.N.w$X2009.i.non.labor1+df.N.w$X2009.i.non.labor2
df.N.w$X2010.i.non.labor <- df.N.w$X2010.i.non.labor1+df.N.w$X2010.i.non.labor2
df.N.w$X2011.i.non.labor <- df.N.w$X2011.i.non.labor1+df.N.w$X2011.i.non.labor2
df.N.w$X2012.i.non.labor <- df.N.w$X2012.i.non.labor1+df.N.w$X2012.i.non.labor2
df.N.w$X2013.i.non.labor <- df.N.w$X2013.i.non.labor1+df.N.w$X2013.i.non.labor2
df.N.w$X2014.i.non.labor <- df.N.w$X2014.i.non.labor1+df.N.w$X2014.i.non.labor2

df.N.w$X2001.i.services <- df.N.w$X2001.i.services1+df.N.w$X2001.i.services2+df.N.w$X2001.i.services3+df.N.w$X2001.i.services4+df.N.w$X2001.i.services5+df.N.w$X2001.i.services6+df.N.w$X2001.i.services7+df.N.w$X2001.i.services8
df.N.w$X2002.i.services <- df.N.w$X2002.i.services1+df.N.w$X2002.i.services2+df.N.w$X2002.i.services3+df.N.w$X2002.i.services4+df.N.w$X2002.i.services5+df.N.w$X2002.i.services6+df.N.w$X2002.i.services7+df.N.w$X2002.i.services8
df.N.w$X2003.i.services <- df.N.w$X2003.i.services1+df.N.w$X2003.i.services2+df.N.w$X2003.i.services3+df.N.w$X2003.i.services4+df.N.w$X2003.i.services5+df.N.w$X2003.i.services6+df.N.w$X2003.i.services7+df.N.w$X2003.i.services8
df.N.w$X2004.i.services <- df.N.w$X2004.i.services1+df.N.w$X2004.i.services2+df.N.w$X2004.i.services3+df.N.w$X2004.i.services4+df.N.w$X2004.i.services5+df.N.w$X2004.i.services6+df.N.w$X2004.i.services7+df.N.w$X2004.i.services8
df.N.w$X2005.i.services <- df.N.w$X2005.i.services1+df.N.w$X2005.i.services2+df.N.w$X2005.i.services3+df.N.w$X2005.i.services4+df.N.w$X2005.i.services5+df.N.w$X2005.i.services6+df.N.w$X2005.i.services7+df.N.w$X2005.i.services8
df.N.w$X2006.i.services <- df.N.w$X2006.i.services1+df.N.w$X2006.i.services2+df.N.w$X2006.i.services3+df.N.w$X2006.i.services4+df.N.w$X2006.i.services5+df.N.w$X2006.i.services6+df.N.w$X2006.i.services7+df.N.w$X2006.i.services8
df.N.w$X2007.i.services <- df.N.w$X2007.i.services1+df.N.w$X2007.i.services2+df.N.w$X2007.i.services3+df.N.w$X2007.i.services4+df.N.w$X2007.i.services5+df.N.w$X2007.i.services6+df.N.w$X2007.i.services7+df.N.w$X2007.i.services8
df.N.w$X2008.i.services <- df.N.w$X2008.i.services1+df.N.w$X2008.i.services2+df.N.w$X2008.i.services3+df.N.w$X2008.i.services4+df.N.w$X2008.i.services5+df.N.w$X2008.i.services6+df.N.w$X2008.i.services7+df.N.w$X2008.i.services8
df.N.w$X2009.i.services <- df.N.w$X2009.i.services1+df.N.w$X2009.i.services2+df.N.w$X2009.i.services3+df.N.w$X2009.i.services4+df.N.w$X2009.i.services5+df.N.w$X2009.i.services6+df.N.w$X2009.i.services7+df.N.w$X2009.i.services8
df.N.w$X2010.i.services <- df.N.w$X2010.i.services1+df.N.w$X2010.i.services2+df.N.w$X2010.i.services3+df.N.w$X2010.i.services4+df.N.w$X2010.i.services5+df.N.w$X2010.i.services6+df.N.w$X2010.i.services7+df.N.w$X2010.i.services8
df.N.w$X2011.i.services <- df.N.w$X2011.i.services1+df.N.w$X2011.i.services2+df.N.w$X2011.i.services3+df.N.w$X2011.i.services4+df.N.w$X2011.i.services5+df.N.w$X2011.i.services6+df.N.w$X2011.i.services7+df.N.w$X2011.i.services8
df.N.w$X2012.i.services <- df.N.w$X2012.i.services1+df.N.w$X2012.i.services2+df.N.w$X2012.i.services3+df.N.w$X2012.i.services4+df.N.w$X2012.i.services5+df.N.w$X2012.i.services6+df.N.w$X2012.i.services7+df.N.w$X2012.i.services8
df.N.w$X2013.i.services <- df.N.w$X2013.i.services1+df.N.w$X2013.i.services2+df.N.w$X2013.i.services3+df.N.w$X2013.i.services4+df.N.w$X2013.i.services5+df.N.w$X2013.i.services6+df.N.w$X2013.i.services7+df.N.w$X2013.i.services8
df.N.w$X2014.i.services <- df.N.w$X2014.i.services1+df.N.w$X2014.i.services2+df.N.w$X2014.i.services3+df.N.w$X2014.i.services4+df.N.w$X2014.i.services5+df.N.w$X2014.i.services6+df.N.w$X2014.i.services7+df.N.w$X2014.i.services8

#MERGE TWO DATASETS HORIZONTALLY
df <- merge(df.S.w,df.N.w,by=c("GeoFIPS"))
df$FIPS <- df$GeoFIPS #rename FIPS for later merge with EIS master data
df <-df[,order(colnames(df))] #ORDER ALL COLUMNS ALPHABETICALLY
df <- df[c(-3:-221)] #REMOVE JUNK COLUMNS (Duplicates from the merge of county names, etc)

#WRITING OUT
write.csv(df, file="df_BEA.csv", col.names=TRUE, row.names=FALSE)
#HELPFUL FOR LATER MAYBE
# (df.S[,grep("^[X]", names(df.S))])) #grab only variables with X
