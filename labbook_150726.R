# author: Steve Harris
# date: 2015-07-26
# subject: Working draft of data management for Perv and obs data

# Readme
# ======
# Aim is to merge excel data
# Version 2


# Todo
# ====


# Log
# ===
# 2015-07-26
# - file duplicated from previous labook



rm(list=ls())

setwd('/Users/steve/aor/p-academic/collab-obs-uclh/src')
library(XLConnect)
library(stringdist)

# Load anaesthetic database
# -------------------------

xlbook <- loadWorkbook('/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-anaes.xlsx')
rdf.anaes <- readWorksheet(xlbook, sheet = 'ANAESTHETIC DATABASE')
dim(rdf.anaes)
str(rdf.anaes)


# Load theatre databases
# ----------------------

xlbook <- loadWorkbook('/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-theatre13-15.xlsx')
rdf.theatre1 <- readWorksheet(xlbook, sheet = 'Theatre Case times 2013-15 orig')
str(rdf.theatre1)

# FIXME: 2015-07-13 - [ ] out of memory issues, manually import as CSV
# xlbook <- loadWorkbook('/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-theatre09-13.xlsx')
# rdf.theatre2 <- readWorksheet(xlbook, sheet = 'Theatre case times 2009-13 orig')
# rm(xlbook)
rdf.theatre2 <- read.csv(
        '/Users/steve/aor/p-academic/collab-obs-uclh/data/150701_obs-db-theatre09-13.csv',
        stringsAsFactors=FALSE)
str(rdf.theatre2)



# Convert to minimal data tables
# ------------------------------
require(data.table)

names(rdf.anaes)
adt <- data.table(rdf.anaes[c('forename', 'surname', 'MRN')])
adt[,name.a := tolower(paste0(surname, ', ', forename))]
adt[, 'id.a':=as.numeric(.I)]
adt[, `:=`(forename=NULL, surname=NULL)]
setcolorder(adt, c('id.a', 'MRN', 'name.a'))
adt[(MRN=='')]
adt[is.na(MRN)]
setkey(adt, 'MRN')
adt <- unique(adt)
adt

names(rdf.theatre1)
bdt.1 <- data.table(rdf.theatre1[c('Patient.Name', 'Hospital.Number')])
bdt.1[, theatre.record := '2013-15']
setnames(bdt.1, 'Patient.Name', 'name.b')
setnames(bdt.1, 'Hospital.Number', 'MRN')
bdt.1

names(rdf.theatre2)
bdt.2 <- data.table(rdf.theatre2[c('PatName', 'ID.helper')])
bdt.2[, theatre.record := '2009-13']
setnames(bdt.2, 'PatName', 'name.b')
setnames(bdt.2, 'ID.helper', 'MRN')
bdt.2

bdt <- rbind(bdt.2, bdt.1)
bdt[, 'id.b':=as.numeric(.I)]
bdt[, name.b := tolower(name.b)]
setcolorder(bdt, c('id.b', 'MRN', 'name.b', 'theatre.record'))
bdt[, MRN := ifelse(MRN=='',NA,MRN)]
bdt[is.na(MRN)]
# Work with unique
setkey(bdt, 'MRN')
bdt <- unique(bdt)
str(bdt)

# Save copy of data
ls()
save(adt, bdt, file='temp')

# Now do exact merge
# ------------------
# First create a list of unique hospital numbers
c <- unique(rbind(adt[,'MRN',with=FALSE],bdt[,'MRN',with=FALSE]))
# Drop missing MRN, and create new ID
c[,"exa.id":=.GRP,by='MRN']
c <- c[!is.na(MRN)]
str(c)

# Now merge
# - drop NAs to avoid cartesian join on this
# - work with unique MRN only - later will need to add dates etc
adt1 <- merge(adt[!is.na(MRN)],bdt[!is.na(MRN)],by='MRN',all.x=T)
head(adt1,30)
library(Hmisc)
describe(adt1$id.b)
adt2 <- adt1[is.na(id.b)]
adt2

bdt1 <- merge(bdt[!is.na(MRN)],adt[!is.na(MRN)],by='MRN',all.x=T)
head(bdt1,30)
describe(bdt1$id.a)
bdt2 <- bdt1[is.na(id.a)]
bdt2

# Try RecordLinkage
require(RecordLinkage)
str(adt2)
a <- adt2[,.(MRN, id=id.a, name=name.a)]
str(bdt2)
b <- bdt2[,.(MRN, id=id.b, name=name.b)]
rpairs <- compare.linkage(a, b, exclude=c('id'))
rpairs <- epiWeights(rpairs) # calculate weights
str(rpairs)
summary(rpairs$Wdata)
tail(getPairs(rpairs, max.weight=Inf, min.weight=0.4)) # run after weights



# Now adt2 and bd2 are the remaining unmatched
sdm <- stringdistmatrix(adt2$name.a, bdt2$name.b, method='osa')
sdm.orig <- sdm
# Use ids as row / col names
rownames(sdm) <- adt2$id.a
colnames(sdm) <- bdt2$id.b

# DEBUGGING: 2015-07-27 - [ ] problem with NA?
x <- sdm[1:10,1:10]
x[1,1] <- NA
x
max.col(-x, ties.method='first')
# try using colRanks from matrixStats

mid <- max.col(-sdm, ties.method='first')
mid <- matrix(c(1:nrow(sdm),mid),ncol=2)
bestdis <- sdm[mid]
r <- data.table(as.numeric(rownames(sdm)), as.numeric(colnames(x)[mid[,2]]), bestdis )
setnames(r, c('id.a', 'id.b', 'dist'))
head(r,30)
describe(r$id.a)
describe(r$id.b)

adt[is.na(exa.id)]
adt
bdt <- merge(bdt,c,by='MRN',all.x=T)
bdt[is.na(exa.id)]
bdt


# Now do fuzzy on remaining unmatched
# -----------------------------------
adt2 <- adt[is.na(exa.id)]
adt2
bdt2 <- bdt[is.na(exa.id)]
bdt2





# Inspect the outcome - but r is temporary 
r <- merge(adt,bdt,by=c('exa.id'),all.x=T, allow.cartesian=T)
r[,dist:=stringdist(name.a,name.b)]
require(Hmisc)
describe(r$dist) # distance missing for 8000+

adt
bdt

# End of re-write of code
# -----------------------
stop()






# Now try a test join
# test.a <- head(rdf.anaes[1:5])
# test.a
# test.t <- head(rdf.theatre1[1:5])
# test.t
test.a <- rdf.anaes[1:5]
nrow(test.a)
names(test.a)
test.t <- rdf.theatre1[1:5]
nrow(test.t)
names(test.t)

j <- merge(test.a, test.t, by.x='MRN', by.y='Hospital.Number')
head(j)
names(j)


# Make a test data set for doing fuzzy matching
library(stringdist)
# Use the merge from above so you know you will get some matches
j2 <- j[1:1000,c(3,4,8)]
str(j2)

# Create an index id based on row order which you will then use for a join
name.a <- sapply(paste(j2$surname, j2$forename, sep=','), tolower, USE.NAMES=F)
name.a <- data.frame(id.a=1:length(name.a), name=name.a, stringsAsFactors=F)
str(name.a)

name.b <- sapply(j2$Patient.Name, tolower, USE.NAMES=F)
name.b <- data.frame(id.b=1:length(name.b), name=name.b, stringsAsFactors=F)
str(name.b)

# Now take these two lists
system.time(
    name.a$id.b <- sapply(name.a$name, function(x) amatch(x, name.b$name, maxDist=5))
    )
str(name.a)

# Now join
tdf <- merge(name.a, name.b, by.x='id.b', by.y='id.b', all.x=T, sort=F)
tdf <- tdf[order(tdf$id.a),] # sort by id.a
head(tdf,20)

# Now let's do this again, but wrap it all as a function
library(assertthat)
'name' %in% name.a
'name' %in% names(name.a)

# DEBUGGING: 2015-07-26 - [ ] 
tdt <- fuzzy(test.a, test.t, x='name', y='Patient.Name', maxDist=5)
df.x <- test.a
df.y <- test.t
x <- 'name'
y <- 'Patient.Name'

fuzzy <- function(df.x, df.y, x, y, lower=TRUE, method='osa', maxDist=2 ) {

    require(assertthat)
    require(stringdist)
    require(data.table)
    # Default method = optimal string alignment (restricted Damerau-Levenshtein distance)
    # NB: amatch will return the first match if there is more than one

    # Housekeeping checks
    # Pass two dataframes to a function
    assert_that(is.data.frame(df.x))
    assert_that(is.data.frame(df.y))
    # Pass two fields for fuzzy matching
    assert_that(x %in% names(df.x))
    assert_that(y %in% names(df.y))

    # Create an index based on row order for the join
    df.x$fuzzy.id.x <- 1:nrow(df.x)
    df.y$fuzzy.id.y <- 1:nrow(df.y)

    # Convert to lower case (default = TRUE)
    if (lower) {
        df.x[[x]] <- sapply(df.x[[x]], tolower, USE.NAMES=F)
        df.y[[y]] <- sapply(df.y[[y]], tolower, USE.NAMES=F)
    }

    # Timer
    fuzzy.timer <- system.time(
        # Now the fuzzy match
        df.x$fuzzy.id.y <- sapply(df.x[[x]], function(a) amatch(a, df.y[[y]],
            method=method, maxDist=maxDist))
        )
    print(fuzzy.timer)

    # Now join the two dataframes
    tdf <- merge(df.x, df.y, by.x='fuzzy.id.y', by.y='fuzzy.id.y', all.x=T)
    tdf <- tdf[order(tdf$fuzzy.id.x),]

    # Now return the distance for the matches
    # Syntax easier using data.table
    tdt <- data.table(tdf)
    if (x==y) {
        # append x, y suffixes if names are the same
        tdt[, fuzzy.dist := stringdist(
            get(paste0(x, '.x')), get(paste0(y, '.y')),
            method=method)]
    }
    else {
        tdt[, fuzzy.dist := stringdist(get(x),get(y), method=method)]
    }

    return(tdt)
}

tdt <- fuzzy(name.a, name.b, x='name', y='name', maxDist=5)
head(tdt,100)
tdt
str(tdt)
summary(tdt$fuzzy.dist)

ls()
str(test.t)
test.a$name <- with(test.a, paste(surname, forename, sep=', '))
str(test.a)
tdt <- fuzzy(test.a[100,], test.t[100,], x='name', y='Patient.Name', maxDist=3)
tdt <- fuzzy(test.a, test.t, x='name', y='Patient.Name', maxDist=3)
head(tdt)
tdt[1:100,.(fuzzy.dist, name, Patient.Name)]

tdt[, fuzzy.dist := stringdist(name,Patient.Name, method='osa')]
tdt

# TODO: 2015-07-25 - [ ] post to SO/CV
# TODO: 2015-07-25 - [ ] add options for other parts of the amatch
# TODO: 2015-07-25 - [ ] Try running on the full sample
#

stop()
# NOTE: 2015-07-26 - [ ]  alternative approach
# http://stackoverflow.com/questions/29449480/fuzzy-merging-in-r-seeking-help-to-improve-my-code
# a = data.table, b= data.table
# .exact = list of variables for exact matching
# .fuzzy = list of variables for fuzzy matching
# .weights = weights for fuzzy matches
merge.fuzzy = function(a,b,.exact,.fuzzy,.weights,.method,.ncores) {
    require(stringdist)
    require(matrixStats)
    require(parallel)

    if (length(.fuzzy)!=length(.weights)) {
        stop(paste0("fuzzy and weigths must have the same length"))
    }

    if (!any(class(a)=="data.table")) {
        stop(paste0("'a' must be of class data.table"))
    }

    if (!any(class(b)=="data.table")) {
        stop(paste0("'b' must be of class data.table"))
    }

    #convert everything to lower
    a[,c(.fuzzy):=lapply(.SD,tolower),.SDcols=.fuzzy]
    b[,c(.fuzzy):=lapply(.SD,tolower),.SDcols=.fuzzy]

    a[,c(.exact):=lapply(.SD,tolower),.SDcols=.exact]
    b[,c(.exact):=lapply(.SD,tolower),.SDcols=.exact]

    #create ids
    a[,"id.a":=as.numeric(.I),by=c(.exact,.fuzzy)]
    b[,"id.b":=as.numeric(.I),by=c(.exact,.fuzzy)]


    c <- unique(rbind(a[,.exact,with=FALSE],b[,.exact,with=FALSE]))
    c[,"exa.id":=.GRP,by=.exact]

    a <- merge(a,c,by=.exact,all=FALSE)
    b <- merge(b,c,by=.exact,all=FALSE)

    ##############

    stringdi <- function(a,b,.weights,.by,.method,.ncores) {
        sdm      <- list()

        if (is.null(.weights)) {.weights <- rep(1,length(.by))}

        if (nrow(a) < nrow(b)) {
            for (i in 1:length(.by)) {
                sdm[[i]] <- stringdistmatrix(a[[.by[i]]],b[[.by[i]]],method=.method,ncores=.ncores,useNames=TRUE)
            }
        } else {
            for (i in 1:length(.by)) { #if a is shorter, switch sides; this enhances  parallelization speed
                sdm[[i]] <- stringdistmatrix(b[[.by[i]]],a[[.by[i]]],method=.method,ncores=.ncores,useNames=FALSE)
            }
        }

        rsdm = dim(sdm[[1]])
        csdm = ncol(sdm[[1]])
        sdm  = matrix(unlist(sdm),ncol=length(by))
        sdm  = rowSums(sdm*.weights,na.rm=T)/((0 + !is.na(sdm)) %*% .weights)
        sdm  = matrix(sdm,nrow=rsdm,ncol=csdm)

        #use ids as row/ column names
        rownames(sdm) <- a$id.a
        colnames(sdm) <- b$id.b

        mid           <- max.col(-sdm,ties.method="first")
        mid           <- matrix(c(1:nrow(sdm),mid),ncol=2)
        bestdis       <- sdm[mid] 

        res           <- data.table(as.numeric(rownames(sdm)),as.numeric(colnames(sdm)[mid[,2]]),bestdis)
        setnames(res,c("id.a","id.b","dist"))

        res
    }

    setkey(b,exa.id)
    distances = a[,stringdi(.SD,b[J(.BY[[1]])],.weights=.weights,.by=.fuzzy,.method=.method,.ncores=.ncores),by=exa.id]

    a    = merge(a,distances,by=c("exa.id","id.a"))
    res  = merge(a,b,by=c("exa.id","id.b"))


    res
}

a1 <- data.table(test.a[1:1000,])
t1 <- data.table(test.t[1:1000,])
t1 <- setnames(t1,'Hospital.Number','MRN')
t1 <- setnames(t1,'Patient.Name','name')
str(a1)
str(t1)

tdt1 <- merge.fuzzy(a1, t1, .exact=c('MRN'), .fuzzy=c('name'),.weights=c(1),.method='osa',.ncores=1) 