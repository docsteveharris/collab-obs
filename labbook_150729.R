# Experiment with the RecordLinkage package
install.packages('RecordLinkage', type='source')
require(RecordLinkage)

data(RLdata500)
RLdata500[1:5,]
str(RLdata500)

# Deduplicate a single data set
rpairs <- compare.dedup(RLdata500, identity = identity.RLdata500)
str(rpairs$pairs)
rpairs$pairs[1:5,]

# Having trouble sub-setting, switching to data.table
require(data.table)
x <- data.table(rpairs$pairs)
x[is_match==1]

# Testing blocking
rpairs <- compare.dedup(RLdata500,
    blockfld = list(1, 5:7),            # must match on first name, dob
    identity = identity.RLdata500)
x <- data.table(rpairs$pairs)
x[is_match==1]

# Testing 'fuzzy' matching
rpairs <- compare.dedup(RLdata500,
    blockfld = c(5,6),            # must match on bm, bd (month, day)
    strcmp = TRUE, strcmpfun=levenshteinSim)
x <- data.table(rpairs$pairs)
head(x)
str(x)
x[is_match==1]

# Record linage
rpairs <- compare.dedup(RLdata500,
    blockfld = list(1, 5:7),            # must match on first name, dob
    identity = identity.RLdata500)
rpairs <- epiWeights(rpairs) # calculate weights
x <- epiClassify(rpairs, threshold.upper = 0.6, threshold.lower = 0.5)
summary(x)
head(x)

tail(getPairs(rpairs, 0.6, 0.5)) # run after weights
editMatch(rpairs) # inspect and edit

# Let's test this
# ---------------
rm(list=ls())
require(RecordLinkage)
require(data.table)
require(Hmisc)
setwd('/Users/steve/aor/academic/collab-obs-uclh/src')
load('temp')
ls()
str(adt)
str(bdt)
a <- adt[1:1000,.(id=id.a, MRN, name=name.a)]
b <- bdt[1:1000,.(id=id.b, MRN, name=name.b)]

rpairs <- compare.linkage(a, b, exclude=c('id'))
str(rpairs)
rpairs <- epiWeights(rpairs) # calculate weights
tail(getPairs(rpairs, max.weight=Inf, min.weight=0.5)) # run after weights
tail(getPairs(rpairs, max.weight=Inf, min.weight=0.5, single.rows=TRUE)) # run after weights

x <- epiClassify(rpairs, threshold.upper = 0.6, threshold.lower = 0.1)
summary(x)
str(x)
table(x$prediction=='P')
str(x$prediction=='P')
p <- x$prediction=='P'
str(x[['pairs']])
y <- x[['pairs']]
str(y)
str(y[p,])

x <- data.table(x$pairs)
x[is_match==TRUE]
sum(is.na(x$is_match))

