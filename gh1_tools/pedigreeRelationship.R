#!/usr/bin/env Rscript

## begin warning handler
withCallingHandlers({

library(methods) # Because Rscript does not always do this

options('useFancyQuotes' = FALSE)

suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("RGalaxy"))


option_list <- list()

option_list$pathPed <- make_option('--pathPed', type='character')
option_list$pathIds <- make_option('--pathIds', type='character')
option_list$ordered <- make_option('--ordered', type='logical')
option_list$outputfile <- make_option('--outputfile', type='character')


opt <- parse_args(OptionParser(option_list=option_list))

suppressPackageStartupMessages(library(Hackathon2))

## function body not needed here, it is in package

params <- list()
for(param in names(opt))
{
    if (!param == "help")
        params[param] <- opt[param]
}

setClass("GalaxyRemoteError", contains="character")
wrappedFunction <- function(f)
{
    tryCatch(do.call(f, params),
        error=function(e) new("GalaxyRemoteError", conditionMessage(e)))
}


suppressPackageStartupMessages(library(RGalaxy))
do.call(pedigreeRelationship, params)

## end warning handler
}, warning = function(w) {
    cat(paste("Warning:", conditionMessage(w), "\n"))
    invokeRestart("muffleWarning")
})
