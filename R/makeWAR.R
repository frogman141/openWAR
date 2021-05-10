#' @title makeWAR
#' @aliases makeWAR.GameDayPlays
#' 
#' @description Computes runs above average (RAA) for each player involved in each play of the 'GameDayPlays' object. 
#' 
#' @details Within a 'GameDayPlays' object, each row consists of a single plate appearance and contains information about the batter, all of the baserunners,
#'  the pitcher, and all of the fielders on the field during the plate appearance. The total value of the play as determined by the change in the run expectancy 
#'  matrix from the beginning of the plate appearence to the end of the plate appearance is partitioned across all players involved in with the play on offense, 
#'  and that same value (with the opposite sign) is partitioned across the pitcher and all of the fielders.  Thus for every single plate appearance a runs above average (RAA) value is assigned
#'  to every player involved in the play.  
#'  If no \code{models} argument is supplied, then all
#' models necessary for the computation of openWAR will be generated on the data set given. 
#' The output of this function is then used in the function getWAR to calculate a Wins Above Replacement (WAR) value for each player. 
#' 
#' If \code{verbose == TRUE}, then various pieces of information will be displayed during the comuptation.
#' 
#' Elements of \code{models}:
#' \itemize{
#' \item{run-expectancy}{: a model for assigning a run expectancy value to any of the 24 (base,out) states. Variables
#' must be 'startCode' [0-7] and 'startOuts' [0-2]}
#' \item{pitching}{: a model for the expected outcome of a plate appearance attributable to the pitcher. Variables
#' must be 'venueId', 'throws' [L/R], and 'stands' [L/R]}
#' \item{offense}{: a model for the expected outcome of a plate appearance attributable to the offense. Variables
#' must be 'venueId', 'throws' [L/R], and 'stands' [L/R]}
#' \item{baserunning}{: a model for the expected contribution of the baserunners to a plate appearance. Variables
#' must be 'event' (the type of batting event), 'startCode' [0-7], and 'startOuts' [0-2]}
#' \item{batting}{: a model for the expected contribution of the batter to a plate appearance. Variables
#' must be 'batterPos' (the defensive position of the batter)}
#' }
#' 
#' @param data An object of class 'GameDayPlays'
#' @param models A named list of models, each with a predict() method. See Details.
#' @param verbose A LOGICAL indicating whether you want various messages and information to be displayed
#' during the computation
#' 
#' @return An object of class 'openWARPlays' which is a list of length 4 containing the following: 
#' \itemize{
#' \item{plays}{A data.frame of class 'GameDayPlays' that is the same as the input to the function.}
#' \item{data}{A data.frame of class 'GameDayPlaysExt' containing the original data along with appended rows containing the RAA values for each player involved in a plate appearance}
#'  \item{models.used}{A list containing all of the model information for each of the models used in computing RAA.}
#'   \item{openWAR}{A data.frame of class 'openWARPlays' containing only the columns necessry for input into the getWAR function.  }
#' }
#' 
#' @import dplyr
#' @importFrom stringr str_count
#' @export makeWAR
#' @export makeWAR.GameDayPlays
#' @examples
#' 
#' # ds = getData(start = '2013-03-31', end = '2013-09-30')
#' res = makeWAR(ds)
#' 

makeWAR = function(data, models = list(), verbose = TRUE, ...) UseMethod("makeWAR")

makeWAR.GameDayPlays = function(data, models = list(), verbose = TRUE, low.memory = TRUE, ...) {
    orig = data
    data$idx = 1:nrow(data)
    ########################################################################################### Step 1: Define \delta, the change in expected runs
    mod.re = getModelRunExpectancy(data[, c("outsInInning", "runsFuture", "startCode", "startOuts")], models[["run-expectancy"]], 
        verbose)
    models.used = list(`run-expectancy` = mod.re)
    deltas = makeWARre24(data[, c("startCode", "startOuts", "endCode", "endOuts", "runsOnPlay")], mod.re, verbose)
    data = cbind(data, deltas)
    
    ########################################################################################### Step 2: Define RAA for the defense Work only with the subset of data for which the ball is in play and keep track of the
    ########################################################################################### indices
    bip.idx = which(data$isBIP == TRUE)
    ds.field = data[bip.idx, ]
    fielding = makeWARFielding(ds.field, models, verbose)
    
    for (col.name in names(fielding)) {
        data[bip.idx, col.name] = fielding[, col.name]
    }
    
    ########################################################################################### Step 3: Define RAA for the pitcher
    data$delta.pitch = with(data, ifelse(is.na(delta.field), delta, delta - delta.field))
    message("...Estimating Pitching Runs Above Average...")
    mod.pitch = getModelPitching(data[, c("delta.pitch", "venueId", "throws", "stand")], models[["pitching"]], verbose)
    if (verbose) {
        message("....Pitching Model....")
        print(sort(coef(mod.pitch)))
    }
    
    models.used[["pitching"]] = mod.pitch
    # Note the pitcher RAA's are the negative residuals!
    data$raa.pitch = predict(mod.pitch, newdata = data[, c("venueId", "throws", "stand")]) - data$delta.pitch
    
    ########################################################################################### Step 4: Define RAA for the batter
    message("...Building model for offense...")
    mod.off = getModelOffense(data[, c("delta", "venueId", "throws", "stand")], models[["offense"]], verbose)
    if (verbose) {
        message("....Offense Model....")
        print(sort(coef(mod.off)))
    }
    models.used[["offense"]] = mod.off
    # delta.off is the contribution above average of the batter AND all of the runners
    data$delta.off = data$delta - predict(mod.off, newdata = data[, c("venueId", "throws", "stand")])
    
    # If runners are on base, partition delta between the batter and baserunners
    br.idx = which(data$startCode > 0)
    message("...Partitioning Offense into Batting and Baserunning...")
    mod.br = getModelBaserunning(data[br.idx, c("delta.off", "event", "startCode", "startOuts")], models[["baserunning"]], verbose)
    if (verbose) {
        message("....Baserunning Model....")
        message(paste("....", length(coef(mod.br)), "coefficients -- suppressing output..."))
        # print(sort(coef(mod.br)))
    }
    models.used[["baserunning"]] = mod.br
    # delta.br is the contribution of the baserunners beyond the event type
    data[br.idx, "delta.br"] = data[br.idx, "delta.off"] - predict(mod.br, newdata = data[br.idx, c("event", "startCode", "startOuts")])
    
    # Whatever is left over goes to the batter -- just control for defensive position
    data$delta.bat = with(data, ifelse(is.na(delta.br), delta, delta - delta.br))
    message("...Estimating Batting Runs Above Average...")
    mod.bat = getModelBatting(data[, c("delta.bat", "batterPos")], models[["batting"]], verbose)
    if (verbose) {
        message("....Batting Model....")
        print(sort(coef(mod.bat)))
    }
    models.used[["batting"]] = mod.bat
    # Control for batter position Note that including 'idx' is not necessary -- it just ensure that the argument passed is a
    # data.frame
    data$raa.bat = data$delta.bat - predict(mod.bat, newdata = data[, c("batterPos", "idx")])
    
    
    ########################################################################################### Step 5: Define RAA for the baserunners
    br.fields = c("idx", "delta.br", "start1B", "start2B", "start3B", "end1B", "end2B", "end3B", "event", "des", "startCode", "startOuts")
    raa.br = makeWARBaserunning(data[br.idx, br.fields], models[["baserunning"]], verbose)
    data = merge(x = data, y = raa.br, by = "idx", all.x = TRUE)
    
    ########################################################################################### Add the new class
    class(data) = c("GameDayPlaysExt", "GameDayPlays", "data.frame")
    # include the computations as a separate data.frame
    id.fields = c("batterId", "start1B", "start2B", "start3B", "pitcherId", "playerId.C", "playerId.1B", "playerId.2B", "playerId.3B", 
        "playerId.SS", "playerId.LF", "playerId.CF", "playerId.RF", "game_id", "event", "isPA")
    delta.fields = c("delta", "delta.field", "delta.pitch", "delta.br", "delta.bat")
    raa.fields = c("raa.bat", "raa.br1", "raa.br2", "raa.br3", "raa.pitch", "raa.P", "raa.C", "raa.1B", "raa.2B", "raa.3B", 
        "raa.SS", "raa.LF", "raa.CF", "raa.RF")
    openWARPlays = data[, c(id.fields, delta.fields, raa.fields)]
    class(openWARPlays) = c("openWARPlays", "data.frame")
    if (low.memory) {
        return(list(plays = NULL, data = NULL, models.used = NULL, openWAR = openWARPlays))
    } else {
        return(list(plays = orig, data = data, models.used = models.used, openWAR = openWARPlays))
    }
}


makeWARre24 = function(data, mod.re = NULL, verbose = TRUE, ...) {
    message("...Estimating Expected Runs...")
    
    begin.states = data[, c("startCode", "startOuts")]
    end.states = data[, c("endCode", "endOuts")]
    end.states$endOuts = with(end.states, ifelse(endOuts == 3, NA, endOuts))
    names(end.states) = names(begin.states)
    
    startExR = predict(mod.re, newdata = begin.states)
    endExR = predict(mod.re, newdata = end.states)
    endExR = ifelse(is.na(endExR), 0, endExR)
    
    out = data.frame(startExR, endExR)
    out$delta = endExR - startExR + data$runsOnPlay
    return(out)
}


makeWARFielding = function(data, models = list(), verbose = TRUE, ...) {
    message("...Estimating Fielding Runs Above Average...")
    
    data = transform(data, wasFielded = !is.na(fielderId))
    # Compute the collective responsibility of all fielders
    p.hat = getModelFieldingCollective(data[, c("wasFielded", "our.x", "our.y")])
    # Step 2a: Define \delta.field for the defense, collectively
    delta.field = data$delta * p.hat
    
    # Compute the individual responsibility of each fielder
    P = getFielderResp(data)
    # Step 2b: Define \delta.field for the defense, individually
    delta.fielders = delta.field * P
    names(delta.fielders) = gsub("resp", "delta", names(delta.fielders))
    
    out = data.frame(p.hat, delta.field, delta.fielders)
    
    # Normalize the delta's into RAA's
    raa.field = getFielderRAA(cbind(out, venueId = data$venueId))
    return(cbind(out, raa.field))
}


#' 
#' @title getFielderRAA
#' 
#' @description Determine the Runs Above Average (RAA) of the fielders
#' 
#' @details RAA is the residuals from a simple fielding model.  Used in the function \code{makeWARFielding}
#' 
#' @param data A data.frame containg an estimate of the probably that a ball on a given play would be fielded, the total delta for fielders, a column of delta for each fielder, and a venueId
#' 
#' @return A data.frame containing fielding RAA values for all plate appearances with a ball in play
#' 
#' @export
#' 
#' 

getFielderRAA = function(data) {
    # Build a model for each fielder's expected change in runs
    mod.P = lm(delta.P ~ factor(venueId), data = data)
    mod.C = lm(delta.C ~ factor(venueId), data = data)
    mod.1B = lm(delta.1B ~ factor(venueId), data = data)
    mod.2B = lm(delta.2B ~ factor(venueId), data = data)
    mod.3B = lm(delta.3B ~ factor(venueId), data = data)
    mod.SS = lm(delta.SS ~ factor(venueId), data = data)
    mod.LF = lm(delta.LF ~ factor(venueId), data = data)
    mod.CF = lm(delta.CF ~ factor(venueId), data = data)
    mod.RF = lm(delta.RF ~ factor(venueId), data = data)
    
    # Define RAA to be the residuals from the individual fielders models
    raa = -data.frame(mod.P$residuals, mod.C$residuals, mod.1B$residuals, mod.2B$residuals, mod.3B$residuals, mod.SS$residuals, 
        mod.LF$residuals, mod.CF$residuals, mod.RF$residuals)
    names(raa) = gsub("mod", "raa", gsub(".residuals", "", names(raa)))
    
    # The column-wise sums should all be zero colSums(raa)
    return(raa)
}




#' @title getFielderResp
#' 
#' @description Find the shared responsibility for balls in play
#' 
#' @details Fits 9 logistic regression models, each giving the probability of 
#' a fielder at one of the 9 defensive positions successfully converting the 
#' ball into at least one out.
#' 
#' @param data An MLBAM data.frame 
#' 
#' @return data.frame with 9 columns, each row representing a ball in play
#' 
#' @export
#' 


getFielderResp = function(data, ...) {
    ds = data
    ds$fielderPos = with(ds, ifelse(is.na(fielderId), "Hit", "Out"))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == pitcherId, "P", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.C, "C", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.1B, "1B", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.2B, "2B", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.3B, "3B", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.SS, "SS", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.LF, "LF", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.CF, "CF", fielderPos))
    ds$fielderPos = with(ds, ifelse(!is.na(fielderId) & fielderId == playerId.RF, "RF", fielderPos))
    
    message("....Building a fielding model for each position...")
    mod.P = getModelFieldingPitcher(ds[, c("fielderPos", "our.x", "our.y")])
    mod.C = getModelFieldingCatcher(ds[, c("fielderPos", "our.x", "our.y")])
    mod.1B = getModelFielding1B(ds[, c("fielderPos", "our.x", "our.y")])
    mod.2B = getModelFielding2B(ds[, c("fielderPos", "our.x", "our.y")])
    mod.3B = getModelFielding3B(ds[, c("fielderPos", "our.x", "our.y")])
    mod.SS = getModelFieldingSS(ds[, c("fielderPos", "our.x", "our.y")])
    mod.LF = getModelFieldingLF(ds[, c("fielderPos", "our.x", "our.y")])
    mod.CF = getModelFieldingCF(ds[, c("fielderPos", "our.x", "our.y")])
    mod.RF = getModelFieldingRF(ds[, c("fielderPos", "our.x", "our.y")])
    
    # mod = mod.CF summary(mod) fit = makeFun(mod) plotFun(fit(x,y) ~ x + y, surface=TRUE, alpha=0.9 , xlim = c(-350, 350), ylim
    # = c(0, 550) , xlab = 'Horizontal Distance from Home Plate (ft.)' , ylab = 'Vertical Distance from Home Plate (ft.)' , zlab
    # = 'Probability of Making a Play' )
    
    out = data.frame(mod.P$fitted, mod.C$fitted, mod.1B$fitted, mod.2B$fitted, mod.3B$fitted, mod.SS$fitted, mod.LF$fitted, 
        mod.CF$fitted, mod.RF$fitted)
    row.sums = apply(out, 1, sum)
    out = out/row.sums
    names(out) = c("resp.P", "resp.C", "resp.1B", "resp.2B", "resp.3B", "resp.SS", "resp.LF", "resp.CF", "resp.RF")
    return(out)
}


makeWARBaserunning = function(data, mod.bat, verbose = TRUE, ...) {
    message("...Estimating Baserunning Runs Above Average...")
    
    data = track_baserunner_movement(data)
    data = calculate_br_cdf(data)
    data = calc_br_raa(data)
    
    return(data[, c("idx", "raa.br1", "raa.br2", "raa.br3")])
} 


track_baserunner_movement = function(dat) {
    # Searching through the baserunning events to identify what happened to the baserunners
    dat = dat %>%
        rowwise %>%
        mutate(# Figuring out what's happened to the baserunner starting at 3rd
            dest.br3 = ifelse(!is.na(start3B) & grepl(paste(start3B, 'out at'), des), "O", NA),
            dest.br3 = ifelse(!is.na(start3B) & grepl(paste(start3B, 'scores'), des), "H", dest.br3),
            dest.br3 = ifelse(!is.na(start3B) & !is.na(end3B) & start3B == end3B, "3B", dest.br3),
            br3.adv = ifelse(dest.br3 == "O", -3, ifelse(dest.br3 == "H", 1, 0)),
            
            # Figuring out what's happened to the baserunner starting at 2nd
            dest.br2 = ifelse(!is.na(start2B) & grepl(paste(start2B, 'out at'), des), "O", NA),
            dest.br2 = ifelse(!is.na(start2B) & !is.na(end3B) & start2B == end3B, "3B", dest.br2),
            dest.br2 = ifelse(!is.na(start2B) & !is.na(end2B) & start2B == end2B, "2B", dest.br2),
            dest.br2 = ifelse(!is.na(start2B) & grepl(paste(start2B, 'scores'), des), "H", dest.br2),
            br2.adv = ifelse(dest.br2 == "O", -2, ifelse(dest.br2 == "3B", 1, ifelse(dest.br2 == "H", 2, 0))),
            
            # Figuring out what's happened to the baserunner starting at 1sd
            dest.br1 = ifelse(!is.na(start1B) & grepl(paste(start1B, 'out at'), des), "O", NA),
            dest.br1 = ifelse(!is.na(start1B) & !is.na(end1B) & start1B == end3B, "3B", dest.br1),
            dest.br1 = ifelse(!is.na(start1B) & !is.na(end1B) & start1B == end2B, "2B", dest.br1),
            dest.br1 = ifelse(!is.na(start1B) & !is.na(end1B) & start1B == end1B, "1B", dest.br1),
            dest.br1 = ifelse(!is.na(start1B) & grepl(paste(start1B, 'scores'), des), "H", dest.br1),
            br1.adv = ifelse(dest.br1 == "O", -1, ifelse(dest.br1 == "2B", 1, ifelse(dest.br1 == "3B", 2, ifelse(dest.br1 == "H", 3, 0)))))
    
    return (dat)
}

calculate_br_cdf = function(dat) {
    join.idx = c("event", "startCode", "startOuts")
    
    # calculate CDF for baserunner at 3rd
    ds3Probs = dat %>% 
        filter(!is.na(start3B)) %>%
        group_by(event, startCode, startOuts, br3.adv) %>%
        select(event, startCode, startOuts, br3.adv) %>%
        do(getCDF(.data))
    
    # calculate CDF for baserunner at 2nd
    ds2Probs = dat %>%
        filter(!is.na(start2B)) %>%
        group_by(event, startCode, startOuts, br2.adv) %>%
        select(event, startCode, startOuts, br2.adv) %>%
        do(getCDF(.data))
    
    # calculate CDF for baserunner at 1st
    ds1Probs = dat %>%
        filter(!is.na(start1B)) %>%
        group_by(event, startCode, startOuts, br1.adv) %>%
        select(event, startCode, startOuts, br1.adv) %>%
        do(getCDF(.data))
    
    dat = dat %>%
        merge(y=ds3Probs[, c(join.idx, "br3.adv", "cdf.lag")], by.x=c(join.idx, "br3.adv"), by.y=c(join.idx, "br3.adv"), all.x=TRUE) %>%
        rename(cdf.br3 = cdf.lag) %>%
        merge(y=ds2Probs[, c(join.idx, "br2.adv", "cdf.lag")], by.x=c(join.idx, "br2.adv"), by.y=c(join.idx, "br2.adv"), all.x=TRUE) %>%
        rename(cdf.br2 = cdf.lag) %>%
        merge(y=ds1Probs[, c(join.idx, "br1.adv", "cdf.lag")], by.x=c(join.idx, "br1.adv"), by.y=c(join.idx, "br1.adv"), all.x=TRUE) %>%
        rename(cdf.br1 = cdf.lag)
    
    return (dat)
}

calc_br_raa = function(dat) {
    dat = dat %>%
        rowwise %>% # Make sure that baserunners who get out have a non-zero share
        mutate(cdf.br1 = ifelse(cdf.br1 == 0,1e-08, cdf.br1),
               cdf.br2 = ifelse(cdf.br1 == 0,1e-08, cdf.br2),
               cdf.br3 = ifelse(cdf.br1 == 0,1e-08, cdf.br3),
               
               # Give zeros to the bases that were not occupied
               cdf.br1 = ifelse(is.na(cdf.br1), 0, cdf.br1),
               cdf.br2 = ifelse(is.na(cdf.br2), 0, cdf.br2),
               cdf.br3 = ifelse(is.na(cdf.br3), 0, cdf.br3),
               
               # normalize the cdf probs
               share.br1 = cdf.br1 / (cdf.br1 + cdf.br2 + cdf.br3),
               share.br2 = cdf.br2 / (cdf.br1 + cdf.br2 + cdf.br3),
               share.br3 = cdf.br3 / (cdf.br1 + cdf.br2 + cdf.br3),
               
               delta.br = ifelse(is.na(delta.br), 0, delta.br),
               raa.br1 = share.br1 * delta.br,
               raa.br2 = share.br2 * delta.br,
               raa.br3 = share.br3 * delta.br)
    
    return (dat)
}

getCDF = function(ds) {
    # summarise data for each base runner separately
    if ("br3.adv" %in% colnames(ds)) {
        events = summarise(group_by(ds, br3.adv), N = length(br3.adv))
    } else if ("br2.adv" %in% colnames(ds)) {
        events = summarise(group_by(ds, br2.adv), N = length(br2.adv))
    } else {
        events = summarise(group_by(ds, br1.adv), N = length(br1.adv))
    }
    
    # calculate CDF for baserunner
    events = events %>% mutate(numObs = nrow(ds))
    events = events %>% mutate(p = N / numObs)
    events$cdf = cumsum(events$p)
    events$cdf.lag = c(0, cumsum(events$p[-nrow(events)])) 
    
    return(events)
}