.EmptyEnv <- if (exists("emptyenv")) emptyenv() else NULL
mkHash <- function() new.env(hash = TRUE, parent = .EmptyEnv)

n2d <- function(name, color = NULL) {
    if (is.null(color) || is.na(color))
        paste("\"", name, "\";\n", sep = "")
    else
        paste("\"", name, "\"[style=filled,color=\"", color, "\"];\n",
              sep = "")
}

e2d <- function(from, to, color = NULL) {
    e <- paste("\"", from, "\" -> \"", to, "\"", sep = "")
    if (is.null(color))
        paste(e, ";\n", sep = "")
    else
        paste(e, "[color=\"", color, "\"];\n", sep = "")
}

# **** A plausible size is 10,7.5
g2d <- function(g, filename = "g.dot", landscape = TRUE,
                nodeColors = NULL, edgeColors = NULL,
                size, center = FALSE, rankdir = c("TB","LR")) {
    if (missing(rankdir))
        rankdir = "LR"
    else match.arg(rankdir)

    con <- file(filename, open = "w")
    on.exit(close(con))

    cat("digraph xyz {\n", file = con)
    if (! missing(size))
        cat(paste("size=\"", size, "\";\n", sep = ""), file = con)
    if (landscape)
        cat("rotate=90;\n", file = con)
    if (center)
        cat("center=1;\n", file = con)
    cat(paste("rankdir=", rankdir, ";\n", sep = ""), file = con)
    for (i in seq(along = g$nodes)) {
        from <- g$nodes[i]
        cat(n2d(from, nodeColors[[i]]), file = con)
        toList <- g$edges[[i]]
        toColors <- edgeColors[[i]]
        for (j in seq(along = toList))
            cat(e2d(from, toList[[j]], toColors[[j]]), file = con)
    }
    cat("}", file = con)
}

g2g <- function(g) {
    if (! require("graph"))
        stop("package graph is needed but not available")
    # **** eventually maybe do new("graph::graphNEL") here
    nodes <- g$nodes
    mke <- function(e) list(edges = match(e, nodes))
    eL <- lapply(g$edges, mke)
    names(eL) <- nodes
    new("graphNEL", nodes = nodes, edgeL = eL, edgemode = "directed")
}
doProfileLines <- function (fun, filename = "Rprof.out", chunksize = 5000) {
    rprof <- file(filename)
    open(rprof, "r")
    on.exit(close(rprof))
    head <- scan(rprof, nlines = 1, what = list("", interval = 0), 
        sep = "=", quiet = TRUE)
    interval <- head$interval
    count <- 0
    repeat {
        chunk <- readLines(rprof, n = chunksize)
        nread <- length(chunk)
        if (nread == 0) 
            break
        count <- count + nread
        stripQuotes <- function(x) substr(x, 2, nchar(x) - 1)
        for (line in lapply(strsplit(chunk, " "), stripQuotes))
            fun(line)
        if (nread < chunksize) 
            break
    }
    if (count == 0) 
        stop("no events were recorded")
    list(interval = interval, count = count)
}
# **** need more abstraction here?
makeProfCallGraphData <- function() mkHash()
getProfCallGraphNodeEntry <- function(name, env)
    get(name, env = env)

incProfCallGraphNodeEntry <- function(name, what, env) {
    if (exists(name, env = env, inherits = FALSE))
        entry <- get(name, env = env)
    else 
        entry <- list(self = 0, total = 0, edges = mkHash())
    entry[[what]] <- entry[[what]] + 1
    assign(name, entry, env = env)    
}

# **** speed up by inlining
incProfCallGraphNodeEntry <- function(name, what, env) {
    if (.Internal(exists(name, env, "any", FALSE)))
        entry <- .Internal(get(name, env, "any", FALSE))
    else 
        entry <- list(self = 0, total = 0, edges = mkHash())
    entry[[what]] <- entry[[what]] + 1
    .Internal(assign(name, entry, env, FALSE))
}

getProfCallGraphEdgeEntry <- function(from, to, env) {
    fromEntry <- getProfCallGraphNodeEntry(from, env)
    get(to, env = fromEntry$edges)
}

incProfCallGraphEdgeEntry <- function(from, to, what, env) {
    fromEntry <- getProfCallGraphNodeEntry(from, env)
    if (exists(to, env = fromEntry$edges, inherits = FALSE))
        entry <- get(to, env = fromEntry$edges)
    else entry <- list(self = 0, total = 0)
    entry[[what]] <- entry[[what]] + 1
    assign(to, entry, env = fromEntry$edges)
}

# **** speed up by inlining
incProfCallGraphEdgeEntry <- function(from, to, what, env) {
    fromEntry <- .Internal(get(from, env, "any", FALSE))
    if (.Internal(exists(to, fromEntry$edges, "any", FALSE)))
        entry <- .Internal(get(to, fromEntry$edges, "any", FALSE))
    else entry <- list(self = 0, total = 0)
    entry[[what]] <- entry[[what]] + 1
    .Internal(assign(to, entry, fromEntry$edges, FALSE))
}
# **** For long Rprof output files this and addCycleInfo are the bottle necks.
# **** lineEdges is slow, as are the increment functions.
# **** need to pre-compute stuff like node list, edge lists, etc
rawProfCallGraph <- function(filename = "Rprof.out", chunksize = 5000) {
    data <- makeProfCallGraphData()
    fun <- function(line) {
        incProfCallGraphNodeEntry(line[1], "self", data)
        for (n in unique(line))
            incProfCallGraphNodeEntry(n, "total", data)
        if (length(line) > 1) {
            incProfCallGraphEdgeEntry(line[2], line[1], "self", data)
            le <- lineEdges(line)
            for (i in seq(along = le$nodes)) {
                from <- le$nodes[i]
                for (to in le$edges[[i]])
                    incProfCallGraphEdgeEntry(from, to, "total", data)
            }
      
        }    
    }
    val <- doProfileLines(fun, filename, chunksize)
    list(interval = val$interval, count = val$count, data = data)
}

# fast(er) versions of match , %in%, when args are guaranteed to be
# character vectors
charMatch <- function(x, table, nomatch = NA)
    .Internal(match(x, table, nomatch))

isIn <- function(x, table)
    .Internal(match(x, table, 0))

lineEdges <- function(line) {
    if (length(line) > 1) {
        from <- unique(line[-1])
        edges <- rep(list(character(0)), length(from))
        for (i in 2 : length(line)) {
            j <- charMatch(line[i], from)
            if (! isIn(line[i - 1], edges[[j]]))
                edges[[j]] <- c(edges[[j]], line[i - 1])
        }
        list(nodes = from, edges = edges)
    }
}

# **** somewhat faster by itself
# **** byte code compiler does better compiling this
lineEdges <- function(line) {
    if (length(line) > 1) {
        rest <- line[-1]
        from <- .Internal(unique(rest))
        edges <- rep(list(character(0)), length(from))
        idx <- charMatch(rest, from)
        for (i in 1 : (length(line) - 1)) {
            j <- idx[i]
            callee <- line[i]
            callerEdges <- edges[[j]]
            if (! .Internal(match(callee, callerEdges, 0)))
                edges[[j]] <- c(callerEdges, callee)
        }
        list(nodes = from, edges = edges)
    }
}
# **** fast(er) version of ls for env, all = TRUE
lsEnv <- function(env)
    .Internal(ls(env, TRUE))

profCallGraphEdges <- function(pd) {
    edges <- mkHash()
    for (from in lsEnv(pd$data)) {
        entry <- getProfCallGraphNodeEntry(from, pd$data)
        assign(from, lsEnv(entry$edges), env = edges)
    }
    edges
}


# **** find cycles
# **** find totals for cycle nodes
# **** find self values for edges into cycles
# **** can also find total, self values for all edges with cycle from/to

# **** given node, find edges to it (reverse graph)
# **** find all nodes (with and without out edges; union of forward, reverse)
# **** print out node info

# **** make graph
# **** use note, edge total (or self) values as scores

makeCycleMap <- function(cycles) {
    cycleMap <- mkHash()
    for (i in seq(along = cycles))
        for (n in cycles[[i]])
            assign(n, paste("<cycle ", i, ">", sep = ""), env = cycleMap)
    cycleMap
}

## **** should use Rem's algorithm from Dijkstra here??
## **** should be able to partial evaluate on exists(..., inherits = FALSE)
## **** scan code for other cases

# cheaper to just assign--not worry about whether assigned already
# may not remain true if we improve things
findReachable2 <- function(names, r1) {
    val <- mkHash()
    for (n in names) {
        toenv <- mkHash()
        for (to in lsEnv(get(n, env = r1)))
            assign(to, TRUE, env = toenv)
        assign(n, toenv, env = val)
    }
    for (n in names) {
        toenv <- get(n, env = r1)
        toenv2 <- get(n, env = val)
        for (to in lsEnv(toenv))
            for (to2 in lsEnv(get(to, env = r1)))
                assign(to2, TRUE, env = toenv2)
    }
    val
}

# **** simplified loop
findReachable2x <- function(names, r1) {
    val <- mkHash()
    for (n in names) {
        toenv <- get(n, env = r1)
        toenv2 <- mkHash()
        for (to in lsEnv(toenv))
            for (to2 in lsEnv(get(to, env = r1)))
                assign(to2, TRUE, env = toenv2)
        assign(n, toenv2, env = val)
    }
    val
}

# **** in-place update (not same but should end up with same result in end?)
findReachable2xx <- function(names, rg) {
    for (n in names) {
        toenv <- get(n, env = rg)
        for (to in lsEnv(toenv))
            for (to2 in lsEnv(get(to, env = rg)))
                assign(to2, TRUE, env = toenv)
    }
    rg
}

findReachable <- function(edges) {
    reachable <- mkHash()
    names <- lsEnv(edges)
    for (n in names) {
        toenv <- mkHash()
        for (to in get(n, env = edges))
            assign(to, TRUE, env = toenv)
        assign(n, TRUE, env = toenv)
        assign(n, toenv, env = reachable)
    }
    n <- length(names)
    k <- 1
    while (k < n) {
        reachable <- findReachable2(names, reachable)
        k <- 2 * k
    }
    reachable
}

findCycles <- function(reachable) {
    names <- lsEnv(reachable)
    cycles <- NULL
    for (n in names) {
        if (exists(n, env = reachable, inherits = FALSE)) {
            v <- n
            toenv <- get(n, env = reachable)
            rm(list = n, envir = reachable)
            for (to in lsEnv(toenv)) {
                if (exists(to, env = reachable, inherits = FALSE)) {
                    backenv <- get(to, env = reachable)
                    if (exists(n, env = backenv, inherits = FALSE)) {
                        v <- c(v, to)
                        rm(list = to, envir = reachable)
                    }
                }
            }
            if (length(v) > 1)
                cycles <- c(cycles, list(v))
        }
    }
    cycles
}
# takes out self-recursive calls
compressLineRuns <- function(line) {
    if (length(line) > 1) {
        keep <- rep(TRUE, length(line))
        last <- line[1]
        for (i in 2 : length(line))
            if (line[i] == last)
                keep[i] <- FALSE
            else last <- line[i]
        line[keep]
    }
    else line
}

compressLineRuns <- function(line) {
    if (length(line) > 1) {
        keep <- rep(TRUE, length(line))
        last <- line[1]
        for (i in 2 : length(line)) {
            val <- line[i]
            if (val == last)
                keep[i] <- FALSE
            else last <- val
        }
        line[keep]
    }
    else line
}

# **** need update pre-computed stuff like node list, edge lists, etc
# **** add more pre-computed stuff (cycle names, cycle map)
addCycleInfo <- function(filename, rgp, map) {
    inCycle <- function(name) exists(name, env = map, inherits = FALSE)
    cycleName <- function(name) get(name, env = map, inherits = FALSE)
    renameCycles <- function(line)
        unlist(lapply(line, function(n) if (inCycle(n)) cycleName(n) else n))
    # **** speed up by inlining loop and calls to 'exists', 'get'
    renameCycles <- function(line) {
        len <- length(line)
        if (len > 0)
            for (i in 1 : len) {
                n <- line[i]
                if (.Internal(exists(n, map, "any", FALSE)))
                    line[i] <- .Internal(get(n, map, "any", FALSE))
            }
        line
    }
    cnames <- unique(unlist(lapply(lsEnv(map), get, map)))
    fun <- function(line) {
        line <- compressLineRuns(renameCycles(line))
        if (isIn(line[1], cnames))
            incProfCallGraphNodeEntry(line[1], "self", rgp$data)
        for (n in unique(line))
            if (isIn(n, cnames))
                incProfCallGraphNodeEntry(n, "total", rgp$data)
        if (length(line) > 1) {
            if (isIn(line[1], cnames) || isIn(line[1], cnames))
                incProfCallGraphEdgeEntry(line[2], line[1], "self", rgp$data)
            le <- lineEdges(line)
            for (i in seq(along = le$nodes)) {
                from <- le$nodes[i]
                for (to in le$edges[[i]])
                    if (isIn(from, cnames) || isIn(to, cnames))
                        incProfCallGraphEdgeEntry(from, to, "total", rgp$data)
            }
        }
    }
    doProfileLines(fun, filename)
}
# **** need to do more pre-computing of things here
# **** (node list, edge lists, reverse map, ...
# **** use some sort of class to make opaque printed representation?
readProfileData <- function(filename = "Rprof.out") {
    rpg <- rawProfCallGraph(filename)
    pge <- profCallGraphEdges(rpg)
    cycles <- findCycles(findReachable(pge))
    cycleMap <- makeCycleMap(cycles)
    if (! is.null(cycles))
        addCycleInfo(filename, rpg, cycleMap)
    rpg$cycles <- cycles
    rpg
}

revProfCallGraphMap <- function(data) {
    rg <- mkHash()
    for (from in lsEnv(data)) {
        entry <- getProfCallGraphNodeEntry(from, data)
        for (to in lsEnv(entry$edges)) {
            if (exists(to, env = rg, inherits = FALSE))
                edges <- get(to, env = rg)
            else edges <- character(0)
            if (! from %in% edges)
                assign(to, c(from, edges), env = rg)
        }
    }
    rg
}
flatProfile <- function(pd, byTotal = TRUE) {
    nodes <- lsEnv(pd$data)
    if (! is.null(pd$cycles)) {
        map <- makeCycleMap(pd$cycles)
        cnames <- unique(unlist(lapply(lsEnv(map), get, map)))
        nodes <- nodes[! nodes %in% cnames]
    }

    if (byTotal) {
        total <- sapply(nodes, function(n) get(n, env = pd$data)$total)
        ord <- order(-total)
    }
    else {
        self <- sapply(nodes, function(n) get(n, env = pd$data)$self)
        ord <- order(-self)
    }

    nodes <- nodes[ord]
    self <- sapply(nodes, function(n) get(n, env = pd$data)$self)
    selftime <- self * pd$interval/1e+06
    selfpct <- 100 * self / pd$count
    total <- sapply(nodes, function(n) get(n, env = pd$data)$total)
    totaltime <- total * pd$interval/1e+06
    totalpct <- 100 * total / pd$count
    
    if (byTotal) {
        val <- cbind(round(totalpct, 2), round(totaltime, 2),
                     round(selfpct, 2), round(selftime, 2))
        colnames(val) <- c("total.pct", "total.time", "self.pct", "self.time")
    }
    else {
        cumselftime <- cumsum(selftime)
        val <- cbind(round(selfpct, 2), round(cumselftime, 2),
                     round(selftime, 2), round(totalpct, 2),
                     round(totaltime, 2))
        colnames(val) <- c("self.pct", "cum.self.time", "self.time", 
                           "total.pct", "total.time")
    }
    rownames(val) <- nodes
    val
}
makePrimaryLine<- function(node, i, pg) {
    idx <- sprintf("%-6s", paste("[", i, "]", sep = ""))
    if (pg$percent) {
        self <- pg$selfpct[i]
        child <- pg$childpct[i]
    }
    else {
        self <- pg$selftime[i]
        child <- pg$childtime[i]
    }
    stats <- sprintf("%8.2f   %8.2f   %8.2f", pg$totalpct[i], self, child)
    if (node %in% pg$cnames)
        name <- paste(substr(node, 1, nchar(node) - 1), "as a whole>")
    else if (node == "<Anonymous>")
        name <- "[Anonymous]"
    else name <- node
    if (pg$inCycle(node))
        extra <- paste(pg$cycleName(node), idx)
    else extra <- idx
    paste(idx, stats, "   ", name, extra, "\n")
}

makeCallerLine <- function(n, node, pg) {
    idx <- paste("[", match(n, pg$nodes), "]", sep = "")
    if (pg$inCycle(n) && pg$inCycle(node) &&
        pg$cycleName(n) == pg$cycleName(node))
        stats <- "                                     "
    else {
        entry <- getProfCallGraphEdgeEntry(n, node, pg$data)
        if (pg$percent) {
            self <- 100 * entry$self / pg$count
            total <- 100 * entry$total / pg$count
        }
        else {
            self <- entry$self * pg$interval/1e+06
            total <- entry$total * pg$interval/1e+06
        }
        child <- total - self
        stats <- sprintf("                  %8.2f   %8.2f",self, child)
    }
    if (pg$inCycle(n))
        extra <- paste(pg$cycleName(n), idx)
    else extra <- idx
    if (n == "<Anonymous>")
        name <- "[Anonymous]"
    else name <- n
    paste(stats, "       ", name, extra, "\n")
}

# **** most of this is the same as for callers--extract the common part.
makeCalleeLine <- function(n, node, pg) {
    idx <- paste("[", match(n, pg$nodes), "]", sep = "")
    if (pg$inCycle(n) && pg$inCycle(node) &&
        pg$cycleName(n) == pg$cycleName(node))
        stats <- "                                     "
    else {
        entry <- getProfCallGraphEdgeEntry(node, n, pg$data)
        if (pg$percent) {
            self <- 100 * entry$self / pg$count
            total <- 100 * entry$total / pg$count
        }
        else {
            self <- entry$self * pg$interval/1e+06
            total <- entry$total * pg$interval/1e+06
        }
        child <- total - self
        stats <- sprintf("                  %8.2f   %8.2f",self, child)
    }
    if (pg$inCycle(n))
        extra <- paste(pg$cycleName(n), idx)
    else extra <- idx
    if (n == "<Anonymous>")
        name <- "[Anonymous]"
    else name <- n
    paste(stats, "       ", name, extra, "\n")
}

makeCycleMemberLine <- function(n, cycle, pg) {
    i <- match(n, pg$nodes)
    idx <- paste("[", i, "]", sep = "")
    extra <- paste(cycle, idx)
    if (pg$percent) self <- pg$selfpct[i]
    else self <- pg$selftime[i]
    stats <- sprintf("                  %8.2f           ", self)
    if (n == "<Anonymous>")
        name <- "[Anonymous]"
    else name <- n
    paste(stats, "       ", name, extra, "\n")
}

# **** a lot of the stuff computed here should be pre-computad and part of pd
printProfileCallGraph <- function(pd, file = stdout(), percent = TRUE) {
    if (is.character(file)) {
        if (file == "") 
            stop("'file' must be non-empty string")
        con <- file(file, "wb")
        on.exit(close(con))
    }
    else if (inherits(file, "connection")) 
        con <- file
    else stop("bad file argument")

    map <- makeCycleMap(pd$cycles)
    if (is.null(pd$cycles))
        cnames <- character(0)
    else cnames <- unique(unlist(lapply(lsEnv(map), get, map)))
    inCycle <- function(name) exists(name, env = map, inherits = FALSE)
    cycleName <- function(name) get(name, env = map, inherits = FALSE)

    nodes <- lsEnv(pd$data)
    total <- sapply(nodes, function(n) get(n, env = pd$data)$total)
    ord <- order(-total)
    nodes <- nodes[ord]
    total <- total[ord]
    totalpct <- 100 * total / pd$count
    totaltime <- total * pd$interval/1e+06
    self <- sapply(nodes, function(n) get(n, env = pd$data)$self)
    selfpct <- 100 * self / pd$count
    selftime <- self * pd$interval/1e+06
    pge <- profCallGraphEdges(pd)
    rpge <- revProfCallGraphMap(pd$data)

    pd$cnames <- cnames
    pd$nodes <- nodes
    pd$totalpct <- totalpct
    pd$selftime <- selftime
    pd$childtime <- totaltime - selftime
    pd$selfpct <- selfpct
    pd$childpct <- totalpct - selfpct
    pd$inCycle <- inCycle
    pd$cycleName <- cycleName
    pd$percent = percent

    cat("Call graph\n\n", file = con)
    if (percent)
        cat("index    % time     % self   % children     name\n\n", file = con)
    else
        cat("index    % time       self   children       name\n\n", file = con)
    for (i in seq(along = nodes)) {
        node <- nodes[i]
        if (exists(node, env = rpge, inherits = FALSE))
            for (n in get(node, env = rpge))
                if (! n %in% cnames)
                    cat(makeCallerLine(n, node, pd), file = con)
        cat(makePrimaryLine(node, i, pd), file = con)
        if (node %in% cnames)
            for (n in lsEnv(map))
                if (cycleName(n) == node)
                    cat(makeCycleMemberLine(n, node, pd), file = con)
        if (exists(node, env = pge, inherits = FALSE))
            for (n in get(nodes[i], env = pge))
                if (! n %in% cnames)
                    cat(makeCalleeLine(n, node, pd), file = con)
        cat("-----------------------------------------------\n", file = con)
    }
}
getOmittedNodes <- function(pd, mergeCycles) {
    map <- makeCycleMap(pd$cycles)
    cnodes <- lsEnv(map)
    if (mergeCycles)
        cnodes
    else if (is.null(pd$cycles))
        character(0)
    else unique(unlist(lapply(cnodes, get, map)))
}

extractProfileNodes <- function(pd, score = c("self", "total"),
                                mergeCycles = TRUE) {
    if (missing(score))
        score <- "total"
    else match.arg(score)
    nodes <- lsEnv(pd$data)
    omitted <- getOmittedNodes(pd, mergeCycles)
    nodes <- nodes[! nodes %in% omitted]
    getScore <- function(n) get(n, env = pd$data)[[score]]
    sval <- unlist(lapply(nodes, getScore)) / pd$count
    list(nodes = nodes, scores = sval)
}

extractProfileEdges <- function(pd, score = c("self", "total"),
                                mergeCycles = TRUE) {
    if (missing(score))
        score <- "total"
    else match.arg(score)
    nodes <- lsEnv(pd$data)
    omitted <- getOmittedNodes(pd, mergeCycles)
    nodes <- nodes[! nodes %in% omitted]
    getToNodes <- function(n) {
        to <- lsEnv(get(n, env = pd$data)$edges)
        to[! to %in% omitted]
    }
    edges <- lapply(nodes, getToNodes)
    getScores <- function(n) {
        env <- get(n, env = pd$data)$edges
        to <- lsEnv(env)
        to <- to[! to %in% omitted]
        unlist(lapply(to, function(v) get(v, env = env)[[score]])) / pd$count
    }
    sval <- lapply(nodes, getScores)
    list(edges = edges, scores = sval)
}

# **** option to suppress some nodes
# **** make edgesColored = FALSE by default?
np2x <- function(pd, score = c("total", "self", "none"),
                 transfer = function(x) x, colorMap = NULL,
                 mergeCycles = FALSE, edgesColored = TRUE) {
    match.arg(score)
    if (score == "none") {
        color <- ecolor <- NULL
        nodes <- extractProfileNodes(pd, mergeCycles = mergeCycles)
        edges <- extractProfileEdges(pd, mergeCycles = mergeCycles)
        p <- list(nodes = nodes$nodes, edges = edges$edges)
    }
    else {
        nodes <- extractProfileNodes(pd, score, mergeCycles = mergeCycles)
        edges <- extractProfileEdges(pd, score, mergeCycles = mergeCycles)
        p <- list(nodes = nodes$nodes, edges = edges$edges)
        color <- lapply(transfer(nodes$scores), colorScore, colorMap)
        if (edgesColored) {
            ecolor <- vector("list", length(p$nodes))
            for (i in seq(along = ecolor)) {
                escore <- transfer(edges$scores[[i]])
                ecolor[[i]] <- lapply(escore, colorScore, colorMap)
            }
        }
        else ecolor <- NULL
    }
    p$nodeColors <- color
    p$edgeColors <- ecolor
    p
}

colorScore <- function(score, colorMap) {
    if (is.null(score) || is.na(score))
        NULL
    else if (! is.null(colorMap)) {
        nc <- length(colorMap)
        colorMap[min(nc, max(ceiling(nc * (1 - score)), 1))]
    }
    else {
        score = min(max(score, 0), 1)
        # from cgprof
        maxhue = 0.6    # from red (.0) to magenta (.6), cf rainbow
        minsat = 0.1    # low saturation
        bri = 1.0       # brightness, always 100%

        # following formulas are totally empirical
        hue <- maxhue * (1.0 - score)
        sat <- minsat + (3.0 - minsat) * score
        paste(hue, ",", sat, ",", bri, sep = "")
    }
}

profileCallGraph2Dot <- function(pd, score = c("total", "self"),
                                 transfer = function(x) x, colorMap = NULL,
                                 filename = "Rprof.dot", landscape = FALSE,
                                 mergeCycles = FALSE, edgesColored = TRUE,
                                 rankdir = "LR", center = FALSE, size) {
    if (missing(score))
        score = "none"
    else match.arg(score)
    p <- np2x(pd, score, transfer, colorMap, mergeCycles, edgesColored)
    g2d(p, filename, nodeColors = p$nodeColors, edgeColors = p$edgeColors,
        landscape = landscape, rankdir = rankdir, size = size, center = center)
}

plotProfileCallGraph <- function(pd, layout = "dot", 
                                 score = c("total", "self"),
                                 transfer = function(x) x, colorMap = NULL,
                                 mergeCycles = FALSE, edgesColored = TRUE,
                                 rankDir = "LR", ...) {
    if (! require(Rgraphviz))
        stop("package Rgraphviz is needed but not available")

    # **** eventually do an import here, or use Rgraphviz::plot
    plot <- get("plot", env = .GlobalEnv)

    if (missing(score))
        score = "none"
    else match.arg(score)
    if (score != "none" && is.null(colorMap))
        colorMap <- heat.colors(100)

    p <- np2x(pd, score, transfer, colorMap, mergeCycles, edgesColored)

    if (! is.null(p$nodeColors)) {
        p$nodeColors <- unlist(p$nodeColors)
        names(p$nodeColors) <- p$nodes
    }

    if (! is.null(p$edgeColors)) {
        for (i in seq(along = p$edgeColors))
            if (length(p$edges[[i]]) > 0)
                names(p$edgeColors[[i]]) <- paste(p$nodes[i], p$edges[[i]],
                                                  sep="~")
        p$edgeColors <- unlist(p$edgeColors)
    }

    attrs <- list(node = list(shape = "ellipse"))
    if (layout == "dot")
        attrs$graph <- list(rankdir = rankDir)
    if (score == "none")
        nodeAttrs <- NULL
    else nodeAttrs <- list(fillcolor = p$nodeColors)
    if (score == "none" || ! edgesColored)
        edgeAttrs <- NULL
    else edgeAttrs <- list(color = p$edgeColors)

    plot(g2g(p), layout, attrs = attrs,
         nodeAttrs = nodeAttrs, edgeAttrs = edgeAttrs, ...)
}
