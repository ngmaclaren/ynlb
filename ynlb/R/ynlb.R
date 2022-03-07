find_house <- function(x, indiv) {
    "Not currently used: for household-level analysis. Associate an individual ID with their household ID."
    if(x %in% indiv$IndivID) indiv$SharingUnitID[indiv$IndivID == x] else NA
}

make_meals_network <- function(loc) {
    "Return an igraph object representing who helps whom with preparing meals for community events. Edges point from the respondent's node to the node for the person whom the respondent reported as helping them with meal preparation."
    require(igraph)
    require(ynlb)
    if(loc == "yn") {
        data(YN_Data)
        indiv <- YN_Data$indiv
        el <- YN_Data$el
    } else if(loc == "lb") {
        data(LB_Data)
        indiv <- LB_Data$indiv
        el <- LB_Data$el
    }

    meals <- el[el$Network == "WhoHelpsPrepCommMeals", c("Ego", "Alter")]

    nodevars <- c("IndivID", "SharingUnitID", "Age", "Gender")
    nodedata <- indiv[indiv$IndivID %in% unlist(meals), nodevars]
    colnames(nodedata) <- c("UID", "HHID", "Age", "Gender")
    nodedata$Respondent <- ifelse(nodedata$UID %in% meals$Ego, TRUE, FALSE)

    g <- graph_from_data_frame(meals, vertices = nodedata)
    dropthese <- V(g)[which(
                      is.na(V(g)$HHID) |
                      is.na(V(g)$Age) |
                      is.na(V(g)$Gender)
                  )]
    g <- g - dropthese

    return(g)
}

make_equipment_network <- function(loc) {
    "Return an igraph object representing who lends farm equipment to whom. There are edges derived from *two* survey questions in this network: respondents were asked whom they leant farm equipment to *and* from whom did they borrow farm equipment. Both edges are included in these networks, and edges point towards the source of the farm equipment (i.e., towards the respondent if the response was to the question about who they leant equipment to, away from the respondent otherwise)."
    require(igraph)
    require(ynlb)
    if(loc == "yn") {
        data(YN_Data)
        indiv <- YN_Data$indiv
        el <- YN_Data$el
    } else if(loc == "lb") {
        data(LB_Data)
        indiv <- LB_Data$indiv
        el <- LB_Data$el
    }

    equipto <- el[el$Network == "FarmEquipTo", c("Ego", "Alter")]
    equipfrom <- el[el$Network == "FarmEquipFrom", c("Ego", "Alter")]

    nodevars <- c("IndivID", "SharingUnitID", "Age", "Gender")
    nodedata <- indiv[indiv$IndivID %in% unlist(rbind(equipto, equipfrom)), nodevars]
    colnames(nodedata) <- c("UID", "HHID", "Age", "Gender")
    nodedata$Respondent <- ifelse(nodedata$UID %in% c(equipto$Ego, equipfrom$Ego), TRUE, FALSE)

    colnames(equipto) <- c("Alter", "Ego") # confusing notation: the renamed Alter is actually the respondant here, but they are the target of the edge, so the order of the columns needs to be switched. Names are retained to support rbind
    equipto <- equipto[, c("Ego", "Alter")]
    equip <- rbind(equipto, equipfrom)

    ## This has to be a mistake?
    equip <- equip[!(equip$Ego == equip$Alter), ]

    g <- graph_from_data_frame(equip, vertices = nodedata)
    dropthese <- V(g)[which(
                      is.na(V(g)$HHID) |
                      is.na(V(g)$Age) |
                      is.na(V(g)$Gender)
                  )]
    g <- g - dropthese
    
    return(g)
}

make_distance_network <- function(loc, g) {
    "Return an igraph object representing the distances (in km) between all the houses in the survey at the given location. This function is written to provide distance between households connected by edges in another network, such as a community meals or equipment lending network." 
    require(igraph)
    require(ynlb)
    if(loc == "yn") {
        data(YN_Data)
        indiv <- YN_Data$indiv
        dist <- YN_Data$dist
    } else if(loc == "lb") {
        data(LB_Data)
        indiv <- LB_Data$indiv
        dist <- LB_Data$dist
    }

    inds <- V(g)$name
    houses <- V(g)$HHID
    
    D <- matrix(nrow = length(inds), ncol = length(inds), dimnames = list(inds, inds))
    for(i in 1:nrow(D)) {
        for(j in 1:nrow(D)) {
            if(is.na(houses[i]) | is.na(houses[j])) {
                d <- NA
            } else d <- dist$distance[dist$ui == houses[i] & dist$uj == houses[j]]
            D[i, j] <- d
        }
    }

    d <- graph_from_adjacency_matrix(D, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    ## I should drop all edges with weight == NA
    dropthese <- E(d)[which(is.na(E(d)$weight))]
    d <- d - dropthese
    
    UIDs <- substr(sapply(V(d)$name,
                          function(x) indiv$IndivID[which(indiv$IndivID == x)], USE.NAMES = FALSE),
                   3, 6)
    HHIDs <- sapply(V(d)$name,
                    function(x) indiv$SharingUnitID[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    Ages <-  sapply(V(d)$name,
                    function(x) indiv$Age[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    Genders <- sapply(V(d)$name,
                      function(x) indiv$Gender[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    vertex_attr(d) <- list(name = V(d)$name, UID = UIDs, HHID = HHIDs, Age = Ages, Gender = Genders)

    return(d) # disconnected if rel = 0 | we have no info
    
}

make_sanguinal_network <- function(loc, g, cutoff = 0.125, dichotomize = FALSE) {
    "Return an igraph object representing sanguinal relatedness between nodes in another network, `g`. The `dichotomize` option is not currently used."
    require(kinship2)
    require(igraph)
    require(ynlb)
    if(loc == "yn") {
        data(YN_Data)
        indiv <- YN_Data$indiv
        kin <- YN_Data$kin
    } else if(loc == "lb") {
        data(LB_Data)
        indiv <- LB_Data$indiv
        kin <- LB_Data$kin
    }
    
    P <- pedigree(id = kin$Ego, dadid = kin$Father, momid = kin$Mother, sex = kin$Sex, missid = "0")
    K <- kinship(P)
    K <- K*2
    K[which(K < cutoff)] <- 0

    inds <- V(g)$name
    
    S <- K[which(rownames(K) %in% inds), which(colnames(K) %in% inds)] # this knocks out a lot of ppl
    ## Power & Ready (2019) cut off sanguinal and affinal kin at c = 0.125
    s <- graph_from_adjacency_matrix(S, mode = "undirected", weighted = TRUE, diag = FALSE)
    ## all people we know very little about, most of whom are from somewhere else.
    missing_inds <- inds[which(!(inds %in% rownames(S)))] 
    s <- s + missing_inds
    
    UIDs <- substr(sapply(V(s)$name,
                          function(x) indiv$IndivID[which(indiv$IndivID == x)], USE.NAMES = FALSE),
                   3, 6)
    HHIDs <- sapply(V(s)$name,
                    function(x) indiv$SharingUnitID[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    Ages <-  sapply(V(s)$name,
                    function(x) indiv$Age[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    Genders <- sapply(V(s)$name,
                      function(x) indiv$Gender[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    vertex_attr(s) <- list(name = V(s)$name, UID = UIDs, HHID = HHIDs, Age = Ages, Gender = Genders)

    return(s) # disconnected if rel = 0 | we have no info
}

make_ep_affinal_network <- function(loc, g, cutoff = 0.125) {
    "Return an igraph object representing affinal relatedness between nodes in another network, `g`. This function is based on code by Eleanor Power (https://github.com/eapower/affinal_relatedness)."
    require(kinship2)
    require(igraph)
    require(ynlb)
    if(loc == "yn") {
        data(YN_Data)
        indiv <- YN_Data$indiv
        kin <- YN_Data$kin
    } else if(loc == "lb") {
        data(LB_Data)
        indiv <- LB_Data$indiv
        kin <- LB_Data$kin
    }

    P <- pedigree(id = kin$Ego, dadid = kin$Father, momid = kin$Mother, sex = kin$Sex, missid = "0")
    K <- kinship(P)
    K <- K*2
    ##K[which(K < 0.125)] <- 0

    inds <- V(g)$name

    ## This matrix should have 1 if i and j are spouses, parents/children, or siblings
    M <- K#matrix(0, nrow = nrow(K), ncol = ncol(K), dimnames = list(rownames(K), colnames(K)))
    M[which(M != 0.5)] <- 0
    M[which(M == 0.5)] <- 1
    for(i in 1:nrow(indiv)) {
        if(is.na(indiv$Spouse[i])) {
            next
        } else {
            spouse <- paste0(toupper(loc), indiv$Spouse[i])
            if(!(spouse %in% colnames(M))) next
            M[indiv$IndivID[i], spouse] <- 1
        }
    }

    m <- graph_from_adjacency_matrix(M, mode = "undirected")
    md <- distances(m)
    md <- 2^-md

    A <- md - K

    A <- A[which(rownames(M) %in% inds), which(colnames(M) %in% inds)]
    A[which(A < cutoff)] <- 0

    a <- graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    ## Drop all edges with weight == NA
    dropthese <- E(a)[which(is.na(E(a)$weight))]
    a <- a - dropthese
    
    UIDs <- substr(sapply(V(a)$name,
                          function(x) indiv$IndivID[which(indiv$IndivID == x)], USE.NAMES = FALSE),
                   3, 6)
    HHIDs <- sapply(V(a)$name,
                    function(x) indiv$SharingUnitID[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    Ages <-  sapply(V(a)$name,
                    function(x) indiv$Age[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    Genders <- sapply(V(a)$name,
                      function(x) indiv$Gender[which(indiv$IndivID == x)], USE.NAMES = FALSE)
    vertex_attr(a) <- list(name = V(a)$name, UID = UIDs, HHID = HHIDs, Age = Ages, Gender = Genders)

    return(a) # disconnected if rel = 0 | we have no info
    
}

make_friendship_network <- function(loc) {
    "Return an igraph object representing who is friends with whom. This is an undirected network for reasons explained in Mattison et al. (2021)."
    require(igraph)
    require(ynlb)
    if(loc == "yn") {
        data(YN_Data)
        indiv <- YN_Data$indiv
        el <- YN_Data$el
        egokey <- YN_Data$egokey
    } else if(loc == "lb") {
        data(LB_Data)
        indiv <- LB_Data$indiv
        el <- LB_Data$el
        egokey <- LB_Data$egokey
    }
    
    ties <- c("WomenHangOutAfterDinner", "MenHangOutAfterDinner")
    el <- el[el$Network %in% ties, ]

    egos <- c(egokey$Ego1, na.omit(egokey$Ego2))

    for(i in 1:nrow(el)) {# This loop handles ambiguity about who the ego is
        q <- el[i, "Network"]
        u <- el[i, "Ego"]

        if(!(u %in% egokey$Ego1)) next

        alter <- el[i, "Alter"]
        keyrow <- subset(egokey, Ego1 == u)
        v <- keyrow$Ego2
        stopifnot(!is.na(v) | is.na(v))

        if(is.na(v)) next
        
        if(alter == v | alter == u) next

        if(q == "WomenHangOutAfterDinner") {
            if(keyrow$Ego1M == 1) el[i, "Ego"] <- v
        } else if(q == "MenHangOutAfterDinner") {
            if(keyrow$Ego1M == 0) el[i, "Ego"] <- v
        } else rowvec[[2]] <- NA
    }

    friendship <- el[el$Ego %in% egos, c("Ego", "Alter")]

    nodevars <- c("IndivID", "SharingUnitID", "Age", "Gender", "HHIncomeTotalLastYear", "NetWorth")
    nodedata <- indiv[indiv$IndivID %in% unlist(friendship), nodevars]
    colnames(nodedata) <- c("UID", "HHID", "Age", "Gender", "HHIncome", "HHWorth")
    nodedata$Respondent <- ifelse(nodedata$UID %in% egokey$Ego1, TRUE, FALSE)

    g <- graph_from_data_frame(friendship, vertices = nodedata, directed = FALSE)
    dropthese <- V(g)[which(
                      is.na(V(g)$HHID) |
                      is.na(V(g)$Age) |
                      is.na(V(g)$Gender)
                  )]
    g <- g - dropthese

    return(g)
}

