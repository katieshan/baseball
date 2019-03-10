

calcpitch <- function(df, GS, IP, ER, PBB, K, SV, HLD){
  #pitcher points: games started, innings pitched, ERs, BBs, Ks, SV, hold
  pitchpoints_vector <- as.matrix(c(-6, 3.5, -2, -1, 1, 5, 5))
  temp <- as.matrix(cbind(df[[GS]], df[[IP]], df[[ER]], df[[PBB]], df[[K]], df[[SV]], df[[HLD]]))
  points <- temp %*% pitchpoints_vector
  return(points)
}

calcbat <- function(df, R, S, D, Tr, HR, RBI, BBB, NSB){
  #batter points: runs, singles, doubles, triples, HRs, RBIs, BBs, net stolen bases
  batpoints_vector <- c(2, 1, 2, 3 ,4, 0.5, 1, 1)
  temp <- as.matrix(cbind(df[[R]], df[[S]], df[[D]], df[[Tr]], df[[HR]], df[[RBI]], df[[BBB]], df[[NSB]]))
  points <- as.data.frame(temp %*% batpoints_vector)
  return(points)
}
