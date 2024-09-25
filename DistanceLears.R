# X : Data matrix N x T
# D : Dissimilarity matrix or corresponding function
# tau : Threshold

TrueDistanceLeadersProcedure <- function(d, tau) {
  
  N = 43848
  
  D = matrix(data = NA, nrow = 800*N, ncol = 3)
  
  # Accelerated Leader Procedure
  Ndistances = 0
  Leaders <- 1
  NLeaders = 1
  Followers = vector(mode = "numeric", length = N)
  Followers[1] = 1
  for (i in 2:N) {
    message(i)
    Set = Leaders
    MIN = Inf
    XNOTfollows = TRUE
    while (XNOTfollows & (length(Set) > 0)) {
      Li = Set[1]
      Set = Set[-1]
      Ndistances = Ndistances + 1
      
      index1 = i
      index2 = Li
      index2 = index2 - (index2 > index1)
      location_in_dist_m = find_index(index1, 43848)[index2]
      
      D[Ndistances,] = c(i, Li,d[location_in_dist_m])
      if (D[Ndistances,3] <= tau) {
        XNOTfollows = FALSE
        Followers[i] = Li
        break
      }
      else if (D[Ndistances,3] < MIN) {
        MIN = D[Ndistances,3]
        Dwhere = intersect(which((is.element(D[1:Ndistances,1],Set))),which(D[1:Ndistances,2]==Li))
        Dlower = abs(D[Dwhere,3] - D[Ndistances,3])
        DeleteLeaders = which(Dlower > tau)
        if (length(DeleteLeaders) > 0) {
          Set = Set[-DeleteLeaders]
        }
      }
    }
    if (XNOTfollows) {
      Dwhere = which(D[1:Ndistances,1]==i)
      Lwhere = setdiff(Leaders, D[Dwhere,2])
      if (length(Lwhere) > 0) {
        for (j in 1:length(Lwhere)) {
          Ndistances = Ndistances + 1
          
          index1 = i
          index2 = Lwhere[j]
          index2 = index2 - (index2 > index1)
          location_in_dist_m = find_index(index1, 43848)[index2]
          
          D[Ndistances, ] = c(i, Lwhere[j], d[location_in_dist_m])
        }
      }
      Leaders = c(Leaders, i)
      Followers[i] = i
      NLeaders = length(Leaders)
    }
  }
  
  #Dwhere = intersect(which((is.element(D[1:Ndistances,1],Leaders))),which(is.element(D[1:Ndistances,2],Leaders)))
  
  #DD = dist(X[Leaders,], method = M)
  
  #HC <- hclust(DD, method = "complete")
  
  #Results <- list(Leaders = Leaders, Followers = Followers, Ndistances = Ndistances, DD = DD, HC = HC)
  
  Results <- list(Leaders = Leaders, Followers = Followers, Ndistances = Ndistances)
  return(Results)
}