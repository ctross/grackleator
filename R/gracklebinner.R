gracklebinner = function (tracks, nbin = c(15, 15), ab_override = NULL) 
{
    Trips <- length(tracks)

    GrackBins <- matrix(NA, nrow = Trips, ncol = nbin[1] * nbin[2])

    bins <- bin2(as.matrix(do.call(rbind,tracks)), nbin = nbin)

    if (length(dim(ab_override)) == 0) {
        for (i in 1:Trips) {
            GrackBins[i, ] <- c(bin2(cbind(tracks[[i]]$X, tracks[[i]]$Y), 
                nbin = nbin, ab = bins$ab)$nc)
        }
    } else {
        for (i in 1:Trips) {
            GrackBins[i, ] <- c(bin2(cbind(tracks[[i]]$X, tracks[[i]]$Y), 
                nbin = nbin, ab = ab_override)$nc)
        }
    }
    return(GrackBins)
}
