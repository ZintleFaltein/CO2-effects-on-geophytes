data=read.table(file="clipboard",header=TRUE,sep = "\t")

data





#####Single Aci replicate fit, normalised to 25oC

f <- fitaci(data)

plot(f)



########Single Aci replicate fit, at leaf temperature needs a column in data.frame called Tleaf

f3 <- fitaci(data, Tcorrect=TRUE)

plot(f3)





##Fitting multiple replicates in one go. Dataframe includes column rep for "replicate number"

f2 <- fitacis(data, "Rep")

plot(f2, how="oneplot")



#######Gives you all the coefficients (Vcmax, Jmax etc) for each replicate.

f3<-coef(f2)

f3