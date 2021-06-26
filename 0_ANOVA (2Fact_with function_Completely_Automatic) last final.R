library(agricolae)
library(readxl)
df <- read_excel(file.choose(), sheet=1)
View(df)
#====@====@====@===@===@===@===@===@===@===@===@===@===@===")

MC2f <- function(df){
  
  DIR <- choose.dir()
  setwd(DIR)
  
  #if "agricolae" is installed?
  if ("agricolae" %in% library()$results[,1]) {
    print("the necessary package (agricolae) is installed")
  } else {
    install.packages("agricolae")
  } 
  
  # load agricolae if it's not loaded yet
  if("agricolae" %in% (.packages())){
    print("agricolae is loaded")
  } else {
    require(agricolae)
  }
  #====@====@====@===@===@===@===@===@===@===@===@===@===@===
  
  alev<-length(unique(df$a));al <-matrix("", alev, 3)
  blev<-length(unique(df$b));bl <-matrix("", blev, 3)
  ablev<-alev*blev		      ;abl<-matrix("", ablev, 3)
  Rlev<-length(unique(df$r))
  #====@====@====@===@===@===@===@===@===@===@===@===@===@===
  
  df$a<-as.factor(df$a)
  df$b<-as.factor(df$b)
  df$r<-as.factor(df$r)
  #====@====@====@===@===@===@===@===@===@===@===@===@===@===
  
  n <- ncol(df)-3
  s1 <- vector(mode = "list", length = n)
  s2 <- vector(mode = "list", length = n)
  s3 <- vector(mode = "list", length = n)
  s4 <- vector(mode = "list", length = n)
  MSerror <- vector(mode = "list", length = n)
  mn <- vector(mode = "list", length = n)
  cv <- vector(mode = "list", length = n)
  x <- matrix('', n, 1)
  #====@====@====@===@===@===@===@===@===@===@===@===@===@===
  
  for (i in 1:n){
    s1[i]    <- as.list(summary(aov(as.matrix(df[i+3]) ~ a*b, data=df)))
      model  <- aov(as.matrix(df[i+3])~df$a*df$b)
    dff<-as.numeric(df.residual(model))
    MSerror[i]<-as.numeric(deviance(model))/dff
    mn[i]   <- as.numeric(sum(df[i+3])/(ablev*Rlev))
    x[i]    <- paste("x",i,sep="")
    cv[i]   <- (sqrt(as.numeric(MSerror[i]))/as.numeric(mn[i]))*100
    s2[i]   <- list(as.matrix(LSD.test(model, trt="df$a")[[5]]))
    s3[i]   <- list(as.matrix(LSD.test(model, trt="df$b")[[5]]))
    s4[i]   <- list(as.matrix(LSD.test(model, trt=c("df$a", "df$b"))[[5]]))
  }
  #====@====@====@===@===@===@===@===@===@===@===@===@===@===
  
  write.csv(s1, paste(DIR, "\\ANOVA.csv", sep=""))
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  cv <- as.matrix(cv)
  row.names(cv) <- x; colnames(cv) <- "CV (%)"
  write.csv(cv, paste(DIR, "\\CV.csv", sep=""))
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  ss <- vector(mode = "list", length = n)
  for (i in 1:n){
    ss[i] =""}
  f <- matrix("", alev, 2)
  for (i in 1:n){
      rn <- as.data.frame(rownames(as.data.frame(s2[i])))
      f <- paste("f", i, sep="")
      as.data.frame.factor(f)
      f <- as.data.frame(s2[i])
      f <- cbind(rn, f)
      colnames(f) <- c('factor.[i]', 'mean.[i]', 'letter.[i]')
      #rownames(f) <- rn
      ss[i] <- list(f)
      print(ss)
      write.csv(ss, paste(DIR, "\\A_Mean.csv", sep=""))
  }
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  ss <- vector(mode = "list", length = n)
  for (i in 1:n){
    ss[i] =""}
  f <- matrix("", blev, 2)
  for (i in 1:n){
    rn <- as.data.frame(rownames(as.data.frame(s3[i])))
    f <- paste("f", i, sep="")
    as.data.frame.factor(f)
    f <- as.data.frame(s3[i])
    f <- cbind(rn, f)
    colnames(f) <- c('factor.[i]', 'mean.[i]', 'letter.[i]')
    #rownames(f) <- rn
    ss[i] <- list(f)
    print(ss)
    write.csv(ss, paste(DIR, "\\B_Mean.csv", sep=""))
  } 
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  ss <- vector(mode = "list", length = n)
  for (i in 1:n){
    ss[i] =""}
  f <- matrix("", ablev, 2)
  for (i in 1:n){
    rn <- as.data.frame(rownames(as.data.frame(s4[i])))
    f <- paste("f", i, sep="")
    as.data.frame.factor(f)
    f <- as.data.frame(s4[i])
    f <- cbind(rn, f)
    colnames(f) <- c('factor.[i]', 'mean.[i]', 'letter.[i]')
    #rownames(f) <- rn
    ss[i] <- list(f)
    print(ss)
    write.csv(ss, paste(DIR, "\\AB_Mean.csv", sep=""))
  }
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  std.A <- aggregate(df[, 4:ncol(df)], list(df$a), sd)
  m.A <- aggregate(df[, 4:ncol(df)], list(df$a), mean)
  Factor.A <- std.A$Group.1
  sea <- std.A[, 2:ncol(std.A)]/(length(unique(df$b))*length(unique(df$r)))
  SE.A <- cbind(Factor.A, Factor.A, sea)
  M.A <- cbind(Factor.A, m.A)

  std.B <- aggregate(df[, 4:ncol(df)], list(df$b), sd)
  m.B <- aggregate(df[, 4:ncol(df)], list(df$b), mean)
  Factor.B <- std.B$Group.1
  seb <- std.B[, 2:ncol(std.B)]/(length(unique(df$a))*length(unique(df$r)))
  SE.B <- cbind(Factor.B, Factor.B, seb)
  M.B <- cbind(Factor.B, m.B)
  
  std.AB <- aggregate(df[, 4:ncol(df)], list(df$a,df$b), sd)
  m.AB <- aggregate(df[, 4:ncol(df)], list(df$a,df$b), mean)
  Factor.AB <- std.AB[, 1:2]
  colnames(Factor.AB) <- c("Factor.A", "Factor.B")
  seab <- std.AB[, 3:ncol(std.AB)]/length(unique(df$r))
  SE.AB <- cbind(Factor.AB, seab)
  
  one2 <- matrix("======", 3, NCOL(SE.A))
  SE <- rbind(as.matrix(SE.A), one2, as.matrix(SE.B), one2, as.matrix(SE.AB))
  Average <- rbind(as.matrix(M.A), one2, as.matrix(M.B), one2, as.matrix(m.AB))

  path3 <- paste(DIR, '\\Standard_Error.csv', sep="")
  path4 <- paste(DIR, '\\Average_Final.csv', sep="")
  write.csv(as.matrix(SE), path3)
  write.csv(as.matrix(Average), path4)
  print(SE)
  print(Average)
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
}

MC2f(df=df)

