probs=c("Accuracy","Detection Rate","False Alarm Rate", "Precision")
mths = c("Lines of Code", "Cyclomatic complexity", "Essential complexity", "Design complexity")
mat = matrix(NA, nrow = length(mths), ncol = length(probs), dimnames=list(mths,probs))
for (i in 1:length(mths)) {
  if(i==1){
    a = tabloc[1,1]
    b = tabloc[1,2]
    c = tabloc[2,1]
    d = tabloc[2,2]
  }
  else if(i==2){
    a = tabvg[1,1]
    b = tabvg[1,2]
    c = tabvg[2,1]
    d = tabvg[2,2]
  }
  else if(i==3){
    a = tabevg[1,1]
    b = tabevg[1,2]
    c = tabevg[2,1]
    d = tabevg[2,2]
  }
  else{
    a = tabivg[1,1]
    b = tabivg[1,2]
    c = tabivg[2,1]
    d = tabivg[2,2]
  }
  #Calculate a,b,c,d here
  for (j in 1:length(probs)) {
    if(j==1){
      mat[i,j] = acc(a,b,c,d)
    }
    else if(j==2){
      mat[i,j] = detect(b,d)
    }
    else if(j==3){
      mat[i,j] = falarm(a,c)
    }
    else{
      mat[i,j] = prec(c,d)
    }
  }
}
mat
