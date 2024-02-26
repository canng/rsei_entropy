rm(list=ls())
library(terra)

p = '../improved_rsei/data/'
f = list.files(p, 'tif$', full.names = T)

normalization = function(img){
  minv = min(values(img), na.rm=T)
  maxv = max(values(img), na.rm=T)
  
  nv = (img - minv) / (maxv - minv)
  return(nv)
}

###

img = rast(f[1])

for (i in 1:length(f)){
  tmp = rast(f[i])
  normalized = normalization(tmp)
  img = c(img, tmp)
}

arr = img[[2:5]]
plot(arr)

#######################################################################
# Function to calculate RSEI by entropy weight method

rsei_entropy = function(img, # stack normalized image
                        img_round=5, # round digit number for image round and weight round
                        wround=3 # round digit number for  weight round
                        ){
  # Denfine function calculate entropy
  entropyij = function(img, img_round){
    r = round(img, img_round)
    vals= values(r)
    df = na.omit(as.data.frame(table(vals)))
    
    df$'pij' = df$Freq / sum(df$Freq)
    df$'sumpij' = df$pij * log(df$pij)
    
    n = nrow(df)
    entropy = - 1/log(n) * sum(df$sumpij)
    
    return(entropy)
  }
  
  # Calculate each entropy
  entropy_1 = entropyij(arr[[1]], img_round = img_round)
  entropy_2 = entropyij(arr[[2]], img_round = img_round)
  entropy_3 = entropyij(arr[[3]], img_round = img_round)
  entropy_4 = entropyij(arr[[4]], img_round = img_round)
  
  w1 = round((1- entropy_1) / (4- sum(entropy_1 + entropy_2 + entropy_3 + entropy_4)), wround)
  w2 = round((1- entropy_2) / (4- sum(entropy_1 + entropy_2 + entropy_3 + entropy_4)), wround)
  w3 = round((1- entropy_3) / (4- sum(entropy_1 + entropy_2 + entropy_3 + entropy_4)), wround)
  w4 = round((1- entropy_4) / (4- sum(entropy_1 + entropy_2 + entropy_3 + entropy_4)), wround)
  
  entropy = list(w1, w2, w3, w4)
  
  rsei = w1*img[[1]] + w1*img[[3]] + w1*img[[3]] + w1*img[[4]] 
  
  print(paste(names(img), entropy))
  return(rsei)
}



### Demo

rsei = rsei_entropy(arr, 3, 3)
plot(rsei)









