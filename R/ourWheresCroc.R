'''
salinity, nitrogen, phosphates
for each turn
  import S, N, P into observables layer for some HNN
  solve forward, find most likely watering hole the croc is in
  go to hole, search.
'''

#initial state just randomly guess where my croc is
initials=c(rep(0.025,40))
print(initials)


#transition matrix
#function get transition matrix and then write individual values in it. 
getTransitionMatrix=function(point,edges){
  edges=getEdges()
  points=getPoints()
  A=matrix(0,nrow=40,ncol=40)
  
  for( k in 1:nrow(A)){
    options=getOptions(k,edges)
    transitionP=1/length(options)
    
    for (j in 1:length(options)){
      A[k,options[j]] = transitionP
    }
  }
  
  return (A)
}

#creating vector for store probability of S, N, P
prosal= c(rep(0,40))
pronit=c(rep(0,40))
propoh=c(rep(0,40))
protot=c(rep(0,40))

# here we get the probability of the reading from our croc that belongs to the waterhole j's distribution.
for (j in 1:40){
  prosal[j]=dnorm(readings[1],probs$salinity[j,1],probs$salinity[j,2])
  pronit[j]=dnorm(readings[2],probs$nitrogen[j,1],probs$nitrogen[j,2])
  propoh[j]=dnorm(readings[3],probs$phosphate[j,1],probs$phosphate[j,2])
  protot[j]=prosal[j]*pronit[j]*propoh[j]
}

#normallize our protot 
protot=normalize(protot)
print(proto)