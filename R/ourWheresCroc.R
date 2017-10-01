'''
salinity, nitrogen, phosphates
for each turn
  import S, N, P into observables layer for some HNN
  solve forward, find most likely watering hole the croc is in
  go to hole, search.
'''

#initial state just randomly guess where my croc is. that is 0.025 chance in each water hole. 
initials=c(rep(0.025,40))
#print(initials)

#alternative way to start our initial croc cannot be at position[1][2][3]
getinitials=function(){
  pstart=1/37
  initialstate=matrix(pstart,nrow=1,ncol=40)
  loc1=positions[1]
  loc2=positions[2]
  loc3=positions[3]
  initialstate[1,loc1]=0
  initialstate[1,loc2]=0
  initialstate[1,loc3]=0
  
  return(initialstate)
  
}
#transition matrix
#function get transition matrix and then write individual values in it. 
#to make sure matrix opertion correct
#the transition matrix need to write column i as P(waterhole i)
getTransitionMatrix=function(point,edges){
  edges=getEdges()
  points=getPoints()
  A=matrix(0,nrow=40,ncol=40)
  
  for( k in 1:ncol(A)){
    options=getOptions(k,edges)
    transitionP=1/length(options)
    
    for (j in 1:length(options)){
      A[options[j],k] = transitionP
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




#transit our initial state using the transition matrix and multiply the probability. so we get the probability of croc is in that water whole
stateold=initials


state = stateold %*% getTransitionMatrix() * protot



#normallize our probability so that the total probability is 1. 
state=state/sum(state)
print('state')
print(state)
croclocation=which.max(state)
print(croclocation)


