'''
if (car$directions != NULL &&
    length(car$directions) > 0 &&
    HEURISTIC) {
  car$nextMove = car$directions[end]
  return car
} else {
  # the optimal path has exceeded heuristics, recalculate path
  aStar to the nearest undelivered package
  store path in car$directions
  return car
}
'''

debug = F

# returns a list of optimal moves to get to goal
aStarSearch <- function(roads, start, goal) {
  visited = c()
  frontier = c()

  return 5
}

ourDeliveryMan <- function(roads, car, packages) {
  if (debug) {
    print(packages)
  }

  for (i in length(packages) {
    package = packages[i,] # ith package
    path = aStarSearch(roads, list(x=package[0], y=package[1]), list(x=package[2], y=package[3]))
    car$nextMove = nextMove
    return (car)
  }
}