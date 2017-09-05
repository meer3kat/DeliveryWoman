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

findCost <- function(roadSet, start, end) {
  val = 0
  while(start != end) {
    if (start < end) {
      val += roads$hroads[start]
      start += 1
    } else if (start > goal) {
      val += roads$hroads[start]
      start -= 1
    }
  }
  return val
}

manhattanCost <- function(roads, start, goal) {
  retX = findCost(roads$hroads, start$x, goal$x)
  retY = findCost(roads$vroads, start$y, goal$y)

  return retX + retY
}

# returns a list of moves to get to goal
aStarSearch <- function(roads, carmem, start, goal) {
  visited = c()
  frontier = c()

  cameFrom = carmem

  gScore = list(start=0)
  fScore = list(start=H(0))

  return 5
}

ourDeliveryMan <- function(roads, car, packages) {
  if (debug) {
    print(packages)
  }

  for (i in length(packages)) {
    package = packages[i,] # ith package
    path = aStarSearch(roads, list(x=package[0], y=package[1]), list(x=package[2], y=package[3]))
    car$nextMove = nextMove
    return (car)
  }
}