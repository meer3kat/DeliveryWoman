"""
Group 66
AI Lab 1 - DeliveryMan
carReady function is `ourDeliveryMan`
"""

debug = F
nodeKeySep = "_"

# ------ Heuristics -----

manhattanDistance <- function(roads, start, goal) {
  return (abs(start[1] - goal[1]) + abs(start[2] - goal[2]))
}

# ------ A* functions -----

leastCostInFrontier <- function(frontier, fScore) {
  minPath = Inf
  pathKey = ""
  for (i in names(frontier)) {
    if (is.element(i, names(fScore)) && fScore[[i]] < minPath) {
      minPath = fScore[[i]]
      pathKey = i
    }
  }
  # print(pathKey)
  # print(names(frontier))
  return (frontier[[pathKey]])
}

getNeighborNodes <- function(pos, roadSize) {
  neighbors = list(left=c(pos[1] - 1, pos[2]),
             right=c(pos[1] + 1, pos[2]),
                up=c(pos[1], pos[2] + 1),
              down=c(pos[1], pos[2] - 1)
  )
  # keep x,y > 0, and x <= roadSize, y <= roadSize
  return (Filter(function(n) { n[1] > 0 && n[2] > 0 &&
    n[1] <= roadSize && n[2] <= roadSize }, neighbors))
}

costBetweenNodes <- function(roads, start, end) {
  if (start[1] < end[1]) {
    return (roads$hroads[end[2], start[1]]) # right
  } else if (start[1] > end[1]) {
    return (roads$hroads[end[2], end[1]]) # left
  } else if (start[2] < end[2]) {
    return (roads$vroads[start[2], end[1]]) # down
  } else { # if (start[2] > end[2])
    return (roads$vroads[end[2], end[1]]) # up
  }
}

nodesEqual <- function(n1, n2) {
  return (n1[1] == n2[1] && n1[2] == n2[2])
}

# list(x=1, y=3) => '1_3'
nodeKey <- function(node) {
  return (paste(node[1], node[2], sep=nodeKeySep))
}

# returns a list of moves to get to goal
# adapted from psuedocode on:
# https://en.wikipedia.org/w/index.php?title=A*_search_algorithm&oldid=801094177#Pseudocode
aStarSearch <- function(roads, start, goal, h=manhattanDistance) {
  startKey = nodeKey(start)
  visited = c() # list of visited nodes as keys
  frontier = list() # key:vector
  frontier[[startKey]] = start

  cameFrom = list()

  # f(n) = g(n) + h(n)
  gScore = list() # key:int
  gScore[startKey] = 0

  fScore = list() # key:int
  fScore[startKey] = h(roads, start, goal)

  # BEGIN LOOP ------------------------------------------------------------
  while(length(frontier) > 0) {
    current = leastCostInFrontier(frontier, fScore)
    if (nodesEqual(current, goal)) {
      # append final move to path
      return (cameFrom)
    }

    # delete from frontier map
    curKey = nodeKey(current)
    frontier[[curKey]] = NULL

    # add to visited list
    visited = append(visited, curKey)

    neighbors = getNeighborNodes(current, max(dim(roads$hroads)))
    for (i in names(neighbors)) {
      neighbor = neighbors[[i]]
      neighborKey = nodeKey(neighbor)
      # Ignore the neighbor which is already evaluated.
      if (is.element(neighborKey, visited)) {
        next
      }

      # Discover a new node
      if (!is.element(neighborKey, names(frontier))) {
        frontier[[neighborKey]] = neighbor
      }

      # The distance from current to a neighbor
      tmp_gScore = gScore[[curKey]] + costBetweenNodes(roads, current, neighbor)

      # This is not a better path.
      if (is.element(neighborKey, names(gScore)) &&
        tmp_gScore >= gScore[[neighborKey]]) {
        next
      }

      # This path is the best until now. Record it!
      cameFrom[[neighborKey]] = curKey
      gScore[[neighborKey]] = tmp_gScore
      fScore[[neighborKey]] = tmp_gScore + h(roads, neighbor, goal)
    }
  }

  print("some failure")
  return (5)
}

# construct the path backwards
determineDirection <- function(from, to) {
  # print(paste("dir", from, to, sep=","))
  fromV = Map(as.integer, strsplit(from, nodeKeySep))[[1]]
  toV = Map(as.integer, strsplit(to, nodeKeySep))[[1]]
  # return the REVERSE direction of `from` to `to`
  if (toV[1] > fromV[1]) {
    return (4)
  } else if (toV[1] < fromV[1]) {
    return (6)
  } else if (toV[2] > fromV[2]) {
    return (2)
  } else if (toV[2] < fromV[2]) {
    return (8)
  }
}

# given paths: list("1_1"="1_0", "1_0"="0_0")
# start: "1_0"
# end  : "1_1"
# -> c(6, 8)
constructNumericalPath <- function(paths, start, end) {
  cur = nodeKey(end)
  startKey = nodeKey(start)

  numPath = c()
  while (cur != startKey) {
    numPath = append(numPath, determineDirection(cur, paths[[cur]]))
    # if (debug) print(paste(cur, " -> ", paths[[cur]]))
    cur = paths[[cur]]
  }
  return (rev(numPath))
}

# n as a as number of packages, returns all permutations of indexes.
# from https://stackoverflow.com/a/20199902
permutations <- function(n){
  if(n == 1){
    return (matrix(1))
} else {
    sub = permutations(n-1)
    p = nrow(sub)
    A = matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1) * p+1 :p,] = cbind(i, sub + (sub >= i))
    }
    return (A)
  }
}

travelingSalesmanPackageOrder <- function(roads, pos, packages) {
  bestDistance = Inf
  bestPath = c()
  perms = permutations(nrow(packages))
  for (i in 1:dim(perms)[1]) {
    distance = 0
    start = pos
    # for each index in routePermutations, calculate the total distance
    for (j in perms[i,]) {
      package = packages[j,]
      distance = distance +
        manhattanDistance(roads, start, package[1:2]) +
        manhattanDistance(roads, package[1:2], package[3:4])
      start = package[3:4]
    }
    if (distance < bestDistance) {
      bestDistance = distance
      bestPath = perms[i,]
    }
  }
  if (debug) print(paste("Best distance:", bestDistance))
  return (bestPath)
}

# ----- Our DeliveryMan -----
ourDeliveryMan <- function(roads, car, packages) {
  start = c(car$x, car$y)
  if (!is.null(car$mem$prevLoad) && car$mem$prevLoad != car$load) {
    car$mem$target = NULL
    if (length(setdiff(car$mem$order, car$load) > 0)) {
      car$mem$order = setdiff(car$mem$order, car$load)
    }
    if (debug) print(paste("current load", car$load, "________________"))
  }
  # no package
  if (car$load == 0) {
    # has a target in mem
    if (!is.null(car$mem$target)) {
      package = packages[car$mem$target,]
      end = c(package[1], package[2])
      paths = aStarSearch(roads, start, end)
      car$mem$directions = constructNumericalPath(paths, start, end)
    # no target
    } else {
      packageIndex = NULL
      # print(averageRoadCondition(roads)) => usually hits 8 by the 5th package.
      if (is.null(car$mem$order)) {
        car$mem$order = travelingSalesmanPackageOrder(roads, start, packages)
        if (debug) print(car$mem$order)
      }
      packageIndex = car$mem$order[1]
      package = packages[packageIndex,]
      if (is.null(package)) {
        print("No available package How did we get here?")
        return (0)
      # sometimes we deliver packages and don't remove them from the mem$order?
      } else if (package[5] != 0) {
        packageIndex = car$mem$order[2]
        package = packages[packageIndex,]
        car$mem$order = tail(car$mem$order, -1)
      }
      car$mem$target = packageIndex
      end = c(package[1], package[2])
      paths = aStarSearch(roads, start, end)
      car$mem$directions = constructNumericalPath(paths, start, end)
    }
  # has package
  } else {
    car$mem$target = NULL
    package = packages[car$load,]
    end = c(package[3], package[4])
    paths = aStarSearch(roads, start, end)
    car$mem$directions = constructNumericalPath(paths, start, end)
  }

  if (debug) print(car$mem$directions)
  car$nextMove = head(car$mem$directions, 1)
  car$mem$prevLoad = car$load

  if (is.null(car$nextMove)) {
    car$nextMove = 5
  }

  if (debug) {
    print(paste("x,y:", car$x, car$y,
      "nextMove:", car$nextMove,
      "target", car$mem$target,
      "load", car$load,
      "destination", packages[car$load,3], packages[car$load,4], sep=" "))
    print(car$mem$directions)
    print(car$mem$order)
    # print(roads)
  }
  return (car)
}

# Takes a size and functions for ourDeliveryMan, returns the average turns.
# optionally this can print results and use MacOS' `say` command
benchmarkTurns <- function(size=5, say=F, print=T, plot=F) {
  min = Inf
  allTurns = c()
  max = 0
  for (i in 1:size) {
    turns = runDeliveryMan(carReady=ourDeliveryMan, doPlot=F, pause=0)
    if (turns > max) {
      max = turns
    } else if (turns < min) {
      min = turns
    }
    #print(turns)
    allTurns = append(allTurns, turns)
  }

  result = mean(allTurns)
  if (print) {
    print(paste("Average turns:", result, "over", size, "runs.", sep=" "))
    print(paste("Min:", min, "Max:", max, sep=" "))
  }

  # for long benchmarks, a Mac can literally say when it's done!
  if (say && Sys.info()['sysname'] == "Darwin") {
    system(paste("say 'Benchmark of ", size, "runs complete, with an average of", result, "turns.' &", sep=" "))
  }

  if (plot) {
    hist(allTurns, main=paste("Histogram of turns over", size, "runs", sep=" "),
      xlab="Number of turns", ylab="Frequency")
  }

  return (result)
}

# runDeliveryMan(carReady=ourDeliveryMan, doPlot=F, pause=0)
