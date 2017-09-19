debug = F
nodeKeySep = "_"

# ------ Heuristic functions -----

findTurnCost <- function(roadSet, start, end) {
  if (start == end) {
    return (0)
  }
  start = min(start, length(roadSet))
  end = min(start, length(roadSet))
  return (sum(roadSet[start:end]))
}

manhattanCost <- function(roads, start, goal) {
  # check turn cost of both paths of manhattan distance.
  over_up = findTurnCost(roads$hroads[start[1],], start[1], goal[1]) +
            findTurnCost(roads$vroads[,goal[2]], start[2], goal[2])
  up_over = findTurnCost(roads$vroads[,start[2]], start[2], goal[2]) +
            findTurnCost(roads$hroads[goal[1],], start[1], goal[1])
  # print(paste("up_over:", up_over, "___ over_up:", over_up, sep=" "))
  return (min(over_up, up_over))
}

averageRoadCondition <- function(roads) {
  return (mean(roads$hroads) + mean(roads$vroads))
}

leastCostInFrontier <- function(frontier, fScore) {
  minPath = Inf
  pathKey = ""
  for (i in names(frontier)) {
    if (is.element(i, names(fScore)) && fScore[[i]] < minPath) {
      minPath = fScore[i]
      pathKey = i
    }
  }
  # print(pathKey)
  # print(names(frontier))
  return (frontier[[pathKey]])
}

# ------ A* functions -----

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

nodesEqual <- function(n1, n2) {
  return (n1[1] == n2[1] && n1[2] == n2[2])
}

# list(x=1, y=3) => '1 3'
nodeKey <- function(node) {
  return (paste(node[1], node[2], sep=nodeKeySep))
}

# returns a list of moves to get to goal
aStarSearch <- function(roads, start, goal, h=manhattanCost) {
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
  while(length(gScore) > 0) {
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

      # The distance from start to a neighbor
      dist = findTurnCost(roads$hroads, current[1], neighbor[1]) +
        findTurnCost(roads$vroads, current[2], neighbor[2])
      tmp_gScore = gScore[[curKey]] + dist

      # This is not a better path.
      if (is.integer(gScore[[neighborKey]]) &&
        tmp_gScore >= gScore[[neighborKey]]) {
        next
      }

      # This path is the best until now. Record it!
      cameFrom[neighborKey] = curKey
      gScore[neighborKey] = tmp_gScore
      fScore[neighborKey] = tmp_gScore + h(roads, neighbor, goal)
    }
  }

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
  } else if ( toV[1] < fromV[1]) {
    return (6)
  } else if (toV[2] > fromV[2]) {
    return (2)
  } else if (toV[2] < fromV[2]) {
    return (8)
  }
}

# given paths: list("1_1"="1_0", "1_0"="0_0")
# start: "0_0"
# end  : "1_1"
# -> c(6, 8)
constructNumericalPath <- function(paths, start, end) {
  cur = nodeKey(end)
  startKey = nodeKey(start)

  numPath = c()
  while (cur != startKey) {
    numPath = append(numPath, determineDirection(cur, paths[[cur]]))
    cur = paths[[cur]]
  }

  return (rev(numPath))
}

# ------ Package strategies -----
# for abstraction, functions must take args: (roads, pos, packages)

# returns the closest package given a position
closestPackage <- function(roads, pos, packages) {
  packageIndex = 0
  minDistance = Inf
  for (i in which(packages[,5] == 0)) {
    package = packages[i,]
    dist = manhattanCost(roads, pos, c(package[1], package[2]))
    if (dist < minDistance) {
      minDistance = dist
      packageIndex = i
    }
  }

  if (packageIndex == 0) {
    return (NULL)
  }

  return (packages[packageIndex,])
}

# get farthest package
farthestPackage <- function(roads, pos, packages) {
  packageIndex = 0
  maxDistance = -1
  for (i in which(packages[,5] == 0)) {
    package = packages[i,]
    dist = manhattanCost(roads, pos, c(package[1], package[2]))
    if (dist > maxDistance) {
      maxDistance = dist
      packageIndex = i
    }
  }

  if (packageIndex == 0) {
    return (NULL)
  }

  return (packages[packageIndex,])
}

# get package with longest delivery path
packageWithLongestPath <- function(roads, pos, packages) {
  package = NULL
  maxDistance = -Inf
  for (i in which(packages[,5] == 0)) {
    tmpPackage = packages[i,]
    dist = manhattanCost(roads, c(tmpPackage[1], tmpPackage[2]), c(tmpPackage[3], tmpPackage[4]))
    if (dist > maxDistance) {
      maxDistance = dist
      package = packages[i,]
    }
  }
  return (package)
}

# Stub
closestPackageWithLongestPath <- function(roads, pos, packages) {
  return (NULL)
}

# Stub
medianPackage <- function(roads, pos, packages) {
  return (NULL)
}

# get random package
randomPackage <- function(roads, pos, packages) {
  packageIndex = sample(which(packages[,5] == 0))[1]
  return (packages[packageIndex,])
}

# ----- Our DeliveryMan -----
ourDeliveryMan <- function(roads, car, packages, findPackageFn=closestPackage, firstPackageFn=NULL) {
  if (car$mem$prevLoad != car$load ||
    is.null(car$mem$directions) || length(car$mem$directions) == 0) {
    # no packages
    if (car$load == 0) {
      # find the closest undelivered package and go to it.
      start = c(car$x, car$y)
      package = NULL
      # print(averageRoadCondition(roads)) => usually hits 8 by the 5th package.
      if (!is.null(firstPackageFn) && averageRoadCondition(roads) <= 3) {
        package = firstPackageFn(roads, start, packages)
      } else {
        package = findPackageFn(roads, start, packages)
      }
      # package = randomPackage(packages)
      if (is.null(package)) {
        print("No closest package How did we get here?")
        return (0)
      }
      end = c(package[1], package[2])
      car$mem$package = package
      paths = aStarSearch(roads, start, end)
      car$mem$directions = constructNumericalPath(paths, start, end)
    # deliver package
    } else {
      # get current package
      package = packages[car$load,]
      car$mem$package = package
      start = c(car$x, car$y)
      # find path to delivery destination
      end = c(package[3], package[4])
      paths = aStarSearch(roads, start, end)
      car$mem$directions = constructNumericalPath(paths, start, end)
    }
  }

  car$nextMove = head(car$mem$directions, 1)
  car$mem$directions = tail(car$mem$directions, -1)
  car$mem$prevLoad = car$load

  if (is.null(car$nextMove)) {
    car$nextMove = 5
  }

  if (debug) {
    print(paste("x,y:", car$x, car$y,
      "nextMove:", car$nextMove,
      "load", car$load,
      "destination", car$mem$package[3], car$mem$package[4], sep=" "))
    print(car$mem$directions)
    # print(roads)
    # print(packages)
  }
  return (car)
}

# Takes a size and functions for ourDeliveryMan, returns the average turns.
# optionally this can print results and use MacOS' `say` command
benchmarkTurns <- function(size=5, findFn=closestPackage, firstFn=NULL, say=F, print=T) {
  min = Inf
  max = 0
  sum = 0
  for (i in 1:size) {
    # print(paste("-> ", i))
    tmp_oDM = ourDeliveryMan
    formals(tmp_oDM)$findPackageFn = findFn
    if (!is.null(firstFn)) {
      formals(tmp_oDM)$firstPackageFn = firstFn
    }
    turns = runDeliveryMan(carReady=tmp_oDM, doPlot=F, pause=0)
    if (turns > max) {
      max = turns
    } else if (turns < min) {
      min = turns
    }
    sum = sum + turns
  }

  result = (sum / size)
  if (print) {
    print(paste("Average turns:", result, "over", size, "runs.", sep=" "))
    print(paste("Min:", min, "Max:", max, sep=" "))
  }

  # for long benchmarks, a Mac can literally say when it's done!
  if (say && Sys.info()['sysname'] == "Darwin") {
    system(paste("say 'Benchmark of ", size, "runs complete, with an average of", result, "turns.'", sep=" "))
  }

  return (result)
}

# with a list of packageFunctions, iterate over every combination of them in ourDeliveryMan
benchmarkFunctions <- function(size=100) {
  pFns = list(closestPackage=closestPackage,
    farthestPackage=farthestPackage,
    packageWithLongestPath=packageWithLongestPath)
    #randomPackage=randomPackage) #not worth testing, doesn't always finish.
  results = matrix(NA, nrow=length(pFns), ncol=length(pFns))
  iter = 1
  for (findFn in names(pFns)) {
    #print(paste("findFn:", findFn, "__________"))
    currentResults = c()
    for(firstFn in names(pFns)) {
      result = benchmarkTurns(size=size, findFn=pFns[[findFn]], firstFn=pFns[[firstFn]], say=F, print=F)
      currentResults = append(currentResults, result)
      #print(paste("     ", firstFn, ":", result))
    }
    results[iter,] = currentResults
    iter = iter + 1
  }

  for (i in 1:length(pFns)) {
    print(paste("findFn:", names(pFns)[i], "__________"))
    row = results[i,]
    for (j in 1:length(row)) {
      print(paste("     ", names(pFns)[j], ":", row[j]))
    }
  }
}

runDeliveryMan(carReady=ourDeliveryMan, doPlot=F, pause=0)
