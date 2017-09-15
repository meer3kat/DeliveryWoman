debug = F
nodeKeySep = "_"

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

# UNUSED
# given roads and position finds the lowest path to the next node
leastCostToNextNode <- function(roads, pos, prev) {
  nodes = list(left=roads$hroads[pos[1]] - 1,
    right=roads$hroads[pos[1]] + 1,
    up=roads$vroads[pos[2]] + 1,
    down=roads$vroads[pos[2]]-1
  )
  minPath = Inf
  pathKey = ""
  for (i in names(nodes)) {
    # some of these values may be "off road" and thus null
    if (is.element(i, names(nodes)) && nodes[i] < minPath) {
      minPath = nodes[i]
      pathKey = i
    }
  }

  switch(pathKey,
    left={ return (c(nodes[i], pos[2])) },
    right={ return (c(nodes[i], pos[2])) },
    up={ return (c(pos[1], nodes[i])) },
    down={ return (c(pos[1], nodes[i])) },
    { print("something went wrong finding leastCostToNextNode") }
  )
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

closestUndeliveredPackage <- function(roads, pos, packages) {
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

ourDeliveryMan <- function(roads, car, packages) {
  if (car$mem$prevLoad != car$load ||
    is.null(car$mem$directions) || length(car$mem$directions) == 0) {
    # no packages
    if (car$load == 0) {
      # find the closest undelivered pacakge and go to it.
      start = c(car$x, car$y)
      package = closestUndeliveredPackage(roads, start, packages)
      if (is.null(package)) {
        print("No closest package. How did we get here?")
        return (0)
      }
      end = c(package[1], package[2])
      car$mem$package = package
      paths = aStarSearch(roads, start, end)
      car$mem$directions = constructNumericalPath(paths, start, end)
    # deliver package
    } else {
      # get current pacakge
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

benchmarkTurns <- function(size=5) {
  min = Inf
  max = 0
  sum = 0
  for (i in 1:size) {
    turns = runDeliveryMan(carReady=ourDeliveryMan, doPlot=F, pause=0)
    if (turns > max) {
      max = turns
    } else if (turns < min) {
      min = turns
    }
    sum = sum + turns
  }

  print(paste("Average turns:", sum / size, "over", size, "runs.", sep=" "))
  print(paste("Min:", min, "Max:", max, sep=" "))
}

runDeliveryMan(carReady=ourDeliveryMan, doPlot=F, pause=0)
