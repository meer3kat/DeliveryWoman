"
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
"

debug = T

findTurnCost <- function(roadSet, start, end) {
  return (sum(roadSet[start:end]))
}

manhattanCost <- function(roads, start, goal) {
  retX = findTurnCost(roads$hroads, start[1], goal[1])
  retY = findTurnCost(roads$vroads, start[2], goal[2])
  return (retX + retY)
}

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
  return (frontier[[pathKey]])
}

getNeighborNodes <- function(pos, prev=NULL) {
  neighbors = list(left=c(pos[1] - 1, pos[2]),
             right=c(pos[1] + 1, pos[2]),
               up=c(pos[1], pos[2] + 1),
            down=c(pos[1], pos[2] - 1)
  )
  if (!is.null(prev)) {
    prev = paste(prev)
    switch(prev,
      "4"={neighbors$left  = NULL},
      "6"={neighbors$right = NULL},
      "8"={neighbors$up    = NULL},
      "2"={neighbors$down  = NULL}
    )
  }
  return (Filter(function(n) n[1] >= 0 && n[2] >= 0, neighbors))
}

nodesEqual <- function(n1, n2) {
  return (n1[1] == n2[1] && n1[2] == n2[2])
}

# list(x=1, y=3) => '1 3'
nodeKey <- function(node) {
  return (paste(node[1], node[2], sep="_"))
}

# returns a list of moves to get to goal
aStarSearch <- function(roads, start, goal, h=manhattanCost) {
  startKey = nodeKey(start)
  visited = c() # list of visited nodes as keys
  frontier = list() # key:vector
  frontier[[startKey]] = start

  path = c()

  # f(n) = g(n) + h(n)
  gScore = list() # key:int
  gScore[startKey] = 0

  fScore = list() # key:int
  fScore[startKey] = h(roads, start, goal)

  # BEGIN LOOP
  while(length(gScore) > 0) {
    current = leastCostInFrontier(frontier, fScore)
    # if (debug) print(paste("current", current, sep=" "))
    if (nodesEqual(current, goal)) {
      # append final move to path
      return (path)
    }

    # delete from frontier map
    curKey = nodeKey(current)
    frontier[curKey] = NULL

    # add to visited list
    visited = append(visited, curKey)

    neighbors = getNeighborNodes(current, tail(path, 1))
    for (i in names(neighbors)) {
      neighbor = neighbors[[i]]
      neighborKey = nodeKey(neighbor)
      # Ignore the neighbor which is already evaluated.
      if (is.element(neighborKey, visited)) {
        print(paste("node already visited ", neighborKey, cat(visited, sep=","), cat(path, sep="▶️")))
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
      print(paste("going", i))
      switch(i,
         left={path = append(path, 4)},
        right={path = append(path, 6)},
           up={path = append(path, 8)},
         down={path = append(path, 2)},
        { path = append(path, 5)}
      )
      gScore[neighborKey] = tmp_gScore
      # print(paste(tmp_gScore, h(roads, neighbor, goal), sep="__"))
      fScore[neighborKey] = tmp_gScore + h(roads, neighbor, goal)
    }
  }

  return (5)
}

ourDeliveryMan <- function(roads, car, packages) {
  if (debug) {
    print(roads)
    print(packages)
  }

  # for (i in length(packages)) {
  package = packages[1,] # ith package
  path = aStarSearch(roads,
    c(package[1], package[2]),
    c(package[3], package[4]))
  #   car$nextMove = nextMove
  #   return (car)
  # }
  print(path)
}

runDeliveryMan(carReady=ourDeliveryMan, doPlot=F)