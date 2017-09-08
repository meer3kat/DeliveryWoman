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

findCost <- function(roadSet, start, end) {
  val = 0
  # print(paste(roadSet, start, end, sep="_"))
  while(start != end) {
    if (start < end) {
      val = val + roadSet[start]
      start = start + 1
    } else if (start > end) {
      val = val + roadSet[start]
      start = start - 1
    }
  }
  return (val)
}

manhattanCost <- function(roads, start, goal) {
  retX = findCost(roads$hroads, start$x, goal$x)
  retY = findCost(roads$vroads, start$y, goal$y)
  return (retX + retY)
}

# given roads and position finds the lowest path to the next node
leastCostToNextNode <- function(roads, pos, prev) {
  nodes = list(left=roads$hroads[pos$x] - 1,
    right=roads$hroads[pos$x] + 1,
    top=roads$vroads[pos$y] + 1,
    bottom=roads$vroads[pos$y]-1
  )
  minPath = Inf
  pathKey = ""
  for (i in names(nodes)) {
    # some of these values may be "off road" and thus null
    if (is.integer(nodes[i]) && nodes[i] < minPath) {
      minPath = nodes[i]
      pathKey = i
    }
  }

  switch(pathKey,
    left={ return (list(nodes[i], pos$y)) },
    right={ return (list(nodes[i], pos$y)) },
    top={ return (list(pos$x, nodes[i])) },
    bottom={ return (list(pos$x, nodes[i])) },
    { print("something went wrong finding leastCostToNextNode") }
  )
}

leastCostInFrontier <- function(frontier, fScore) {
  minPath = Inf
  pathKey = ""
  for (i in names(frontier)) {
    if (!is.null(fScore[i]) && fScore[i] < minPath) {
      minPath = frontier[i]
      pathKey = i
    }
  }
  print(paste("front, fscore", frontier, fScore, pathKey, sep="-"))
  return (fScore[pathKey])
}

getNeighborNodes <- function(pos, prev) {
  neighbors = list(left=list(x=pos$x - 1, y=pos$y),
             right=list(x=pos$x + 1, y=pos$y),
               top=list(x=pos$x, y=pos$y + 1),
            bottom=list(x=pos$x, y=pos$y - 1)
  )
  return (Filter(function(x) nodesEqual(x, prev) == F, neighbors))
}

nodesEqual <- function(n1, n2) {
  print(paste("nodesX", n1$x, n2$x, sep="-"))
  print(paste("nodesY", n1$y, n2$y, sep="-"))
  return (n1$x == n2$x && n1$y == n2$y)
}

# list(x=1, y=3) => '1 3'
nodeKey <- function(node) {
  return (paste(node$x, node$y, sep="_"))
}

# returns a list of moves to get to goal
aStarSearch <- function(roads, start, goal, h=manhattanCost) {
  startKey = nodeKey(start)
  visited = list()
  frontier = list()
  frontier[[startKey]] = start

  path = c()

  # f(n) = g(n) + h(n)
  gScore = list()
  gScore[startKey] = 0

  fScore = list()
  fScore[startKey] = h(roads, start, goal)

  # BEGIN LOOP
  while(length(gScore) > 0) {
    current = leastCostInFrontier(frontier, fScore)
    print(paste("current", current, sep=" "))
    if (nodesEqual(current, goal)) {
      # append final move to path
      return (path)
    }

    # delete from frontier map
    frontier[nodeKey(current)] = NULL

    # add to visited map
    visited[nodeKey(current)] = T

    neighbors = getNeighborNodes(pos, tail(path, 1))
    for (i in names(neighbors)) {
      neighbor = neighbors[i]
      curKey = nodeKey(neighbor)
      # Ignore the neighbor which is already evaluated.
      if (visited[curKey]) {
        next
      }

      # Discover a new node
      if (!is.string(frontier[curKey])) {
        frontier[curKey] = neighbor
      }

      # The distance from start to a neighbor
      tmp_gScore = gScore[current] + dist_between(current, neighbor)

      # This is not a better path.
      if (tmp_gScore >= gScore[neighbor]) {
        next
      }

      # This path is the best until now. Record it!
      switch(i,
         left={path[length(path) + 1] = 4},
        right={path[length(path) + 1] = 6},
           up={path[length(path) + 1] = 8},
         down={path[length(path) + 1] = 2},
        { path[length(path) + 1] = 5 }
      )
      gScore[nodeKey] = tmp_gScore
      fScore[nodeKey] = gScore[nodeKey] + h(roads, neighbor, goal)
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
    list(x=package[1], y=package[2]),
    list(x=package[3], y=package[4]))
  #   car$nextMove = nextMove
  #   return (car)
  # }
  print(path)
}

runDeliveryMan(carReady=ourDeliveryMan)