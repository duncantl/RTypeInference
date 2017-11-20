# Description:
#   Definitions for the knn experiment.

# TODO:
#   * Map math functions to calls in libm (we already do for `pow`).
#   * Automatically get vector lengths.

# FIXME: Compiler should translate abs() -> fabs().
fabs = abs

minkowski = function(x, y, len, p)
{
  # FIXME: Unless we assign a non-integer value first, the compiler will mark
  # `result` as an integer.
  #result = 1.1
  #result = 0

  for (i in 1:len) {
    sum = fabs(x[i] - y[i])^p
    result = result +  sum
  }

  return(result^(1/p))
}

distance2 = function(x, y, nrow, ncol_x, ncol_y, p)
  # Compute distances from columns of x to columns of y.
  #
  # This version is non-idiomatic to suit the compiler's quirks.
{
  .typeInfo(x = ArrayType(RealType(), NA), 
    y = ArrayType(RealType(), NA),
    nrow = IntegerType(), ncol_x = IntegerType(), ncol_y = IntegerType(),
    p = RealType())

  # Allocate a vector. Use column-major order.
  distances = numeric(ncol_x * ncol_y)
  x_vec = numeric(nrow)
  y_vec = numeric(nrow)

  for (i in 1:ncol_x) {
    for (j in 1:ncol_y) {

      for (k in 1:nrow) {
        id1 = (i - 1L) * nrow + k
        x_vec[k] = x[id1]

        id2 = (j - 1L) * nrow + k
        y_vec[k] = y[id2]
      }

      ids = (j - 1L) * ncol_x + i
      distances[ids] = minkowski(REAL(x_vec), REAL(y_vec), nrow, p)
    }
  }

  return(distances)
}

distance = function(x, y, nrow, ncol_x, ncol_y, p)
    # Compute distances from columns of x to columns of y.
{
  # TODO: detect call to matrix in compiler and allocate memory for an
  # appropriately sized array.
  distances = matrix(0, ncol_x, ncol_y)

  for (i in 1:ncol_x) {
    for (j in 1:ncol_y) {
      distances[i, j] = minkowski(x[, i], y[, j], nrow, p)
    }
  }

  return(distances)
}

order2 = function(x, len)
  # Compute an index into x which specifies the elements in ascending order.
  #
  # This is a feature-limited clone of R's `order()` function.
{
  idx = integer(len)
  idx[1] = 1L

  for (insert_idx in 2L:len) { # each number
    #printf("insert_idx: %i\n", insert_idx)
    # TODO:
    #   The compiler generates a lot of unnecessary `load` instructions here.
    #   Also, calls to `sub` with constants are not consolidated.
    idx[insert_idx] = insert_idx
    insert_val = x[insert_idx]

    # Working from the right edge, shift elements of idx right until the
    # index is at the correct insertion point.
    # TODO:
    #   The compiler doesn't support descending for loops, so we fake it here
    #   with a while loop.
    j = insert_idx
    while(j > 1L) {
      #printf("  j: %i\n", j)

      active_idx = idx[j - 1L]
      active_val = x[active_idx]

      if(insert_val < active_val) {
        #printf("  Shuffled!\n")
        idx[j] = active_idx
      } else {
        #printf("  Broke!\n")
        break
      }

      j = j - 1L
    }
    #printf("Exit j: %i\n", j)
    idx[j] = insert_idx
  }

  return(idx)
}

which.max2 = function(x, len)
  # Compute index of max element.
  #
  # This is a clone of R's `which.max()`.
{
  max_idx = 1L
  max = x[1L]

  for (i in 2L:len) {
    if (x[i] > max) {
      max_idx = i
      max = x[i]
    }
  }

  return(max_idx)
}

knn2 = function(distances, labels, n_class, k, n_train, n_test)
{
  predictions = integer(n_test)
  counter = integer(n_class)
  current_distances = numeric(n_train)

  for (j in 1:n_test) { # each test observation
    # FIXME:
    printf("j: %i\n", j)

    # Zero the counter.
    for (i1 in 1:n_class) {
      counter[i1] = 0L
    }

    # Copy out distances for the current test observation.
    # TODO:
    #   The compiler should automatically do this for R's correpsonding subset
    #   operation, and if possible we should pass a pointer rather than a copy.
    for (i2 in 1L:n_train) {
      id = (j - 1L) * n_train + i2
      current_distances[i2] = distances[id]

      # FIXME:
      printf("  id: %i (%f)\n", id, distances[id])
    }

    # Order the current distances.
    order_idx = order2(REAL(current_distances), n_train)
    order_idx_int = INTEGER(order_idx)

    # FIXME:
    printf("  order_idx_int: ")
    for (dbg1 in 1L:n_train)
      printf("%i ", order_idx_int[dbg1])
    printf("\n")

    # Get the class labels for the k closest training points, and use them to
    # increment the counter.
    for (i3 in 1L:k) {
      ord = order_idx_int[i3]
      label = labels[ord]
      counter[label] = counter[label] + 1L
    }
    
    # FIXME:
    printf("  counter: ")
    for(dbg2 in 1L:n_class)
      printf("%i ", counter[dbg2])
    printf("\n")

    # Get class with the most votes.
    predictions[j] = which.max2(INTEGER(counter), n_class)

  }

  return(predictions)
}


knn = function(test, train, labels, nclass, k)
    # k-nearest neighbors.
    #
    # Args:
    #   test    matrix of test points, one COLUMN per observation
    #   train   matrix of training ponits, one COLUMN per observation
    #   labels  class labels, as integers starting at 1
    #   nclass  number of classes
    #   k
{
    ncol_test = ncol(test)

    distances = distance(train, test, nrow(train), ncol(train), ncol_test, 2)

    # Allocate space for predictions and label counter.
    predictions = integer(ncol_test)
    counter = integer(nclass)

    # Columns correspond to test points, so sort distances in each column.
    for (j in 1:ncol_test) {
        # Zero the counter.
        for (i in 1:nclass) counter[i] = 0

        # TODO: A partial ordering algorithm would be more efficient here.
        order_idx = order(distances[, j])

        # Only consider the k nearest neighbors.
        for (i in 1:k) {
            label = labels[order_idx[i]]
            counter[label] = counter[label] + 1L
        }

        predictions[j] = which.max(counter)
    }

    return(predictions)
}
