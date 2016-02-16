"""SMAWK.py

Totally monotone matrix searching algorithms.

The offline algorithm in ConcaveMinima is from Agarwal, Klawe, Moran,
Shor, and Wilbur, Geometric applications of a matrix searching algorithm,
Algorithmica 2, pp. 195-208 (1987).

The online algorithm in OnlineConcaveMinima is from Galil and Park,
A linear time algorithm for concave one-dimensional dynamic programming,
manuscript, 1989, which simplifies earlier work on the same problem
by Wilbur (J. Algorithms 1988) and Eppstein (J. Algorithms 1990).

D. Eppstein, March 2002, significantly revised August 2005
"""

def ConcaveMinima(RowIndices,ColIndices,Matrix):
    """
    Search for the minimum value in each column of a matrix.
    The return value is a dictionary mapping ColIndices to pairs
    (value,rowindex). We break ties in favor of earlier rows.
    
    The matrix is defined implicitly as a function, passed
    as the third argument to this routine, where Matrix(i,j)
    gives the matrix value at row index i and column index j.
    The matrix must be concave, that is, satisfy the property
        Matrix(i,j) > Matrix(i',j) => Matrix(i,j') > Matrix(i',j')
    for every i<i' and j<j'; that is, in every submatrix of
    the input matrix, the positions of the column minima
    must be monotonically nondecreasing.
    
    The rows and columns of the matrix are labeled by the indices
    given in order by the first two arguments. In most applications,
    these arguments can simply be integer ranges.
    """

    # Base case of recursion
    if not ColIndices: return {}
    
    # Reduce phase: make number of rows at most equal to number of cols
    stack = []
    for r in RowIndices:
        while len(stack) >= 1 and \
                Matrix(stack[-1], ColIndices[len(stack)-1]) \
                > Matrix(r, ColIndices[len(stack)-1]):
            stack.pop()
        if len(stack) != len(ColIndices):
            stack.append(r)
    RowIndices = stack

    # Recursive call to search for every odd column
    minima = ConcaveMinima(RowIndices,
                [ColIndices[i] for i in range(1,len(ColIndices),2)],
                Matrix)

    # Go back and fill in the even rows
    r = 0
    for c in range(0,len(ColIndices),2):
        col = ColIndices[c]
        row = RowIndices[r]
        if c == len(ColIndices) - 1:
            lastrow = RowIndices[-1]
        else:
            lastrow = minima[ColIndices[c+1]][1]
        pair = (Matrix(row,col),row)
        while row != lastrow:
            r += 1
            row = RowIndices[r]
            pair = min(pair,(Matrix(row,col),row))
        minima[col] = pair

    return minima

class OnlineConcaveMinima:
    """
    Online concave minimization algorithm of Galil and Park.
    
    OnlineConcaveMinima(Matrix,initial) creates a sequence of pairs
    (self.value(j),self.index(j)), where
        self.value(0) = initial,
        self.value(j) = min { Matrix(i,j) | i < j } for j > 0,
    and where self.index(j) is the value of j that provides the minimum.
    Matrix(i,j) must be concave, in the same sense as for ConcaveMinima.
    
    We never call Matrix(i,j) until value(i) has already been computed,
    so that the Matrix function may examine previously computed values.
    Calling value(i) for an i that has not yet been computed forces
    the sequence to be continued until the desired index is reached.
    Calling iter(self) produces a sequence of (value,index) pairs.
    
    Matrix(i,j) should always return a value, rather than raising an
    exception, even for j larger than the range we expect to compute.
    If j is out of range, a suitable value to return that will not
    violate concavity is Matrix(i,j) = -i.  It will not work correctly
    to return a flag value such as None for large j, because the ties
    formed by the equalities among such flags may violate concavity.
    """
    
    def __init__(self,Matrix,initial):
        """Initialize a OnlineConcaveMinima object."""

        # State used by self.value(), self.index(), and iter(self)
        self._values = [initial]    # tentative solution values...
        self._indices = [None]      # ...and their indices
        self._finished = 0          # index of last non-tentative value

        # State used by the internal algorithm
        #
        # We allow self._values to be nonempty for indices > finished,
        # keeping invariant that
        # (1) self._values[i] = Matrix(self._indices[i], i),
        # (2) if the eventual correct value of self.index(i) < base,
        #     then self._values[i] is nonempty and correct.
        #
        # In addition, we keep a column index self._tentative, such that
        # (3) if i <= tentative, and the eventual correct value of
        #     self.index(i) <= finished, then self._values[i] is correct.
        #
        self._matrix = Matrix
        self._base = 0
        self._tentative = 0

    def __str__(self):
        return "%s" % self._values

    def __iter__(self):
        """Loop through (value,index) pairs."""
        i = 0
        while True:
            yield self.value(i),self.index(i)
            i += 1

    def value(self,j):
        """Return min { Matrix(i,j) | i < j }."""
        while self._finished < j:
            self._advance()
        return self._values[j]

    def index(self,j):
        """Return argmin { Matrix(i,j) | i < j }."""
        while self._finished < j:
            self._advance()
        return self._indices[j]

    def _advance(self):
        """Finish another value,index pair."""
        # First case: we have already advanced past the previous tentative
        # value.  We make a new tentative value by applying ConcaveMinima
        # to the largest square submatrix that fits under the base.
        i = self._finished + 1
        if i > self._tentative:
            rows = range(self._base,self._finished+1)
            self._tentative = self._finished+len(rows)
            cols = range(self._finished+1,self._tentative+1)
            minima = ConcaveMinima(rows,cols,self._matrix)
            for col in cols:
                if col >= len(self._values):
                    self._values.append(minima[col][0])
                    self._indices.append(minima[col][1])
                elif minima[col][0] < self._values[col]:
                    self._values[col],self._indices[col] = minima[col]
            self._finished = i
            return
        
        # Second case: the new column minimum is on the diagonal.
        # All subsequent ones will be at least as low,
        # so we can clear out all our work from higher rows.
        # As in the fourth case, the loss of tentative is
        # amortized against the increase in base.
        diag = self._matrix(i-1,i)
        if diag < self._values[i]:
            self._values[i] = diag
            self._indices[i] = self._base = i-1
            self._tentative = self._finished = i
            return
        
        # Third case: row i-1 does not supply a column minimum in
        # any column up to tentative. We simply advance finished
        # while maintaining the invariant.
        if self._matrix(i-1,self._tentative) >= self._values[self._tentative]:
            self._finished = i
            return
        
        # Fourth and final case: a new column minimum at self._tentative.
        # This allows us to make progress by incorporating rows
        # prior to finished into the base.  The base invariant holds
        # because these rows cannot supply any later column minima.
        # The work done when we last advanced tentative (and undone by
        # this step) can be amortized against the increase in base.
        self._base = i-1
        self._tentative = self._finished = i
        return