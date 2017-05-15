"use strict";

var mathjs = require("mathjs")

function argmax(from, to, f) {
  var state = [from, f(from)]
  var r
  for (var i = from+1; i < to; i++) {
    f_i = f(i)
    if (f_i >= state[1]) {
      result = [i, f_i]
    }
  }
  return state[0]
}

// Mutates the argument.
function swapRows(mat, i, j) {
  var ncols = mat.length
  for (var k = 0; k < ncols; k++) {
    var tmp = mat[k][i]
    mat[k][i] = mat[j][i]
    mat[j][i] = tmp
  }
}

// Perform gaussian elimination and return the row echelon form of the input
// matrix. Beware; a[j][i] is the ith row and jth column, because of how
// matrices are represented.
//
// TODO: this seems broken
exports.gaussEliminateImpl = function(m) {
  return function(n) {
    return function(a1) {
      // See https://en.wikipedia.org/wiki/Gaussian_elimination#Pseudocode
      var a = a1.map(function(col) { return col.slice() }).slice()

      var iterations = Math.min(m, n)

      for (var k = 0; k < iterations; k++) {
        var i_max = argmax(k, m, function(i) { return Math.abs(a1[k][i]) })
        if (a1[k][i_max] === 0) {
          // matrix is singular
          return null
        }
        swapRows(a1, k, i_max)
        
        for (var i = k+1; i < m; i++) {
          var f = a1[k][i] / a1[k][k]

          for (var j = k+1; j < n; j++) {
            a1[j][i] = a1[j][i] - a1[j][k] * f
          }

          a1[k][i] = 0
        }
      }

      return a1
    }
  }
}

exports.linearSolve = function(dictNat) {
  return function(mat) {
    return function (vec) {
      return mathjs.lusolve(mat, vec)
    }
  }
}
