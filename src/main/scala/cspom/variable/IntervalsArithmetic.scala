package cspom.variable

object IntervalsArithmetic {

  def apply(f: (Interval, Interval) => Interval, ii: Intervals, jj: Intervals): Intervals = {
    var result = Intervals.empty
    for (i <- ii.intervals; j <- jj.intervals) {
      // + means union here
      result += f(i, j)
    }
    result
  }

  def apply(f: Interval => Interval, ii: Intervals): Intervals = {
    ii.intervals.map(f).foldLeft(Intervals.empty)(_ + _)
  }

}