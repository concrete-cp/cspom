package cspom.variable;

public final class Interval implements Domain {
    private final Number lb;
    private final Number ub;

    public Interval(Number lb, Number ub) {
        this.lb = lb;
        this.ub = ub;
    }

    public Number getLb() {
        return lb;
    }

    public Number getUb() {
        return ub;
    }

    public String toString() {
        return lb + ".." + ub;
    }
}
