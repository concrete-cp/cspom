package cspom.variable;

/**
 * A singleton class representing the boolean True value.
 * 
 * @author vion
 * 
 */
public final class True implements Domain {

    /**
     * True constant.
     */
    public static final True TRUE = new True();

    /**
     * Manual instances of True are not allowed by the Singleton design pattern.
     */
    private True() {
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException();
    }

    @Override
    public String toString() {
        return "TRUE";
    }
}
