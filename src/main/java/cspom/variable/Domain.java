package cspom.variable;

import java.util.List;

/**
 * All classes implementing some domain type should implement this interface.
 * 
 * @author vion
 * 
 */
public interface Domain {
    /**
     * @param <T>
     *            Type of the domain values.
     * @return The list of potential values for this domain.
     */
    <T> List<T> getValues();
}
