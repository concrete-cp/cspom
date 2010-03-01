package cspom.variable;

import java.util.List;

/**
 * All classes implementing some domain type should implement this interface.
 * 
 * @param <T>
 *            Type of the domain values.
 * @author vion
 * 
 */
public interface CSPOMDomain<T> {
    /**
     * @return The list of potential values for this domain.
     */
    List<T> getValues();
}
