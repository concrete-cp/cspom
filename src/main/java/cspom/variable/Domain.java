package cspom.variable;

import java.util.List;

public interface Domain {
    List<Number> getValues();

    String getName() ;
    
    int getNbValues();
}
