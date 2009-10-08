package cspom.variable;

import java.util.List;

public interface Domain {
	String getName();

	int getNbValues();

	List<Number> getValues();
}
