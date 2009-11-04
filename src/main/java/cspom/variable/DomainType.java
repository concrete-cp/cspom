/**
 * 
 */
package cspom.variable;

public enum DomainType {
	UNKNOWN, INTEGER, EXT_INT, INTERVAL_INT, BOOLEAN, CONSTANT;

	public boolean isInt() {
		switch (this) {
		case INTEGER:
		case EXT_INT:
		case INTERVAL_INT:
			return true;
		default:
			return false;
		}
	}
}