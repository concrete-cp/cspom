package cspom.variable;

import java.util.Arrays;
import java.util.List;

/**
 * A singleton class representing a boolean domain or True/False constants.
 * 
 * @author vion
 * 
 */
public final class BooleanDomain implements CSPOMDomain<Boolean> {

	/**
	 * True constant.
	 */
	public static final BooleanDomain TRUE = new BooleanDomain(true);

	/**
	 * False constant.
	 */
	public static final BooleanDomain FALSE = new BooleanDomain(false);

	/**
	 * Static domain.
	 */
	public static final BooleanDomain DOMAIN = new BooleanDomain();

	private static enum BooleanValue {
		UNKNOWN, TRUE, FALSE
	};

	/**
	 * Truth value of this instance of Boolean.
	 */
	private final BooleanValue value;

	/**
	 * @param constant
	 *            a truth value
	 * @return a constant Boolean instance according to the given truth value.
	 */
	public static BooleanDomain valueOf(final boolean constant) {
		if (constant) {
			return TRUE;
		}
		return FALSE;
	}

	/**
	 * Manual instances of True are not allowed by the Singleton design pattern.
	 * 
	 * @param value
	 *            Truth value of this instance.
	 */
	private BooleanDomain(final boolean value) {
		if (value) {
			this.value = BooleanValue.TRUE;
		} else {
			this.value = BooleanValue.FALSE;
		}
	}

	/**
	 * Manual instances of True are not allowed by the Singleton design pattern.
	 */
	private BooleanDomain() {
		this.value = BooleanValue.UNKNOWN;
	}

	/**
	 * @return The constant truth value of this Boolean instance.
	 * 
	 * @throws IllegalStateException
	 *             if this instance is not a constant.
	 */
	public boolean getBoolean() {
		switch (value) {
		case TRUE:
			return true;
		case FALSE:
			return false;
		default:
			throw new IllegalStateException(
					"only legal on non-constant Boolean instances");
		}
	}

	/**
	 * @return Whether this instance of Boolean represents a constant.
	 */
	public boolean isConstant() {
		switch (value) {
		case TRUE:
		case FALSE:
			return true;
		default:
			return false;
		}
	}

	@Override
	public String toString() {
		switch (value) {
		case TRUE:
			return "true";
		case FALSE:
			return "false";
		default:
			return "[false, true]";
		}
	}

	@Override
	public List<Boolean> getValues() {
		switch (value) {
		case TRUE:
			return Arrays.asList(true);
		case FALSE:
			return Arrays.asList(false);
		case UNKNOWN:
			return Arrays.asList(false, true);
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public int getSize() {
		switch (value) {
		case TRUE:
		case FALSE:
			return 1;
		case UNKNOWN:
			return 2;
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public CSPOMDomain<Boolean> merge(final CSPOMDomain<?> merged) {
		switch (value) {
		case TRUE:
			if (!FALSE.equals(merged)) {
				return TRUE;
			}
			throw new IllegalArgumentException("Inconsistent merge");
		case FALSE:
			if (!TRUE.equals(merged)) {
				return FALSE;
			}
			throw new IllegalArgumentException("Inconsistent merge");
		case UNKNOWN:
			return DOMAIN;
		default:
			throw new IllegalArgumentException();
		}
	}
}
