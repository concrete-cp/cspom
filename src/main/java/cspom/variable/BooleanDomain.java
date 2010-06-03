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

	/**
	 * Truth value of this instance of Boolean.
	 */
	private final boolean truthValue;

	/**
	 * Whether this instance is a constant (true or false) or a standard boolean
	 * domain.
	 */
	private final boolean constant;

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
		this.truthValue = value;
		this.constant = true;
	}

	/**
	 * Manual instances of True are not allowed by the Singleton design pattern.
	 */
	private BooleanDomain() {
		this.constant = false;
		this.truthValue = false;
	}

	/**
	 * @return The constant truth value of this Boolean instance.
	 * 
	 * @throws IllegalStateException
	 *             if this instance is not a constant.
	 */
	public boolean getBoolean() {
		if (!constant) {
			throw new IllegalStateException(
					"only legal on non-constant Boolean instances");
		}
		return truthValue;
	}

	/**
	 * @return Whether this instance of Boolean represents a constant.
	 */
	public boolean isConstant() {
		return constant;
	}

	@Override
	public String toString() {
		if (constant) {
			if (truthValue) {
				return "true";
			}
			return "false";
		}
		return "[false, true]";
	}

	@Override
	public List<Boolean> getValues() {
		if (constant) {
			return Arrays.asList(truthValue);
		}

		return Arrays.asList(false, true);
	}

	@Override
	public int getSize() {
		if (constant) {
			return 1;
		}
		return 2;
	}
}
