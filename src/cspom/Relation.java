/*
 * Created on 8 janv. 08
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspom;

import javax.script.ScriptException;

public interface Relation {
	String getName();
	boolean evaluate(Number[] values) throws ScriptException;
}
