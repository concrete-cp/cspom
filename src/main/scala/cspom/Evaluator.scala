package cspom
import javax.script.ScriptEngineManager
import java.io.InputStreamReader
import java.net.URL
import javax.script.ScriptEngine
import javax.script.ScriptEngineManager
import javax.script.ScriptException;
import org.mozilla.javascript.Context

object Evaluator {
  val cx = Context.enter
  val scope = cx.initStandardObjects

  {
    val url = getClass.getResource("predefinedFunctions.js");
    cx.evaluateReader(scope, new InputStreamReader(url.openStream), "predefinedFunctions.js", 1, null)
  }

  def evaluate(expression: String) = try {
    cx.evaluateString(scope, expression.replace("if(", "ite("), "eval", 1, null).asInstanceOf[Boolean];
  } catch {
    case e: Exception => throw new IllegalArgumentException(expression, e)
  }
}
