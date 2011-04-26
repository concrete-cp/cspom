package cspom
import javax.script.ScriptEngineManager
import java.io.InputStreamReader
import java.net.URL;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

object Evaluator {
  val ENGINE = {
    val scriptEngine = new ScriptEngineManager().getEngineByName("JavaScript");
    if (scriptEngine == null) {
      throw new IllegalStateException("Could not find JavaScript engine");
    }
    try {
      val url = getClass.getResource("predefinedFunctions.js");

      scriptEngine.eval(new InputStreamReader(url.openStream()));
    } catch {
      case e: Exception =>
        throw new IllegalStateException(e);
    }
    scriptEngine;
  }

  @throws(classOf[ScriptException])
  def evaluate(expression: String) =
    ENGINE.eval(expression.replace("if(", "ite(")).asInstanceOf[Boolean];

}
