package cspom.constraint

import cspom.variable.CSPOMVariable

class FunctionalConstraint(val result: CSPOMVariable,
  function: String, parameters: String, val arguments: List[CSPOMVariable])
  extends CSPOMConstraint(function, parameters, result :: arguments) {
  require(!arguments.isEmpty, "Must have at least one argument")

  def this(result: CSPOMVariable, function: String, parameters: String,
    arguments: CSPOMVariable*) = this(result, function, parameters,
    arguments.toList)

  override def toString = {
    val stb = new StringBuilder
    stb.append(result).append(" = ").append(description);
    if (parameters != null) {
      stb.append('{').append(parameters).append('}');
    }
    arguments.addString(stb, "(", ", ", ")").toString
  }

  override def replaceVar(which: CSPOMVariable, by: CSPOMVariable) = {
    if (which == result) {
      new FunctionalConstraint(by,
        function, parameters, arguments)
    } else {
      new FunctionalConstraint(result,
        function, parameters, arguments.map { case which => by; case x => x })
    }
  }
  //
  //    @Override
  //    public void replaceVar(final CSPOMVariable merged, final CSPOMVariable var) {
  //        super.replaceVar(merged, var);
  //        if (result == merged) {
  //            result = var;
  //        }
  //        for (final ListIterator<CSPOMVariable> itr = arguments.listIterator(); itr
  //                .hasNext();) {
  //            if (itr.next() == merged) {
  //                itr.set(var);
  //            }
  //        }
  //    }
  //
  //    @Override
  //    public boolean evaluate(final Object[] tuple) {
  //        final StringBuilder stb = new StringBuilder();
  //        stb.append(tuple[0]).append(" == ").append(getDescription())
  //                .append('(');
  //        Joiner.on(", ").appendTo(stb, Iterables.skip(Arrays.asList(tuple), 1));
  //
  //        if (getParameters() != null) {
  //            stb.append(", ").append(getParameters());
  //        }
  //
  //        try {
  //            return Evaluator.evaluate(stb.append(")").toString());
  //        } catch (ScriptException e) {
  //            LOGGER.throwing(FunctionalConstraint.class.getName(), "evaluate", e);
  //            throw new IllegalStateException(e);
  //        }
  //
  //    }
  //
  //    public static Predicate<CSPOMConstraint> matchesDescription(
  //            final String description) {
  //        return Predicates.and(
  //                AbstractConstraint.matchesDescription(description),
  //                Predicates.instanceOf(FunctionalConstraint.class));
  //    }
  //}

}