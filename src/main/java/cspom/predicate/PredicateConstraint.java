package cspom.predicate;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import javax.script.ScriptException;

import cspom.RelationManager;
import cspom.constraint.AbstractConstraint;
import cspom.constraint.ConstantParameter;
import cspom.constraint.Parameter;
import cspom.constraint.VariableParameter;
import cspom.predicate.Predicate.Type;
import cspom.variable.Variable;

public class PredicateConstraint extends AbstractConstraint {

    private Predicate predicate;

    private List<Parameter> parameters;

    private List<Pointer> pointers;

    private List<Pointer> varPointers;

    private Map<String, Parameter> parametersNames;

    private int nbParameters;

    private final static Pattern CONSTANT_PATTERN = Pattern
            .compile(ConstantParameter.PATTERN);

    private final static Logger logger = Logger
            .getLogger(PredicateConstraint.class.getSimpleName());

    public PredicateConstraint(String name,
            List<Parameter> parameters, Predicate predicate,
            Variable... scope) {
        super(name, scope);

        this.predicate = predicate;
        this.parameters = parameters;
        this.nbParameters = parameters.size();
        this.parametersNames = new HashMap<String, Parameter>(
                nbParameters);

        this.varPointers = new ArrayList<Pointer>(getArity());

        for (int i = getArity(); --i >= 0;) {
            this.varPointers.add(new Pointer(scope[i]));
        }

        this.pointers = new ArrayList<Pointer>(nbParameters);

        for (Parameter p : parameters) {
            parametersNames.put(p.getName(), p);

            if (p instanceof ConstantParameter) {
                this.pointers.add(new Pointer(((ConstantParameter) p)
                        .getNumber()));
            } else {
                this.pointers.add(varPointers.get(seekVariable(
                        ((VariableParameter) p).getVariable()
                                .getName(), scope)));
            }
        }

    }

    public PredicateConstraint(String name, String parameters,
            Predicate predicate, Variable... scope) {
        this(name, parseParameters(parameters, predicate, scope),
                predicate, scope);

    }

    private static List<Parameter> parseParameters(
            final String allParameters, final Predicate predicate,
            final Variable[] scope) {
        final List<Parameter> parameters = new ArrayList<Parameter>();

        final String[] stringParameters = allParameters.trim().split(
                " +");

        for (int i = 0; i < stringParameters.length; i++) {
            final String parameterName = predicate.getParameters()
                    .get(i);

            if (CONSTANT_PATTERN.matcher(stringParameters[i])
                    .matches()) {
                parameters.add(new ConstantParameter(parameterName,
                        Integer.parseInt(stringParameters[i])));
            } else {
                final int variable = seekVariable(
                        stringParameters[i], scope);

                if (variable >= 0) {
                    parameters.add(new VariableParameter(
                            parameterName, scope[variable]));
                } else {
                    throw new IllegalArgumentException(
                            "Could not find variable "
                                    + stringParameters[i] + " "
                                    + scope);
                }
            }
        }
        return parameters;
    }

    private static int seekVariable(final String name,
            final Variable[] scope) {
        for (int i = scope.length; --i >= 0;) {
            if (name.equals(scope[i].getName())) {
                return i;
            }
        }
        return -1;
    }

    public String toString() {
        return getName() + ": " + predicate + " / " + parameters;
    }

    public Predicate getRelation() {
        return predicate;
    }

    public boolean evaluate(final Number[] numbers)
            throws ScriptException {

        for (int i = getArity(); --i >= 0;) {
            varPointers.get(i).set(numbers[i]);
        }

        final Number[] parameters = new Number[nbParameters];
        for (int i = nbParameters; --i >= 0;) {
            parameters[i] = pointers.get(i).get();
        }

        return predicate.evaluate(parameters);
    }

    public final List<Parameter> getParameters() {
        return parameters;
    }

    public Parameter getParameterByName(final String name) {
        return parametersNames.get(name);
    }

    private class Pointer {
        private Number content;

        private final Variable variable;

        public Pointer(Variable variable) {
            super();
            this.variable = variable;
        }

        public Pointer(Number content) {
            super();
            this.content = content;
            this.variable = null;
        }

        public void set(final Number content) {
            this.content = content;
        }

        public Number get() {
            return content;
        }

        public Variable getVariable() {
            return variable;
        }
    }

    public void semiCompile(final RelationManager predicateManager)
            throws ScriptException {
        String expression = predicate.getExpression();
        // logger.finer("Semicompiling " + predicate);
        final List<VariableParameter> newVars = new ArrayList<VariableParameter>(
                varPointers.size());
        for (int i = 0; i < varPointers.size(); i++) {
            newVars.add(new VariableParameter("mouk" + i, varPointers
                    .get(i).getVariable()));
        }

        for (int p = this.parameters.size(); --p >= 0;) {
            final Parameter parameter = this.parameters.get(p);
            if (parameter instanceof ConstantParameter) {
                expression = expression.replaceAll(parameter
                        .getName(), ((ConstantParameter) parameter)
                        .getNumber().toString());
            } else if (parameter instanceof VariableParameter) {
                final int varnum = varPointers.indexOf(pointers
                        .get(p));

                expression = expression.replaceAll(parameter
                        .getName(), newVars.get(varnum).getName());
            }
        }
        // logger.finest("Expression : " + expression);
        final Map<String, Type> types = new HashMap<String, Type>(
                newVars.size());
        final List<String> stringParameters = new ArrayList<String>(
                newVars.size());
        nbParameters = newVars.size();
        assert nbParameters == getArity();
        pointers = varPointers = new ArrayList<Pointer>(nbParameters);
        parameters = new ArrayList<Parameter>(nbParameters);
        parametersNames = new HashMap<String, Parameter>();
        for (VariableParameter var : newVars) {
            types.put(var.getName(), Type.INTEGER);
            stringParameters.add(var.getName());
            pointers.add(new Pointer(var.getVariable()));
            parameters.add(var);
            parametersNames.put(var.getName(), var);
        }
        final Predicate compiled = new Predicate(predicate.getName()
                + "-compiled", stringParameters, types, expression);
        // System.out.println(predicate + " -> " + compiled);
        logger.finest("Compiled : " + compiled);
        Predicate existing = (Predicate) predicateManager
                .seekRelation(compiled);
        if (existing == null) {
            existing = compiled;
        }

        predicateManager.unlinkRelation(this.predicate, this);
        this.predicate = existing;

        predicateManager.linkRelation(existing, this);

    }

    @Override
    public PredicateConstraint standardize(final Variable[] scope) {
        // final Map<Variable, Variable> newOrder = newOrder(scope);

        // final List<Parameter> newParameters = new ArrayList<Parameter>(
        // nbParameters);
        //
        // for (Parameter p : parameters) {
        // if (p instanceof ConstantParameter) {
        // newParameters.add(p);
        // } else if (p instanceof VariableParameter) {
        // newParameters.add(new VariableParameter(p.getName(), newOrder
        // .get(((VariableParameter) p).getVariable())));
        // }
        // }
        final List<Parameter> newParameters = new ArrayList<Parameter>(
                parameters);

        return new PredicateConstraint(getName(), newParameters,
                predicate, scope);
    }

}
