package cspom.dimacs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cspom.CSPOM;
import cspom.CSPParseException;
import cspom.compiler.PredicateParseException;
import cspom.variable.CSPOMVariable;
import cspom.variable.UnknownBooleanDomain$;

public final class CNFParser {
    private final CSPOM problem;

    private static final Pattern PARAMETER = Pattern
            .compile("^p cnf (\\d+) (\\d+)$");
    private static final Pattern VAR = Pattern.compile("(-?\\d+)");

    public CNFParser(final CSPOM problem) {
        this.problem = problem;
    }

    public void parse(final InputStream is) throws IOException,
            CSPParseException {
        final BufferedReader reader = new BufferedReader(new InputStreamReader(
                is));

        List<Integer> currentClause = new ArrayList<Integer>();
        boolean parameter = false;
        for (String line = reader.readLine(); line != null; line = reader
                .readLine()) {
            if (line.startsWith("c")) {
                continue;
            }
            if (!parameter) {
                final Matcher matcher = PARAMETER.matcher(line);
                if (matcher.matches()) {
                    parameter = true;
                    final int nbVars = Integer.valueOf(matcher.group(1));
                    for (int i = 1; i <= nbVars; i++) {
                        problem.addVariable(new CSPOMVariable("V" + i,
                                UnknownBooleanDomain$.MODULE$));
                    }
                    continue;
                }
                throw new CSPParseException("Parameter line not found");
            }
            final Matcher varMatcher = VAR.matcher(line);
            while (varMatcher.find()) {
                final int var = Integer.valueOf(varMatcher.group(1));
                if (var == 0) {
                    try {
                        problem.ctr(clause(currentClause));
                    } catch (PredicateParseException e) {
                        throw new IllegalStateException(e);
                    }
                    currentClause.clear();
                } else {
                    currentClause.add(var);
                }
            }
        }
    }

    private String clause(final List<Integer> currentClause) {

        final StringBuilder clause = new StringBuilder();
        final StringBuilder parameters = new StringBuilder();
        parameters.append("or{");

        final Iterator<Integer> i = currentClause.iterator();

        for (;;) {
            int v = i.next();
            clause.append('V').append(Math.abs(v));
            if (v > 0) {
                parameters.append('0');
            } else {
                parameters.append('1');
            }

            if (!i.hasNext()) {
                return parameters.append("}(").append(clause.toString())
                        .append(')').toString();
            }
            clause.append(", ");
            parameters.append(", ");
        }

    }
}
