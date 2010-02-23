package cspom.compiler;

import java.util.Deque;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

public final class PredicateScanner {

	public static final Pattern INTEGER = Pattern.compile("[0-9]*");

	public static final Pattern IDENTIFIER = Pattern.compile("[a-zA-Z_]\\w*");

	private PredicateScanner() {

	}

	public static Node scan(String expression) throws PredicateParseException {
		final StringTokenizer st = new StringTokenizer(expression, " (),", true);
		final Deque<Node> stack = new LinkedList<Node>();
		Node currentNode = new Node();
		while (st.hasMoreElements()) {
			final String token = st.nextToken();
			if (" ".equals(token)) {
				continue;
			} else if ("(".equals(token)) {
				if (currentNode.operator == null) {
					throw new PredicateParseException("Empty operator");
				}
				final Node newNode = new Node();
				currentNode.child = newNode;
				stack.push(currentNode);
				currentNode = newNode;
			} else if (")".equals(token)) {
				if (stack.isEmpty()) {
					throw new PredicateParseException("Too many )s");
				}
				currentNode = stack.pop();

			} else if (",".equals(token)) {
				if (currentNode.operator == null) {
					throw new PredicateParseException("Empty argument");
				}
				final Node newNode = new Node();
				currentNode.sibling = newNode;
				currentNode = newNode;
			} else {
				if (currentNode.operator != null) {
					throw new PredicateParseException("Delimiter expected in "
							+ currentNode + " (" + expression + ")");
				}
				currentNode.operator = token;
			}
		}
		return currentNode;
	}

	public static final class Node {
		private Node sibling;
		private Node child;
		private String operator;

		public Node getSibling() {
			return sibling;
		}

		public Node getChild() {
			return child;
		}

		public String getOperator() {
			return operator;
		}

		public String toString() {
			final StringBuilder stb = new StringBuilder();
			tree(stb, 0);
			return stb.toString();
		}

		public boolean isInteger() {
			if (operator == null) {
				return false;
			}
			return INTEGER.matcher(operator).matches();
		}

		public boolean isIdentifier() {
			if (operator == null) {
				return false;
			}
			return IDENTIFIER.matcher(operator).matches();
		}

		public boolean isLeaf() throws PredicateParseException {
			if (child == null) {
				if (!isInteger() && !isIdentifier()) {
					throw new PredicateParseException(
							"Leaves should be variables or constants");
				}
				return true;
			}
			return false;
		}

		private void tree(StringBuilder stb, int level) {
			for (int i = level; --i >= 0;) {
				stb.append("-");
			}
			stb.append(operator).append("\n");
			if (child != null) {
				child.tree(stb, level + 1);
			}
			if (sibling != null) {
				sibling.tree(stb, level);
			}
		}
	}
}
