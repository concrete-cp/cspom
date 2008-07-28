package cspom;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import cspom.constraint.Constraint;
import cspom.constraint.DomainSignature;


public class RelationManager implements Iterable<Relation>{
	private final Map<Relation, Map<DomainSignature, Collection<Constraint>>> relations;

	public RelationManager() {
		super();
		relations = new HashMap<Relation, Map<DomainSignature, Collection<Constraint>>>();
	}

	public Relation seekRelation(final Relation relation) {
		if (!relations.containsKey(relation)) {
			return null;
		}
		for (Relation p : relations.keySet()) {
			if (p.equals(relation)) {
				return p;
			}
		}
		return null;
	}

	public void linkRelation(final Relation relation,
			final Constraint constraint) {
		final Map<DomainSignature, Collection<Constraint>> constraints;
		if (relations.containsKey(relation)) {
			constraints = relations.get(relation);
		} else {
			constraints = new HashMap<DomainSignature, Collection<Constraint>>();
			relations.put(relation, constraints);
		}
		final Collection<Constraint> signedConstraints;
		if (constraints.containsKey(constraint.signature())) {
			signedConstraints = constraints.get(constraint.signature());
		} else {
			signedConstraints = new ArrayList<Constraint>();
			constraints.put(constraint.signature(), signedConstraints);
		}

		signedConstraints.add(constraint);
	}

	public void unlinkRelation(final Relation relation,
			final Constraint constraint) {
		final Map<DomainSignature, Collection<Constraint>> constraints = relations
				.get(relation);
		if (!constraints.containsKey(constraint.signature())) {
			throw new IllegalArgumentException("Could not find " + constraint.signature() + " in " + 
					constraints);
		}
		final Collection<Constraint> signedConstraints = constraints
				.get(constraint.signature());
		if (!signedConstraints.contains(constraint)) {
			throw new IllegalArgumentException();
		}

		if (signedConstraints.size() == 1) {
			if (constraints.size() == 1) {
				relations.remove(relation);
			} else {
				constraints.remove(constraint.signature());
			}

		} else {
			signedConstraints.remove(constraint);
		}
	}

	public Collection<Relation> getRelations() {
		return relations.keySet();
	}

	public Map<DomainSignature, Collection<Constraint>> getSignatures(
			final Relation relation) {
		return relations.get(relation);
	}
	
	public int size() {
		return relations.size();
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();
		for (Relation p : relations.keySet()) {
			stb.append(p).append('\n');
			for (DomainSignature i : relations.get(p).keySet()) {
				stb.append('\t').append(i).append('\n');
				for (Constraint c : relations.get(p).get(i)) {
					stb.append("\t\t").append(c).append('\n');
				}
			}

		}
		return stb.toString();
	}

	public Iterator<Relation> iterator() {
		return getRelations().iterator();
	}

}
