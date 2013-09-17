package cspom.xcsp

import cspom.variable.IntDomain
import cspom.CSPOM
import cspom.extension.Relation
import cspom.variable.IntVariable
import cspom.variable.CSPOMVariable

// TODO : very incomplete, do not use
object XCSPWriter {
  def toXCSP(problem: CSPOM) = {
    val domains = collection.mutable.Map[IntDomain, String]()
    var did = 0
    problem.variables.foreach {
      case v: IntVariable =>
        domains += v.domain -> ("D" + did); did += 1
      case _ => throw new UnsupportedOperationException("Unsupported domain type")
    }

    val relations = collection.mutable.Map[(Relation, Boolean), String]()
    val predicates = collection.mutable.Map[(String, Int), String]()

    var rid = 0
    var pid = 0

    problem.constraints.foreach { c =>
      if (c.function == "extension") {
        val relation = c.params("relation").asInstanceOf[Relation]
        val init = c.params("init").asInstanceOf[Boolean]
        relations.getOrElseUpdate((relation, init), { rid += 1; "R" + rid })
      } else {
        predicates.getOrElseUpdate((c.function, 1 + c.arguments.size), { pid += 1; "P" + pid })
      }

      //
      //      case c: FunctionalConstraint if (!funcPredicates.contains((c.predicate, c.arity))) =>
      //        funcPredicates.put((c.predicate, c.arity), "P" + pid)
      //        pid += 1
      //
      //      case c: GeneralConstraint if (!genPredicates.contains((c.predicate, c.arity))) =>
      //        genPredicates.put((c.predicate, c.arity), "P" + pid)
      //        pid += 1
      //
      //      case _ =>

    }

    <instance>
      <presentation maxConstraintArity={ problem.constraints.map(_.arity).max.toString }/>
      <domains>
        {
          domains.map {
            case (d, n) =>
              <domain name={ n } nbValues={ d.size.toString }>{ d.toXCSP }</domain>
          }
        }
      </domains>
      <variables nbVariables={ problem.variables.size.toString }>
        {
          problem.variables map {
            case v: IntVariable => <variable name={ v.name } domain={ domains(v.domain) }/>
            case _ => ???
          }
        }
      </variables>
      <relations nbRelations={ relations.size.toString }>
        {
          relations map {
            case ((r, init), n) =>
              <relation name={ n } arity={ r.arity.toString } nbTuples={ r.size.toString } semantics={ (if (init) "conflicts" else "supports") }>
                { r.tupleString }
              </relation>
          }
        }
      </relations>
      <predicates nbPredicates={ (predicates.size).toString }>
        {
          (
            predicates map {
              case ((p, a), n) =>

                <predicate name={ n }>
                  <parameters>{ (0 until a) map ("int X" + _) }</parameters>
                  <expression><functional>eq(X0, { p + (1 until a).map("X" + _).mkString("(", ", ", ")") })</functional></expression>
                </predicate>

            })
        }
      </predicates>
      <constraints nbConstraints={ problem.constraints.size.toString }>
        {

          problem.constraints.zipWithIndex map {
            case (c, i) =>
              <constraint name={
                "C" + i
              } arity={
                c.arity.toString
              } scope={
                (c.result +: c.arguments).collect { case v: CSPOMVariable => v.name }.mkString(" ")
              } reference={
                if (c.function == "extension") {
                  relations((c.params("relation").asInstanceOf[Relation], c.params("init").asInstanceOf[Boolean]))
                } else {
                  predicates(c.function, 1 + c.arguments.size)
                }
              }>
                <parameters>{ (c.result +: c.arguments).collect { case v: CSPOMVariable => v.name }.mkString(" ") }</parameters>
              </constraint>

          }
        }
      </constraints>
    </instance>
  }
}