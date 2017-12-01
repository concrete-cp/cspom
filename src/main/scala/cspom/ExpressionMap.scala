package cspom

import java.util.{IdentityHashMap, LinkedHashSet}

import com.typesafe.scalalogging.LazyLogging
import cspom.variable.{CSPOMConstant, CSPOMExpression, CSPOMSeq, CSPOMVariable}

import scala.collection.{SortedSet, mutable}
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.CharSequenceReader
import scala.reflect.runtime.universe.TypeTag

class ExpressionMap extends LazyLogging {
  /**
    * Map used to easily retrieve a variable according to its name.
    */
  private val namedExpressions = collection.mutable.HashMap[String, CSPOMExpression[_]]()
  private val expressionNames = collection.mutable.HashMap[CSPOMExpression[_], SortedSet[String]]().withDefaultValue(SortedSet.empty)
  private[cspom] val containers = new IdentityHashMap[CSPOMExpression[_], LinkedHashSet[(CSPOMSeq[_], Int)]]()


  private val generatedNames = collection.mutable.Map[CSPOMExpression[_], String]()

  private var generatedId = 0

  def count: Int = namedExpressions.size

  def expressions: Iterable[CSPOMExpression[_]] = expressionNames.keys

  def getExpressions: java.util.Map[String, CSPOMExpression[_]] = namedExpressions.asJava

  /**
    * @param name
    * A variable name.
    * @return The variable with the corresponding name.
    */
  def expression(name: String): Option[CSPOMExpression[_]] = {
    NameParser.parse(new CharSequenceReader(name)).map(Some(_)).getOrElse(None).flatMap {
      case (n, s) => getInSeq(namedExpressions.get(n), s)
    }
      .orElse(namedExpressions.get(name))

  }

  def getContainers(e: CSPOMExpression[_]): Option[LinkedHashSet[(CSPOMSeq[_], Int)]] = Option(containers.get(e))

  def registerContainer(e: CSPOMExpression[_]): Unit = {

    for {
      s <- PartialFunction.condOpt(e) { case s: CSPOMSeq[_] => s }
      (c, i) <- s.withIndex
    } {
      logger.trace(s"Registering $s")
      val set = Option(containers.get(c)).getOrElse {
        val set = new LinkedHashSet[(CSPOMSeq[_], Int)]()
        containers.put(c, set)
        set
      }
      set.add((s, i))
      registerContainer(c)
    }

  }

  def namesOf(e: CSPOMExpression[_]): Iterable[String] = {
    val direct = expressionNames(e)
    val inContainers = for {
      cl <- Option(containers.get(e)).toIterable
      (seq, index) <- cl.asScala
      s <- namesOf(seq)
    } yield {
      s"$s[$index]"
    }

    direct ++ inContainers
  }

  def displayName(e: CSPOMExpression[_]): String = e match {
    case CSPOMConstant(c) => c.toString
    case expression =>
      namesOf(expression).toSeq match {
        case Seq() => generatedNames.getOrElseUpdate(expression, nextGeneratedName(expression))
        case cspomNames => cspomNames.sorted.mkString("||")
      }
  }

  def variable(name: String): Option[CSPOMVariable[_]] = {
    expression(name).collect {
      case v: CSPOMVariable[_] => v
    }
  }


  private def getInSeq(e: Option[CSPOMExpression[_]], s: Seq[Int]): Option[CSPOMExpression[_]] = s match {
    case Seq() => e
    case head +: tail =>
      e
        .collect {
          case v: CSPOMSeq[_] => getInSeq(Some(v(head)), tail)
        }
        .flatten
  }

  private  def cloneContainers(e: CSPOMExpression[_]): Option[Seq[(CSPOMSeq[Any], Seq[Int])]] =
      getContainers(e).map { l =>
        val m = new mutable.LinkedHashMap[CSPOMSeq[Any], ArrayBuffer[Int]]()
        for (elem <- l.asScala) {
          val key = elem._1
          val bldr = m.getOrElseUpdate(key, new ArrayBuffer[Int]())
          bldr += elem._2
        }

        m.toSeq
      }

  def nameExpression[A <: CSPOMExpression[_]](e: A, n: String): Unit = {
    require(!namedExpressions.contains(n), s"${namedExpressions(n)} is already named $n")
    namedExpressions += n -> e
    expressionNames(e) += n
    registerContainer(e)
  }


  def removeContainer(e: CSPOMExpression[_]): Unit = {
    for {
      s <- PartialFunction.condOpt(e) { case s: CSPOMSeq[_] => s }
      (e, i) <- s.withIndex
    } {
      logger.trace(s"Deregistering $s")
      val set = containers.get(e)
      set.remove((s, i))
      if (!isReferenced(e)) {
        removeContainer(e)
      }
    }
  }


  def isReferenced(e: CSPOMExpression[_]): Boolean =
    expressionNames(e).nonEmpty || Option(containers.get(e)).exists(s => !s.isEmpty)


  def expressionsWithNames: Iterator[(String, CSPOMExpression[_])] = {
    namedExpressions.iterator
  }

  def allNames = namedExpressions.keySet

  def nextGeneratedName(e: CSPOMExpression[_]): String = e match {
    case CSPOMConstant(v) => v.toString
    //case CSPOMSeq(v) => v.map(displayName).mkString("CSPOMSeq(", ", ", ")")
    case _ =>
      generatedId += 1
      "_" + generatedId
  }

  def replaceExpression[R: TypeTag, T <: R](which: CSPOMExpression[R], by: CSPOMExpression[T]): Seq[(CSPOMExpression[_], CSPOMExpression[_])] = {
    // lazy val namesOfWhich = which.toString(displayName) // { x => ??? }
    require(which != by, s"Replacing $which with $by")
    //require((namesOf(which).toSet & namesOf(by).toSet).isEmpty)
    var replaced = List[(CSPOMExpression[_], CSPOMExpression[_])]()

    for (n <- expressionNames(which)) {
      namedExpressions(n) = by
      expressionNames(by) += n
    }
    expressionNames.remove(which)

    logger.debug(s"Replacing $which with $by: contained in ${getContainers(which)}")


    for {
      get <- cloneContainers(which)
      (container, indices) <- get // cloning is required to obtain a copy and prevent ConcurrentModificationException
    } {
      val nc = indices.foldLeft(container)((acc, index) => acc.replaceIndex(index, by))
      replaced ++:= replaceExpression(container, nc)

      removeContainer(container)
      registerContainer(nc)
    }
    logger.debug(s"replacing ${which.toString(displayName)} with ${by.toString(displayName)}") // from ${Thread.currentThread().getStackTrace.toSeq}")


    (which, by) :: replaced
  }
}
