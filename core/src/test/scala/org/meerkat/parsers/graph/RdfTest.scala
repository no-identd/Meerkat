package org.meerkat.parsers.graph

import java.io.FileInputStream
import java.net.URI

import org.meerkat.Syntax._
import org.meerkat.graph._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.JavaConverters._
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge
import org.apache.jena.rdf.model.ModelFactory

class RdfTest extends FunSuite{
  val G1: Nonterminal = syn(
    "subclassof-1" ~~ G1 ~~ "subclassof" |
    "type-1" ~~ G1 ~~ "type" |
    "subclassof-1" ~~ "subclassof" |
    "type-1" ~~ "type"
  )

  val G2: Nonterminal = syn(B)
  val B: Nonterminal  = syn(
    "subclassof-1" ~~ B ~~ "subclassof" |
    "subclassof"
  )

  val rdfs =
    List(
      ("atom-primitive.owl",              15454, 122) ,
      ("biomedical-mesure-primitive.owl", 15156, 2871),
      ("foaf.rdf",                        4118,  10),
      ("funding.rdf",                     17634, 1158),
      ("generations.owl",                 2164,  0),
      ("people_pets.rdf",                 9472,  37),
      ("pizza.owl",                       56195, 1262),
      ("skos.rdf",                        810,   1),
      ("travel.owl",                      2499,  63),
      ("univ-bench.owl",                  2540,  81),
      ("wine.rdf",                        66572, 133)
    )

  def triplesToGraph(triples: List[(String, String, String)]): Graph[Int, LkDiEdge] = {
    val nodes =
      triples
        .flatMap { case (f, _, t) => List(f, t) }
        .toSet
        .zipWithIndex
        .toMap
    val edges =
      triples
        .flatMap { case (f, l, t) =>
          val from = nodes(f)
          val to = nodes(t)
          val label = Option(new URI(l).getFragment).map(_.toLowerCase)
          label match {
            case Some("type") =>
              List((from ~+#> to) ("type"), (to ~+#> from) ("type-1"))
            case Some("subclassof") =>
              List((from ~+#> to) ("subclassof"), (to ~+#> from) ("subclassof-1"))
            case None => List((from ~+#> to) ("noLabel"))
            case Some(lbl) => List((from ~+#> to) (lbl))
          }
        }
    Graph(edges: _*)
  }


  def getTriples(file: String): List[(String, String, String)] = {
    val inputStream = new FileInputStream(getClass.getResource(s"/rdf/$file").getFile)

    val model = ModelFactory.createDefaultModel
    model.read(inputStream, null)
    model.listStatements()
      .asScala
        .map {st =>
          (st.getObject.toString, st.getPredicate.toString, st.getSubject.toString)
        }
      .toList
  }

  def testRdf(file: String, expected1: Int, expected2: Int): ((Long, Int), (Long, Int)) = {
    val triples = getTriples(file)
    val graph = triplesToGraph(triples)

    def parseAndGetRunningTime(grammar: AbstractCPSParsers.AbstractSymbol[_, _]) = {
      val start = System.currentTimeMillis
      val results = parseGraphFromAllPositions(grammar, IGraph(graph)).length
      val end = System.currentTimeMillis
      (end - start, results)
    }

    val (time1, res1) = parseAndGetRunningTime(G1)
    val (time2, res2) = parseAndGetRunningTime(G2)

    ((time1, res1), (time2, res2))
  }

  def getResults: List[(((Long, Int), (Long, Int)), (String, Int, Int))] =
    rdfs.map { case (f, e1, e2) => testRdf(f, e1, e2) }.zip(rdfs)


  test(s"rdfTest") {
    val results = getResults
    println(f"${"Name"}%-28s ${"Time1(ms)"}%-10s ${"#resulst1"}%-10s ${"Time2(ms)"}%-10s ${"#resulsts2"}%-10s")
    results.foreach { case (((time1, res1), (time2, res2)), (file, _, _)) =>
      val withoutExt = file.split('.').head
      println(f"$withoutExt%-28s $time1%-10d $res1%-10d $time2%-10d $res2%-10d")
    }
  }

  test("rdfPerformance") {
    val results = getResults
    results.foreach { case (((_, res1), (_, res2)), (_, expected1, expected2)) =>
      res1 shouldBe expected1
      res2 shouldBe expected2
    }
  }
}