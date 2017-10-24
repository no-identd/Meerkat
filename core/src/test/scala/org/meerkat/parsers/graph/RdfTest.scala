package org.meerkat.parsers.graph

import java.io.FileInputStream
import java.net.URI

import org.meerkat.Syntax._
import org.meerkat.graph._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.util.IGraph
import org.scalatest.FunSuite
import org.semanticweb.yars.rdfxml.RdfXmlParser

import scala.collection.JavaConverters._
import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LkDiEdge


class RdfTest extends FunSuite {
  val G1: Nonterminal = syn(
    "subclassof-1" ~~ G1 ~~ "subclassof" |
    "type-1" ~~ G1 ~~ "type" |
    "subclassof-1" ~~ "subclassof" |
    "type-1" ~~ "type"
  )

  val G2: Nonterminal = syn(B ~~ "subclassof")
  val B: Nonterminal = syn(
    "subclassof-1" ~~ B ~~ "subclassof" |
    "subclassof-1" ~~ "subclassof"
  )

  val rdfs =
    List(
      "atom-primitive.owl",
      "biomedical-mesure-primitive.owl",
      "foaf.rdf",
      "funding.rdf",
      "generations.owl",
      "people_pets.rdf",
      "pizza.owl",
      "skos.rdf",
      "travel.owl",
      "univ-bench.owl",
      "wine.rdf"
    )

  def triplesToGraph(triples: List[(String, String, String)]): Graph[Int, LkDiEdge] = {
    val nodes =
      triples
        .flatMap { case (f, _, t) => List(f, t) }
        .filterNot(_.isEmpty)
        .toSet
        .zipWithIndex
        .toMap
    val edges =
      triples
        .collect { case (f, l, t) if !f.isEmpty && !t.isEmpty =>
          val from = nodes(f)
          val to = nodes(t)
          val label = new URI(l).getFragment
          label match {
            case "type" =>
              List((from ~+#> to) ("type"), (to ~+#> from) ("type-1"))
            case "subClassOf" =>
              List((from ~+#> to) ("subclassof"), (to ~+#> from) ("subclassof-1"))
            case null => List((from ~+#> to) ("noLabel"))
            case lbl => List((from ~+#> to) (lbl))
          }
        }
        .flatten
    Graph(edges: _*)
  }

  def processRdf(file: String): Unit = {
    val inputStream = new FileInputStream(getClass.getResource(s"/rdf/$file").getFile)
    val rdfParser = new RdfXmlParser
    val triples =
      rdfParser.parse(inputStream, "http://base.com")
        .asScala
        .map {
          case Array(f, l, t) => (f.getLabel, l.getLabel, t.getLabel)
        }
        .toList
    val graph = triplesToGraph(triples)

    def parseAndGetRunningTime(grammar: AbstractCPSParsers.AbstractSymbol[_, _]) = {
      val start = System.currentTimeMillis
      parseGraphFromAllPositions(grammar, IGraph(graph))
      val end = System.currentTimeMillis
      end - start
    }

    val time1 = parseAndGetRunningTime(G1)
    val time2 = parseAndGetRunningTime(G2)
    val withoutExt = file.split('.').head
    println(f"$withoutExt%-28s ${time1.toString}%-9s $time2")
  }

  test(s"rdfTest") {
    println(f"${"Name"}%-28s Time1(ms) Time2(ms)")
    rdfs.foreach(processRdf)
  }
}