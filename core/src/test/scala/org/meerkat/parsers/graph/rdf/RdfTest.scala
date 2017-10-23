package org.meerkat.parsers.graph.rdf

import java.net.URI

import org.semanticweb.yars.rdfxml.RdfXmlParser
import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode
import org.meerkat.util.{IGraph, InputGraph}

import scala.collection.JavaConverters._
import scalax.collection.Graph
import scalax.collection.edge.LkDiEdge
import scalax.collection.edge.Implicits._
import java.io.FileInputStream
import java.nio.file._
import scala.collection.JavaConverters._



object RdfTest extends App {
  val G1: Nonterminal = syn(
    "subclassof-1" ~~ G1 ~~ "subclassof" |
      "type-1" ~~ G1 ~~ "type" |
      "subclassof-1" ~~ "subclassof" |
      "type-1" ~~ "type"
  )
  val rdfs =
    List(
      "1.1.ttl",
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
  def query(parser: AbstractCPSParsers.AbstractSymbol[_, _], graph: Graph[Int, LkDiEdge]): List[NonPackedNode] = {
    val vertexCount = graph.order
    0.until(vertexCount)
      .toList
      .flatMap { i =>
        parseGraph(parser, new InputGraph(IGraph(graph), i)) match {
          case Right(ParseGraphSuccess(roots, _, _)) =>
            roots
          case _ => List.empty
        }
      }
  }

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
        .collect { case (f, l, t)  if !f.isEmpty&& ! t.isEmpty=>
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

    val nxParser = new RdfXmlParser
    val triples =
      nxParser.parse(inputStream, "http://base.com")
        .asScala
        .map {
          case Array(f, l, t) => (f.getLabel, l.getLabel, t.getLabel)
        }
        .toList

    val graph = triplesToGraph(triples)
    try {
      val start = System.currentTimeMillis
      query(G1, graph)
      val end = System.currentTimeMillis()
      println(file, end - start)
    }catch {
      case _ => println(file, "Ooops")
    }

  }

  rdfs.foreach(processRdf)

//  List("atom-primitive.owl").foreach(processRdf)
}
