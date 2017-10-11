package org.meerkat.parsers.graph.neo4j

import java.io.File

import org.meerkat.Syntax.syn
import org.meerkat.parsers.parseGraph
import org.meerkat.util.{EmbeddedNeo4hGraph, IGraph, Input}
import org.neo4j.graphdb.factory.GraphDatabaseFactory

import org.meerkat.Syntax._
import org.meerkat.parsers.Parsers._

import org.meerkat.parsers.examplesgraph._
import org.meerkat.util.{Input, Neo4jGraph}
import org.meerkat.parsers._

import scala.collection.JavaConverters._
import org.neo4j.graphdb.{Label, RelationshipType}


object Main extends App {
  val graphDbFactory = new GraphDatabaseFactory
  val graphDb = graphDbFactory
    .newEmbeddedDatabase(new File("data/test_1"))
  val t = graphDb.beginTx()
//  graphDb.getAllNodes.asScala.foreach(_.delete())
//
//  val car = graphDb.createNode(Label.label("Car"))
//  val person = graphDb.createNode(Label.label("Person"))
//
//  car.createRelationshipTo(person, () => "OWNS")

  val E = syn("OWNS")
  val graph = new EmbeddedNeo4hGraph(graphDb)

  val r = parseGraph(E, Input(graph))
  println(r)
  t.success()
  t.close()
}
