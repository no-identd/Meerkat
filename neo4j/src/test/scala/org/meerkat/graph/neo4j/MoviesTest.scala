package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.SemanticAction
import org.neo4j.graphdb.{GraphDatabaseService, Label, Node, Relationship}
import org.scalatest.Matchers

import scala.language.postfixOps

class MoviesTest extends Neo4jGraphTest("moviesTest") with Matchers {

  //noinspection ZeroIndexToHead
  override def fillDb(db: GraphDatabaseService): Unit = {
    val movies = List.tabulate(2) { i =>
      val n = db.createNode(Label.label(s"movie$i"))
      n.setProperty("ntype", "movie")
      n
    }
    val actors = List.tabulate(3) { i =>
      val n = db.createNode(Label.label(s"actor$i"))
      n.setProperty("ntype", "actor")
      n
    }

    implicit class ActorNode(actor: Node) {
      def starsIn(movie: Node): Unit = {
        actor.createRelationshipTo(movie, () => "stars_in")
        movie.createRelationshipTo(actor, () => "has_actor")
      }
    }

    actors(1) starsIn movies(0)
    actors(2) starsIn movies(0)
    actors(2) starsIn movies(1)
    actors(0) starsIn movies(0)
    actors(0) starsIn movies(1)
  }

  override def createParser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, _, _] = {
    val fixedActor = syn(V((e: Entity) => e.ntype == "actor" && e.value() == "actor0"))
    val fixedMovie = syn(V((e: Entity) => e.ntype == "movie"  && e.value() == "movie0"))
    val starsIn = syn(E((e: Entity) => e.value() == "stars_in"))
    val hasActor = syn(E((e: Entity) => e.value() == "has_actor"))
    val actor = syn(V((e: Entity) => e.ntype == "actor" && e.value() != "actor0") ^^)
    val movie = syn(V((e: Entity) => e.ntype == "movie" && e.value() != "movie0"))
    val actors =
      syn((fixedActor ~ starsIn ~ fixedMovie ~ hasActor ~ actor ~ starsIn ~ movie ~ hasActor ~ fixedActor) &&)
    actors
  }


  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, _, _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit = {
    val actors = getAllSPPFs(parser, graph)
      .map(sppf => SemanticAction.execute(sppf)(graph))
      .map{case (x: Entity) => x.value()}
    actors shouldBe List("actor2")
  }
}
