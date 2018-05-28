package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.SemanticAction
import org.meerkat.sppf.NonPackedNode
import org.neo4j.graphdb.{GraphDatabaseService, Label, Node, Relationship}
import org.scalatest.Matchers

import scala.language.postfixOps

class MoviesTest extends Neo4jGraphTest("moviesTest") with Matchers {

  //noinspection ZeroIndexToHead
  override def fillDb(db: GraphDatabaseService): Unit = {

    def createMovie(title: String) = {
      val n = db.createNode(Label.label(title))
      n.setProperty("ntype", "movie")
      n
    }

    def createActor(name: String) = {
      val n = db.createNode(Label.label(name))
      n.setProperty("ntype", "actor")
      n
    }
    val indianaJones = createMovie("Indiana Jones")
    val starWars = createMovie("Star Wars")
    val bladeRunner = createMovie("Blade Runner")

    val harrisonFord = createActor("Harrison Ford")
    val gulianGlover = createActor("Julian Glover")
    val brionJames = createActor("Brion James")

    implicit class ActorNode(actor: Node) {
      def starsIn(movie: Node): Unit = {
        actor.createRelationshipTo(movie, () => "stars_in")
        movie.createRelationshipTo(actor, () => "has_actor")
      }
    }

    harrisonFord starsIn indianaJones
    harrisonFord starsIn starWars
    harrisonFord starsIn bladeRunner
    gulianGlover starsIn indianaJones
    gulianGlover starsIn starWars
    brionJames starsIn bladeRunner
  }

  override def createParser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _] = {
    val fixedActor = syn(V((e: Entity) => e.ntype == "actor" && e.value() == "Harrison Ford"))
    val fixedMovie = syn(V((e: Entity) => e.ntype == "movie"  && e.value() == "Indiana Jones"))
    val starsIn = syn(E((e: Entity) => e.value() == "stars_in"))
    val hasActor = syn(E((e: Entity) => e.value() == "has_actor"))
    val actor = syn(V((e: Entity) => e.ntype == "actor" && e.value() != "Harrison Ford") ^^)
    val movie = syn(V((e: Entity) => e.ntype == "movie" && e.value() != "Indiana Jones"))
    val actors =
      syn((fixedActor ~ starsIn ~ fixedMovie ~ hasActor ~ actor ~ starsIn ~ movie ~ hasActor ~ fixedActor) &&)
    actors
  }


  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit = {
    val actors = executeQuery(parser, graph)
      .map{ case (x: Entity) => x.value()}
    actors shouldBe List("Julian Glover")
  }
}
