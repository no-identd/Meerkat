package org.meerkat.graph.neo4j

import org.meerkat.Syntax._
import org.meerkat.graph.neo4j.Neo4jInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode
import org.neo4j.graphdb.{GraphDatabaseService, Label, Node}
import org.scalatest.Matchers

import scala.language.postfixOps

class CitiesTest extends Neo4jGraphTest("citiesTest") with Matchers {

  override def fillDb(db: GraphDatabaseService): Unit = {

    def createCity(title: String, country: String) = {
      val n = db.createNode(Label.label(title))
      n.setProperty("country", country)
      n
    }

    val a = createCity("a", "X")
    val b = createCity("b", "Y")
    val c = createCity("c", "X")
    val d = createCity("d", "Y")
    val e = createCity("e", "X")

    implicit class CityNode(city1: Node) {
      def way(city2: Node): Unit = {
        city1.createRelationshipTo(city2, () => "road_to")
      }
    }

    a way c
    a way d
    b way a
    c way b
    d way e
  }

  override def createParser
    : AbstractCPSParsers.AbstractSymbol[Entity, Entity, NonPackedNode, _] = {
    val query = new AnyRef {
      def city(country: String): Nonterminal[Entity, Entity] & Entity =
        syn(V((_: Entity).country == country) ^^)

      val middleCity   = syn(V((_: Entity).label() == "a") ^^) & (List(_))
      val roadTo       = outE((_: Entity) => true)
      val countryNames = List("X", "Y")

      def wayPart(e: String): Nonterminal[Entity, Entity] & List[Entity] =
        syn((city(e) ~ roadTo ~ path ~ roadTo ~ city(e)) & {
          case (a: Entity) ~ (b: List[Entity]) ~ (c: Entity) => a +: b :+ c
        })

      val path: Nonterminal[Entity, Entity] & List[Entity] =
        countryNames.map(wayPart) match {
          case x :: Nil     => syn(x | middleCity)
          case x :: y :: xs => syn(xs.foldLeft(x | y)(_ | _) | middleCity)
        }
    }
    query.path
  }

  override def doTest(parser: AbstractCPSParsers.AbstractSymbol[Entity,
                                                                Entity,
                                                                NonPackedNode,
                                                                _],
                      graph: Neo4jInput,
                      db: GraphDatabaseService): Unit = {
    val actors = executeQuery(
      parser.asInstanceOf[AbstractCPSParsers.AbstractSymbol[Entity,
                                                            Entity,
                                                            NonPackedNode,
                                                            List[Entity]]],
      graph)
      .map { l: List[Entity] =>
        l.map(_.label()).mkString("->")
      }
    //    println(org.meerkat.util.visualization.buildDot(parser, graph))
    actors.toSet shouldBe Set("a", "b->a->d", "c->b->a->d->e")
  }
}
