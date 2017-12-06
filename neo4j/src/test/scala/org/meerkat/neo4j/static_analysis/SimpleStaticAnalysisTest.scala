package org.meerkat.neo4j.static_analysis

import org.neo4j.graphdb.{GraphDatabaseService, Label, Node}

class SimpleStaticAnalysisTest extends StaticAnalysisTest {
  override def fillDb(db: GraphDatabaseService): Unit = {
    val a = db.createNode(Label.label("a"))
    val b = db.createNode(Label.label("b"))
    val c = db.createNode(Label.label("c"))
    val d = db.createNode(Label.label("d"))
    val e = db.createNode(Label.label("e"))
    val `&b` = db.createNode(Label.label("&b"))
    val `&c` = db.createNode(Label.label("&c"))
    val `&d` = db.createNode(Label.label("&d"))
    val `*c` = db.createNode(Label.label("*c"))
    val `*d` = db.createNode(Label.label("*d"))

    def createEdgeWithReversed(f: Node, t: Node, l: String) = {
      f.createRelationshipTo(t, () => l)
      t.createRelationshipTo(f, () => "n" + l)
    }

    createEdgeWithReversed(a, b, "a")
    createEdgeWithReversed(`&b`, b, "d")
    createEdgeWithReversed(`&b`, c, "a")
    createEdgeWithReversed(`&c`, c, "d")
    createEdgeWithReversed(c, `*c`, "d")
    createEdgeWithReversed(e, `*c`, "a")
    createEdgeWithReversed(`*c`, `*d`, "a")
    createEdgeWithReversed(d, `*d`, "d")
    createEdgeWithReversed(`&d`, d, "d")
  }
}
