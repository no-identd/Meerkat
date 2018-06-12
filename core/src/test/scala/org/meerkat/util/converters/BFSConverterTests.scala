package org.meerkat.util.converters

import org.scalatest.FunSuite

class BFSConverterTests extends FunSuite {
  test("Graphs.NonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness") {
    GraphInputTestCases.nonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness(
      BFSConverter)
  }

  test("Graphs.NonAmbiguousGrammarTestPaths") {
    GraphInputTestCases.nonAmbiguousGrammarTestPaths(BFSConverter)
  }

  test("Graphs.NonAmbiguousGrammarTwoPathsTestCorrectness") {
    GraphInputTestCases.nonAmbiguousGrammarTwoPathsTestCorrectness(BFSConverter)
  }

  test("Graphs.AmbiguousGrammarTwoPathsTestQuantity") {
    GraphInputTestCases.ambiguousGrammarTwoPathsTestQuantity(BFSConverter)
  }

  test("Linear.NonAmbiguousGrammarTestQuantity") {
    LinearInputTestCases.nonAmbiguousGrammarTestQuantity(BFSConverter)
  }

  test("Linear.NonAmbiguousGrammarTestCorrectness") {
    LinearInputTestCases.nonAmbiguousGrammarTestCorrectness(BFSConverter)
  }

  test("Linear.AmbiguousGrammarTestQuantity") {
    LinearInputTestCases.ambiguousGrammarTestQuantity(BFSConverter)
  }

  test("Linear.InfiniteLoopTestSPPFNodeUniqueness") {
    LinearInputTestCases.infiniteLoopTestSPPFNodeUniqueness(BFSConverter)
  }
}
