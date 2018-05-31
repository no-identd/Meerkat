package org.meerkat.util.converters

import org.scalatest.FunSuite

class EnumeratingConverterTests extends FunSuite {
  test("Graphs.NonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness") {
    GraphInputTestCases.nonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness(EnumeratingConverter)
  }

  test("Graphs.NonAmbiguousGrammarTestPaths") {
    GraphInputTestCases.nonAmbiguousGrammarTestPaths(EnumeratingConverter)
  }

  test("Graphs.NonAmbiguousGrammarTwoPathsTestCorrectness") {
    GraphInputTestCases.nonAmbiguousGrammarTwoPathsTestCorrectness(EnumeratingConverter)
  }

  test("Graphs.AmbiguousGrammarTwoPathsTestQuantity") {
    GraphInputTestCases.ambiguousGrammarTwoPathsTestQuantity(EnumeratingConverter)
  }

  test("Linear.NonAmbiguousGrammarTestQuantity") {
    LinearInputTestCases.nonAmbiguousGrammarTestQuantity(EnumeratingConverter)
  }

  test("Linear.NonAmbiguousGrammarTestCorrectness") {
    LinearInputTestCases.nonAmbiguousGrammarTestCorrectness(EnumeratingConverter)
  }

  test("Linear.AmbiguousGrammarTestQuantity") {
    LinearInputTestCases.ambiguousGrammarTestQuantity(EnumeratingConverter)
  }

  test("Linear.InfiniteLoopTestSPPFNodeUniqueness") {
    LinearInputTestCases.infiniteLoopTestSPPFNodeUniqueness(EnumeratingConverter)
  }
}
