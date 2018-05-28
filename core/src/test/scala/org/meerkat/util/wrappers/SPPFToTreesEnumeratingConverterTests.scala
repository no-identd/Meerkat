package org.meerkat.util.wrappers

import org.scalatest.FunSuite

class SPPFToTreesEnumeratingConverterTests extends FunSuite {
  test("Graphs.NonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness") {
    SPPFToTreesGraphInputTestCases.nonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness(SPPFToTreesEnumeratingConverter)
  }

  test("Graphs.NonAmbiguousGrammarTestPaths") {
    SPPFToTreesGraphInputTestCases.nonAmbiguousGrammarTestPaths(SPPFToTreesEnumeratingConverter)
  }

  test("Graphs.NonAmbiguousGrammarTwoPathsTestCorrectness") {
    SPPFToTreesGraphInputTestCases.nonAmbiguousGrammarTwoPathsTestCorrectness(SPPFToTreesEnumeratingConverter)
  }

  test("Graphs.AmbiguousGrammarTwoPathsTestQuantity") {
    SPPFToTreesGraphInputTestCases.ambiguousGrammarTwoPathsTestQuantity(SPPFToTreesEnumeratingConverter)
  }

  test("Linear.NonAmbiguousGrammarTestQuantity") {
    SPPFToTreesLinearInputTestCases.nonAmbiguousGrammarTestQuantity(SPPFToTreesEnumeratingConverter)
  }

  test("Linear.NonAmbiguousGrammarTestCorrectness") {
    SPPFToTreesLinearInputTestCases.nonAmbiguousGrammarTestCorrectness(SPPFToTreesEnumeratingConverter)
  }

  test("Linear.AmbiguousGrammarTestQuantity") {
    SPPFToTreesLinearInputTestCases.ambiguousGrammarTestQuantity(SPPFToTreesEnumeratingConverter)
  }

  test("Linear.InfiniteLoopTestSPPFNodeUniqueness") {
    SPPFToTreesLinearInputTestCases.infiniteLoopTestSPPFNodeUniqueness(SPPFToTreesEnumeratingConverter)
  }
}
