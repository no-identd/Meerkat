package org.meerkat.util.wrappers

import org.scalatest.FunSuite

class SPPFToTreesBFSConverterTests extends FunSuite {
  test("Graphs.NonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness") {
    SPPFToTreesGraphInputTestCases.nonAmbiguousGrammarInfiniteNumberOfPathsTestCorrectness(SPPFToTreesBFSConverter)
  }

  test("Graphs.NonAmbiguousGrammarTestPaths") {
    SPPFToTreesGraphInputTestCases.nonAmbiguousGrammarTestPaths(SPPFToTreesBFSConverter)
  }

  test("Graphs.NonAmbiguousGrammarTwoPathsTestCorrectness") {
    SPPFToTreesGraphInputTestCases.nonAmbiguousGrammarTwoPathsTestCorrectness(SPPFToTreesBFSConverter)
  }

  test("Graphs.AmbiguousGrammarTwoPathsTestQuantity") {
    SPPFToTreesGraphInputTestCases.ambiguousGrammarTwoPathsTestQuantity(SPPFToTreesBFSConverter)
  }

  test("Linear.NonAmbiguousGrammarTestQuantity") {
    SPPFToTreesLinearInputTestCases.nonAmbiguousGrammarTestQuantity(SPPFToTreesBFSConverter)
  }

  test("Linear.NonAmbiguousGrammarTestCorrectness") {
    SPPFToTreesLinearInputTestCases.nonAmbiguousGrammarTestCorrectness(SPPFToTreesBFSConverter)
  }

  test("Linear.AmbiguousGrammarTestQuantity") {
    SPPFToTreesLinearInputTestCases.ambiguousGrammarTestQuantity(SPPFToTreesBFSConverter)
  }

  test("Linear.InfiniteLoopTestSPPFNodeUniqueness") {
    SPPFToTreesLinearInputTestCases.infiniteLoopTestSPPFNodeUniqueness(SPPFToTreesBFSConverter)
  }
}
