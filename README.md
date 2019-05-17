# Meerkat
[![Build Status](https://travis-ci.org/YaccConstructor/Meerkat.svg?branch=master)](https://travis-ci.org/YaccConstructor/Meerkat)

The Meerat library enables general combinator-style context-free path querying.

For details, see the documentation:

https://github.com/YaccConstructor/Meerkat/blob/master/documentation/development.md

And the paper "Parser Combinators for Context-Free Path Querying", available for free via https://www.sigplan.org/OpenTOC/scala18.html

Abstract:
"Transparent integration of a domain-specific language for specification of context-free path queries (CFPQs) into a general-purpose programming language as well as static checking of errors in queries may greatly simplify the development of applications using CFPQs. LINQ and ORM can be used for the integration, but they have issues with flexibility: query decomposition and reusing of subqueries are a challenge. Adaptation of parser combinators technique for paths querying may solve these problems. Conventional parser combinators process linear input, and only the Trails library is known to apply this technique for path querying. Trails suffers the common parser combinators issue: it does not support left-recursive grammars and also experiences problems in cycles handling. We demonstrate that it is possible to create general parser combinators for CFPQ which support arbitrary context-free grammars and arbitrary input graphs. We implement a library of such parser combinators and show that it is applicable for realistic tasks."
