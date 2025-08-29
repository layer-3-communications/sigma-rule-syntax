# sigma-rule-syntax

Decode rules from the [Sigma](https://github.com/SigmaHQ/sigma) project.
This library also includes a `ToJSON` instance for `Rule`, but this instance
is intended for use in the test suite, not use by applications.

This library was originally just for decoding rules, but I've added an
application that performs simple analyses and transformations. It can be
used like this:

* `sigma-rule-insights summary DIR`: Summarize all rules in the directory.
  Gather information about what fields are used most often and and what
  modifier sets are used.
* `sigma-rule-insights sql DIR`: Generate SQL predicates corresponding to
  each rule and write them to the `generated` directory. This reads the
  environment variable `SIGMA_FIELD_ALLOWLIST` to restriction the rule set
  to only rules that reference certain fields.
