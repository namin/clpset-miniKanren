CLP(Set) in miniKanren
======================

Work in progress.

A crude attempt at CLP(Set) in miniKanren.

Implementing the rewrite rules as described in [_Sets and Constraint
Logic Programming_ by Dovier et al](http://dl.acm.org/citation.cfm?id=365169).

I also double-check the [CLP(Set) tests](clpset-tests.scm) by running
them in [`{log}`](http://www.math.unipr.it/~gianfr/setlog.Home.html),
a CLP(Set) implementation in Prolog. I am using
[release 4.3](http://www.math.unipr.it/~gianfr/SETLOG/setlog_4_3.pl),
which seems to be more faithful to the paper than the latest release.
