CLP(Set) in miniKanren
======================

A prototype of CLP(Set) in miniKanren.

Implemented the rewrite rules as described in
[_Sets and Constraint Logic Programming_ by Dovier et al](http://dl.acm.org/citation.cfm?id=365169).
The prototype is feature-complete wrt to the paper, except for cases 6
and 7 of Figure 7 (Rewriting procedures for U3). The difference in the
solved forms are that unions are solved when each parameter is
variable, regardless of the disequations between them.

I also double-check the [CLP(Set) tests](clpset-tests.scm) by running
them in [`{log}`](http://www.math.unipr.it/~gianfr/setlog.Home.html),
a CLP(Set) implementation in Prolog. I am using
[release 4.3](http://www.math.unipr.it/~gianfr/SETLOG/setlog_4_3.pl),
which seems to be more faithful to the paper than the latest release.
