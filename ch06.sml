use "prelude.sml";
use "test.sml";

datatype fruit = Peach
  | Apple
  | Pear
  | Lemon
  | Fig;

datatype tree = Bud
  | Flat of fruit * tree
  | Split of tree * tree;

fun flat_only(Bud) = true
  | flat_only(Flat(f,t)) = flat_only(t)
  | flat_only(Split(s,t)) = false;

(flat_only: tree -> bool);

assert(
  flat_only(
    Flat(Apple,
      Flat(Peach,
        Bud))));

assert(
  flat_only(
    Flat(Pear,Bud)));

assert_not(
  flat_only(
    Split(
      Bud,
      Flat(Fig,
        Split(
          Bud,
          Bud)))));

assert_not(
  flat_only(
    Split(
      Split(
        Bud,
        Flat(Lemon,
          Bud)),
          Flat(Fig,
            Split(
              Bud,
              Bud)))));

fun split_only(Bud) = true
  | split_only(Flat(f,t)) = false
  | split_only(Split(s,t))
    = split_only(s) andalso split_only(t);

(split_only: tree -> bool);

assert(split_only(Bud));

assert(
  split_only(
    Split(
      Split(
        Bud,
        Split(
          Bud,
          Bud)),
          Split(
            Bud,
            Split(
            Bud,
            Bud)))));

assert_not(split_only(Flat(Fig, Bud)));

fun contains_fruit(Bud) = false
  | contains_fruit(Flat(f,t)) = true
  | contains_fruit(Split(s,t))
    = contains_fruit(s) orelse contains_fruit(t);

(contains_fruit: tree -> bool);

assert(contains_fruit(Flat(Fig,Bud)));

fun contains_fruit(x)
  = not(split_only(x));

(contains_fruit: tree -> bool);

assert(contains_fruit(Flat(Fig,Bud)));

fun less_than(n:int, m:int) = (n < m);

assert(less_than(3,120));
assert_not(less_than(13,1));

fun larger_of(n,m)
  = if less_than(n, m)
      then m
      else n;

(larger_of: (int * int) -> int);

assert_equal(larger_of(3,10), 10);
assert_equal(larger_of(12,5), 12);

fun height(Bud) = 0
  | height(Flat(f,t)) = 1 + height(t)
  | height(Split(s,t)) = 1 + larger_of(height(s),height(t));

(height: tree -> int);

assert_equal(
  height(Split(Bud,Bud)),
  1);

assert_equal(
  height(
    Split(
      Split(
        Bud,
        Bud),
      Flat(Fig,
        Flat(Lemon,
          Flat(Apple,
            Bud))))),
    4);

fun eq_fruit(Peach,Peach) = true
  | eq_fruit(Apple,Apple) = true
  | eq_fruit(Pear,Pear) = true
  | eq_fruit(Lemon,Lemon) = true
  | eq_fruit(Fig, Fig) = true
  | eq_fruit(a_fruit,another_fruit) = false;

(eq_fruit: (fruit * fruit) -> bool);

fun subst_in_tree(n,a,Bud) = Bud
  | subst_in_tree(n,a,Flat(f,t))
    = if eq_fruit(f,a)
        then Flat(n,subst_in_tree(n,a,t))
        else Flat(f,subst_in_tree(n,a,t))
  | subst_in_tree(n,a,Split(s,t))
    = Split(
        subst_in_tree(n,a,t),
        subst_in_tree(n,a,s));

(subst_in_tree: (fruit * fruit * tree) -> tree);

subst_in_tree(Apple,Fig,
  Split(
    Split(
      Flat(Fig,
        Bud),
      Flat(Fig,
        Bud)),
    Flat(Fig,
      Flat(Lemon,
        Flat(Apple,
          Bud)))));

fun occours(a,Bud) = 0
  | occours(a,Flat(f,t))
    = if eq_fruit(f,a)
        then 1 + occours(a,t)
        else occours(a,t)
  | occours(a,Split(s,t))
    = occours(a,s) + occours(a,t);

(occours: (fruit * tree) -> int);

assert_equal(
  occours(Fig,
    Split(
      Split(
        Flat(Fig,
          Bud),
        Flat(Fig,
          Bud)),
      Flat(Fig,
        Flat(Lemon,
          Flat(Apple,
            Bud))))),
   3);

datatype 'a slist = Empty
  | Scons of (('a sexp) * ('a slist))
and 'a sexp = An_atom of 'a
  | A_slist of ('a slist);

An_atom(5) : int sexp;

An_atom(Fig) : fruit sexp;

A_slist(Empty) : 'a sexp;

Scons(An_atom(5),
  Scons(An_atom(13),
    Scons(An_atom(1),
      Empty))) : int slist;

Scons(An_atom(Fig),
  Empty) : fruit slist;

fun occours_in_slist(a,Empty) = 0
  | occours_in_slist(a,Scons(s,y))
    = occours_in_sexp(a,s) +
      occours_in_slist(a,y)
and occours_in_sexp(a,An_atom(b))
  = if eq_fruit(b,a)
      then 1
      else 0
  | occours_in_sexp(a,A_slist(y))
  = occours_in_slist(a,y);

(occours_in_slist: (fruit * fruit slist) -> int);
(occours_in_sexp: (fruit * fruit sexp) -> int);

assert_equal(
  occours_in_slist(Fig,
    Scons(A_slist(
            Scons(An_atom(Fig),
              Scons(An_atom(Peach),
                Empty))),
      Scons(An_atom(Fig),
        Scons(An_atom(Lemon),
          Empty)))),
   2);

assert_equal(
  occours_in_sexp(Fig,
    A_slist(
      Scons(An_atom(Fig),
        Scons(An_atom(Peach),
          Empty)))),
  1);

fun subst_in_slist(n,a,Empty) = Empty
  | subst_in_slist(n,a,Scons(s,y))
    = Scons(subst_in_sexp(n,a,s), subst_in_slist(n,a,y))
and subst_in_sexp(n,a,An_atom(b))
  = if eq_fruit(b,a)
      then An_atom(n)
      else An_atom(b)
  | subst_in_sexp(n,a,A_slist(y))
    = A_slist(subst_in_slist(n,a,y));

(subst_in_slist: (fruit * fruit * fruit slist) -> fruit slist);
(subst_in_sexp: (fruit * fruit * fruit sexp) -> fruit sexp);

assert_equal(
  subst_in_slist(Apple,Fig,
    Scons(A_slist(
            Scons(An_atom(Fig),
              Scons(An_atom(Peach),
                Empty))),
      Scons(An_atom(Fig),
        Scons(An_atom(Lemon),
          Empty)))),
    Scons(A_slist(
            Scons(An_atom(Apple),
              Scons(An_atom(Peach),
                Empty))),
      Scons(An_atom(Apple),
        Scons(An_atom(Lemon),
          Empty))));

assert_equal(
  subst_in_sexp(Apple,Fig,
    A_slist(
      Scons(An_atom(Fig),
        Scons(An_atom(Peach),
          Empty)))),
   A_slist(
    Scons(An_atom(Apple),
      Scons(An_atom(Peach),
        Empty))));

fun eq_fruit_in_atom(a,An_atom(s)) = eq_fruit(a,s)
  | eq_fruit_in_atom(a_fruit,A_slist(y)) = false;

(eq_fruit_in_atom: (fruit * fruit sexp) -> bool);

fun rem_from_slist(a,Empty) = Empty
  | rem_from_slist(a,Scons(s,y))
    = if eq_fruit_in_atom(a,s)
        then rem_from_slist(a,y)
        else Scons(rem_from_sexp(a,s),
                   rem_from_slist(a,y))
and rem_from_sexp(a,An_atom(b))
    = An_atom(b)
  | rem_from_sexp(a,A_slist(y))
    = A_slist(rem_from_slist(a,y));

(rem_from_slist: (fruit * fruit slist) -> fruit slist);
(rem_from_sexp: (fruit * fruit sexp) -> fruit sexp);

assert_equal(
  rem_from_slist(Fig,
    Scons(A_slist(
            Scons(An_atom(Fig),
              Scons(An_atom(Peach),
                Empty))),
      Scons(An_atom(Fig),
        Scons(An_atom(Lemon),
          Empty)))),
      Scons(A_slist(
            Scons(An_atom(Peach),
              Empty)),
            Scons(An_atom(Lemon),
              Empty)));

fun rem_from_slist(a,Empty) = Empty
  | rem_from_slist(a,Scons(An_atom(b),y))
    = if eq_fruit(a,b)
        then rem_from_slist(a,y)
        else Scons(An_atom(b),
                   rem_from_slist(a,y))
  | rem_from_slist(a,Scons(A_slist(x),y))
    = Scons(A_slist(rem_from_slist(a,x)),
            rem_from_slist(a,y));

(rem_from_slist: (fruit * fruit slist) -> fruit slist);

assert_equal(
  rem_from_slist(Fig,
    Scons(A_slist(
            Scons(An_atom(Fig),
              Scons(An_atom(Peach),
                Empty))),
      Scons(An_atom(Fig),
        Scons(An_atom(Lemon),
          Empty)))),
      Scons(A_slist(
            Scons(An_atom(Peach),
              Empty)),
            Scons(An_atom(Lemon),
              Empty)));
