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

