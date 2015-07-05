use "prelude.sml";
use "test.sml";
use "ch05.sml";
use "ch06.sml";

datatype 'a list = Empty
  | Cons of 'a * 'a list;

datatype orapl = Orange
  | Apple;

fun eq_orapl(Orange,Orange) = true
  | eq_orapl(Apple,Apple) = true
  | eq_orapl(one, another) = false;

(eq_orapl: (orapl * orapl) -> bool);

fun subst_int(n,a,Empty) = Empty
  | subst_int(n,a,Cons(e,t))
    = if eq_int(a,e)
        then Cons(n,subst_int(n,a,t))
        else Cons(e,subst_int(n,a,t));

(subst_int: (int * int * int list) -> int list);

subst_int(11,15,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

fun subst_orqpl(n,a,Empty) = Empty
  | subst_orqpl(n,a,Cons(e,t))
    = if eq_orapl(a,e)
        then Cons(n,subst_orqpl(n,a,t))
        else Cons(e,subst_orqpl(n,a,t));

(subst_orqpl: (orapl * orapl * orapl list) -> orapl list);

subst_orqpl(Orange,Apple,
  Cons(Apple,
    Cons(Orange,
      Cons(Apple,
        Cons(Orange,
          Empty)))));

fun subst(rel,n,a,Empty) = Empty
  | subst(rel,n,a,Cons(e,t))
    = if rel(a,e)
        then Cons(n,subst(rel,n,a,t))
        else Cons(e,subst(rel,n,a,t));

(subst: ((('b * 'a) -> bool) * 'a * 'b * 'a list) -> 'a list);

subst(eq_int,11,15,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

assert(less_than(11,15));

subst(less_than,11,15,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));


fun in_range((small,large),x)
  = if less_than(small,x)
      then less_than(x,large)
      else false;

(in_range: ((int * int) * int) -> bool);

assert(in_range((11,16), 15));

assert_not(in_range((11,15), 15));

subst(in_range,22,(11,16),
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

fun subst_pred(pred,n,Empty) = Empty
  | subst_pred(pred,n,Cons(e,t))
    = if pred(e)
        then Cons(n,subst_pred(pred,n,t))
        else Cons(e,subst_pred(pred,n,t));

(subst_pred: (('a -> bool) * 'a *  'a list) -> 'a list);

fun is_15(x)
  = eq_int(x,15);
(is_15: int -> bool);

subst_pred(is_15,11,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

fun less_than_15(x)
  = less_than(x,15);
(less_than_15: int -> bool);

subst_pred(less_than_15,11,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

fun in_range_11_16(x)
  = if less_than(11,x)
      then less_than(x,16)
      else false;
(in_range_11_16: int -> bool);

subst_pred(in_range_11_16,22,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

fun in_range_c(small,large)(x)
  = if less_than(small,x)
      then less_than(x,large)
      else false;

(in_range_c: (int * int) -> (int -> bool));

assert(in_range_c(11,16)(15));

fun in_range_c_11_16(x)
  = if less_than(11,x)
      then less_than(x,16)
      else false;
(in_range_c_11_16: int -> bool);

subst_pred(in_range_c(11,16),22,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

subst_pred(in_range_c(3,16),22,
  Cons(15,
    Cons(6,
      Cons(15,
        Cons(17,
          Cons(15,
            Cons(8,
              Empty)))))));

fun subst_c(pred)(n,Empty) = Empty
  | subst_c(pred)(n,Cons(e,t))
    = if pred(e)
        then Cons(n,subst_c(pred)(n,t))
        else Cons(e,subst_c(pred)(n,t));

(subst_c: ('a -> bool) -> (('a * ('a list)) -> 'a list));

fun subst_c_in_range_11_16(n,Empty) = Empty
  | subst_c_in_range_11_16(n,Cons(e,t))
    = if in_range_11_16(e)
        then
          Cons(n,subst_c_in_range_11_16(n,t))
        else
          Cons(e,subst_c_in_range_11_16(n,t));

(subst_c_in_range_11_16: (int * int list) -> int list);

fun combine(Empty,l2) = l2
  | combine(Cons(a,l1),l2)
    = Cons(a,combine(l1,l2));

(combine: (('a list) * ('a list)) -> 'a list);

(combine(
  Cons(1,
    Cons(2,
      Cons(3,
        Empty))),
  Cons(5,
    Cons(4,
      Cons(7,
        Cons(9,
          Empty))))));

(combine(
  Cons(1,
    Cons(2,
      Cons(3,
        Empty))),
  Cons(12,
    Cons(11,
      Cons(5,
        Cons(7,
          Empty))))));

fun combine_c(Empty)(l2) = l2
  | combine_c(Cons(a,l1))(l2)
    = Cons(a,combine_c(l1)(l2));

(combine_c: 'a list -> ('a list -> 'a list));

combine_c(
  Cons(1,
    Cons(2,
      Cons(3,
        Empty))));

fun prefixer_123(l2)
  = Cons(1,
      Cons(2,
        Cons(3,
          l2)));

(prefixer_123: int list -> int list);

fun waiting_prefix_123(l2)
  = Cons(1,
      combine_c(
        Cons(2,
          Cons(3,
            Empty)))
            (l2));

(waiting_prefix_123: int list -> int list);

fun base(l2)
  = l2;

(base: 'a list -> 'a list);

fun combine_s(Empty) = base
  | combine_s(Cons(a,l1))
    = make_cons(a,combine_s(l1))
and
  make_cons(a,f)(l2)
  = Cons(a,f(l2));

(combine_s: 'a list -> ('a list -> 'a list));
(make_cons: ('a * ('a list -> 'a list)) -> ('a list -> 'a list));

combine_s(
  Cons(1,
    Cons(2,
      Cons(3,
        Empty))));

make_cons(3, base);

fun prefix_3(l2)
  = Cons(3,base(l2));
(prefix_3: int list -> int list);

make_cons(2, prefix_3);

fun prefix_23(l2)
  = Cons(2,prefix_23(l2));
(prefix_23: int list -> int list);

make_cons(1, prefix_23);

fun prefix_123(l2)
  = Cons(1,prefix_123(l2));
(prefix_123: int list -> int list);
