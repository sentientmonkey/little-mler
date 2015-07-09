use "prelude.sml";
use "test.sml";
use "ch05.sml";

fun is_zero(n)
  = eq_int(n,0);
(is_zero: int -> bool);

exception Too_small;

fun pred(n)
  = if eq_int(n,0)
    then raise Too_small
    else n - 1;
(pred: int -> int);

fun succ(n)
  = n + 1;
(succ: int -> int);

fun plus(n,m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n),m));
(plus: (int * int) -> int);

assert_equal(plus(0,1), 1);
assert_equal(plus(1,1), 2);
assert_equal(plus(2,1), 3);

datatype num = Zero
  | One_more_than of num;

fun is_zero(Zero) = true
  | is_zero(not_zero) = false;

exception Too_small;

fun pred(Zero)
  = raise Too_small
  | pred(One_more_than(n)) = n;
(pred: num -> num);

fun succ(n) = One_more_than(n);
(succ: num -> num);

fun plus(n,m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n),m));
(plus: (num * num) -> num);

plus(
  One_more_than(
    One_more_than(
      Zero)),
  One_more_than(
    One_more_than(
      One_more_than(
        Zero))));

signature N =
  sig
    type number
    exception Too_small
    val succ: number -> number
    val pred: number -> number
    val is_zero: number -> bool
  end

functor NumberAsNum()
  :>
  N
  =
  struct
    datatype num = Zero
    | One_more_than of num;
    type number = num;
    exception Too_small;
    fun succ(n) = One_more_than(n);
    fun pred(Zero) = raise Too_small
      | pred(One_more_than(n)) = n;
    fun is_zero(Zero) = true
      | is_zero(a_num) = false;
  end

functor NumberAsInt()
  :>
  N
  =
  struct
    type number = int;
    exception Too_small;
    fun succ(n)
      = n + 1;
    fun pred(n)
      = if eq_int(n,0)
        then raise Too_small
        else n - 1;
    fun is_zero(n) = eq_int(n,0);
  end

structure IntStruct = NumberAsInt();
structure NumStruct = NumberAsNum();

signature P =
  sig
    type number
    val plus: (number * number) -> number
  end

functor PON(structure a_N : N)
  :>
  P
  =
  struct
    type number = a_N.number
    fun plus(n,m)
      = if a_N.is_zero(n)
        then m
        else a_N.succ(
              plus(a_N.pred(n),m))
  end

structure IntAirth = PON(structure a_N = IntStruct);

signature N_C_R =
  sig
    type number
    exception Too_small
    val conceal: int -> number
    val succ: number -> number
    val pred: number -> number
    val is_zero: number -> bool
    val reveal: number -> int
  end

functor NumberAsInt()
  :>
  N_C_R
  =
  struct
    type number = int
    exception Too_small
    fun conceal(n)
      = n
    fun succ(n)
      = n + 1
    fun pred(n)
      = if eq_int(n,0)
          then raise Too_small
          else n - 1
    fun is_zero(n)
      = eq_int(n,0)
    fun reveal(n)
      = n
  end

functor NumberAsNum()
  :>
  N_C_R
  =
  struct
    datatype num = Zero
      | One_more_than of num
    type number = num
    exception Too_small
    fun conceal(n)
      = if eq_int(n,0)
        then Zero
        else One_more_than(
              conceal(n - 1))
    fun succ(n)
      = One_more_than(n)
    fun pred(Zero) = raise Too_small
      | pred(One_more_than(n)) = n
    fun is_zero(Zero) = true
      | is_zero(a_num) = false
    fun reveal(n)
      = if is_zero(n)
          then 0
          else 1 + reveal(pred(n))
  end

structure IntStruct = NumberAsInt();
structure IntArith = PON(structure a_N = IntStruct);

structure NumStruct = NumberAsNum();
structure NumArith = PON(structure a_N = NumStruct);

assert_equal(
  NumStruct.reveal(
    NumStruct.succ(
      NumStruct.conceal(0))),
  1);

functor PON(structure a_N : N)
  :>
  P where type number = a_N.number
  =
  struct
    type number = a_N.number
    fun plus(n,m)
      = if a_N.is_zero(n)
          then m
          else a_N.succ(
                  plus(a_N.pred(n),m))
  end

structure NumArith = PON(structure a_N = NumStruct);
structure IntArith = PON(structure a_N = IntStruct);

assert_equal(
  NumStruct.reveal(
    NumArith.plus(
      NumStruct.conceal(1),
      NumStruct.conceal(2))),
  3);

assert_equal(
  IntStruct.reveal(
    IntArith.plus(
      IntStruct.conceal(1),
      IntStruct.conceal(2))),
  3);
