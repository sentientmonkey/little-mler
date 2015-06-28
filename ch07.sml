use "prelude.sml";
use "test.sml";
use "ch05.sml";

fun identity(x) = x;
(identity: 'a -> 'a);

fun true_maker(x) = true;
(true_maker: 'a -> bool);

datatype bool_or_int =
         Hot of bool
         | Cold of int;

Hot(true) : bool_or_int;
Cold(10) : bool_or_int;
Cold(5) : bool_or_int;

fun hot_maker(x) = Hot;
(hot_maker: 'a -> (bool -> bool_or_int));

fun help(f) = Hot(
      true_maker(
          if true_maker(5)
          then f
          else true_maker));

(help: ('a -> bool) -> bool_or_int);

datatype chain = Link of (int * (int -> chain));

fun ints(n) = Link(n+1,ints);
(ints: int -> chain);

ints(0);
ints(6);
ints(13);
ints(50005);

fun skips(n) = Link(n+2,skips);
(skips: int -> chain);

skips(8);
skips(17);

fun divides_evenly(n,c) = eq_int((n mod c),0);
(divides_evenly: (int * int) -> bool);

assert(divides_evenly(10,5));
assert_not(divides_evenly(10,3));

fun is_mod_5_or_7(n)
  = divides_evenly(n,5)
    orelse divides_evenly(n,7);
(is_mod_5_or_7: int -> bool);

assert(is_mod_5_or_7(15));
assert(is_mod_5_or_7(21));
assert_not(is_mod_5_or_7(36));

fun some_ints(n)
  = if is_mod_5_or_7(n+1)
     then Link(n+1,some_ints)
     else some_ints(n+1);
(some_ints: int -> chain);

some_ints(1);
some_ints(17);
some_ints(116);

ints(0);
some_ints(0);

fun chain_item(n,Link(i,f))
  = if eq_int(n,1)
      then i
      else chain_item(n-1,f(i));
(chain_item: (int * chain) -> int);

assert_equal(chain_item(1,some_ints(0)), 5);
assert_equal(chain_item(6,some_ints(0)), 20);
assert_equal(chain_item(37,some_ints(0)), 119);

fun is_prime(n)
    = has_nodivisors(n,n-1)
and has_nodivisors(n,c)
    = if eq_int(c,1)
        then true
        else if divides_evenly(n,c)
               then false
               else has_nodivisors(n,c-1);
(is_prime: int -> bool);
(has_nodivisors: (int * int) -> bool);

assert(is_prime(3));
assert_not(is_prime(81));

fun primes(n)
  = if is_prime(n+1)
      then Link(n+1,primes)
      else primes(n+1);
(primes: int -> chain);

assert_equal(chain_item(12,primes(1)), 37);

fun fibs(n)(m)
  = Link(n + m, fibs(m));
(fibs: int -> (int -> chain));

Link(0,fibs(1)) : chain;

fibs(1) : (int -> chain);

fun fibs_1(m)
  = Link(1 + m,fibs(m));
(fibs_1: int -> chain);

fibs(1)(1) : chain;
fibs_1(1) : chain;
fibs_1(2) : chain;
fibs(2) : (int -> chain);

fun fibs_2(m)
  = Link(2 + m,fibs(m));
(fibs_2: int -> chain);

fibs_2(0) : chain;
