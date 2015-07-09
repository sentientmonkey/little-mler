use "prelude.sml";
use "test.sml";
use "ch05.sml";

datatype 'a list = Empty
  | Cons of 'a * 'a list;

datatype box = Bacon
  | Ix of int;

fun is_bacon(Bacon) = true
  | is_bacon(Ix(n)) = false;

(is_bacon: box -> bool);

fun where_is(Empty) = 0
  | where_is(Cons(a_box,rest))
    = if is_bacon(a_box)
        then 1
        else 1 + where_is(rest);

(where_is: box list -> int);

assert_equal(
  where_is(
    Cons(Ix(5),
      Cons(Ix(13),
        Cons(Bacon,
          Cons(Ix(8),
            Empty))))),
  3);

assert_equal(
  where_is(
    Cons(Bacon,
      Cons(Ix(8),
        Empty))),
  1);

assert_equal(
  where_is(
    Cons(Ix(5),
      Cons(Ix(13),
        Cons(Ix(8),
          Empty)))),
  3);

exception No_bacon of int;

fun where_is(Empty) = raise No_bacon(0)
  | where_is(Cons(a_box,rest))
    = if is_bacon(a_box)
      then 1
      else 1 + where_is(rest);

(where_is: box list -> int);

assert_equal(
  where_is(
    Cons(Ix(5),
      Cons(Ix(13),
        Cons(Ix(8),
          Empty))))
    handle
      No_bacon(an_int)
      => an_int,
   0);

assert_equal(
  where_is(
    Cons(Ix(5),
      Cons(Bacon,
        Cons(Ix(8),
          Empty))))
    handle
      No_bacon(an_int)
      => an_int,
   2);

exception Out_of_range;

fun list_item(n,Empty) = raise Out_of_range
  | list_item(n,Cons(abox,rest))
    = if eq_int(n,1)
      then abox
      else list_item(n - 1, rest);

(list_item: (int * box list) -> box);

fun
  find(n,boxes)
  = check(n,boxes,list_item(n,boxes))
and
  check(n,boxes,Bacon) = n
  | check(n,boxes,Ix(i)) = find(i,boxes);

(find: (int * (box list)) -> int);
(check: (int * (box list) * box) -> int);

val t = Cons(Ix(5),
          Cons(Ix(4),
            Cons(Bacon,
              Cons(Ix(2),
                Cons(Ix(7),
                  Empty)))));

assert_equal(
  find(1,t)
    handle Out_of_range
    => 0,
  0);

assert_equal(
  find(5,t)
    handle Out_of_range
    => 0,
  0);

assert_equal(
  find(7,t)
    handle Out_of_range
    => 0,
  0);

assert_equal(
  find(3,t),
  3);

assert_equal(8 div 2, 4);
assert_equal(7 div 2, 3);

fun find(n,boxes)
  = (check(n,boxes,list_item(n,boxes))
     handle
     Out_of_range
     => find(n div 2, boxes))
and
  check(n,boxes,Bacon) = n
  | check(n,boxes,Ix(i)) = find(i,boxes);

(find: (int * (box list)) -> int);
(check: (int * (box list) * box) -> int);

assert_equal(find(1,t), 3);

fun path(n,boxes)
  = Cons(n,
      (check(boxes,list_item(n,boxes))
       handle
       Out_of_range
       => path(n div 2,boxes)))
and
  check(boxes,Bacon) = Empty
  | check(boxes,Ix(i)) = path(i,boxes);

(path: (int * (box list)) -> (int list));
(check: ((box list) * box) -> (int list));

path(1,t);
