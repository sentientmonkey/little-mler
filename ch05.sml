use "prelude.sml";
use "ch01.sml";

datatype 'a pizza = Bottom
  | Topping of ('a * ('a pizza));

datatype fish = Anchovy
  | Lox
  | Tuna;

Topping(Anchovy,
  Topping(Tuna,
    Topping(Anchovy,
      Bottom)));

Topping(Tuna,
  Topping(Anchovy,
    Bottom));

Topping(Anchovy, Bottom);

fun rem_anchovy(Bottom) = Bottom
  | rem_anchovy(Topping(Anchovy,p)) = rem_anchovy(p)
  | rem_anchovy(Topping(t,p)) = Topping(t,rem_anchovy(p));

(rem_anchovy: (fish pizza) -> (fish pizza));

fun rem_tuna(Bottom) = Bottom
  | rem_tuna(Topping(Tuna,p)) = rem_tuna(p)
  | rem_tuna(Topping(t,p)) = Topping(t,rem_tuna(p));

(rem_tuna: (fish pizza) -> (fish pizza));

fun rem_fish(x,Bottom) = Bottom
  | rem_fish(Tuna,Topping(Tuna,p)) = rem_fish(Tuna,p)
  | rem_fish(Tuna,Topping(t,p)) = Topping(t,rem_fish(Tuna,p))
  | rem_fish(Anchovy,Topping(Anchovy,p)) = rem_fish(Anchovy,p)
  | rem_fish(Anchovy,Topping(t,p)) = Topping(t,rem_fish(Anchovy,p))
  | rem_fish(Lox,Topping(Lox,p)) = rem_fish(Lox,p)
  | rem_fish(Lox,Topping(t,p)) = Topping(t,rem_fish(Lox,p));

(rem_fish: (fish * (fish pizza)) -> (fish pizza));

fun eq_fish(Anchovy,Anchovy) = true
  | eq_fish(Tuna,Tuna) = true
  | eq_fish(Lox,Lox) = true
  | eq_fish(a_fish, another_fish) = false;

(eq_fish: (fish * fish) -> bool);

assert(eq_fish(Anchovy,Anchovy));
assert(not(eq_fish(Anchovy,Tuna)));

fun rem_fish(x,Bottom) = Bottom
  | rem_fish(x,Topping(t,p))
  = if eq_fish(t,x)
    then rem_fish(x,p)
    else Topping(t,(rem_fish(x,p)));

(rem_fish: (fish * (fish pizza)) -> (fish pizza));

assert_equal(
  rem_fish(Anchovy,
    Topping(Anchovy,
      Bottom)),
  Bottom);

assert_equal(
  rem_fish(Tuna,
    Topping(Anchovy,
      Topping(Tuna,
        Topping(Anchovy,
          Bottom)))),
    Topping(Anchovy,
      Topping(Anchovy,
        Bottom)));

fun eq_int(x:int,y:int) = (x = y);
(eq_int: (int * int) -> bool);

fun rem_int(x,Bottom) = Bottom
  | rem_int(x,Topping(t,p))
  = if eq_int(t,x)
    then rem_int(x,p)
    else Topping(t,(rem_int(x,p)));

(rem_int: (int * (int pizza)) -> (int pizza));

assert_equal(
  rem_int(3,
    Topping(2,
      Topping(3,
        Topping(2,
          Bottom)))),
  Topping(2,
    Topping(2,
      Bottom)));

fun subst_fish(n,a,Bottom) = Bottom
  | subst_fish(n,a,Topping(t,p))
  = if eq_fish(t,a)
    then Topping(n,subst_fish(n,a,p))
    else Topping(t,subst_fish(n,a,p));

(subst_fish: (fish * fish * (fish pizza)) -> (fish pizza));

assert_equal(
  subst_fish(Lox,Anchovy,
    Topping(Anchovy,
      Topping(Tuna,
        Topping(Anchovy,
          Bottom)))),
    Topping(Lox,
      Topping(Tuna,
        Topping(Lox,
          Bottom))));

fun subst_int(n,a,Bottom) = Bottom
  | subst_int(n,a,Topping(t,p))
  = if eq_int(t,a)
    then Topping(n,subst_int(n,a,p))
    else Topping(t,subst_int(n,a,p));

(subst_int: (int * int * (int pizza)) -> (int pizza));

assert_equal(
  subst_int(5,3,
    Topping(3,
      Topping(2,
        Topping(3,
          Bottom)))),
  Topping(5,
    Topping(2,
      Topping(5,
        Bottom))));

assert(not(eq_int(17,0)));

fun eq_num(Zero,Zero) = true
  | eq_num(One_more_than(n),Zero) = false
  | eq_num(Zero,One_more_than(n)) = false
  | eq_num(One_more_than(n),One_more_than(m))
  = eq_num(n,m);

assert(
  eq_num(
    One_more_than(
      Zero),
    One_more_than(
      Zero)));

