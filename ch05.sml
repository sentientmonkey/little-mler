use "prelude.sml";
use "test.sml";

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

(rem_anchovy: (fish pizza) -> (fish pizza));
