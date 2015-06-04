use "prelude.sml";

datatype meza = Shrimp
  | Clamari
  | Escargots
  | Hummus;

datatype main = Steak
  | Ravioli
  | Chicken
  | Eggplant;

datatype salad = Green
  | Cucumber
  | Greek;

datatype dessert = Sundae
  | Mousse
  | Torte;

fun add_a_steak(x:meza):(meza * main) = (x,Steak);

(add_a_steak : meza -> (meza * main));

add_a_steak(Shrimp);
add_a_steak(Hummus);
add_a_steak(Escargots);
(*
* no longer works after revison
add_a_steak(5);
*)

(*
fun remove_anchovy(Crust) = Crust
  | remove_anchovy(Anchovy(x)) = remove_anchovy(x)
  | remove_anchovy(C(x)) = C(remove_anchovy(x));

* Doesn't work because apparently we can't have placeholders for functions. hm.
*)

fun eq_main(Steak,Steak) = true
  | eq_main(Ravioli,Ravioli) = true
  | eq_main(Chicken,Chicken) = true
  | eq_main(Eggplant,Eggplant) = true
  | eq_main(a_main,another_main) = false;

eq_main(Steak,Steak);
eq_main(Steak,Eggplant);

fun has_steak(a:meza,Steak,d:dessert):bool = true
  | has_steak(a:meza,ns,d:dessert):bool = false;

(has_steak : (meza * main * dessert) -> bool);

has_steak(Hummus,Ravioli,Sundae);
has_steak(Shrimp,Steak,Mousse);

(*
* can no longer be called because of type checking.
has_steak(5,Steak,true);
has_steak(5,Ravioli,6);
*)

