use "prelude.sml";
use "ch01.sml";

datatype shish_kehab = Skewer
                     | Onion of shish_kehab
                     | Lamb of shish_kehab
                     | Tomato of shish_kehab;

Skewer;

Onion(
 Skewer);

Onion(
 Lamb(
  Onion(
   Skewer)));

Lamb(
 Skewer);

Onion(
 Onion(
  Onion(
   Skewer)));

fun only_onions(Skewer) = true
  | only_onions(Onion(x)) = only_onions(x)
  | only_onions(Lamb(x)) = false
  | only_onions(Tomato(x)) = false;

(only_onions : shish_kehab -> bool);

only_onions(
 Onion(
  Onion(
   Skewer)));

only_onions(
 Onion(
  Lamb(
   Skewer)));

fun is_vegetarian(Skewer) = true
  | is_vegetarian(Lamb(x)) = false
  | is_vegetarian(Onion(x)) = is_vegetarian(x)
  | is_vegetarian(Tomato(x)) = is_vegetarian(x);

(is_vegetarian : shish_kehab -> bool);

is_vegetarian(
 Tomato(
  Onion(
   Tomato(
    Skewer))));

is_vegetarian(
 Onion(
  Onion(
   Onion(
    Skewer))));

is_vegetarian(
 Onion(
  Lamb(
   Skewer)));

datatype 'a shish = Bottom of 'a
                  | Onion of 'a shish
                  | Lamb of 'a shish
                  | Tomato of 'a shish;

datatype rod = Dagger
             | Fork
             | Sword;

datatype dish = Bowl
              | Plate
              | Platter;

Onion(
 Tomato(
  Bottom(Dagger)));

Onion(
 Tomato(
  Bottom(Plate)));

fun is_veggie(Bottom(x)) = true
  | is_veggie(Lamb(x)) = false
  | is_veggie(Onion(x)) = is_veggie(x)
  | is_veggie(Tomato(x)) = is_veggie(x);

(is_veggie : 'a shish -> bool);

is_veggie(
 Onion(
  Tomato(
   Bottom(Plate))));

is_veggie(
 Onion(
  Tomato(
   Bottom(Sword))));

is_veggie(
 Onion(
  Tomato(
   Bottom(52))));

is_veggie(
 Onion(
  Tomato(
   Bottom(
     One_more_than(Zero)))));

is_veggie(
 Onion(
  Tomato(
   Bottom(false))));

Onion(
  Tomato(
    Bottom(Dagger)));

Onion(
  Tomato(
    Bottom(Platter)));

Onion(
  Tomato(
    Bottom(52)));

fun what_bottom(Bottom(x)) = x
  | what_bottom(Lamb(x)) = what_bottom(x)
  | what_bottom(Onion(x)) = what_bottom(x)
  | what_bottom(Tomato(x)) = what_bottom(x);

(what_bottom : 'a shish -> 'a);

what_bottom(
  Onion(
    Tomato(
     Bottom(Dagger))));

what_bottom(Bottom(52));

what_bottom(Bottom(Sword));

what_bottom(
  Tomato(
    Onion(
      Lamb(
        Bottom(52)))));

what_bottom(
  Onion(
    Lamb(
      Bottom(52))));

what_bottom(
  Lamb(
    Bottom(52)));

what_bottom(
  Bottom(52));
