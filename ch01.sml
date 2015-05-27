use "prelude.sml";

datatype seasoning = Salt
                   | Pepper;

Salt;
Pepper;

datatype num = Zero
             | One_more_than of num;

One_more_than(Zero);

One_more_than(
 One_more_than(
  Zero));

datatype 'a open_faced_sandwich = Bread of 'a
                                | Slice of 'a open_faced_sandwich;

Bread(0);

Bread(true);

Bread(
 One_more_than(
  Zero));

Bread(Bread(0));

Bread(
 Bread(
  One_more_than(
   Zero)));
