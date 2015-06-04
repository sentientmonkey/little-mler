use "prelude.sml";
use "test.sml";

datatype seasoning = Salt
                   | Pepper;

Salt : seasoning;
Pepper : seasoning;

datatype num = Zero
             | One_more_than of num;

One_more_than(Zero) : num;

One_more_than(
 One_more_than(
  Zero)) : num;

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
