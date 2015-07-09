exception Failed;

fun assert(expected) =
  if expected <> true then
    raise Failed
  else true;

fun assert_not(expected) =
  if expected <> false then
    raise Failed
  else true;

fun assert_equal(expected,actual) =
  if expected <> actual then
    raise Failed
  else true;
