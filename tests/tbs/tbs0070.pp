Program Test;

type
  myenum = (YES,NO,MAYBE);
var
 myvar:set of myenum;
Begin
 Include(myvar,Yes);
 Exclude(myvar,No);
end.
