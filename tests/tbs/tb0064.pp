{ Old file: tbs0070.pp }
{  shows missing include and exclude from rtl           OK 0.99.6 (MVC) }

Program Test;

type
  myenum = (YES,NO,MAYBE);
var
 myvar:set of myenum;
Begin
 Include(myvar,Yes);
 Exclude(myvar,No);
end.
