{ boolean args are accepted for fillchar in BP }

program test;

  var l : array[1..10] of boolean;

begin
   fillchar(l,sizeof(l),true);
end.

