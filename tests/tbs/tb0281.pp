{ Old file: tbs0333.pp }
{  }

{$ifdef cpu68k}
  {$define COMP_IS_INT64}
{$endif cpu68k}


var
  a,b : comp;
  s1,s2 : string;
begin
  a:=11384563;
  b:=a*a;
{$ifdef COMP_IS_INT64}
  str(a*a,s1);
  str(b,s2);
{$else not COMP_IS_INT64}
  str(a*a:0:0,s1);
  str(b:0:0,s2);
{$endif COMP_IS_INT64}
  writeln(s1);
  writeln(s2);
  if (s1<>'129608274700969') or (s2<>'129608274700969') then
   begin
     writeln('Error with comp type rounding');
     halt(1);
   end;
end.
