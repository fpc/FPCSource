program test;
{$INLINE ON}

var
  s2 : string;
  j : longint;

  procedure Tst(s: ShortString;var j : longint); inline;
  var
    i : longint;
  begin
    s:=s + ' Yes';
    i:=5;
    j:=j+i;
    WriteLn(s);
    s2:=s;
  end;
begin
   s2:='Before inline';
   j:=5;
   Tst('Hello Hello Hello',j);
   if (s2<>'Hello Hello Hello Yes') or (j<>10) then
     begin
       if (s2<>'Hello Hello Hello Yes') then
         writeln('s2 = ',s2);
       if (j<>10) then
         writeln('j = ',s2);
       halt(1);
     end;
end.
