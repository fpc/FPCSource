{ %opt=-vw -Sew }
{ %norun }
procedure p;
  var
    c : char;
    w : widechar;
    a : array[0..1] of longint;
    i : integer;
    e : (e1,e2,e3);
  begin
    writeln(length(c));
    writeln(length(w));
    writeln(length(a));
    writeln(low(i));
    writeln(high(i));
    writeln(ord(low(e)));
    writeln(ord(high(e)));
    writeln(low(a));
    writeln(high(a));
  end;

begin
end.
