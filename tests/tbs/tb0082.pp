{ Old file: tbs0095.pp }
{ case with ranges starting with #0 bugss                OK 0.99.1 (FK) }

var
  ch : char;
begin
  ch:=#3;
  case ch of
   #0..#31 : ;
  else
   writeln('bug');
  end;
  case ch of
   #0,#1,#3 : ;
  else
   writeln('bug');
  end;
end.
