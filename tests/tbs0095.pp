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
