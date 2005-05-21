{$ifdef UNIX}
uses
  cwstring;
{$endif UNIX}

var
  i : longint;
  w,w2 : widestring;
  a : ansistring;
  
begin
  setlength(w,1000);
  for i:=1 to 1000 do
    w[i]:=widechar(i);
  for i:=1 to 10 do
    begin
      a:=w;
      w2:=a;
    end;
end.
