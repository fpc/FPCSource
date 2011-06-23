{ %skiptarget=win32,win64 }
{ %needlibrary }
{ %fail }
{ %opt=-vw -Sew }
{ %neededafter }

program tlib3b;

  procedure p_proc(var a : dword); external 'tlib3a' name 'p';

  function p(a1, a2, a3, a4, a5, a6, a7 : dword) : dword; external 'tlib3b' name 'p';

  var
    a : dword;
begin
  a:=0;
  p_proc(a);
  if a<>1 then
    begin
      Writeln('Error calling tlib3a p procedure');
      halt(1);
    end;
  a:=p(0,1,0,1,0,10,0);
  if a<>12 then
    begin
      Writeln('Error calling tlib3b p function');
      halt(1);
    end;
  Writeln('Everything works OK');
end.
