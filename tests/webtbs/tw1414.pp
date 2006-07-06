{ %skiptarget=wince }
uses
  dos;
 var
   i : longint;
begin
{$i-}
  chdir('');
  i:=ioresult;
  if i<>0 then
    begin
      Writeln('chdir('''') fails wih error ',i);
      Halt(i);
    end;
end.
