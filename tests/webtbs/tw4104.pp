{ Source provided for Free Pascal Bug Report 4104 }
{ Submitted by "Daniël Mantione" on  2005-06-22 }
{ e-mail: daniel@freepascal.org }
program bug;

type junk=record
       data:ansistring;
     end;

operator :=(x:longint) result:junk;

begin
  str(x,result.data);
end;

procedure write_junk(const data:array of junk);

var i:cardinal;

begin
  for i:=low(data) to high(data) do
   begin
     write(data[i].data);
     write('<-->');
     writeln(Pchar(data[i].data));
   end;
end;

begin
  write_junk([1,2]);
end.
