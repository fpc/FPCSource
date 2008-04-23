function f(c: char): char; overload;
begin
  halt(1);
end;

function f(const s: shortstring): shortstring; overload;
begin
  f:=lowercase(s);
end;

function f(const a: ansistring): ansistring; overload;
begin
  halt(3);
end;

Procedure DoIt;
var avar:variant;
      txt:String;
Begin
avar:='Hello';
txt:=f(avar);//this line causes the compilation error
if (txt<>'hello') then
  halt(4);
end;

begin
  doit;
end.
