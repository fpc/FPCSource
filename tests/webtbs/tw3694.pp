program bug1;

function f : tobject;
begin
  f := tobject.create;
end;

var
{$ifdef CPU64}
  s : double;
{$else}
  s : single;
{$endif CPU64}
begin
{$ifdef CPU64}
  s := double(f);
{$else CPU64}
  s := single(f);
{$endif CPU64}
end.
