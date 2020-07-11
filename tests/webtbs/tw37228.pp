{$mode objfpc} {$h+}
var
  i: longint;

function CreateString: pString;
begin
    if i<>0 then
      halt(1);
    inc(i);
    writeln('Creating a string');
    new(result);
end;

begin
    dispose(CreateString); // prints “Creating a string” twice
    if i<>1 then
      halt(2);
end.
