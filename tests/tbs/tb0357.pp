{ %version=1.1 }

{$ifdef fpc}{$MODE OBJFPC}{$endif}
uses sysutils;
var
  p:pointer;
begin
  try
    getmem(p, 1000000000);
  except
    on eoutofmemory do writeln('out of memory!');
  end;
  writeln('program lasts...')
end.
