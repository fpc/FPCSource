{ %recompile }

uses
  uw28964;

begin
  if stringcodepage(externalconst)<>CP_UTF8 then
    halt(1);
end.
