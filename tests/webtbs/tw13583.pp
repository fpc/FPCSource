{ %recompile }

uses
  uw13583;

begin
  writeln(DEFAULT_SIGNATURE);
  if ('edb_signature' <> DEFAULT_SIGNATURE) then
    halt(1);
  writeln(DEFAULT_SIGNATURE2);
  if ('edb_signature2' <> DEFAULT_SIGNATURE2) then
    halt(1);
end.
