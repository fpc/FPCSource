{ %OPT=-gh }

program tw35862;

{$modeswitch result}

uses sysutils;

function do_term:boolean;
begin
  writeln('In terminate proc');
  Result:=true;
end;

begin
  HaltOnNotReleased:=True;

  writeln('Adding terminate proc');
  AddTerminateproc(@do_term);
  writeln('terminating');
  CallterminateProcs;
end.
