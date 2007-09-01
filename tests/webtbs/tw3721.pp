{ %opt=-gh}

uses sysutils;

var ps : pstring;

begin
  HaltOnNotReleased := true;  
  ps:=newstr('TEST');
  writeln(ps^);
  disposestr(ps);
end.
