{ %opt=-gh}

uses sysutils;

var ps : pstring;

begin
  ps:=newstr('TEST');
  writeln(ps^);
  disposestr(ps);
end.
