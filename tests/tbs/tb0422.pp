{$ifdef fpc}{$mode delphi}{$endif}

type
  tcl = class
    function f1 : tvarrec; virtual;
  end;

var
   f : function : tvarrec of object;

function tcl.f1 : tvarrec;
begin
  fillchar(result,sizeof(result),0);
end;


procedure p1(v : tvarrec);
  begin
  end;


var
  c : tcl;
begin
   c:=tcl.create;
   f:=c.f1;
   p1(f);
end.
