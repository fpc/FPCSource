{$ifdef fpc}{$mode delphi}{$endif}

function f1:pointer;
begin
  result:=nil;
end;

var
  func: function:pointer;
begin
  func:=f1;
  { Assigned() works on the procvar and does not
    call func }
  if not assigned(func) then
   begin
     writeln('ERROR!');
     halt(1);
   end;
end.
