{$ifdef fpc}{$mode delphi}{$endif}

type
parent = class
end;

child = class
 procedure test;
end;

procedure child.test;
begin
inherited;
end;

var
  o : child;
begin
  o:=child.create;
  o.test;
  o.free;
end.
