{$ifdef fpc}{$mode tp}{$endif}

type tmpproc=function:longint;

function a:longint;{$ifndef fpc}far;{$endif}
begin
 a:=-1;
end;

procedure tmp(a: tmpproc);
begin
  writeln(a);           { "Cannot read/write variables of this type", TP kan dit
wel? }
end;

begin
  tmp(a);               { de TP manier , in FPC moet dit zijn tmp(@a); }
end.
