{ Old file: tbs0217.pp }
{ in tp mode can't use the procvar in writeln          OK 0.99.11 (PFV) }

{$ifdef fpc}{$mode tp}{$endif}

type tmpproc=function:longint;

function a:longint;{$ifndef fpc}far;{$endif}
begin
 a:=-1;
end;

procedure tmp(aa: tmpproc);
begin
  writeln(aa);           { "Cannot read/write variables of this type", TP kan dit
wel? }
  if aa<>-1 then halt(1);
end;

begin
  tmp(a);               { de TP manier , in FPC moet dit zijn tmp(@a); }
end.
