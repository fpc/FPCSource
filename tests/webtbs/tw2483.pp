{$ifdef fpc}{$mode delphi}{$endif}

type
  TUpdateProc = procedure( Self : TObject; n : Integer ) of object;

  TCl = class
     FOnUpdate : TUpdateProc;
     procedure HandleUpdate(obj:tobject;n:integer);
     procedure p;
   end;

procedure tcl.HandleUpdate(obj:tobject;n:integer);
begin
  writeln(n);
end;

procedure tcl.p;
begin
  FOnUpdate := HandleUpdate;
  FOnUpdate( Self, 1 );
end;


var
  c  : TCl;
begin
  c:=TCl.create;
  c.p;
end.
