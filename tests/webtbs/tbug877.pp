{$mode objfpc}

program testlist;
uses
  Sysutils,
  Classes;
var
  l: TList;
  IsCaught: boolean;

begin
  L:= TList.Create;
  IsCaught:=false;
  Try
     WriteLn(LongInt(L[0]));{L[0] not exist, ==> access violation}
     L.Free;
  Except
    on eListError do
      IsCaught:=true;
  end;
  If not IsCaught then
    begin
      Writeln('Error in TList');
      Halt(1);
    end;
end.
