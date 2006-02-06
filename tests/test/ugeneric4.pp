unit ugeneric4;

interface

{$mode objfpc}

type
   generic TList<_T>=class(TObject)
     data : _T;
     procedure Fill;
   end;

var
  globaldata : string;

implementation

procedure LocalFill;
begin
  globaldata:='Unit';
end;


procedure TList.Fill;
begin
  LocalFill;
  data:=globaldata;
end;

end.
