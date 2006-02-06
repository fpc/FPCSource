unit ugeneric3;

interface

{$mode objfpc}

type
   generic TList<_T>=class(TObject)
     data : _T;
     procedure Add(item: _T);
   end;

implementation

procedure TList.Add(item: _T);
begin
  data:=item;
end;

end.
