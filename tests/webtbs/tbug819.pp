{$mode objfpc}
type
  T1 = class
    function Get(I: Integer): Integer; virtual; abstract;
    property T[I: Integer]: Integer read Get; default;
  end;

  T2 = class(T1)
    function Get(I: Integer): Integer; override;
    property T[I: Integer]: Integer read Get; default;
  end;

function T2.Get(I: Integer): Integer;
begin
  Result:=I;
end;

var
   c2 : t2;

begin
   c2:=t2.create;
   if c2[9]<>9 then
     halt(1)
   else
     halt(0);
end.
