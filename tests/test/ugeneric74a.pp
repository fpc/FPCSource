unit ugeneric74a;

{$mode objfpc}

interface

uses
  ugeneric74b;

type
  generic TGeneric<T> = class
    procedure Test;
  end;

implementation

procedure TGeneric.Test;
begin
  ugeneric74b.Test;
end;


end.
