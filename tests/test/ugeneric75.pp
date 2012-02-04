unit ugeneric75;

{$mode objfpc}

interface

uses
  ugeneric.test75;

type
  generic TGeneric<T> = class
    procedure Test;
  end;

implementation

procedure TGeneric.Test;
begin
  ugeneric.test75.Test;
end;

end.
