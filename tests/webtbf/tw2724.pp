{ %fail }

{ Source provided for Free Pascal Bug Report 2724 }
{ Submitted by "marco" on  2003-10-08 }
{ e-mail:  }
{$ifdef fpc}{$Mode delphi}{$endif}
type
  TBitSet = class(TBits)
    constructor Create(aSize: integer);
  end;

{ TBitSet }

constructor TBitSet.Create(aSize: integer);
begin
  inherited Create;
  Size := aSize;
end;

begin
end.
