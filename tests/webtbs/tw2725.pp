{ Source provided for Free Pascal Bug Report 2725 }
{ Submitted by "marco" on  2003-10-08 }
{ e-mail:  }

{$Mode delphi}

Uses Classes;

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
