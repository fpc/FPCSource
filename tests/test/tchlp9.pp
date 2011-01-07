{%NORUN}

{ class helper inheritance syntax }
program tchlp9;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelperA = class helper for TObject
    procedure SomeMethodA;
  end;

  TObjectHelperB = class helper(TObjectHelperA) for TObject
    procedure SomeMethodB;
  end;

procedure TObjectHelperA.SomeMethodA;
begin

end;

procedure TObjectHelperB.SomeMethodB;
begin

end;

begin

end.

