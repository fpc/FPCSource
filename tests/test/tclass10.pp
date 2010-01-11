program tclass10;
{$ifdef fpc}
  {$mode delphi}
{$endif}

type

  { c }

  c=class
   type
     TSomeType = (st1, st2);
    function DoSomething: TSomeType;
  end;

{ c }

function c.DoSomething: TSomeType;
begin
  Result := st1;
end;

begin
end.