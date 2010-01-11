program tclass12c;
{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TSomeClass = class
  strict private
    const
      PrivateConst = 1;
    type
      PrivateType = type Integer;
  public
    procedure DoSomething(Value: PrivateType = PrivateConst);
  end;

  procedure TSomeClass.DoSomething(Value: PrivateType = PrivateConst);
  begin
  end;

begin
end.
