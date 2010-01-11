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
    var
      FPrivateField: PrivateType;
  public
    procedure DoSomething(Value: PrivateType = PrivateConst);
    function ReturnSomething: PrivateType;
    property SomeProp: PrivateType read FPrivateField write FPrivateField default PrivateConst;
  end;

  procedure TSomeClass.DoSomething(Value: PrivateType = PrivateConst);
  begin
  end;

  function TSomeClass.ReturnSomething: PrivateType;
  begin
    Result := PrivateConst;
  end;

begin
end.