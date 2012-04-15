unit tw20796a;

{$MODE DELPHI}
{$DEFINE CRASHCOMPILER}

interface

type
  TWrapper<TValue> = class
  strict private
    FValue: TValue;
  public
    property Value: TValue read FValue write FValue;
  {$IFDEF CRASHCOMPILER}
    procedure SomeMethod;
  {$ENDIF}
  end;

  TTestClass = class
  public
    procedure DoTest;
  end;

implementation

{$IFDEF CRASHCOMPILER}
procedure TWrapper<TValue>.SomeMethod;
begin
end;
{$ENDIF}

procedure TTestClass.DoTest;
var
  w: TWrapper<Byte>;
begin
end;

end.

