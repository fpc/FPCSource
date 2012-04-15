unit tw20796b;

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
type
  TByteWrapper = TWrapper<Byte>;
var
  w: TByteWrapper;
begin
end;

end.
