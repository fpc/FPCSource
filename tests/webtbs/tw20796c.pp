unit tw20796c;

{$MODE OBJFPC}
{$DEFINE CRASHCOMPILER}

interface

type
  generic TWrapper<TValue> = class
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
procedure TWrapper.SomeMethod;
begin
end;
{$ENDIF}

procedure TTestClass.DoTest;
type
  TByteWrapper = specialize TWrapper<Byte>;
var
  w: TByteWrapper;
begin
end;

end.
