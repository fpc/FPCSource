program Project1;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}


uses
  Classes;

type

  { TMyAttribute }

  TMyAttribute = class(TCustomAttribute)
    constructor Create(AVal: Boolean);
  private
    FVal: Boolean;
  published
    property Val: Boolean read FVal;
  end;

  TMyObject = class
  public
    type
      TFoobar = (fbOne, fbTwo);
      [TMyAttribute(False)] // this works fine
      TFoo = (fooOne, fooTwo);
  public
    type
      [TMyAttribute(False)] // project1.lpr(30,7) Error: Syntax error, "identifier" expected but "[" found
      TBar = (barOne, barTwo);
  end;

{ TMyAttribute }

constructor TMyAttribute.Create(AVal: Boolean);
begin
  FVal := AVal;

end;

begin

end.
