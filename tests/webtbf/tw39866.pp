{ %FAIL }

program RTTITest;

{$mode objfpc}{$h+}

uses
  SysUtils, Classes, TypInfo;

type
  TMyEnum = (meOne=1, meThree=3, meFive=5, meSix);

  TMyClass = class(TPersistent)
  private
    FEnum: TMyEnum;
  Published
    property Enum: TMyEnum read FEnum write FEnum;
  end;

begin
end.
