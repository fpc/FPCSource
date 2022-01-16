unit uw35918b;

{$mode objfpc}{$H+}
{$ModeSwitch prefixedattributes}

interface

uses
  Classes, SysUtils, uw35918c;

type

  { TTest1 }

  TTest1 = class
  private
    FField1: String;
  published
    [Attruvute()]
    property Field1: String read FField1;
  end;

implementation

end.

