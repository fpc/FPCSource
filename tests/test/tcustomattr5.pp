{ %fail }
program tcustomattr5;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type

  { tmyt }

  tmyt = class(TCustomAttribute)
  private
    FID: integer;
  public
    constructor create(Id: integer);
  end;

type
  // Delphi XE does compile attributes with invalid parameters.
  // That's clearly a Delphi-bug, so fpc should fail on the following:
  [Tmyt(924,32)]
  TMyObject = class(TObject)
  end;

begin
end.

