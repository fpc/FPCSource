program tcustomattr9;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  { tmyt }
  // an attribute class without a constructor of its own uses TObject.Create
  tmyt = class(TCustomAttribute);

type
  [Tmyt]
  TMyObject = class(TObject)
  end;

begin
end.

