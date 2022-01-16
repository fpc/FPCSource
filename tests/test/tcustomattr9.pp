{ %FAIL }

program tcustomattr9;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type
  { tmyt }
  // TCustomAttribute's constructor is private!
  tmyt = class(TCustomAttribute);

type
  [Tmyt]
  TMyObject = class(TObject)
  end;

begin
end.

