{ %fail }
program tcustomattr8;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

uses
  typinfo;

type

  { tmyt }

  TMyt = class(TCustomAttribute)
    constructor create;
  end;

type

  { TMyObject }

  [TMyt]
  TMyObject = class(TObject)
  end;
  // Attributes for integers are not allowed, so the following should fail, since
  // there is nothing to bind the attribute to.
  [TMyt]
  int = integer;

constructor TMyt.create;
begin

end;

begin
end.

