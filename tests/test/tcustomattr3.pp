{ %fail }
program tcustomattr3;

{$mode objfpc}{$H+}
{$modeswitch prefixedattributes}

type

  { tmyt }

  tmyt = class
    constructor create;
  end;

  // tmyt is not a TCustomAttribute, so this should fail.
  [tmyt]
  TMyObject = class(TObject)
  end;

begin
end.

