{ %fail }
program tclassattribute3;

{$mode objfpc}{$H+}

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

