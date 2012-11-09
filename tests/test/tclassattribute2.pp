{ %fail }
program tclassattribute2;

{$mode objfpc}{$H+}

type
  // Delphi XE does compile attributes that are not defined, but ignores them.
  // That's clearly a Delphi-bug, so fpc should fail on the following:
  [TMyAttributeDoesNotExist]
  TMyObject = class(TObject)
  end;

begin
end.

