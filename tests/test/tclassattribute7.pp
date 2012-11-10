{ %fail }
program tclassattribute7;

{$mode objfpc}{$H+}

uses
  typinfo;

type

  { tmyt }

  TMyt = class(TCustomAttribute)
    constructor create;
  end;

type

  { TMyObject }

  TMyObject = class(TObject)
  private
    FInt: integer;
  published
    // Should fail because there is nothing to bind the custom attribute to.
    [TMyt]
  end;

constructor TMyt.create;
begin
//
end;


begin
//
end.

