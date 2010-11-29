program Project1;

{$mode delphi}

uses
  uw18087a, uw18087b;

type
  TFoo1 = class
  strict private
    type
      TFoo2 = record
      end;
      TFoo3 = class
        FFoo2: TFoo2; // was error: Identifier not found "TFoo2"
      end;
  end;

begin
end.

