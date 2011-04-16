{ %NORUN }

{ This tests that the bottom most helper in another unit is used for a type }
program thlp40;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp39;

begin
  TObject.Test2;
end.

