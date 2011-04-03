{ %FAIL }

{ This tests that the bottom most helper in another unit is used for a type }
program thlp39;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp39;

begin
  TObject.Test1;
end.
