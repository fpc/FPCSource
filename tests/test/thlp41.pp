{ %FAIL }

{ This tests that latest added helper in other units is used for a type }
program thlp41;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp41a, uhlp41b;

begin
  TObject.Test1;
end.

