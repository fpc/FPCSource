{ %FAIL }

{ This tests that latest added helper in other units is used for a type }
program thlp42;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp41b, uhlp41a;

begin
  TObject.Test1;
end.

