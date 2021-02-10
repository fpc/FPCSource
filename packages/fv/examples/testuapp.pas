program testuapp;

uses
  uapp;

type
  TMyUnicodeApp = object(TApplication)
  end;

var
  MyUnicodeApp: TMyUnicodeApp;

begin
  MyUnicodeApp.Init;
  MyUnicodeApp.Run;
  MyUnicodeApp.Done;
end.

