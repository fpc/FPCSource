program testclasses;

{$mode objfpc}{$H+}

uses
  Classes, tcfindnested, tcstringlist, tccollection, tclist,
  tcpersistent, tclinkedlist, tccomponent, tcstreaming, tccompstreaming,
  tcresref,
  consoletestrunner;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='Test classes';
  Application.Run;
  Application.Free;
end.
