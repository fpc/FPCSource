program testcompat;

{$mode objfpc}{$H+}

uses
  // valgrind sees frees across threads only with the C memory manager.
  {$IFDEF FPC_VALGRIND}cmem,{$ENDIF}
  {$IFDEF UNIX}cwstring, cthreads,{$ENDIF}
  Classes, consoletestrunner, tcnetencoding, tciotuils, utmessagemanager, utcdevices, utcanalytics, utcimagelist, utcnotifications,
  utcjson, utcpush, utchash, utcregex, utcjsontypes, utcregexapi, utthreading, utccredentials, utcjsonbuilders, utcjsoniterator,
  utcjsonreaders, utcjsonconverters, utcjsonserializers;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  DefaultRunAllTests:=true;
  DefaultFormat:=fPlain;
  Application := TMyTestRunner.Create(nil);

  Application.Initialize;
  Application.Title:='testcompat';
  Application.Run;
  Application.Free;
end.
