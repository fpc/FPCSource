program testjson2code;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, tcjsontocode, fpjsontopas;

type

  { TLazTestRunner }

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
    function GetShortOpts: string; override;
    procedure AppendLongOpts; override;
    procedure DoRun; override;
  end;

var
  Application: TMyTestRunner;

{ TMyTestRunner }

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts;
  Result:=Result+'t:';
end;

procedure TMyTestRunner.AppendLongOpts;
begin
  inherited AppendLongOpts;
  LongOpts.Add('testunitdir:');
end;

procedure TMyTestRunner.DoRun;
begin
  TestUnitDir:=GetOptionValue('t','testunitdir');
  inherited DoRun;
end;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.