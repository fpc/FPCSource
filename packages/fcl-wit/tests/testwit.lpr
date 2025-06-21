program testwit;

uses 
  WIT.Model, WIT.Scanner, WIT.Parser, 
  utcwitscanner, utcwitparser, utcwitmodel, utcrundirtests,
  consoletestrunner,  testregistry;

type

  { TTestRunner }

  TTestRunner = class(consoletestrunner.TTestRunner)
    function GetShortOpts: string; override;
    procedure AppendLongOpts; override;
  end;

var
  Application : TTestRunner;

{ TTestRunner }

function TTestRunner.GetShortOpts: string;
begin
  Result:='d'+inherited GetShortOpts;
end;

procedure TTestRunner.AppendLongOpts;
begin
  Inherited;
  LongOpts.Add('test-dir');
end;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application:=TTestRunner.Create(Nil);
  Application.Initialize;
  if Application.HasOption('d','test-dir') then
    GetTestRegistry.AddTest(TDirectoryFileTests.CreateFromDir(Application.GetOptionValue('d','test-dir')));
  Application.Run;
  Application.Free;

end.

