{$mode objfpc}
{$h+}

program testapp;

uses custapp,classes;

Const
  ShortOpts = 'abc:d:012';
  Longopts : Array[1..6] of String = (
    'add:','append','delete:','verbose','create:','file:');

Type
  TTestApp = Class(TCustomApplication)
    Procedure DoRun ; Override;
  end;

Procedure TTestApp.DoRun;

Var
  I : Integer;
  S : String;
  Opts,FN,Args : TStrings;

begin
  Writeln('Exe name            : ',ExeName);
  Writeln('Help file           : ',HelpFile);
  Writeln('Terminated          : ',Terminated);
  Writeln('Title               : ',Title);
  Writeln('Console app         : ',ConsoleApplication);
  Writeln('Location            : ',Location);
  Writeln('ParamCount          : ',ParamCount);
  For I:=0 to ParamCount do
    Writeln('Params [',I:3,']        : ',Params[i]);
  Writeln('Option char         : ',OptionChar);
  Writeln('Case sensitive opts : ',CaseSensitiveOptions);
  Writeln('StopOnException     : ',StopOnException);
  Writeln('----------------------------------------');
  Writeln('Simple options check');
  S:=CheckOptions(ShortOpts,LongOpts);
  If (S<>'') then
    Writeln(S);
  Writeln('Longer options check');
  Opts:=TstringList.Create;
  FN:=TStringList.Create;
  Args:=TStringList.Create;
  Try
    For I:=1 to 6 do
      Opts.Add(LongOpts[i]);
    S:=CheckOptions(ShortOpts,Opts,Args,FN);
    Writeln('Found ',Args.Count,' options and ',FN.Count,' non-options (filenames)');
    For I:=0 to Args.Count-1 do
      Writeln('Option ',I:2,': ',Args[i]);
    For I:=0 to FN.Count-1 do
      Writeln('Non-Option ',I:2,': ',FN[i]);
    Writeln('Getting option value "add"');
    S:=GetOptionValue('add');
    Writeln('Value for "add": ',S);
    Writeln('Testing Hasoption "a"');
    Writeln('Option append found: ',HasOption('append'));
    Writeln('Option a or append found: ',HasOption('a','append'));
    Writeln('-----------------------');
    GetEnvironmentList(Opts,True);
    Writeln('Found ',Opts.Count,' environment variables');
    For I:=0 to Opts.Count-1 do
      Writeln(I:3,': ',Opts[i],' with value "',EnvironmentVariable[Opts[i]],'"');
  Finally
    Opts.Free;
    FN.Free;
    Args.Free;
  end;
  Terminate;
  Writeln('-------------------------');
  Writeln('After terminate, "terminated" is ',Terminated);
end;

Var
  App : TTestApp;

begin
  App:=TTestApp.Create(Nil);
  App.Initialize;
  App.Title:='CustomApplication class test application.';
  App.Run;
  App.Free;
end.
