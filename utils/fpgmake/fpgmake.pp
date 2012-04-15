program fpgmake;

{$mode objfpc}{$H+}

uses
{$ifdef UNIX}
  cthreads,
{$endif UNIX}
  Classes,
  sysutils,
  fpmkunit,
  fpTemplate,
  fpmakeParseJSon, fpmakecreatefile;

{
  data2inc -b -s fpmake.cft fpmake.inc fpmake
}

{$i fpmake.inc}

Resourcestring
  SUsage00  = 'Usage: %s [options]';
  SUsage10  = 'Where options is one or more of';
  SUSage20  = '  -t filename   Template file name. Default is built-in';
  SUSage30  = '  -o filename   Set output file. Default is standard output.';
  SUsage40  = '  -d name=value define name=value pair.';
  SUsage50  = '  -h            show this help and exit.';
  SUsage60  = '  -u name       remove name from list of name/value pairs.';
  SUsage70  = '  -m            show builtin macros and exit.';
  SUsage80  = '  -b            show builtin template and exit.';
  SUsage90  = '  -s            skip the creation of a backup-file.';
  SUsage95  = '  -p            force directory creation.';
  SError              = 'Error:';
  SErrUnknownOption   = 'Error: Unknown option (%s).';
  SErrArgExpected     = 'Error: Option "%s" requires an argument.';
  SErrIncompletePair  = 'Error: Incomplete name-value pair "%s".';
  SErrNoSuchFile      = 'Error: File "%s" does not exist.';

  SWarnIgnoringFile   = 'Warning: Ignoring non-existent file: ';
  SWarnIgnoringPair   = 'Warning: Ignoring wrong name/value pair: ';
  SWarngccNotFound    = 'Warning: Could not find gcc. Unable to determine the gcclib path.';
  SWarnCouldNotExecute= 'Warning: Could not execute command ''%s''';

Var
  SkipBackup : Boolean;
  CreateDir: Boolean;
  Cfg : TStringList;
  TemplateFileName,
  OutputFileName : String;

const
  InputFileName = 'fpmake.fpc';

procedure Usage;

begin
  Writeln(Format(SUsage00,[ExtractFileName(ApplicationName)]));
  Writeln(SUsage10);
  Writeln(SUsage20);
  Writeln(SUsage30);
  Writeln(SUsage40);
  Writeln(SUsage50);
  Writeln(SUsage60);
  Writeln(SUsage70);
  Writeln(SUsage80);
  Writeln(SUsage90);
  Writeln(SUsage95);
end;

Procedure UnknownOption(Const S : String);

begin
  Writeln(Format(SErrUnknownOption,[S]));
  Usage;
  Halt(1);
end;

procedure Init;

begin
  Cfg:=TStringList.Create;
  Cfg.Text:=StrPas(Addr(fpmake[0][1]));
end;

procedure Done;

begin
  Cfg.Free;
end;

Procedure ShowBuiltInMacros;

Var
  I : Integer;

begin
  For I:=0 to TemplateParser.ValueCount-1 do
    Writeln(TemplateParser.NamesByIndex[I]+'='+TemplateParser.ValuesByIndex[I]);
end;

Procedure ShowBuiltIn;

Var
  I : Integer;


begin
  For I:=0 to Cfg.Count-1 do
    Writeln(Cfg[I]);
end;


Procedure ProcessCommandline;

Var
  I : Integer;
  S : String;
  ShowBuiltinCommand : boolean;

  Function GetOptArg : String;

  begin
    If I=ParamCount then
      begin
      Writeln(StdErr,Format(SErrArgExpected,[S]));
      Halt(1);
      end;
    inc(I);
    Result:=ParamStr(I);
  end;

  procedure AddPair(const Value: String);
  var P: integer;
      N,V: String;
  begin
    P:=Pos('=',Value);
    If p=0 then
      begin
      Writeln(StdErr,Format(SErrIncompletePair,[Value]));
      Halt(1);
      end;
    V:=Value;
    N:=Copy(V,1,P-1);
    Delete(V,1,P);
    TemplateParser.Values[N] := V;
  end;

begin
  I:=1;
  ShowBuiltinCommand := False;
  SkipBackup := False;
  CreateDir := False;
  While( I<=ParamCount) do
    begin
    S:=Paramstr(i);
    If Length(S)<=1 then
      UnknownOption(S)
    else
      case S[2] of
        'h' : begin
              Usage;
              halt(0);
              end;
        'b' : ShowBuiltinCommand := true;
        'm' : begin
              ShowBuiltinMacros;
              halt(0);
              end;
        't' : TemplateFileName:=GetOptArg;
        'd' : AddPair(GetOptArg);
        'u' : TemplateParser.Values[GetOptArg]:='';
        'o' : OutputFileName:=GetoptArg;
        's' : SkipBackup:=True;
        'p' : CreateDir:=True;
      else
        UnknownOption(S);
      end;
    Inc(I);
    end;
  If (TemplateFileName<>'') then
    begin
    If Not FileExists(TemplateFileName) then
      begin
      Writeln(StdErr,Format(SErrNoSuchFile,[TemplateFileName]));
      Halt(1);
      end;
    Cfg.LoadFromFile(TemplateFileName);
    TemplateParser.Values['TEMPLATEFILE'] := TemplateFileName;
    end;
  if ShowBuiltinCommand then
    begin
    ShowBuiltIn;
    halt(0);
    end;
end;

var
  APackages: TPackages;

begin
  Init;
  Try
    ProcessCommandLine;
    APackages := ParseFpmakeFile(InputFileName);
    if assigned(APackages) then
      CreateFile(OutputFileName, Cfg, APackages, SkipBackup, CreateDir);
  Finally
    APackages.Free;
    Done;
  end;
end.
