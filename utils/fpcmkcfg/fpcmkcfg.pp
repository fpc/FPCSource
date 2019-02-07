{$mode objfpc}
{$H+}
{
    This file is part of Free Pascal Build tools
    Copyright (c) 2005 by Michael Van Canneyt

    Create a configuration file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fpcmkcfg;

uses
  fpmkunit,
  SysUtils,
  Classes,
{$ifdef unix}
  baseunix,
{$endif}
  fpTemplate;

{
  The inc files must be built from a template with the data2inc
  command.
  data2inc -b -s fpc.cft fpccfg.inc DefaultConfig
  data2inc -b -s fpinc.ini fpini.inc fpini
  data2inc -b -s fpinc.cfg fpcfg.inc fpcfg
  data2inc -b -s fppkg.cfg fppkg.inc fppkg
  data2inc -b -s default.cft default.inc fppkg_default
}

{$i fpccfg.inc}
{$i fpini.inc}
{$i fpcfg.inc}
{$i fppkg.inc}
{$i default.inc}

Const
  BuildVersion={$I %FPCVERSION%};
  BuildTarget={$I %FPCTARGET%};
  BuildOSTarget={$I %FPCTARGETOS%};
{$ifdef unix}
  ExeExt = '';
{$else unix}
  ExeExt = '.exe';
{$endif unix}


Resourcestring
  SUsage00  = 'Usage: %s [options]';
  SUsage10  = 'Where options is one or more of';
  SUSage20  = '  -t filename   Template file name. Default is built-in';
  SUSage30  = '  -o filename   Set output file. Default is standard output.';
  SUsage40  = '  -d name=value define name=value pair.';
  SUsage50  = '  -h            show this help and exit.';
  SUsage60  = '  -u name       remove name from list of name/value pairs.';
//  SUsage70  = '  -l filename   read name/value pairs from filename';
  SUsage70  = '  -m            show builtin macros and exit.';
  SUsage80  = '  -b            show builtin template and exit.';
  SUsage84  = '  -s            skip the creation of a backup-file.';
  SUsage87  = '  -p            force directory creation.';
  SUsage90  = '  -v            be verbose.';
  Susage100 = '  -0            use built in fpc.cfg template (default)';
  Susage110 = '  -1            use built in fp.cfg template';
  Susage120 = '  -2            use built in fp.ini template';
  Susage130 = '  -3            use built in fppkg.cfg template';
  Susage140 = '  -4            use built in fppkg default compiler template';
  SErrUnknownOption   = 'Error: Unknown option.';
  SErrArgExpected     = 'Error: Option "%s" requires an argument.';
  SErrIncompletePair  = 'Error: Incomplete name-value pair "%s".';
  SErrNoSuchFile      = 'Error: File "%s" does not exist.';
  SErrNoSuchDirectory = 'Error: Directory of file "%s" does not exists. User -p to force creation.';
  SErrBackupFailed    = 'Error: Backup of file "%s" to "%s" failed.';
  SErrDelBackupFailed = 'Error: Delete of old backup file "%s" failed.';
  SErrCreateDirFailed = 'Error: Could not create the directory for file "%s".';
  SErrDestDirectory   = 'Error: The output file "%s" is a directory.';

  SWarnIgnoringFile   = 'Warning: Ignoring non-existent file: ';
  SWarnIgnoringPair   = 'Warning: Ignoring wrong name/value pair: ';
  SWarngccNotFound    = 'Warning: Could not find gcc. Unable to determine the gcclib path.';
  SWarnCouldNotExecute= 'Warning: Could not execute command ''%s''';

  SBackupCreated      = 'Saved old "%s" to "%s"';


Var
  Verbose : Boolean;
  SkipBackup : Boolean;
  CreateDir: Boolean;
  Cfg : TStringList;
  TemplateParser: TTemplateParser;
  TemplateFileName,
  OutputFileName : String;
  IDEBuildin : Integer;

function IsSuperUser:boolean;
begin
{$ifdef unix}
  result:=(fpGetUID=0);
{$else unix}
  result:=false;
{$endif unix}
end;


function GetDefaultLocalRepository: string;

begin
{$IFDEF Unix}
  result := '{UserDir}.fppkg'+PathDelim;
{$ELSE Unix}
  result := '{AppConfigDir}';
{$ENDIF Unix}
end;

function GetDefaultLocalBasepath: string;

begin
{$IFDEF Unix}
  result := '~/.fppkg'+PathDelim+'lib'+PathDelim+'fpc'+PathDelim+'$fpcversion';
{$ELSE Unix}
  result := '$LOCAL_APPDATA'+PathDelim+'FreePascal'+PathDelim+'fppkg';
{$ENDIF Unix}
end;

function GetDefaultCompilerConfigDir: string;

begin
{$IFDEF Unix}
  if IsSuperUser then
    result := '/etc/fppkg/'
  else
{$ENDIF}
  result := '{LocalRepository}config/';
end;

function GetDefaultNeedCrossBinutilsIfdef: string;

begin
  result := '';
  // On Darwin there is never a need for a crossbinutils prefix
  if SameText(BuildOSTarget,'Darwin') then
    result := '#IFNDEF ' + BuildOSTarget + LineEnding +
              '#DEFINE NEEDCROSSBINUTILS' + LineEnding +
              '#ENDIF'
  else if (BuildTarget = 'i386') or (BuildTarget = 'x86_64') then
    begin
    // Cross-binutils are not needed to compile for i386 on an x86_64 system
    result := '#IFNDEF CPUI386' + LineEnding +
              '#IFNDEF CPUAMD64' + LineEnding +
              '#DEFINE NEEDCROSSBINUTILS' + LineEnding +
              '#ENDIF' + LineEnding +
              '#ENDIF' + LineEnding +
              LineEnding +
              '#IFNDEF ' + BuildOSTarget + LineEnding +
              '#DEFINE NEEDCROSSBINUTILS' + LineEnding +
              '#ENDIF';
    end
  else
    result := '#DEFINE NEEDCROSSBINUTILS';
end;

function GetDefaultGCCDir: string;

  var
    OS: TOS;
    CPU: TCPU;
    s: string;

  procedure AddConditionalLinkerPath(const aCpuType: string; const ACPU: TCPU; var ConfigFileOption: string);
  var
    path: string;
    ErrS: string;
  begin
    path := GetDefaultLibGCCDir(ACPU, OS, ErrS);
    if ErrS<>'' then
      Writeln(StdErr, ErrS);
    if path <> '' then
      begin
      if ConfigFileOption<>'' then ConfigFileOption:=ConfigFileOption+LineEnding;
      ConfigFileOption := ConfigFileOption + '#ifdef ' + ACpuType + LineEnding + '-Fl' + Path + LineEnding + '#endif';
      end;
  end;

begin
  CPU := StringToCPU(BuildTarget);
  OS := StringToOS(BuildOSTarget);
  result := '';

  case OS of
    freebsd, openbsd, netbsd :
      result := '-Fl'+GetDefaultLibGCCDir(CPU, OS, S);
    linux :
       begin
       if CPU in [i386, x86_64] then
         begin
         AddConditionalLinkerPath('cpui386', i386, result);
         AddConditionalLinkerPath('cpux86_64', x86_64, result);
         end
       else if CPU in [powerpc, powerpc64] then
         begin
         AddConditionalLinkerPath('cpupowerpc', powerpc, result);
         AddConditionalLinkerPath('cpupowerpc64', powerpc64, result);
         end
       end;
    darwin :
       begin
       AddConditionalLinkerPath('cpui386', i386, result);
       AddConditionalLinkerPath('cpux86_64', x86_64, result);
       AddConditionalLinkerPath('cpupowerpc', powerpc, result);
       AddConditionalLinkerPath('cpupowerpc64', powerpc64, result);
       { macOS 10.14 or later:
          1) command line tools are installed under /Library/Developer/CommandLineTools
          2) the system libraries still contain i386 code, but the 10.14 sdk doesn't
            (-> only use the 10.14 sdk when targeting x86_64 or unknown architectures )
          3) crt1.o is no longer installed under /usr -> add its directory explicitly via
             -Fl
            
        We can't detect the macOS version inside fpc.cfg, unfortunately, so we can only
        insert this while generating the configuration file.
        
        This will stop working when macOS 10.15 is released without i386 support, but then
        users will be responsible for supplying their own i386 SDK anyway.
       }
       if DirectoryExists('/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk') then
         begin
           result:=result + LineEnding +
             '-FD/Library/Developer/CommandLineTools/usr/bin' + LineEnding +
             '#ifdef cpui386' + LineEnding +
             '-Fl/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib' + LineEnding +
             '#endif' + LineEnding +
             '#ifndef cpui386' + LineEnding +
             '#ifndef cpupowerpc' + LineEnding +
             '#ifndef cpupowerpc64' + LineEnding +
             '-XR/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk' + LineEnding +
             '#endif' + LineEnding +
             '#endif' + LineEnding +
             '#endif';
         end
       else
         begin
           { add Xcode.app binutils to search path}
           result:=result + LineEnding +
             '-FD/Applications/Xcode.app/Contents/Developer/usr/bin';
         end;
      end;
  end; {case}
end;


procedure Init;

begin
  Verbose:=False;
  IDEBuildIn:=0;

  TemplateParser := TTemplateParser.Create;
  TemplateParser.StartDelimiter:='%';
  TemplateParser.EndDelimiter:='%';
  TemplateParser.Values['FPCVERSION'] := BuildVersion;
  TemplateParser.Values['FPCTARGET'] := BuildTarget;
  TemplateParser.Values['FPCTARGETOS'] := BuildOSTarget;
  TemplateParser.Values['FPCBIN'] := 'fpc';
  TemplateParser.Values['PWD'] := GetCurrentDir;
  TemplateParser.Values['BUILDDATE'] := DateToStr(Date);
  TemplateParser.Values['BUILDTIME'] := TimeToStr(Time);

  TemplateParser.Values['LOCALREPOSITORY'] := GetDefaultLocalRepository;
  TemplateParser.Values['LOCALBASEPATH'] := GetDefaultLocalBasepath;
  TemplateParser.Values['COMPILERCONFIGDIR'] := GetDefaultCompilerConfigDir;
  TemplateParser.Values['NEEDCROSSBINUTILSIFDEF'] := GetDefaultNeedCrossBinutilsIfdef;
  TemplateParser.Values['GCCLIBPATH'] := GetDefaultGCCDIR;

  Cfg:=TStringList.Create;
  Cfg.Text:=StrPas(Addr(DefaultConfig[0][1]));
end;

Procedure Done;

begin
  FreeAndNil(Cfg);
  FreeAndNil(TemplateParser);
end;

Procedure Usage;

begin
  Writeln(Format(SUsage00,[ExtractFileName(Paramstr(0))]));
  Writeln(SUsage10);
  Writeln(SUsage20);
  Writeln(SUsage30);
  Writeln(SUsage40);
  Writeln(SUsage50);
  Writeln(SUsage60);
  Writeln(SUsage70);
  Writeln(SUsage80);
  Writeln(SUsage84);
  Writeln(SUsage87);
  Writeln(SUsage90);
  Writeln(SUsage100);
  Writeln(SUsage110);
  Writeln(SUsage120);
  Writeln(SUsage130);
  Writeln(SUsage140);
  Halt(1);
end;

Procedure UnknownOption(Const S : String);

begin
  Writeln(SErrUnknownOption,S);
  Usage;
end;

Procedure ShowBuiltIn;

Var
  I : Integer;


begin
  For I:=0 to Cfg.Count-1 do
    Writeln(Cfg[I]);
end;


Procedure ShowBuiltInMacros;

Var
  I : Integer;

begin
  For I:=0 to TemplateParser.ValueCount-1 do
    Writeln(TemplateParser.NamesByIndex[I]+'='+TemplateParser.ValuesByIndex[I]);
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
        'v' : Verbose:=True;
        'h' : Usage;
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
        '0' : IDEBuildin:=0;
        '1' : IDEBuildin:=1;
        '2' : IDEBuildin:=2;
        '3' : IDEBuildin:=3;
        '4' : IDEBuildin:=4;
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
    end
  else
    begin
      case IDEBuildin of
        1:
           Cfg.Text:=StrPas(Addr(fpcfg[0][1]));
        2:
           Cfg.Text:=StrPas(Addr(fpini[0][1]));
        3:
           Cfg.Text:=StrPas(Addr(fppkg[0][1]));
        4:
           Cfg.Text:=StrPas(Addr(fppkg_default[0][1]));
      end;

    TemplateParser.Values['TEMPLATEFILE'] := 'builtin';
    end;
  if ShowBuiltinCommand then
    begin
    ShowBuiltIn;
    halt(0);
    end;
end;


Procedure CreateFile;

Var
  Fout : Text;
  S,BFN : String;
  I : Integer;

begin
  if (OutputFileName<>'') and
     DirectoryExists(OutputFileName) then
    begin
      Writeln(StdErr,Format(SErrDestDirectory,[OutputFileName]));
      Halt(1);
    end;
  If (OutputFileName<>'')
     and FileExists(OutputFileName)
     and not SkipBackup then
    begin
    BFN:=ChangeFileExt(OutputFileName,'.bak');
    If FileExists(BFN) and not DeleteFile(BFN) then
      begin
      Writeln(StdErr,Format(SErrDelBackupFailed,[BFN]));
      Halt(1);
      end;
    If not RenameFile(OutputFileName,BFN) then
      begin
      Writeln(StdErr,Format(SErrBackupFailed,[OutputFileName,BFN]));
      Halt(1);
      end
    else
      Writeln(Format(SBackupCreated,[ExtractFileName(OutputFileName),ExtractFileName(BFN)]));
    end;
  if (OutputFileName<>'') and not DirectoryExists(ExtractFilePath(OutputFileName)) then
    begin
    if CreateDir then
      begin
      if not ForceDirectories(ExtractFilePath(OutputFileName)) then
        begin
        Writeln(StdErr,Format(SErrCreateDirFailed,[OutputFileName]));
        Halt(1);
        end;
      end
    else
      begin
      Writeln(StdErr,Format(SErrNoSuchDirectory,[OutputFileName]));
      Halt(1);
      end;
    end;
  Assign(Fout,OutputFileName);
  Rewrite(FOut);
  Try
    For I:=0 to Cfg.Count-1 do
      begin
      S:=Cfg[i];
      S := TemplateParser.ParseString(S);
      Writeln(FOut,S);
      end;
  Finally
    Close(Fout);
  end;
end;

begin
  Init;
  Try
    ProcessCommandLine;
    CreateFile;
  Finally
    Done;
  end;
end.
