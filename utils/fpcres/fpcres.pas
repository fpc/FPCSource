{$ifdef fpc}
{$mode objfpc}
{$endif}
{$apptype console}
{$H+}
program fpcres;

uses
  Classes, SysUtils, elfres, elfresfix;

resourcestring
  SError           = 'Error:';
  SErrUnknownParam = 'Unknown command-line parameter : %s';
  SErrNeedArgument = 'Option at pos %d (%s) needs an argument.';
  SErrNoInputFile  = 'No input filename was specified';
  SErrNoOutputFile = 'Outputfile is not allowed when multiple input files are given';
  SErrOutputFileIllegal = 'Outputfile not allowed when fixing headers.';

  SUsage010 = 'fpcres - Free Pascal Resource to ELF object compiler';
  SUsage020 = 'Part of the Free Pascal and CrossFPC distributions';
  SUsage030 = 'Copyright (C) 2005 Simon Kissel';
  SUsage040 = '--------------------------------------------------------';
  SUsage050 = 'Usage: fpcres [options] -i inputfile [-i inputfile] [-o outputfile]';
  SUsage055 = ' where options are one or more of:';
  SUsage060 = ' -i --input=inputfile';
  SUsage065 = '                   A file in windows .res resource format, ';
  SUsage070 = '                   or a Delphi/Kylix form file in dfm/xfm  ';
  SUsage080 = '                   format (binary or text).';
  SUSage085 = '                   More than one inputfile may be specified.';
  SUsage090 = ' -o --output=outputfile';
  SUsage095 = '                   Name of the object file to generate. If ';
  SUsage100 = '                   omitted, the name of the input file will';
  SUsage110 = '                   be used, with .or as extension.';
  SUSage115 = '                   (not allowed with multiple input files.)';
  SUsage116 = ' -e --extension=ext';
  SUsage117 = '                   use ext as the extension for output filenames';
  SUsage120 = ' -h --help         show this help message.';
  SUsage130 = ' -f --fixheader    fix resource block header.';
  SUsage140 = ' -6 --64bit        Use 64-bit elf resources.';
  SUsage150 = ' -v --verbose      be verbose.';


Type
  TRunMode = (rmCreateElfRes,rmFixHeader);

Var
  RunMode        : TRunMode;
  InputFiles     : TStringList;
  OutputFileName : String;
  Use64bit       : Boolean;
  BeVerbose      : Boolean;
  UseExt         : String;

Procedure Usage(ExitStatus : Word);

begin
  Writeln(SUsage010);
  Writeln(SUsage020);
  Writeln(SUsage030);
  Writeln(SUsage040);
  Writeln(SUsage050);
  Writeln(SUsage055);
  Writeln(SUsage060);
  Writeln(SUsage065);
  Writeln(SUsage070);
  Writeln(SUsage080);
  Writeln(SUSage085);
  Writeln(SUsage090);
  Writeln(SUsage095);
  Writeln(SUsage100);
  Writeln(SUsage110);
  Writeln(SUSage115);
  Writeln(SUSage116);
  Writeln(SUSage117);
  Writeln(SUsage120);
  Writeln(SUsage130);
  Writeln(SUsage140);
  Writeln(SUsage150);
  Halt(ExitStatus);
end;

Procedure DoError(Msg : String; Args : Array of const);

begin
  Writeln(SError,' ',Format(Msg,Args));
  Usage(1);
end;

Procedure AnalyzeParams;

  Function CheckOption(Index : Integer;Short,Long : String): Boolean;

  var
    O : String;

  begin
    O:=Paramstr(Index);
    Result:=(O='-'+short) or (copy(O,1,Length(Long)+3)=('--'+long+'='));
  end;

  Function OptionArg(Var Index : Integer) : String;

  Var
    P : Integer;

  begin
    if (Length(ParamStr(Index))>1) and (Paramstr(Index)[2]<>'-') then
      begin
      If Index<ParamCount then
        begin
        Inc(Index);
        Result:=Paramstr(Index);
        end
      else
        DoError(SErrNeedArgument,[Index,ParamStr(Index)]);
      end
    else If length(ParamStr(Index))>2 then
      begin
      P:=Pos('=',Paramstr(Index));
      If (P=0) then
        DoError(SErrNeedArgument,[Index,ParamStr(Index)])
      else
        begin
        Result:=Paramstr(Index);
        Delete(Result,1,P);
        end;
      end;
  end;

Var
  I : Integer;
  P : String;
  
begin
  RunMode:=rmCreateElfres;
  Use64bit:=False;
  BeVerbose:=False;
  I:=0;
  While (I<ParamCount) do
    begin
    Inc(I);
    // Values.
    If Checkoption(I,'o','output') then
      OutPutFileName:=OptionArg(I)
    else if Checkoption(I,'i','input') then
      InputFiles.Add(OptionArg(I))
    else If Checkoption(I,'e','extension') then
      UseExt:=OptionArg(I)
    else if CheckOption(I,'f','fixheader') then
      RunMode:=rmFixHeader
    else if CheckOption(I,'6','64bit') then
      Use64bit:=True
    else if CheckOption(I,'h','help') then
      Usage(0)
    else if CheckOption(I,'v','verbose') then
      BeVerbose:=True
    else
      begin
      P:=ParamStr(I);
      if (Length(P)>0) and (P[1]<>'-') then
        begin
        if (I=ParamCount) then
          OutputFileName:=P
        else
          InputFiles.Add(P)
        end
      else
        begin
        DoError(SErrUnknownParam,[P]);
        Usage(1);
        end;
      end;
    end;
  If (InputFiles.Count=0) then
    DoError(SErrNoInputFile,[]);
  If (InputFiles.Count>1) and (OutputFileName<>'') then
    DoError(SErrNoOutputFile,[]);
  If (RunMode=rmFixHeader) and (OutputFileName<>'') then
    DoError(SErrOutputFileIllegal,[])
end;

Type
  TLogger = Class(TObject)
    Procedure Log(Const Msg : String);
  end;

Procedure TLogger.Log(Const Msg : string);

begin
  Writeln(Msg);
end;

Procedure FixHeader(AFileName : String);

Var
  F : TElfResourceFixer;
  O : TLogger;
  
begin
  if Use64bit then
    F:=TElf64ResourceFixer.Create
  else
    F:=TElf32ResourceFixer.Create;
  try
    F.Verbose:=BeVerbose;
    if BeVerbose then
      begin
      O:=TLogger.Create;
      F.OnVerbose:={$ifdef fpc}@{$endif}O.Log;
      end
    else
      O:=Nil;
    Try
      F.FixFile(AFileName);
    Finally
      O.Free;
    end;
  Finally
    F.Free;
  end;
end;

Procedure CreateRes(AFileName : String);

Var
  C : TElfResCreator;

begin
  If Use64Bit then
    C:=TElf64ResCreator.Create
  else
    C:=TElf32ResCreator.Create;
  With C do
    Try
      Verbose:=BeVerbose;
      If (UseExt<>'') then
        Extension:=UseExt;
      Convert(AFileName,OutputFileName);
    Finally
      Free;
    end;
end;

Procedure Run;

Var
  I : Integer;
  
begin
  Try
    Case RunMode of
      rmFixHeader:
        For I:=0 to InputFiles.Count-1 do
          FixHeader(InputFiles[i]);
      rmCreateElfRes:
        For I:=0 to InputFiles.Count-1 do
          CreateRes(InputFiles[i]);
    end;
  except
    On E : Exception do
      begin
      Writeln(SError,' ',E.Message);
      Halt(1);
      end;
  end;
end;

begin
  InputFiles:=TStringList.Create;
  Try
    AnalyzeParams;
    Run;
  Finally
    FreeAndNil(InputFiles);
  end;
end.

