Program PtoP;
{$mode objfpc}
{$H+}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by Michael Van Canneyt, member of
    the Free Pascal development team

    Pascal pretty print program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Uses
  {$ifdef FPC_DOTTEDUNITS}
  system.sysutils,
  system.classes,
  system.types,
  fcl.custapp,
  fcl.bufstream,
  {$else}
  sysutils,
  classes,
  types,
  custapp,
  bufstream,
  {$endif}
  ptop.beautifier,
  ptop.types,
  ptop.config
  ;

ResourceString
  Version            = 'Version 3.3.1';
  ATitle             = 'PToP';
  Copyright          = 'Copyright (c) 1999-2026 by the Free Pascal Development Team';
  SErrNoInputOutput  = 'No input and output file given';
  SErrTooManyNonOpts = 'Too many non options';
  UsageMSG           =
    'Preety-prints Pascal source file (infile), save the result in outfile.' +
    LineEnding + 'Usage:';
  OptUsage           =
    '     -c [optsfile]: read options from optsfile' + LineEnding +
    '     -i [indent]: Set number of indent spaces.' + LineEnding +
    '     -l [linesize]: Set maximum output linesize.' + LineEnding +
    '     -b [bufsize]: Use buffers of size bufsize' + LineEnding +
    '     -g [ofile]: generate default options file, ' +
    'all other options will be ignored.' + LineEnding +
    '     -v : be verbose' + LineEnding +
    '     -h : This help' + LineEnding +
    'It''s possible to not provide a value for outfile, in that case' +
    ' the input file will be OVERWRITTEN.' + LineEnding +
    'Read ptop(5) and ptop.cfg(5) for more usage details.';

Type
  TPToP = Class(TCustomApplication)
  Private
    Infilename,
    OutFileName,
    ConfigFile : String;
    BeVerbose : Boolean;
    TheIndent,
    TheBufSize,
    TheLineSize : Integer;
    Procedure Usage(ECode : Word);
    Procedure GenOpts;
    Procedure ProcessOpts;
  Public
    Procedure DoRun; override;
  end;

Procedure TPToP.Usage(ECode : Word);
begin
  Writeln (UsageMSG);
  Writeln ('ptop [-v] [-i indent] [-b bufsize] [-c optsfile] [-l linesize] infile outfile');
  Writeln (OptUsage);
  Halt(ECode);
end;

Procedure TPToP.Genopts;
Var S : TFileStream;
begin
  S := TFileStream.Create(ConfigFile,fmCreate);
  Try
    GenerateNewConfig(S);
  Finally
    S.Free;
  end;
end;

Procedure TPToP.ProcessOpts;
Var
  S : String;
  F : TStringDynArray;

begin
  { Set defaults }
  Infilename:='';
  OutFileName:='';
  ConfigFile:='';
  TheIndent:=2;
  TheBufSize:=255;
  TheLineSize:=DefLineSize;
  BeVerbose:=False;

  S := CheckOptions('i:c:g:l:b:hv', '');
  F := GetNonOptions('i:c:g:l:b:hv', []);

  If (S <> '') then
    begin
    Writeln(stderr,S);
    Usage(1);
    end;

  if HasOption('h') then
    usage(0);

  If HasOption('g') then
    begin
    ConfigFile:=GetOptionValue('g','');
    GenOpts;
    halt(0);
    end;

  case Length(F) of
    0: begin
      writeln(stderr,SErrNoInputOutput);
      Usage(1);
    end;
    1: begin
      InFileName:=F[0];
      OutFileName:=F[0];
    end;
    2: begin
       InFileName:=F[0];
       OutFileName:=F[1];
       end;
    else
      begin
      Writeln(stderr,SErrTooManyNonOpts);
      Usage(1);
      end;
  end;

  TheIndent:=StrToIntDef(GetOptionValue('i',''),2);
  TheBufSize:=StrToIntDef(GetOptionValue('b',''),255);
  TheLineSize:=StrToIntDef(GetOptionValue('l',''),DefLineSize);

  ConfigFile:=GetOptionValue('c','');
  BeVerbose:=HasOption('v');
end; { Of ProcessOpts }

Procedure TPToP.DoRun;

Var
  F,InS,OutS,cfgS : TSTream;
  PPrinter : TPrettyPrinter;

begin
  ProcessOpts;

  if BeVerbose then
    begin
    writeln(Title+' '+Version);
    writeln(Copyright);
    Writeln;
    end;

  Ins:=TMemoryStream.Create;
  try
    F:=TFileStream.Create(InFileName,fmOpenRead);
    Try
      Ins.CopyFrom(F,0);
      Ins.Position:=0;
    Finally
      F.Free;
    end;
    OutS:=TwriteBufStream.Create(TFileStream.Create(OutFileName,fmCreate));
    Try
      If ConfigFile<>'' then
        CfgS:=TFileStream.Create(ConfigFile,fmOpenRead)
      else
        CfgS:=Nil;
      try
        PPrinter:=TPrettyPrinter.Create;
        Try
          PPrinter.Indent:=TheIndent;
          PPrinter.LineSize:=TheLineSize;
          PPrinter.Source:=Ins;
          PPrinter.Dest:=OutS;
          PPrinter.Config:=CfgS;
          PPrinter.BeVerbose:=BeVerbose;
          PPrinter.PrettyPrint;
        Finally
          FreeAndNil(PPrinter);
        end;
      Finally
        FreeAndNil(CfgS);
      end;
    Finally
      FreeAndNil(OutS);
    end;
  Finally
    FreeAndNil(Ins);
  end;
  Terminate;
end;

begin
  With TPToP.Create(Nil) do
    Try
      Title:= ATitle;
      StopOnException:=True;
      Initialize;
      Run;
    Finally
      Free;
    end;
end.
