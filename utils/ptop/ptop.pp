
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
ptop.config,
ptop.strutils
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
WWriteToSource     = 'Are you sure you want to overwrite the source file? [yYnN] ';

Type 
  TPToP = Class(TCustomApplication)
    Private 
      Infilename,
      OutFileName,
      ConfigFile : String;
      TheIndent,
      TheBufSize,
      TheLineSize : Integer;
      Procedure Usage(ECode : Word);
      Procedure GenOpts;
      Procedure ProcessOpts;
    Public 
      Procedure DoRun;
      override;
  End;

Procedure TPToP.Usage(ECode : Word);
Begin
  Writeln (UsageMSG);
  Writeln ('ptop [-v] [-i indent] [-b bufsize] [-c optsfile] [-l linesize] infile outfile');
  Writeln (OptUsage);
  Halt(ECode);
End;

Procedure TPToP.Genopts;

Var S : TFileStream;
Begin
  S := TFileStream.Create(ConfigFile,fmCreate);
  Try
    GenerateNewConfig(S);
  Finally
    S.Free;
End;
End;

Procedure TPToP.ProcessOpts;

Var 
  S : String;
  F : TStringDynArray;

Begin
  { Set defaults }
  Infilename := '';
  OutFileName := '';
  ConfigFile := '';
  TheIndent := 2;
  TheBufSize := 255;
  TheLineSize := DefLineSize;

  S := CheckOptions('i:c:g:l:b:hv', '');
  F := GetNonOptions('i:c:g:l:b:hv', []);

  If (S <> '') Then
    Begin
      Writeln(stderr,S);
      Usage(1);
    End;

  If HasOption('h') Then
    usage(0);

  If HasOption('g') Then
    Begin
      ConfigFile := GetOptionValue('g','');
      GenOpts;
      halt(0);
    End;

  Case Length(F) Of 
    0:
       Begin
         writeln(stderr,SErrNoInputOutput);
         Usage(1);
       End;
    1:
       Begin
         InFileName := F[0];
         OutFileName := F[0];
       End;
    2:
       Begin
         InFileName := F[0];
         OutFileName := F[1];
       End;
    Else
      Begin
        Writeln(stderr,SErrTooManyNonOpts);
        Usage(1);
      End;
  End;

  TheIndent := StrToIntDef(GetOptionValue('i',''),2);
  TheBufSize := StrToIntDef(GetOptionValue('b',''),255);
  TheLineSize := StrToIntDef(GetOptionValue('l',''),DefLineSize);

  ConfigFile := GetOptionValue('c','');
  BeVerbose := HasOption('v');
End; { Of ProcessOpts }

Procedure TPToP.DoRun;

Var 
  F,InS,OutS,cfgS : TSTream;
  PPrinter : TPrettyPrinter;
  WConfirm: string;

Begin
  ProcessOpts;

  If BeVerbose Then
    Begin
      writeln(Title+' '+Version);
      writeln(Copyright);
      Writeln;
    End;

  If InFileName = OutFileName Then
    Begin
      write(WWriteToSource);
      readln(WConfirm);
      WConfirm := Trim(WConfirm);
      If Not ((WConfirm = 'y') Or (WConfirm = 'Y')) Then
        Terminate;
    End;

  Ins := TMemoryStream.Create;
  Try
    F := TFileStream.Create(InFileName,fmOpenRead);
    Try
      Ins.CopyFrom(F,0);
      Ins.Position := 0;
    Finally
      F.Free;
End;
OutS := TwriteBufStream.Create(TFileStream.Create(OutFileName,fmCreate));
Try
  If ConfigFile<>'' Then
    CfgS := TFileStream.Create(ConfigFile,fmOpenRead)
  Else
    CfgS := Nil;
  Try
    PPrinter := TPrettyPrinter.Create;
    Try
      PPrinter.Indent := TheIndent;
      PPrinter.LineSize := TheLineSize;
      PPrinter.Source := Ins;
      PPrinter.Dest := OutS;
      PPrinter.Config := CfgS;
      PPrinter.PrettyPrint;
    Finally
      FreeAndNil(PPrinter);
End;
Finally
  FreeAndNil(CfgS);
End;
Finally
  FreeAndNil(OutS);
End;
Finally
  FreeAndNil(Ins);
End;
Terminate;
End;

Begin
  With TPToP.Create(Nil) Do
    Try
      Title := ATitle;
      StopOnException := True;
      Initialize;
      Run;
    Finally
      Free;
End;
End.
