{
    Copyright (c) 2001 by Peter Vreman

    Convert Makefile.fpc to Makefile

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
program fpcmake;

{ Define to not catch exceptions and output backtraces }
{ define NOEXCEPT}

    uses
      getopts,
      sysutils,
      fpcmmain,fpcmwr,fpcmpkg, fpcmdic;

    type
      { Verbosity Level }
      TVerboseLevel = (v_Quiet,v_Default,v_Verbose);

      { Operation mode }
      TMode = (m_None,m_PackageFpc,m_Makefile);

      TFPCMakeConsole=class(TFPCMake)
        procedure Verbose(lvl:TFPCMakeVerbose;const s:string);override;
      end;

    var
      ParaMode : TMode;
      ParaVerboseLevel : TVerboseLevel;
      paraExtra : string;
      ParaTargets : string;
      ParaOutputFileName : string;
      ParaRecursive : boolean;
      ParaSkipPackageInfo : Boolean;


{*****************************************************************************
                                 Helpers
*****************************************************************************}

    procedure Show(lvl:TVerboseLevel;const s:string);
      begin
        if ParaVerboseLevel>=lvl then
         Writeln(s);
      end;


    procedure Error(const s:string);
      begin
        Writeln('Error: ',s);
        Halt(1);
      end;


{*****************************************************************************
                              TFPCMakeConsole
*****************************************************************************}

    procedure TFPCMakeConsole.Verbose(lvl:TFPCMakeVerbose;const s:string);
      begin
        case lvl of
          FPCMakeInfo :
            Show(V_Default,' '+VerboseIdent+s);
          FPCMakeDebug :
            Show(V_Verbose,' '+VerboseIdent+s);
          FPCMakeError :
            Error(s);
        end;
      end;



{*****************************************************************************
                             Makefile output
*****************************************************************************}

    procedure ProcessFile_Makefile(const fn:string; const aOutputfile : string; aextra: string);
      var
        CurrFPCMake : TFPCMakeConsole;
        CurrMakefile : TMakefileWriter;
        s,s2,Subdirs : string;
        c : tcpu;
        t : tos;
      begin
        Show(V_Default,'Processing '+fn);
        CurrFPCMake:=nil;
{$ifndef NOEXCEPT}
        try
{$endif NOEXCEPT}
          { Load Makefile.fpc }
          CurrFPCMake:=TFPCMakeConsole.Create(fn);
          CurrFPCMake.ExtraTargetsFile:=aExtra;
          if ParaTargets<>'' then
           CurrFPCMake.SetTargets(ParaTargets);
          CurrFPCMake.LoadMakefileFPC;
//          CurrFPCMake.Print;

          { Add the subdirs }
          subdirs:='';
          for c:=succ(low(tcpu)) to high(tcpu) do
           for t:=succ(low(tos)) to high(tos) do
            if CurrFPCMake.IncludeTargets[c,t] then
             begin
               s2:=CurrFPCMake.GetTargetVariable(c,t,'target_dirs',true);
               repeat
                 s:=GetToken(s2,' ');
                 if s='' then
                  break;
                 AddTokenNoDup(subdirs,s,' ');
               until false;
             end;
          for c:=succ(low(tcpu)) to high(tcpu) do
           for t:=succ(low(tos)) to high(tos) do
            if CurrFPCMake.IncludeTargets[c,t] then
             begin
               s2:=CurrFPCMake.GetTargetVariable(c,t,'target_exampledirs',true);
               repeat
                 s:=GetToken(s2,' ');
                 if s='' then
                  break;
                 AddTokenNoDup(subdirs,s,' ');
               until false;
             end;

          { Write Makefile }
          CurrMakefile:=TMakefileWriter.Create(CurrFPCMake,ExtractFilePath(fn)+aOutputFile);
          CurrMakefile.SkipPackageInfo:=ParaSkipPackageInfo;
          CurrMakefile.WriteGenericMakefile;
          CurrMakefile.Free;

{$ifndef NOEXCEPT}
        except
          on e : exception do
           begin
             Error(e.message);
             Subdirs:='';
           end;
        end;
{$endif NOEXCEPT}
        CurrFPCMake.Free;

        { Process subdirs }
        if (Subdirs<>'') and
           ParaRecursive then
         begin
           Show(v_Verbose,'Subdirs found: '+subdirs);
           repeat
             s:=GetToken(subdirs,' ');
             if s='' then
              break;
             ProcessFile_Makefile(ExtractFilePath(fn)+s+'/Makefile.fpc',aOutputFile, paraExtra);
           until false;
         end;

      end;

{*****************************************************************************
                             Package.fpc output
*****************************************************************************}

    procedure ProcessFile_PackageFpc(const fn:string; const aOutputFile : string; aExtra : String);
      var
        CurrFPCMake : TFPCMakeConsole;
        CurrPackageFpc : TPackageFpcWriter;
      begin
        Show(V_Default,'Processing '+fn);
        CurrFPCMake:=nil;
{$ifndef NOEXCEPT}
        try
{$endif NOEXCEPT}
          { Load Makefile.fpc }
          CurrFPCMake:=TFPCMakeConsole.Create(fn);
          if ParaTargets<>'' then
           CurrFPCMake.SetTargets(ParaTargets);
          CurrFPCMake.ExtraTargetsFile:=aExtra;
          CurrFPCMake.LoadMakefileFPC;
//          CurrFPCMake.Print;

          { Write Package.fpc }
          CurrPackageFpc:=TPackageFpcWriter.Create(CurrFPCMake,ExtractFilePath(fn)+aOutputFile);
          CurrPackageFpc.WritePackageFpc;
          CurrPackageFpc.Free;

{$ifndef NOEXCEPT}
        except
          on e : exception do
           begin
             Error(e.message);
           end;
        end;
{$endif NOEXCEPT}
        CurrFPCMake.Free;
      end;


    procedure ProcessFile(const fn:string; const aOutputFile : string; const aExtra : string);

    var
      ofn : String;

      begin
        Show(V_Verbose,TitleDate);
        case ParaMode of
          m_None :
            Error('No operation specified, see -h for help');
          m_Makefile :
            begin
            ofn:=aOutputFile;
            if ofn='' then
              ofn:='Makefile';
            ProcessFile_Makefile(fn,ofn,aExtra);
            end;
          m_PackageFpc :
            begin
            ofn:=aOutputFile;
            if ofn='' then
              ofn:='Package.fpc';
            ProcessFile_PackageFpc(fn,ofn,aextra);
            end;
        end;
      end;


procedure UseMakefilefpc;
var
  fn : string;
begin
  if FileExists('Makefile.fpc') then
   fn:='Makefile.fpc'
  else
   fn:='makefile.fpc';
  ProcessFile(fn,ParaOutputFilename, paraExtra);
end;


procedure UseParameters;
var
  i : integer;
begin
  for i:=OptInd to ParamCount do
   ProcessFile(ParamStr(i),ParaOutputFilename,ParaExtra);
end;


Procedure Usage;
{
  Print usage and exit.
}
begin
  writeln(paramstr(0),': <-pw> [-vqh] [file] [file ...]');
  writeln('Operations:');
  writeln(' -p  Generate Package.fpc');
  writeln(' -w  Write Makefile');
  writeln(' -V  Print fpcmake version and exit');
  writeln('');
  writeln('Options:');
  writeln(' -T<target>[,target] Support only specified targets. If "-Tall", all targets are');
  writeln('                     supported. If omitted only default target is supported');
  writeln(' -r                  Recursively process target directories from Makefile.fpc');
  writeln(' -v                  Be more verbose');
  writeln(' -ooutputfile        Use outputfile as filename instead of the default Makefile or Package.fpc');
  writeln(' -s                  Skip writing package name');
  writeln(' -q                  Be quiet');
  writeln(' -h                  This help screen');
  writeln(' -x file             Read extra target definitions from file.');
  Halt(0);
end;


procedure printVersion;
begin
  writeln (TitleDate);
  halt(0);
end;


Procedure ProcessOpts;
{
  Process command line opions, and checks if command line options OK.
}
const
  ShortOpts = 'pwqrvh?VsT:o:x:';
var
  C : char;
begin
{ Reset }
  ParaSkipPackageInfo:=False;
  ParaMode:=m_Makefile;
  ParaVerboseLevel:=v_default;
  ParaTargets:=LowerCase({$I %FPCTARGETCPU})+'-'+LowerCase({$I %FPCTARGETOS});
{ Parse options }
  repeat
    c:=Getopt (ShortOpts);
    Case C of
      EndOfOptions : break;
      'p' : ParaMode:=m_PackageFpc;
      'w' : ParaMode:=m_Makefile;
      'o' : ParaOutputFileName:=OptArg;
      'q' : ParaVerboseLevel:=v_quiet;
      'r' : ParaRecursive:=true;
      's' : ParaSkipPackageInfo:=True;
      'v' : ParaVerboseLevel:=v_verbose;
      'T' : ParaTargets:=OptArg;
      'x' : ParaExtra:=OptArg;
      '?' : Usage;
      'h' : Usage;
      'V' : printVersion;
    end;
  until false;
end;


begin
  ProcessOpts;
  if (OptInd>Paramcount) then
   UseMakefilefpc
  else
   UseParameters;
end.
