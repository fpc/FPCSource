{
    $Id$
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

    uses
      getopts,
      sysutils,
      fpcmmain,fpcmwr,fpcmpkg;

    type
      { Verbosity Level }
      TVerboseLevel = (v_Quiet,v_Default,v_Verbose);

      { Operation mode }
      TMode = (m_None,m_PackageFpc,m_Makefile);

      TFPCMakeConsole=class(TFPCMake)
        procedure Verbose(lvl:TFPCMakeVerbose;const s:string);override;
      end;

    var
      Mode : TMode;
      VerboseLevel : TVerboseLevel;


{*****************************************************************************
                                 Helpers
*****************************************************************************}

    procedure Show(lvl:TVerboseLevel;const s:string);
      begin
        if VerboseLevel>=lvl then
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

    procedure ProcessFile_Makefile(const fn:string);
      var
        CurrFPCMake : TFPCMakeConsole;
        CurrMakefile : TMakefileWriter;
{$ifdef SUBDIRS}
        s,Subdirs : string;
        t : ttarget;
{$endif SUBDIRS}
      begin
        Show(V_Default,'Processing '+fn);
        CurrFPCMake:=nil;
//        try
          { Load Makefile.fpc }
          CurrFPCMake:=TFPCMakeConsole.Create(fn);
          CurrFPCMake.LoadMakefileFPC;
//          CurrFPCMake.Print;

{$ifdef SUBDIRS}
          subdirs:=CurrFPCMake.GetVariable('target_dirs',true);
          for t:=low(ttarget) to high(ttarget) do
           subdirs:=subdirs+' '+CurrFPCMake.GetVariable('target_dirs'+targetsuffix[t],true);
{$endif SUBDIRS}

          { Write Makefile }
          CurrMakefile:=TMakefileWriter.Create(CurrFPCMake,ExtractFilePath(fn)+'Makefile');
          CurrMakefile.WriteGenericMakefile;
          CurrMakefile.Free;

//        except
//          on e : exception do
//           begin
//             Error(e.message);
//             Subdirs:='';
//           end;
//        end;
        CurrFPCMake.Free;

{$ifdef SUBDIRS}
        { Process subdirs }
        writeln('Subdirs found: ',subdirs);
        repeat
          s:=GetToken(subdirs);
          if s='' then
           break;
          ProcessFile(ExtractFilePath(fn)+s+'/Makefile.fpc');
        until false;
{$endif SUBDIRS}

      end;

{*****************************************************************************
                             Package.fpc output
*****************************************************************************}

    procedure ProcessFile_PackageFpc(const fn:string);
      var
        CurrFPCMake : TFPCMakeConsole;
        CurrPackageFpc : TPackageFpcWriter;
      begin
        Show(V_Default,'Processing '+fn);
        CurrFPCMake:=nil;
//        try
          { Load Makefile.fpc }
          CurrFPCMake:=TFPCMakeConsole.Create(fn);
          CurrFPCMake.LoadMakefileFPC;
          CurrFPCMake.Print;

          { Write Package.fpc }
          CurrPackageFpc:=TPackageFpcWriter.Create(CurrFPCMake,ExtractFilePath(fn)+'Package.fpc');
          CurrPackageFpc.WritePackageFpc;
          CurrPackageFpc.Free;

//        except
//          on e : exception do
//           begin
//             Error(e.message);
//             Subdirs:='';
//           end;
//        end;
        CurrFPCMake.Free;
      end;


    procedure ProcessFile(const fn:string);
      begin
        case Mode of
          m_None :
            Error('No operation specified, see -h for help');
          m_Makefile :
            ProcessFile_Makefile(fn);
          m_PackageFpc :
            ProcessFile_PackageFpc(fn);
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
  ProcessFile(fn);
end;


procedure UseParameters;
var
  i : integer;
begin
  for i:=OptInd to ParamCount do
   ProcessFile(ParamStr(i));
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
  writeln('');
  writeln('Options:');
  writeln(' -v  Be more verbose');
  writeln(' -q  Be quiet');
  writeln(' -h  This help screen');
  Halt(0);
end;



Procedure ProcessOpts;
{
  Process command line opions, and checks if command line options OK.
}
const
  ShortOpts = 'pwqvh';
var
  C : char;
begin
  if paramcount=0 then
   usage;
{ Reset }
  Mode:=m_none;
  VerboseLevel:=v_default;
  repeat
    c:=Getopt (ShortOpts);
    Case C of
      EndOfOptions : break;
      'p' : Mode:=m_PackageFpc;
      'w' : Mode:=m_Makefile;
      'q' : VerboseLevel:=v_quiet;
      'v' : VerboseLevel:=v_verbose;
      '?' : Usage;
      'h' : Usage;
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
{
  $Log$
  Revision 1.4  2001-06-04 21:42:57  peter
    * Arguments added
    * Start of Package.fpc creation

  Revision 1.3  2001/02/22 21:11:24  peter
    * fpcdir detection added
    * fixed loading of variables in fpcmake itself

  Revision 1.2  2001/01/29 21:49:10  peter
    * lot of updates

  Revision 1.1  2001/01/24 21:59:36  peter
    * first commit of new fpcmake

}
