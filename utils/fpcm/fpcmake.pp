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
      dos,sysutils,
      fpcmmain,fpcmwr;

    procedure Verbose(s:string);
      begin
        writeln(s);
      end;


    procedure Error(s:string);
      begin
        Writeln('Error: ',s);
        Halt(1);
      end;


    procedure ProcessFile(const fn:string);
      var
        CurrFPCMake : TFPCMake;
        CurrMakefile : TMakefileWriter;
        s : string;
      begin
        CurrFPCMake:=nil;
//        try
          writeln('Processing ',fn);
          CurrFPCMake:=TFPCMake.Create(fn);
          s:=GetEnv('FPCDIR');
          if s<>'' then
           CurrFPCMake.Variables.Add('FPCDIR',s)
          else
           CurrFPCMake.Variables.Add('FPCDIR','c:/pp');
          CurrFPCMake.Variables.Add('UNITSDIR','$(FPCDIR)/units');
          CurrFPCMake.Variables.Add('PACKAGESDIR','$(FPCDIR)/packages');
          CurrFPCMake.LoadMakefileFPC;
//          CurrFPCMake.Print;

          CurrMakefile:=TMakefileWriter.Create(CurrFPCMake,ExtractFilePath(fn)+'Makefile');
          CurrMakefile.WriteGenericMakefile;
          CurrMakefile.Free;

//        except
//          on e : exception do
//           Error(e.message);
//        end;
        CurrFPCMake.Free;
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
  for i:=1 to ParamCount do
   ProcessFile(ParamStr(i));
end;


begin
  if ParamCount=0 then
   UseMakefilefpc
  else
   UseParameters;
end.
{
  $Log$
  Revision 1.2  2001-01-29 21:49:10  peter
    * lot of updates

  Revision 1.1  2001/01/24 21:59:36  peter
    * first commit of new fpcmake

}
