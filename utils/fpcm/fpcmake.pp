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

    procedure ProcessFile(const fn:string);
      var
        CurrFPCMake : TFPCMake;
        CurrMakefile : TMakefileWriter;
        s : string;
      begin
        CurrFPCMake:=nil;
//        try
          CurrFPCMake:=TFPCMake.Create(fn);
          s:=GetEnv('FPCDIR');
          if s<>'' then
           CurrFPCMake.Variables.Add('FPCDIR',s)
          else
           CurrFPCMake.Variables.Add('FPCDIR','c:/pp');
          CurrFPCMake.Variables.Add('UNITSDIR','$(FPCDIR)/units');
          CurrFPCMake.Variables.Add('PACKAGESDIR','$(FPCDIR)/packages');
          CurrFPCMake.LoadMakefileFPC;
          CurrFPCMake.LoadPackageSection;
          CurrFPCMake.LoadRequires(CurrFPCMake);
//          CurrFPCMake.Print;

          CurrMakefile:=TMakefileWriter.Create(CurrFPCMake,'Makefile');
          CurrMakefile.WriteGenericMakefile;
          CurrMakefile.Free;

//        except
//          on e : exception do
//           writeln('Error: ',e.message);
//        end;
        CurrFPCMake.Free;
      end;

begin
  ProcessFile('Makefile.fpc');
end.
{
  $Log$
  Revision 1.1  2001-01-24 21:59:36  peter
    * first commit of new fpcmake

}
