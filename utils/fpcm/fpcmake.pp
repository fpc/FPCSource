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
      sysutils,
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
{$ifdef SUBDIRS}
        s,Subdirs : string;
        t : ttarget;
{$endif SUBDIRS}
      begin
        CurrFPCMake:=nil;
//        try
          writeln('Processing ',fn);
          { Load Makefile.fpc }
          CurrFPCMake:=TFPCMake.Create(fn);
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
          { Free }
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
  Revision 1.3  2001-02-22 21:11:24  peter
    * fpcdir detection added
    * fixed loading of variables in fpcmake itself

  Revision 1.2  2001/01/29 21:49:10  peter
    * lot of updates

  Revision 1.1  2001/01/24 21:59:36  peter
    * first commit of new fpcmake

}
