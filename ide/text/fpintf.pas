{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Misc routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPIntf;
interface

{ Run }
function  GetRunParameters: string;
procedure SetRunParameters(const Params: string);

{ Compile }
procedure Compile(const FileName: string);
procedure SetPrimaryFile(const fn:string);


implementation

uses
  Compiler,
{$ifndef NODEBUG}
  FPDebug,
{$endif NODEBUG}
  FPVars,FPUtils,FPSwitch;

{****************************************************************************
                                   Run
****************************************************************************}

var
  RunParameters : string;

function GetRunParameters: string;
begin
  GetRunParameters:=RunParameters;
end;

procedure SetRunParameters(const Params: string);
begin
  RunParameters:=Params;
{$ifndef NODEBUG}
  If assigned(Debugger) then
    Debugger^.SetArgs(RunParameters);
{$endif}
end;


{****************************************************************************
                                   Compile
****************************************************************************}

procedure Compile(const FileName: string);
var
  cmd : string;
begin
  cmd:='[fp.cfg] -d'+SwitchesModeStr[SwitchesMode];
  if LinkAfter then
    cmd:=cmd+' -s';
{ Add the switches from the primary file }
  if PrimaryFileSwitches<>'' then
   cmd:=cmd+' '+PrimaryFileSwitches;
{ call the compiler }
  Compiler.Compile(cmd+' '+FileName);
end;


procedure SetPrimaryFile(const fn:string);
var
  t : text;
begin
  PrimaryFile:='';
  PrimaryFileMain:='';
  PrimaryFileSwitches:='';
  PrimaryFilePara:='';
  if UpcaseStr(ExtOf(fn))='.PRI' then
   begin
     assign(t,fn);
     {$I-}
     reset(t);
     if ioresult=0 then
      begin
        PrimaryFile:=fn;
        readln(t,PrimaryFileMain);
        readln(t,PrimaryFileSwitches);
        readln(t,PrimaryFilePara);
        close(t);
      end;
     {$I+}
     EatIO;
   end
  else
   begin
     PrimaryFile:=fn;
     PrimaryFileMain:=fn;
   end;
  if PrimaryFilePara<>'' then
   SetRunParameters(PrimaryFilePara);
end;



end.
{
  $Log$
  Revision 1.8  2000-01-03 11:38:34  michael
  Changes from Gabor

  Revision 1.7  1999/09/16 14:34:59  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.6  1999/06/30 23:58:15  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.5  1999/06/21 23:38:37  pierre
   + support for LinkAfter var

  Revision 1.4  1999/03/12 01:12:22  peter
    * extended primaryfile to load a .pri file

  Revision 1.3  1999/02/05 13:51:41  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.2  1998/12/28 15:47:45  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.4  1998/12/22 10:39:43  peter
    + options are now written/read
    + find and replace routines

}
