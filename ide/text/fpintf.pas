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

implementation

uses
  Compiler,
  FPSwitch;

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
end;


{****************************************************************************
                                   Compile
****************************************************************************}

procedure Compile(const FileName: string);
begin
{   WriteSwitches('fp.cfg'); }
{ call the compiler }
  Compiler.Compile('[fp.cfg] -d'+SwitchesModeStr[SwitchesMode]+' '+FileName);
end;

end.
{
  $Log$
  Revision 1.3  1999-02-05 13:51:41  peter
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
