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
  FPCfgs;

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
  WriteOptions('fp.cfg');
{ call the compiler }
  Compiler.Compile('[fp.cfg] '+FileName);
end;

end.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.4  1998/12/22 10:39:43  peter
    + options are now written/read
    + find and replace routines

}
