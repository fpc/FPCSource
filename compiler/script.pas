{
    $Id$
    Copyright (c) 1998 by Peter Vreman

    This unit handles the writing of script files

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit Script;
interface

uses
  CObjects;

type
  PScript=^TScript;
  TScript=object
    fn   : string[80];
    data : TStringQueue;
    constructor Init(const s:string);
    destructor Done;
    procedure AddStart(const s:string);
    procedure Add(const s:string);
    Function Empty:boolean;
    procedure WriteToDisk;virtual;
  end;

  TAsmScript = Object (TScript)
    Constructor Init (Const ScriptName : String);
    Procedure AddAsmCommand (Const Command, Options,FileName : String);
    Procedure AddLinkCommand (Const Command, Options, FileName : String);
    Procedure AddDeleteCommand (Const FileName : String);
    Procedure WriteToDisk;virtual;
    end;
  PAsmScript = ^TAsmScript;

{ Asm response file }
var
  AsmRes : TAsmScript;


implementation

uses
{$ifdef linux}
  linux,
{$endif}
  globals,systems;


{****************************************************************************
                                  TScript
****************************************************************************}

constructor TScript.Init(const s:string);
begin
  fn:=FixFileName(s)+source_info.scriptext;
  data.Init;
end;


destructor TScript.Done;
begin
  data.done;
end;


procedure TScript.AddStart(const s:string);
begin
  data.Insert(s);
end;


procedure TScript.Add(const s:string);
begin
  data.Concat(s);
end;


Function TScript.Empty:boolean;
begin
  Empty:=Data.Empty;
end;


procedure TScript.WriteToDisk;
var
  t : Text;
begin
  Assign(t,fn);
  Rewrite(t);
  while not data.Empty do
   Writeln(t,data.Get);
  Close(t);
{$ifdef linux}
  ChMod(fn,493);
{$endif}
end;


{****************************************************************************
                                  Asm Response
****************************************************************************}

Constructor TAsmScript.Init (Const ScriptName : String);

begin
  Inherited Init(ScriptName);
end;

Procedure TAsmScript.AddAsmCommand (Const Command, Options,FileName : String);

begin
  {$ifdef linux}
  Add('echo Assembling '+FileName);
  Add (Command+' '+Options);
  Add('if [ $? != 0 ]; then DoExitAsm '+FileName+'; fi');
  {$else}
  Add('SET THEFILE='+FileName);
  Add('echo Assembling %THEFILE%');
  Add(command+' '+Options);
  Add('if errorlevel 1 goto asmend');
  {$endif}
end;

Procedure TasmScript.AddLinkCommand (Const Command, Options, FileName : String);

begin
  {$ifdef linux}
  Add('echo Linking '+FileName);
  Add (Command+' '+Options);
  Add('if [ $? != 0 ]; then DoExitLink '+FileName+'; fi');
  {$else}
  Add('SET THEFILE='+FileName);
  Add('echo Linking %THEFILE%');
  Add (Command+' '+Options);
  Add('if errorlevel 1 goto linkend');
  {$endif}
end;


 Procedure TAsmScript.AddDeleteCommand (Const FileName : String);

begin
 {$ifdef linux}
 Add('rm '+FileName);
 {$else}
 Add('Del '+FileName);
 {$endif}
end;


Procedure TAsmScript.WriteToDisk;

Begin
{$ifdef linux}
  AddStart('{ echo "An error occurred while linking $1"; exit 1; }');
  AddStart('DoExitLink ()');
  AddStart('{ echo "An error occurred while assembling $1"; exit 1; }');
  AddStart('DoExitAsm ()');
  AddStart('#!/bin/bash');
{$else}
  AddStart('@echo off');
  Add('goto end');
  Add(':asmend');
  Add('echo An error occured while assembling %THEFILE%');
  Add('goto end');
  Add(':linkend');
  Add('echo An error occured while linking %THEFILE%');
  Add(':end');
{$endif}
  TScript.WriteToDisk;
end;


end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:13  root
  Initial revision

  Revision 1.6  1998/03/10 01:17:28  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.5  1998/02/23 12:53:46  pierre
    + added some info when running ppas.bat
      * updated makefile

  Revision 1.4  1998/02/19 00:11:08  peter
    * fixed -g to work again
    * fixed some typos with the scriptobject

  Revision 1.3  1998/02/18 14:18:30  michael
  + added log at end of file (retroactively)

  revision 1.2
  date: 1998/02/18 13:43:15;  author: michael;  state: Exp;  lines: +75 -20
  + Implemented an OS independent AsmRes object.
  ----------------------------
  revision 1.1
  date: 1998/02/17 21:44:04;  author: peter;  state: Exp;
    + Initial implementation
}
