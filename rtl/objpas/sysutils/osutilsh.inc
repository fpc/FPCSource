{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ OS handling utilities }

{$ifdef HAS_OSERROR}
Function GetLastOSError : Integer; 
{$endif}
Procedure RaiseLastOSError;
Function GetEnvironmentVariable(Const EnvVar : String) : String;
{$IFDEF HAS_SLEEP}
procedure Sleep(milliseconds: Cardinal);
{$ENDIF}
function ExecuteProcess(Const Path: AnsiString; Const ComLine: AnsiString):integer;
function ExecuteProcess(Const Path: AnsiString; Const ComLine: Array of AnsiString):integer;

Function GetAppConfigDir(Global : Boolean) : String;
Function GetAppConfigFile(Global : Boolean) : String;
Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;
Function ApplicationName : String;

Const
  ConfigExtension : String = '.cfg';
  SysConfigDir    : String = '';   
  
Type 
  TGetAppNameEvent = Function : String;

Var
  OnGetApplicationName : TGetAppNameEvent;



{
  $Log$
  Revision 1.7  2004-08-05 07:28:01  michael
  + Added getappconfigdir calls

  Revision 1.6  2004/02/13 13:02:21  marco
   * addition of second executeprocess prototype

  Revision 1.5  2004/02/08 11:02:40  michael
  + Implemented RaiseLastOSError

  Revision 1.4  2004/01/10 17:34:36  michael
  + Implemented sleep() on Unix.

  Revision 1.3  2004/01/05 22:37:24  florian
    * changed sysutils.exec to ExecuteProcess

  Revision 1.2  2004/01/03 08:55:58  marco
   * Exec(ansistring) function

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.3  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

}