{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses DosCalls;

{ Declared EXPLICITLY with Ansistring, so NO mistaking is possible }

function GetEnv (var EnvVar: AnsiString): AnsiString;

var RC: longint;
    P: PChar;

begin
 RC := DosScanEnv (PChar (EnvVar), P);
 GetEnv := P;
end;

{
  $Log$
  Revision 1.4  2000-10-28 16:56:02  hajny
    + real (ansistring) implementation

  Revision 1.3  2000/08/25 17:33:44  hajny
    * Made compilable again (missing bracket at the begin of logs)

  Revision 1.2  2000/07/13 11:33:01  michael
  + removed logs
 
}
