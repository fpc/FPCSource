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

var P: PChar;

begin
 if DosScanEnv (PChar (EnvVar), P) = 0 then GetEnv := P else GetEnv := nil;
end;

{
  $Log$
  Revision 1.1.2.2  2001-01-28 18:41:07  hajny
    * error checking added

  Revision 1.1.2.1  2000/10/28 16:56:43  hajny
    + real (ansistring) implementation

 
}
