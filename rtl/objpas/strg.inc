{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file implements english error message strings
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  German string constants for any messages issued by the sysutils unit.
  Please have them ordered by constant name.
}

Const

   { Error messages for exceptions }
   
   SAccessDenied = 'Zugriff verweigert';
   SDiskFull = 'Plattenspeichermedium voll';
   SEndOfFile = 'Lesezugriff hinter Dateiende'; 
   SInValidFileName = 'Ungltiger Dateiname';
   SInvalidInput = 'Ungltige Eingabe';
   SInvalidPointer = 'Ungltiger Zeigeroperation';
   SOutOfMemory = 'Speicher voll';
   STooManyOpenFiles = 'Zu viele offene Dateien';
   SUnKnownRunTimeError = 'Unbekannter Laufzeitfehler : %3.3d';
   SFileNotFound = 'Datei nicht gefunden';   
   
{
  $Log$
  Revision 1.4  2000-01-07 16:41:44  daniel
    * copyright 2000

  Revision 1.3  1999/08/28 14:53:27  florian
    * bug 471 fixed: run time error 2 is now converted into a file not
      found exception

  Revision 1.2  1999/07/02 17:03:23  florian
    + added some runtime->excpetin wrappers: eintoverflow, eoverflow, eunderflow, einvalidop

  Revision 1.1  1998/10/01 16:26:06  florian
    * Initial revision
}   
