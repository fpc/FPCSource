{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

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
   SInValidFileName = 'UngÅltiger Dateiname';
   SInvalidInput = 'UngÅltige Eingabe';
   SInvalidPointer = 'UngÅltiger Zeigeroperation';
   SOutOfMemory = 'Speicher voll';
   STooManyOpenFiles = 'Zu viele offene Dateien';
   SUnKnownRunTimeError = 'Unbekannter Laufzeitfehler : %3.3d';
   
   
{
  $Log$
  Revision 1.1  1998-10-01 16:26:06  florian
    * Initial revision

}   
