{
    $Id$
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Unit to read a CDROM disc TOC and get a list of CD Rom devices

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit cdrom;

{$mode objfpc}
{$h+}


interface

Type 
  TTocEntry = Record 
    min, sec, frame : Integer;
  end;
  PTocEntry = ^TTocEntry;

Function ReadCDTOC(Device : String; Var CDTOC : Array of TTocEntry) : Integer;
Function GetCDRomDevices(Var Devices : Array of string) : Integer;

Implementation

{$ifdef linux}
{$i cdromlin.inc}
{$else}
{$i cdromw32.inc}
{$endif}

end.
{
  $Log$
  Revision 1.2  2003-01-05 20:46:32  michael
  + Added ; at the end of the record declaration for fpdoc

  Revision 1.1  2002/09/08 14:03:31  michael
  + Initial implementation on Linux/win32

}