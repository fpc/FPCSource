{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

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
    *********************************************************************

    System Utilities For Free Pascal
}

Const
  DirSeparators : set of char = ['/','\'];  
  OSDirSeparator = system.pathdelim;
   

function ChangeFileExt(const FileName, Extension: string): string;
function ExtractFilePath(const FileName: string): string;
function ExtractFileDrive(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string; 
function ExtractFileDir(Const FileName : string): string;
function ExpandFileName (Const FileName : string): String;
function ExpandUNCFileName (Const FileName : string): String;
function ExtractRelativepath (Const BaseName,DestNAme : String): String;
Procedure DoDirSeparators (Var FileName : String);
Function SetDirSeparators (Const FileName : String) : String;
Function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint; 

{
  $Log$
  Revision 1.5  2001-11-07 14:58:24  michael
  + Added PathDelim,DriveDelim,PathSep; Removed PathSeparator

  Revision 1.4  2001/11/07 13:57:26  michael
  + Added PathSeparator and OSDirSeparator

  Revision 1.3  2000/11/13 14:41:20  marco
   * Unix renamefest for defines

  Revision 1.2  2000/07/13 11:33:51  michael
  + removed logs
 
}
