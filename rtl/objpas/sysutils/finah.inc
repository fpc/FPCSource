{
    *********************************************************************
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

function ChangeFileExt(const FileName, Extension: RtlString): RtlString;
function ExtractFilePath(const FileName: RtlString): RtlString;
function ExtractFileDrive(const FileName: RtlString): RtlString;
function ExtractFileName(const FileName: RtlString): RtlString;
function ExtractFileExt(const FileName: RtlString): RtlString;
function ExtractFileDir(Const FileName : RtlString): RtlString;
function ExtractShortPathName(Const FileName : RtlString) : RtlString;
function ExpandFileName (Const FileName : RtlString): RtlString;
function ExpandUNCFileName (Const FileName : RtlString): RtlString;
function ExtractRelativepath (Const BaseName,DestNAme : RtlString): RtlString;
function IncludeTrailingPathDelimiter(Const Path : RtlString) : RtlString;
function IncludeTrailingBackslash(Const Path : RtlString) : RtlString;
function ExcludeTrailingBackslash(Const Path: RtlString): RtlString;
function ExcludeTrailingPathDelimiter(Const Path: RtlString): RtlString;
function IsPathDelimiter(Const Path: RtlString; Index: Integer): Boolean;
Procedure DoDirSeparators (Var FileName : RtlString);
Function SetDirSeparators (Const FileName : RtlString) : RtlString;
Function GetDirs (Var DirName : RtlString; Var Dirs : Array of PRtlChar) : Longint;
Function SameFileName(const S1, S2: RtlString): Boolean;

