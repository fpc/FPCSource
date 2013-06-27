{
    *********************************************************************
    Copyright (C) 1997, 1998 Gertjan Schouten

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

    System Utilities For Free Pascal
}

{$if not declared(TFilenameCaseMatch)}
type
  TFilenameCaseMatch = (mkNone, mkExactMatch, mkSingleMatch, mkAmbiguous);
{$endif}

function ChangeFileExt(const FileName, Extension: string): string;
function ExtractFilePath(const FileName: string): string;
function ExtractFileDrive(const FileName: string): string;
function ExtractFileName(const FileName: string): string;
function ExtractFileExt(const FileName: string): string;
function ExtractFileDir(Const FileName : string): string;
function ExtractShortPathName(Const FileName : String) : String;
function ExpandFileName (Const FileName : string): String;
function ExpandFileNameCase (const FileName: string; out MatchFound: TFilenameCaseMatch): string;
function ExpandUNCFileName (Const FileName : string): String;
function ExtractRelativepath (Const BaseName,DestNAme : String): String;
function IncludeTrailingPathDelimiter(Const Path : String) : String;
function IncludeTrailingBackslash(Const Path : String) : String;
function ExcludeTrailingBackslash(Const Path: string): string;
function ExcludeTrailingPathDelimiter(Const Path: string): string;
function IncludeLeadingPathDelimiter(Const Path : String) : String;
function ExcludeLeadingPathDelimiter(Const Path: string): string;
function IsPathDelimiter(Const Path: string; Index: Integer): Boolean;
Procedure DoDirSeparators (Var FileName : String);
Function SetDirSeparators (Const FileName : String) : String;
Function GetDirs (Var DirName : String; Var Dirs : Array of pchar) : Longint;
function ConcatPaths(const Paths: array of String): String;

