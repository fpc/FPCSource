{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Olle Raab

    Some utilities specific for Mac OS
    Modified portions from Peter N. Lewis (PNL Libraries). Thanks !

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit macutils;

interface

uses
  macostp;

function FourCharCodeToLongword(fourcharcode: Shortstring): Longword;

function BitIsSet(arg: Longint; bitnr: Integer): Boolean;

{ Converts MacOS specific error codes to the correct FPC error code.
  All non zero MacOS errors corresponds to a nonzero FPC error.}
function MacOSErr2RTEerr(err: OSErr): Integer;


{Translates a unix or dos path to a mac path. Even a mac path can be input, }
{then it is returned as is. A trailing directory separator in input}
{will result in a trailing mac directory separator. For absolute paths, the }
{parameter mpw affects how the root volume is denoted. If mpw is true, }
{the path is intended for use in MPW, and the environment variable Boot is}
{prepended. Otherwise the actual boot volume name is appended.}
{All kinds of paths are attempted to be translated, except relative path on}
{a certain drive, like: C:xxx\yyy }

function TranslatePathToMac (const path: string; mpw: Boolean): string;


{Concats the relative or full path1 to the relative path2.}
function ConcatMacPath (path1, path2: string): string;


function IsMacFullPath (const path: string): Boolean;


function IsDirectory (var spec: FSSpec): Boolean;

function PathArgToFSSpec(s: string; var spec: FSSpec): Integer;

function PathArgToFullPath(s: string; var fullpath: AnsiString): Integer;

{Gives the volume name (with appended colon) for a given volume reference number.}
function GetVolumeName(vRefNum: Integer; var volName: String): OSErr;

function GetWorkingDirectoryVRefNum: Integer;

{Find an application with the given creator, in any of the mounted volumes.}
function FindApplication (creator: OSType; var fs: FSSpec): OSErr;

{Launch the application given by applicationFileSpec. If toFront is true
 it will be brought to the foreground when launched.}
function LaunchFSSpec (tofront: Boolean; const applicationFileSpec: FSSpec): OSErr;

implementation

{Actually defined in system.pp. Declared here to be used in macutils.inc: }
var
  {emulated working directory}
  workingDirectorySpec: FSSpec; cvar; external;

{$I macutils.inc}

end.
