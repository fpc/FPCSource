{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function  DiskFree(drive: byte) : int64;
Function  DiskSize(drive: byte) : int64;
Function GetCurrentDir : String;
Function SetCurrentDir (Const NewDir : String) : Boolean;
Function CreateDir (Const NewDir : String) : Boolean;
Function RemoveDir (Const Dir : String) : Boolean;
Function ForceDirectories(Const Dir: string): Boolean;

