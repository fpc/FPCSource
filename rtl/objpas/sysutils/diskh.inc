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
Function GetCurrentDir : {$ifdef FPC_UNICODE_RTL}UnicodeString{$else}AnsiString{$endif};

Function SetCurrentDir (Const NewDir : RawByteString) : Boolean;
Function CreateDir (Const NewDir : RawByteString) : Boolean;
Function RemoveDir (Const Dir : RawByteString) : Boolean;
Function ForceDirectories(Const Dir: RawByteString): Boolean;

Function SetCurrentDir (Const NewDir : UnicodeString) : Boolean;
Function CreateDir (Const NewDir : UnicodeString) : Boolean;
Function RemoveDir (Const Dir : UnicodeString) : Boolean;
Function ForceDirectories(Const Dir: UnicodeString): Boolean;

