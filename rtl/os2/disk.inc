{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas
        
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFDEF INT64}

function DiskFree (Drive : Byte) : int64;

begin
end;


function DiskSize (Drive : Byte) : int64;

begin
end;

{$ELSE}

function DiskFree (Drive : Byte) : Longint;

begin
end;


function DiskSize (Drive : Byte) : Longint;

begin
end;

{$ENDIF}

Function GetCurrentDir : String;

begin
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;

begin
end;


Function CreateDir (Const NewDir : String) : Boolean;

begin
end;


Function RemoveDir (Const Dir : String) : Boolean;

begin
end;



{
 $Log$
 Revision 1.4  2000-05-21 15:55:11  hajny
   * int64 result for Disk* functions

 Revision 1.3  2000/02/09 16:59:33  peter
   * truncated log

 Revision 1.2  2000/01/07 16:41:45  daniel
   * copyright 2000

}
