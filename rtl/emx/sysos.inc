{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  ProcessID: SizeUInt;

function GetProcessID:SizeUInt;
begin
 GetProcessID := ProcessID;
end;


procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                                        var Handle: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 318;

function DosQueryProcAddr (Handle, Ordinal: cardinal; ProcName: PChar;
                                        var Address: pointer): cardinal; cdecl;
external 'DOSCALLS' index 321;

function DosSetRelMaxFH (var ReqCount: longint; var CurMaxFH: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 382;

function DosSetCurrentDir (Name:PChar): cardinal; cdecl;
external 'DOSCALLS' index 255;

function DosSetDefaultDisk (DiskNum:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 220;

{ This is not real prototype, but is close enough }
{ for us (the 2nd parameter is actually a pointer }
{ to a structure).                                }
function DosCreateDir (Name: PChar; P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 270;

function DosDeleteDir (Name: PChar): cardinal; cdecl;
external 'DOSCALLS' index 226;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';

{
procedure syscall; external 'EMX' index 2;

procedure emx_init; external 'EMX' index 1;
}



   { converts an OS/2 error code to a TP compatible error }
   { code. Same thing exists under most other supported   }
   { systems.                                             }
   { Only call for OS/2 DLL imported routines             }
   Procedure Errno2InOutRes;
   Begin
     { errors 1..18 are the same as in DOS }
     case InOutRes of
      { simple offset to convert these error codes }
      { exactly like the error codes in Win32      }
      19..31 : InOutRes := InOutRes + 131;
      { gets a bit more complicated ... }
      32..33 : InOutRes := 5;
      38 : InOutRes := 100;
      39 : InOutRes := 101;
      112 : InOutRes := 101;
      110 : InOutRes := 5;
      114 : InOutRes := 6;
      290 : InOutRes := 290;
     end;
     { all other cases ... we keep the same error code }
   end;




