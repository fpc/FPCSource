{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
 This file implements the system-dependent calls needed for the 
 classes unit.
}

{
  oscalls contains a copy of the needed functions from syscalls.inc
  to make things go faster.
}

Function OSCreateFile (Const Filename : string; Mode : Longint) : longint;
{
  Creates a file with name FileName, using mode MODE
  --> Filename : String giving the path to the file.
      Mode     : Mode can be
                 fmCreate    : Create file;
                 fmOpenRead  : open for reading;
                 fmOpenWrite : open for writing;
  <-- File Handle, or -1 on error.
}

Begin
End;

Function OSReadHandle(Handle : Longint;Var Buffer; Count : Longint): Longint;
{
  Read from a handle
  --> Handle : file,pipe,etc Handle to read from.
      Buffer : Location where to put the read bytes.
      Count  : Number of bytes that should be read
  <-- Number of bytes actually read, or -1 on error.   
}

begin
end;

Function OSWriteHandle(Handle : Longint;var Buffer; Count : longint) : Longint;
{ 
  Write to a handle
  --> Handle : file,pipe,etc Handle to write to.
      Buffer : Location where to get the bytes.
      Count  : Number of bytes that should be written.
  <-- Number of bytes actually written, or -1 on error.   
}

begin
end;


Function OSSetHandleSize (Handle,Size : Longint) : longint;
{
  Set size of handle (for files only)
  --> Handle : Handle to set size for.
      Size   : Size to be set.
  <-- 0 on succes or -1 on error.
}

begin
end;

Function OSSeekHandle (Handle,OffSet,Origin : longint) : longint;
{
  Seek Handle position starting from Origin
  --> Handle : Handle of file to do seek on.
      Offset : Position to seek.
      Origin : Where to start seek: 
               soFromBeginning 
               soFromCurrent
               soFromEnd 
  <-- -1 on error, else the new fileposition in bytes from the origin.
}

begin
end;

Function OSCloseHandle (Handle : longint) : longint;
{
  Close file associated with HAndle.
  --> Handle : Handle of file to do seek on.
  <-- 0 on succes, -1 on error.
}

begin
end;
