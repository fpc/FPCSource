{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    File utility calls
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Function FileOpen (Const FileName : string; Mode : Integer) : Longint;

const
  AccessMode: array[0..2] of Integer = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);

Var FN : string;

begin
  FN:=FileName+#0;
  Result := CreateFile(@FN[1], AccessMode[Mode and 3],
                         ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileCreate (Const FileName : String) : Longint;

Var FN : string;

begin
  FN:=FileName+#0;
 Result := CreateFile(@FN[1], GENERIC_READ or GENERIC_WRITE,
     0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;

Var res : Longint;

begin
  if not ReadFile(Handle, @Buffer, Count, @res, nil) then res := -1;
  FileRead:=Res;
end;


Function FileWrite (Handle : Longint; Var Buffer; Count : Longint) : Longint;

Var Res : longint;

begin
  if not WriteFile(Handle, @Buffer, Count, @Res, nil) then Res:= -1;
  FileWrite:=Res;
end;


Function FileSeek (Handle,Offset,Origin : Longint) : Longint;

begin
  Result := SetFilePointer(Handle, Offset, nil, Origin);
end;


Procedure FileClose (Handle : Longint);

begin
  CloseHandle(Handle);
end;

Function FileTruncate (Handle,Size: Longint) : boolean;

begin
  Result:=SetFilePointer(handle,Size,nil,FILE_BEGIN)<>-1;
  If Result then 
    Result:=SetEndOfFile(handle);
end;


Function FileAge (Const FileName : String): Longint;

var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
  Fn : String;
  
begin
  FN:=FN+#0;
  Handle := FindFirstFile(@FN[1], @FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
{
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, @LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
                               LongRec(Result).Lo) then Exit;
}      end;
    end;
  Result := -1;
end;


Function FileExists (Const FileName : String) : Boolean;

begin
  Result := FileAge(FileName) <> -1;
end;


Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;

begin
  //!! Needs implementing    
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;

begin
  //!! Needs implementing    
end;


Procedure FindClose (Var F : TSearchrec);

begin
  //!! Needs implementing    
end;


Function FileGetDate (Handle : Longint) : Longint;

begin
  //!! Needs implementing    
end;


Function FileSetDate (Handle,Age : Longint) : Longint;

begin
  //!! Needs implementing    
end;


Function FileGetAttr (Const FileName : String) : Longint;

begin
  //!! Needs implementing    
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;

begin
  //!! Needs implementing    
end;


Function DeleteFile (Const FileName : String) : Boolean;

begin
  //!! Needs implementing    
end;


Function RenameFile (Const OldName, NewName : String) : Boolean;

begin
  //!! Needs implementing    
end;


Function FileSearch (Const Name, DirList : String) : String;

begin
  //!! Needs implementing    
end;


{
  $Log$
  Revision 1.3  1999-02-09 12:01:03  michael
  + Implemented filetruncate

  Revision 1.2  1999/02/03 11:41:30  michael
  + Added filetruncate

  Revision 1.1  1998/10/11 12:21:01  michael
  Added file calls. Implemented for linux only

}
