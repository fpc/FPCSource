{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2010 by Sven Barth

    FPC Pascal system unit for the Native NT API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure MkDir(s: pchar; len: sizeuint); [IOCheck, public, alias : 'FPC_SYS_MKDIR'];
var
  objattr: TObjectAttributes;
  name: TNtUnicodeString;
  res: LongInt;
  iostatus: TIOStatusBlock;
  h: THandle;
begin
  if not Assigned(s) or (len <= 1) or (InOutRes <> 0) then
    Exit;

  SysPCharToNtStr(name, s, len);

  { first we try to create a directory object }
  SysInitializeObjectAttributes(objattr, @name, OBJ_PERMANENT, 0, Nil);

  res := NtCreateDirectoryObject(@h, 0, @objattr);
  if res <> STATUS_OBJECT_TYPE_MISMATCH then begin
    if res = STATUS_SUCCESS then
      NtClose(h);
    errno := res;
    Errno2InoutRes;
    SysFreeNtStr(name);
    Exit;
  end;

  { so the parent directory isn't a directory object... retry as normal file
    object }

  objattr.Attributes := 0; // OBJ_PERMANENT is not valid for file objects

  { the flags are based on ReactOS' CreateDirectoryW except the missing LIST
    access }
  res := NtCreateFile(@h, NT_SYNCHRONIZE, @objattr, @iostatus, Nil,
              FILE_ATTRIBUTE_NORMAL, FILE_SHARE_READ or FILE_SHARE_WRITE,
              FILE_CREATE, FILE_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT,
              Nil, 0);
  if res = STATUS_SUCCESS then
    NtClose(h);
  errno := res;
  Errno2InOutRes;
  SysFreeNtStr(name);
end;

procedure RmDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_RMDIR'];
var
  ntstr: TNtUnicodeString;
  objattr: TObjectAttributes;
  iostatus: TIOStatusBlock;
  h: THandle;
  disp: TFileDispositionInformation;
  res: LongInt;
begin
  if (len = 1) and (s^ = '.') then
    InOutRes := 16;
  if not assigned(s) or (len = 0) or (InOutRes <> 0) then
    Exit;
  if (len = 2) and (s[0] = '.') and (s[1] = '.') then
    InOutRes := 5;

  SysPCharToNtStr(ntstr, s, len);
  SysInitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);

  res := NtOpenDirectoryObject(@h, STANDARD_RIGHTS_REQUIRED, @objattr);
  if res >= 0 then begin
    { this is a directory object, so just make it temporary }
{$message warning 'Add check for subdirectories'}
    res := NtMakeTemporaryObject(h);
    NtClose(h);

    errno := res;
    Errno2InoutRes;

    SysFreeNtStr(ntstr);
  end else
  if res = STATUS_OBJECT_TYPE_MISMATCH then begin
    { this is a file directory or file, so do it like RemoveDirectoryW }
    res := NtCreateFile(@h, NT_DELETE or NT_SYNCHRONIZE, @objattr, @iostatus, Nil,
              0, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
              FILE_OPEN, FILE_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT,
              Nil, 0);

    if res >= 0 then begin
      disp.DeleteFile := True;

      { NtDeleteFile does not work here... }
      res := NtSetInformationFile(h, @iostatus, @disp,
        SizeOf(TFileDispositionInformation), FileDispositionInformation);

      NtClose(h);
    end;
  end;

  SysFreeNtStr(ntstr);
  errno := res;
  Errno2InoutRes;
end;

procedure ChDir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_CHDIR'];
begin
  { for now this is not supported }
  InOutRes := 3;
end;

procedure do_GetDir(DriveNr: byte; var Dir: UnicodeString);
begin
  { for now we return simply the root directory }
  Dir := DirectorySeparator;
end;
