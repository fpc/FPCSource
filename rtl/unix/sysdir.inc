{
    This file is part of the Free Pascal run time library.

    Main OS dependant body of the system unit, loosely modelled
    after POSIX.  *BSD version (Linux version is near identical)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure MkDir(Const s: String);[IOCheck];
const
  { read/write search permission for everyone }
  MODE_MKDIR = S_IWUSR OR S_IRUSR OR
               S_IWGRP OR S_IRGRP OR
               S_IWOTH OR S_IROTH OR
               S_IXUSR OR S_IXGRP OR S_IXOTH;
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fpmkdir(@buffer[0], MODE_MKDIR)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  if (s = '.') then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fprmdir(@buffer[0])<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fpchdir(@buffer[0])<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
  { file not exists is path not found under tp7 }
  if InOutRes=2 then
   InOutRes:=3;
End;

// !! for now we use getcwd, unless we are fpc_use_libc.
// !! the old code  is _still needed_ since the syscall sometimes doesn't work
// !! on special filesystems like NFS etc.
// !! In the libc versions, the alt code is already integrated in the libc code.
// !! Also significantly boosted buffersize. This will make failure of the 
// !! dos legacy api's better visibile due to cut-off path, instead of "empty"
procedure getdir(drivenr : byte;var dir : shortstring);
var
  buf          : array[0..2047] of char;
  cwdinfo      : stat;
  rootinfo     : stat;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  name         : string[255];
  thisdir      : stat;
  tmp          : string[255];

begin
  dir:='';
 if Fpgetcwd(@buf[0],sizeof(buf))<>nil then
   dir:=strpas(buf)
{$ifndef FPC_USE_LIBC}
 else 
  begin
  thedir:='';
  dummy:='';

  { get root directory information }
  tmp := '/'+#0;
  if Fpstat(@tmp[1],rootinfo)<0 then
    Exit;
  repeat
    tmp := dummy+'.'+#0;
    { get current directory information }
    if Fpstat(@tmp[1],cwdinfo)<0 then
      Exit;
    tmp:=dummy+'..'+#0;
    { open directory stream }
    { try to find the current inode number of the cwd }
    dirstream:=Fpopendir(@tmp[1]);
    if dirstream=nil then
       exit;
    repeat
      name:='';
      d:=Fpreaddir(dirstream);
      { no more entries to read ... }
      if not assigned(d) then
        break;
      tmp:=dummy+'../'+strpas(d^.d_name) + #0;
      if (Fpstat(@tmp[1],thisdir)=0) then
       begin
         { found the entry for this directory name }
         if (cwdinfo.st_dev=thisdir.st_dev) and (cwdinfo.st_ino=thisdir.st_ino) then
          begin
            { are the filenames of type '.' or '..' ? }
            { then do not set the name.               }
            if (not ((d^.d_name[0]='.') and ((d^.d_name[1]=#0) or
                    ((d^.d_name[1]='.') and (d^.d_name[2]=#0))))) then
              name:='/'+strpas(d^.d_name);
          end;
       end;
    until (name<>'');
    if Fpclosedir(dirstream)<0 then
      Exit;
    thedir:=name+thedir;
    dummy:=dummy+'../';
    if ((cwdinfo.st_dev=rootinfo.st_dev) and (cwdinfo.st_ino=rootinfo.st_ino)) then
      begin
        if thedir='' then
          dir:='/'
        else
          dir:=thedir;
        exit;
      end;
  until false;
  end;
 {$endif}
end;

