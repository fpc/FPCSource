{
    $Id$
    This file is part of the Free Pascal run time library.

    POSIX Interface to the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This is the core of the system unit *nix systems (now FreeBSD
     and Unix).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure MkDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fpmkdir(@buffer, MODE_MKDIR)<0 Then
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
  If Fprmdir(@buffer)<0 Then
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
  If Fpchdir(@buffer)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
  { file not exists is path not found under tp7 }
  if InOutRes=2 then
   InOutRes:=3;
End;

procedure getdir(drivenr : byte;var dir : shortstring);
var
{$ifndef usegetcwd}
  cwdinfo      : stat;
  rootinfo     : stat;
  thedir,dummy : string[255];
  dirstream    : pdir;
  d            : pdirent;
  name         : string[255];
  thisdir      : stat;
  tmp          : string[255];
{$else}
  tmp          : array[0..4095] of char;
{$endif}

begin
{$ifdef usegetcwd}
 if Fpgetcwd(@tmp,10240+512)<>NIL then
  dir:=pchar(@tmp)
 else
  begin
    dir:='';
    writeln(geterrno);
  end;
{$else}
  dir:='';
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
    If Fpclosedir(dirstream)<0 THen
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
 {$endif}
end;

{
  $Log$
  Revision 1.1  2005-02-06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

}