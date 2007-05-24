Procedure MkDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  {$warning TODO BeOS MkDir implementation}
{  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fpmkdir(@buffer, MODE_MKDIR)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
}
End;


Procedure RmDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  {$warning TODO BeOS RmDir implementation}
{  if (s = '.') then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  Move(s[1], Buffer, Length(s));
  Buffer[Length(s)] := #0;
  If Fprmdir(@buffer)<0 Then
   Errno2Inoutres
  Else
   InOutRes:=0;
}
End;


Procedure ChDir(Const s: String);[IOCheck];
Var
  Buffer: Array[0..255] of Char;
Begin
  {$warning TODO BeOS ChDir implementation}
{  If (s='') or (InOutRes <> 0) then
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
}
End;

{ // $define usegetcwd}

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
{$endif}
  tmp          : string[255];

begin
{$ifdef usegetcwd}
 Fpgetcwd(@tmp[1],4096);
 dir:=tmp;
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
         if (cwdinfo.dev=thisdir.dev) and (cwdinfo.ino=thisdir.ino) then
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
    if ((cwdinfo.dev=rootinfo.dev) and (cwdinfo.ino=rootinfo.ino)) then
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
