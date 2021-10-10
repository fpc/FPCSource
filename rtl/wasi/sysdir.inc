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

procedure Do_MkDir(s: rawbytestring);
var
  fd: __wasi_fd_t;
  pr: ansistring;
  res: __wasi_errno_t;
begin
  if not ConvertToFdRelativePath(s,fd,pr) then
    exit;
  res:=__wasi_path_create_directory(fd,PChar(pr),Length(pr));
  if res=__WASI_ERRNO_SUCCESS then
    InOutRes:=0
  else
    InOutRes:=Errno2InoutRes(res);
end;

procedure Do_RmDir(s: rawbytestring);
var
  fd: __wasi_fd_t;
  pr: ansistring;
  res: __wasi_errno_t;
begin
  if not ConvertToFdRelativePath(s,fd,pr) then
    exit;
  res:=__wasi_path_remove_directory(fd,PChar(pr),Length(pr));
  if res=__WASI_ERRNO_SUCCESS then
    InOutRes:=0
  else
    InOutRes:=Errno2InoutRes(res);
end;

procedure do_ChDir_internal(s: rawbytestring; SymLinkFollowCount: longint);

  function GetNextPart: ansistring;
    var
      slashpos,backslashpos: longint;
    begin
      slashpos:=Pos('/',s);
      backslashpos:=Pos('\',s);
      if (slashpos<>0) and ((slashpos<backslashpos) or (backslashpos=0)) then
      begin
        result:=Copy(s,1,slashpos-1);
        delete(s,1,slashpos);
      end
      else if backslashpos<>0 then
      begin
        result:=Copy(s,1,backslashpos-1);
        delete(s,1,backslashpos);
      end
      else
      begin
        result:=s;
        s:='';
      end;
      while (s<>'') and (s[1] in ['/','\']) do
        delete(s,1,1);
    end;

const
  MaxSymLinkSize = 4096;
var
  new_drive_nr: longint;
  new_dir,new_dir_save,next_dir_part: ansistring;
  fd: __wasi_fd_t;
  pr: ansistring;
  st: __wasi_filestat_t;
  res: __wasi_errno_t;
  symlink: ansistring;
  symlink_len: __wasi_size_t;
begin
  if SymLinkFollowCount<0 then
  begin
    InOutRes:=40;
    exit;
  end;
  if HasDriveLetter(s) then
  begin
    new_drive_nr:=Ord(UpCase(s[1]))-(Ord('A')-1);
    delete(s,1,2);
  end
  else
    new_drive_nr:=current_drive;
  if (new_drive_nr>drives_count) or (current_dirs[new_drive_nr].dir_name='') then
  begin
    InoutRes:=15;
    exit;
  end;
  new_dir:=current_dirs[new_drive_nr].dir_name;
  if s<>'' then
  begin
    if s[1] in ['/','\'] then
    begin
      delete(s,1,1);
      if new_drive_nr>0 then
        new_dir:=Copy(new_dir,1,2)+'/'
      else
        new_dir:='/';
    end;
    while s<>'' do
    begin
      next_dir_part:=GetNextPart;
      if next_dir_part='.' then
        {nothing to do}
      else if next_dir_part='..' then
      begin
        if (new_dir<>'') and not (new_dir[Length(new_dir)] in ['/','\']) then
        begin
          while (new_dir<>'') and not (new_dir[Length(new_dir)] in ['/','\']) do
            delete(new_dir,Length(new_dir),1);
          while (new_dir<>'') and (new_dir[Length(new_dir)] in ['/','\']) do
            delete(new_dir,Length(new_dir),1);
          if (Pos('/',new_dir)=0) and (Pos('\',new_dir)=0) then
            new_dir:=new_dir+'/';
        end;
      end
      else
      begin
        new_dir_save:=new_dir;
        if (new_dir<>'') and (new_dir[Length(new_dir)] in ['/','\']) then
          new_dir:=new_dir+next_dir_part
        else
          new_dir:=new_dir+'/'+next_dir_part;
        if not ConvertToFdRelativePath(new_dir,fd,pr) then
        begin
          {...}
          InOutRes:=3;
          exit;
        end;
        res:=__wasi_path_filestat_get(fd,0,PChar(pr),Length(pr),@st);
        if res<>__WASI_ERRNO_SUCCESS then
        begin
          if res=__WASI_ERRNO_NOENT then
            InOutRes:=3
          else
            InOutRes:=Errno2InoutRes(res);
          exit;
        end;
        if st.filetype=__WASI_FILETYPE_SYMBOLIC_LINK then
        begin
          SetLength(symlink,MaxSymLinkSize);
          res:=__wasi_path_readlink(fd,PChar(pr),Length(pr),@symlink[1],Length(symlink),@symlink_len);
          if res<>__WASI_ERRNO_SUCCESS then
          begin
            InOutRes:=Errno2InoutRes(res);
            exit;
          end;
          SetLength(symlink,symlink_len);
          if (symlink<>'') and (symlink[1] in ['/', '\']) then
            do_ChDir_internal(symlink,SymLinkFollowCount-1)
          else if (new_dir_save<>'') and (new_dir_save[1] in ['/', '\']) then
            do_ChDir_internal(new_dir_save+symlink,SymLinkFollowCount-1)
          else
            do_ChDir_internal(new_dir_save+'/'+symlink,SymLinkFollowCount-1);
          exit;
        end
        else if st.filetype<>__WASI_FILETYPE_DIRECTORY then
        begin
          InOutRes:=5;
          exit;
        end;
      end;
    end;
  end;
  current_drive:=new_drive_nr;
  current_dirs[new_drive_nr].dir_name:=new_dir;
  InOutRes:=0;
end;

procedure do_ChDir(s: rawbytestring);
begin
  do_ChDir_internal(s, 40);
end;

procedure do_getdir(drivenr : byte;var dir : rawbytestring);
begin
  if drivenr=0 then
    drivenr:=current_drive;
  if (drivenr<=drives_count) and (current_dirs[drivenr].dir_name<>'') then
    dir:=current_dirs[drivenr].dir_name
  else
    InoutRes:=15;
end;
