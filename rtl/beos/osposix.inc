{
    $Id$
    Copyright (c) 2001 by Carl Eric Codere

    Implements POSIX 1003.1  interface

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}


const           
      syscall_nr_exit   =   $3F;
      syscall_nr_chdir  =   $57; 
      syscall_nr_mkdir  =   $1E; 
      syscall_nr_unlink =   $27;
      syscall_nr_rmdir  =   $60;
      syscall_nr_close  =   $01;
      syscall_nr_read   =   $02;
      syscall_nr_write  =   $03;
      syscall_nr_stat   =   $30;
      syscall_nr_fstat  =   $30;
      syscall_nr_rename =   $26;
      syscall_nr_access =   $58;
      syscall_nr_opendir=   $0C;
      syscall_nr_closedir=  $0F;
      syscall_nr_sigaction= $70;
      syscall_nr_time     = $07;
      syscall_nr_open     = $00;
      syscall_nr_readdir  = $1C;
      syscall_nr_lseek    = $05;
      syscall_nr_ftruncate = $4b;

      S_IFDIR   =$004000; { Directory.     }
      S_IFCHR   =$002000; { Character device. }
      S_IFBLK   =$006000; { Block device.  }
      S_IFREG   =$008000; { Regular file.  }
      S_IFIFO   =$001000; { FIFO.          }
      S_IFLNK   =$00A000; { Symbolic link. }

type
  { _kwstat_ kernel call structure }
  pwstat = ^twstat;
  twstat = packed record
{00}   filler : array[1..3] of longint;
{12}   newmode : mode_t;     { chmod mode_t parameter }
{16}   unknown1 : longint;  
{20}   newuser : uid_t;      { chown uid_t parameter  } 
{24}   newgroup : gid_t;     { chown gid_t parameter  }
{28}   trunc_offset : off_t; { ftrucnate parameter    }
{36}   unknown2 : array[1..2] of longint;
{44}   utime_param: int64;  
{52}   unknown3 : array[1..2] of longint;
  end;






    { These routines are currently not required for BeOS }
    function sys_fork : pid_t; 
    begin
    end;

    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;
    begin
    end;

    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t;
    begin
    end;


    function sys_uname(var name: utsname): cint;
    begin
      FillChar(name, sizeof(utsname), #0);
      name.machine := 'BePC'#0;

    end;




    function S_ISDIR(m : mode_t): boolean;
    begin
	 if (m and S_IFDIR)= S_IFDIR then
	   S_ISDIR := true
	 else
	   S_ISDIR := false;
    end;

    function S_ISCHR(m : mode_t): boolean;
    begin
	  if (m and S_IFCHR) = S_IFCHR then
	    S_ISCHR := true
	  else
	   S_ISCHR := false;
    end;

    function S_ISBLK(m : mode_t): boolean;
      begin
        if (m and S_IFBLK) = S_IFBLK then
          S_ISBLK := true
	    else
	      S_ISBLK := false;
      end;

    function S_ISREG(m : mode_t): boolean;
      begin
       if (m and S_IFREG) = S_IFREG then
	     S_ISREG := true
       else
	     S_ISREG := false;
      end;

    function S_ISFIFO(m : mode_t): boolean;
      begin
	   if (m and S_IFIFO) = S_IFIFO then
	     S_ISFIFO := true
       else
	     S_ISFIFO := false;
      end;

    function wifexited(status : cint): cint;
     begin
       wifexited := byte(boolean((status and not $FF) = 0));
     end;

    function wexitstatus(status : cint): cint;
     begin
       wexitstatus := status and $FF;
     end;

    function wstopsig(status : cint): cint;
     begin
       wstopsig:=(status shr 16) and $FF;
     end;

    function wifsignaled(status : cint): cint;
     begin
       if (((status) shr 8) and $ff) <> 0 then
         wifsignaled := 1
       else  
         wifsignaled := 0;
     end;


 {$i syscall.inc}

  procedure sys_exit(status : cint); external name 'sys_exit';
(*
  procedure sys_exit(status : cint);
  var
   args: SysCallArgs;
  begin
   args.param[1] := status;
   SysCall(syscall_nr_exit,args);
  end;
*)

  function sys_close(fd : cint): cint;
  var
   args : SysCallArgs;
  begin
    args.param[1] := fd;
    sys_close:=SysCall(syscall_nr_close,args);
  end;


  function sys_time(var tloc:time_t): time_t;
  var
   args : SysCallArgs;
  begin
    { don't treat errno, since there is never any }
    tloc := Do_Syscall(syscall_nr_time,args);
    sys_time := tloc;
  end;



  function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint;
  var
   args : SysCallArgs;
  begin
    args.param[1] := sig;
    args.param[2] := cint(@act);
    args.param[3] := cint(@oact);
    sys_sigaction := SysCall(syscall_nr_sigaction, args);
  end;


  function sys_closedir(dirp : pdir): cint;
  var
    args : SysCallArgs;
  begin
    if assigned(dirp) then
      begin
	args.param[1] := dirp^.fd;
	sys_closedir := SysCall(syscall_nr_closedir,args);
	Dispose(dirp);
	dirp := nil;
	exit;
      end;
    Errno := Sys_EBADF;
    sys_closedir := -1;
  end;


   function sys_opendir(const dirname : pchar): pdir;
   var
    args : SysCallArgs;
    dirp: pdir;
    fd : cint;
   begin
      New(dirp);
      { just in case }
      FillChar(dirp^,sizeof(dir),#0);
      if assigned(dirp) then
	  begin
	    args.param[1] := $FFFFFFFF;
	    args.param[2] := cint(dirname);
	    args.param[3] := 0;
        fd:=SysCall(syscall_nr_opendir,args);
	    if fd = -1 then
	      begin
	        Dispose(dirp);
	        sys_opendir := nil;
	        exit;
	      end;
	    dirp^.fd := fd;
	    sys_opendir := dirp;
	    exit;
	  end;
      Errno := Sys_EMFILE;
      sys_opendir := nil;
   end;


    function sys_access(const pathname : pchar; amode : cint): cint;
    var
     args : SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(pathname);
      args.param[3] := amode;
      sys_access := SysCall(syscall_nr_access,args);
    end;


    function sys_rename(const old : pchar; const newpath: pchar): cint;
    var
     args: SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(old);
      args.param[3] := $FFFFFFFF;
      args.param[4] := cint(newpath);
      sys_rename := SysCall(syscall_nr_rename,args);
    end;


    function sys_rmdir(const path : pchar): cint;
    var
     args: SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(path);
      sys_rmdir := SysCall(syscall_nr_rmdir,args);
    end;


    function sys_unlink(const path: pchar): cint;
    var
     args :SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(path);
      sys_unlink := SysCall(syscall_nr_unlink,args);
    end;



    function sys_mkdir(const path : pchar; mode: mode_t):cint;
    var
     args :SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(path);
      args.param[3] := cint(mode);
      sys_mkdir := SysCall(syscall_nr_mkdir,args);
    end;


    function sys_fstat(fd : cint; var sb : stat): cint;
    var
     args : SysCallArgs;
    begin
      args.param[1] := fd;
      args.param[2] := $00;
      args.param[3] := cint(@sb);
      args.param[4] := $00000001;
      sys_fstat := SysCall(syscall_nr_fstat, args);
    end;


    function sys_stat(const path: pchar; var buf : stat): cint;
    var
     args : SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(path);
      args.param[3] := cint(@buf);
      args.param[4] := $01000000;
      sys_stat := SysCall(syscall_nr_stat, args);
    end;


    function sys_read(fd: cint; buf:pchar; nbytes : size_t): ssize_t;
    var
     args : SysCallArgs;
     funcresult: ssize_t;
     errorcode : cint;
    begin
      args.param[1] := fd;
      args.param[2] := cint(buf);
      args.param[3] := cint(nbytes);
      args.param[4] := cint(@errorcode);
      funcresult := ssize_t(Do_SysCall(syscall_nr_read,args));
      if funcresult >= 0 then
       begin
         sys_read := funcresult;
         errno := 0;
       end
      else
       begin
         sys_read := -1;
         errno := errorcode;
       end;
    end;


    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t;
     var
      args : SysCallArgs;
      funcresult : ssize_t;
      errorcode : cint;
    begin
      args.param[1] := fd;
      args.param[2] := cint(buf);
      args.param[3] := cint(nbytes);
      args.param[4] := cint(@errorcode);
      funcresult := Do_SysCall(syscall_nr_write,args);
      if funcresult >= 0 then
       begin
         sys_write := funcresult;
         errno := 0;
       end
      else
       begin
         sys_write := -1;
         errno := errorcode;
       end;
    end;



    function sys_chdir(const path : pchar): cint;
    var
     args: SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(path);
      sys_chdir := SysCall(syscall_nr_chdir, args);
    end;


    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; 
    var
     args: SysCallArgs;
    begin
      args.param[1] := $FFFFFFFF;
      args.param[2] := cint(path);
      args.param[3] := flags;
      args.param[4] := cint(mode);
      args.param[5] := 0;               { close on execute flag }
      sys_open:= SysCall(syscall_nr_open, args);   
    end;


    function sys_readdir(dirp : pdir) : pdirent;
    var
      args : SysCallArgs;
      funcresult : cint;
    begin
      args.param[1] := dirp^.fd;
      args.param[2] := cint(@(dirp^.ent));
      args.param[3] := $0000011C;
      args.param[4] := $00000001;
      { the error will be processed here }
      funcresult := Do_SysCall(syscall_nr_readdir, args);
      if funcresult <> 1 then
        begin
          if funcresult <> 0 then
             errno := funcresult;
          sys_readdir := nil;
          exit;
        end;
      errno := 0;
      sys_readdir := @dirp^.ent
    end;


    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; 
    var
     args: SysCallArgs;

    begin
      args.param[1] := fd;
      args.param[2] := cint(offset and $FFFFFFFF);      
      args.param[3] := cint((offset shr 32) and $FFFFFFFF);
      args.param[4] := whence;
      { we currently only support seeks upto 32-bit in length }
      sys_lseek := off_t(SysCall(syscall_nr_lseek,args));
    end;


    function sys_ftruncate(fd : cint; flength : off_t): cint;
    var
     args: SysCallArgs;
     wstat : pwstat;
    begin
      New(wstat);
      FillChar(wstat^,sizeof(wstat),0);
      wstat^.trunc_offset := flength;
      args.param[1] := fd;
      args.param[2] := $00000000;
      args.param[3] := cint(wstat);
      args.param[4] := $00000008;
      args.param[5] := $00000001;
      sys_ftruncate:=SysCall(syscall_nr_ftruncate, args);
      Dispose(wstat);
    end;

{ 

  $Log$
  Revision 1.2  2003-01-08 22:32:28  marco
   * Small fixes and quick merge with 1.0.x. At least the compiler builds now,
      but it could crash hard, since there are lots of unimplemented funcs.

  Revision 1.1.2.13  2001/12/17 02:14:28  carl
  + wifsignaled() added

  Revision 1.1.2.12  2001/12/03 03:11:05  carl
  * update for new posix prototype (caused problem with other OS)

  Revision 1.1.2.11  2001/08/22 02:38:12  carl
  - sys_exit now written in assembler

  Revision 1.1.2.10  2001/08/15 01:01:51  carl
  - moved SysCall to syscall.inc

  Revision 1.1.2.9  2001/08/13 09:40:43  carl
  * bugfix of problems of changing signs with errno!
  * changed prototype of sys_readdir() to conform to POSIX

  Revision 1.1.2.8  2001/08/13 05:57:53  carl
  * corrected written/read value returned for sys_read() and sys_write(). errno now correctly set.

  Revision 1.1.2.7  2001/08/12 15:15:21  carl
  * bugfix of call to sys_time (would always return weird results)

  Revision 1.1.2.6  2001/08/09 01:12:46  carl
  * fstat() call now correct
  + ftruncate() support

  Revision 1.1.2.5  2001/08/08 01:55:43  carl
  * bugfix of sys_opendir()
  * bugfix of sys_readdir() should be var parameter not const :(

  Revision 1.1.2.4  2001/07/14 04:20:33  carl
  + sys_lseek()
  + sys_open()
  * bugfix of sys_write()
  * bugfix of sys_readdir()
  + started testing

  Revision 1.1.2.3  2001/07/13 03:14:55  carl
  + more syscalls (not all verified) working

}
