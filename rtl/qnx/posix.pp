{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Carl Eric Codere
    development team

    POSIX Compliant interface unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit posix;

interface

{***********************************************************************}
{                       POSIX PUBLIC INTERFACE                          }
{***********************************************************************}


{$i errno.inc}
{$i osposixh.inc}

    function sys_fork : pid_t; 
    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; 
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; 
    procedure sys_exit(status : cint); cdecl; external name '_exit';
    function sys_uname(var name: utsname): cint; 
    function sys_opendir(const dirname : pchar): pdir; 
    function sys_readdir(dirp : pdir) : pdirent;
    function sys_closedir(dirp : pdir): cint; 
    function sys_chdir(const path : pchar): cint; 
    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; 
    function sys_mkdir(const path : pchar; mode: mode_t):cint; 
    function sys_unlink(const path: pchar): cint; 
    function sys_rmdir(const path : pchar): cint; 
    function sys_rename(const old : pchar; const newpath: pchar): cint; 
    function sys_access(const pathname : pchar; amode : cint): cint; 
    function sys_close(fd : cint): cint; 
    function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; 
    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; 
    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; 
    function sys_time(var tloc:time_t): time_t; 
    function sys_ftruncate(fd : cint; flength : off_t): cint; 
    function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; 
    function sys_fstat(fd : cint; var sb : stat): cint; 
    function sys_stat(const path: pchar; var buf : stat): cint; 


    function S_ISDIR(m : mode_t): boolean;
    function S_ISCHR(m : mode_t): boolean;
    function S_ISBLK(m : mode_t): boolean;
    function S_ISREG(m : mode_t): boolean;
    function S_ISFIFO(m : mode_t): boolean;

    function wifexited(status : cint): cint;
    function wexitstatus(status : cint): cint;
    function wstopsig(status : cint): cint;
    function wifsignaled(status : cint): cint;



implementation
 
    function int_fork : pid_t; cdecl; external name 'fork';
    function int_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; cdecl; external name 'execve';
    function int_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    function int_uname(var name: utsname): cint; cdecl; external name 'uname';
    function int_opendir(const dirname : pchar): pdir; cdecl; external name 'opendir';
    function int_readdir(dirp : pdir) : pdirent;cdecl; external name 'readdir';
    function int_closedir(dirp : pdir): cint; cdecl; external name 'closedir';
    function int_chdir(const path : pchar): cint; cdecl; external name 'chdir';
    function int_open(const path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function int_mkdir(const path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function int_unlink(const path: pchar): cint; cdecl; external name 'unlink';
    function int_rmdir(const path : pchar): cint; cdecl; external name 'rmdir';
    function int_rename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function int_access(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';
    function int_close(fd : cint): cint; cdecl; external name 'close';
    function int_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function int_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function int_lseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function int_time(var tloc:time_t): time_t; cdecl; external name 'time';
    function int_ftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function int_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
    function int_fstat(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function int_stat(const path: pchar; var buf : stat): cint; cdecl; external name 'stat';


    function sys_fork : pid_t; 
     begin
       sys_fork := int_fork;
       if sys_fork <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
     end;
     
     
    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;
    begin
       sys_execve := int_execve(path, argv, envp);
       if sys_execve <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t;
    begin
       sys_waitpid := int_waitpid(pid, stat_loc, options);
       if sys_waitpid <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;


    function sys_uname(var name: utsname): cint; 
     begin
       sys_uname := int_uname(name);
       if sys_uname <> - 1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
     end;
     
    function sys_opendir(const dirname : pchar): pdir; 
    begin
       sys_opendir := int_opendir(dirname);
       if sys_opendir <> nil then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_readdir(dirp : pdir) : pdirent;
    begin
       sys_readdir := int_readdir(dirp);
       if sys_readdir <> nil then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    

    function sys_closedir(dirp : pdir): cint; 
    begin
       sys_closedir := int_closedir(dirp);
       if sys_closedir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_chdir(const path : pchar): cint; 
    begin
       sys_chdir := int_chdir(path);
       if sys_chdir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; 
    begin
       sys_open:= int_open(path, flags, mode);
       if sys_open <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_mkdir(const path : pchar; mode: mode_t):cint; 
    begin
       sys_mkdir:= int_mkdir(path, mode);
       if sys_mkdir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_unlink(const path: pchar): cint; 
    begin
       sys_unlink := int_unlink(path);
       if sys_unlink <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_rmdir(const path : pchar): cint; 
    begin
       sys_rmdir := int_rmdir(path);
       if sys_rmdir <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_rename(const old : pchar; const newpath: pchar): cint; 
    begin
       sys_rename := int_rename(old, newpath);
       if sys_rename <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_access(const pathname : pchar; amode : cint): cint; 
    begin
       sys_access := int_access(pathname, amode);
       if sys_access <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_close(fd : cint): cint; 
    begin
       sys_close := int_close(fd);
       if sys_close <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; 
    begin
       sys_read := int_read(fd, buf, nbytes);
       if sys_read <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; 
    begin
       sys_write := int_write(fd, buf, nbytes);
       if sys_write <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    
    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; 
    begin
       sys_lseek := int_lseek(fd, offset, whence);
       if sys_lseek <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_time(var tloc:time_t): time_t;
    begin
      sys_time := int_time(tloc);
      if sys_time <> -1 then
        begin
          errno := 0;         { reset errno when the call succeeds, contrary to libc }
        end;
    end;
    
    function sys_ftruncate(fd : cint; flength : off_t): cint;
    begin
       sys_ftruncate := int_ftruncate(fd, flength);
       if sys_ftruncate <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;
    
    function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; 
    begin
       sys_sigaction := int_sigaction(sig, act, oact);
       if sys_sigaction <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
    end;

       
    function sys_fstat(fd : cint; var sb : stat): cint; 
      begin
        sys_fstat := int_fstat(fd, sb);
        if sys_fstat <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
      end;
      
    function sys_stat(const path: pchar; var buf : stat): cint; 
      begin
        sys_stat := int_stat(path, buf);
        if sys_stat <> -1 then
         begin
           errno := 0;         { reset errno when the call succeeds, contrary to libc }
         end;
      end;

const
   _S_IFMT      = $F000;             (*  Type of file                    *)
   _S_IFIFO     = $1000;             (*  FIFO                            *)
   _S_IFCHR     = $2000;             (*  Character special               *)
   _S_IFDIR     = $4000;             (*  Directory                       *)
   _S_IFNAM     = $5000;             (*  Special named file              *)
   _S_IFBLK     = $6000;             (*  Block special                   *)
   _S_IFREG     = $8000;             (*  Regular                         *)
   _S_IFLNK     = $A000;             (*  Symbolic link                   *)
   _S_IFSOCK    = $C000;             (*  Socket                          *)
   

    function S_ISDIR(m : mode_t): boolean;
      begin
        if (m and _S_IFMT) = _S_IFDIR then
          S_ISDIR := true
        else
          S_ISDIR := false;
      end;

    function S_ISCHR(m : mode_t): boolean;
      begin
        if (m and _S_IFMT) = _S_IFCHR then
          S_ISCHR := true
        else
          S_ISCHR := false;
      end;

    function S_ISBLK(m : mode_t): boolean;
      begin
        if (m and _S_IFMT) = _S_IFBLK then
          S_ISBLK := true
        else
          S_ISBLK := false;
      end;

    function S_ISREG(m : mode_t): boolean;
      begin
        if (m and _S_IFMT) = _S_IFREG then
          S_ISREG := true
        else
          S_ISREG := false;
      end;

    function S_ISFIFO(m : mode_t): boolean;
      begin
        if (m and _S_IFMT) = _S_IFIFO then
          S_ISFIFO := true
        else
          S_ISFIFO := false;
      end;

    function wifexited(status : cint): cint;
      begin
          wifexited := longint((status and $FF) = 0);
      end;

    function wexitstatus(status : cint): cint;
     begin
       wexitstatus := (((status) shr 8) and $FF);
     end;

    function wstopsig(status : cint): cint;
     begin
       wstopsig := (((status) shr 8) and $FF);
     end;

    function wifsignaled(status : cint): cint;
     begin
       if ((status and $FF) <> 0) and ((status and $FF00)=0) then
         wifsignaled := 1
       else  
         wifsignaled := 0;
     end;

end.

