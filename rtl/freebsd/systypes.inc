{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type

{
 Linux system calls take arguments as follows :

   i386/m68k:

   %eax/%d0 : System call number
   %ebx/%d1 : first argument
   %ecx/%d2 : second argument
   %edx/%d3 : third argumens
   %esi/%d3 : fourth argument
   %edi/%d4 : fifth argument

  That is why we define a special type, with only these arguments
  To make it processor independent, we don't give any system dependent
  names, but the rather abstract reg1,reg2 etc;

  SysCallRegs=record
    reg1,reg2,reg3,reg4,reg5,reg6 : longint;
  end;
  PSysCallRegs=^SysCallRegs;
  TSysCallRegs=SysCallRegs;
}
{ The following are records for system calls BSD updated }
  dirent = packed record
    ino    : cardinal;          { This is not inode number, but "a number
                                  unique for each file on a filesystem"}
    reclen : word;
    d_type,
    namlen : byte;
    name   : array [0..255] of char;
  end;

  pdirent =^dirent;
  TDirEnt = dirent;

    TDir= packed record {BSD libc record.}
            fd    : longint;    { file descriptor associated with directory }
            loc,                { offset in current buffer }
            size  : cardinal;   { amount of data returned by getdirentries}
            buf   : pdirent;    { data buffer, actually a pchar}
            len   : longint;    { size of data buffer   }
            seek,               { magic cookie returned by getdirentries}
            rewind: cardinal;   { magic cookie for rewinding}
            flags : longint;    { flags for readdir }
            end;

{ Linux kernel record
    TDir = packed record
            fd     : longint;
            loc    : longint;
            size   : integer;
            buf    : pdirent;
   {The following are used in libc, but NOT in the linux kernel sources ??}
            nextoff: longint;
            dd_max : integer; {size of buf. Irrelevant, as buf is of type dirent}
            lock   : pointer;
            dummy  : array[0..1023] of char;
            end;}

  PDir =^TDir;
 dev_t  = CARDINAL;
{$packrecords C}
 Stat =record {BSD version}
        dev        :  dev_t;            { inode's device }
        ino        :  cardinal;         { inode's number }
        mode,                           { inode protection mode }
        nlink      :  word;             { number of hard links }
        uid,                            { user ID of the file's owner }
        gid,                            { group ID of the file's group }
        dev_t      :  cardinal;         { device type }
        atime,                          { time of last access }
        atime_nsec,                     { nsec of last access }
        mtime,                          { time of last data modification }
        mtime_nsec,                     { nsec of last data modification }
        ctime,                          { time of last file status change }
        ctime_nsec : longint;           { nsec of last file status change }
        size,                           { file size, in bytes }
        blocks     : Int64;             { blocks allocated for file }
        blksze,                         { optimal blocksize for I/O }
        flags,                          { user defined flags for file }
        filegen    : cardinal;          { file generation number }
        lspare     : longint;
        qspare     : array[0..1] of int64;
       end;

  PStat=^Stat;
  TStat=Stat;

  TStatfs = packed record
    spare2,            { place holder}
    bsize,             { fundamental block size}
    iosize,            { optimal block size }
    blocks,            {  total blocks}
    bfree,             { blocks free}
    bavail,            { block available for mortal users}
    files,             { Total file nodes}
    ffree          : longint;    { file nodes free}
    fsid           : array[0..1] of longint;
    fowner         : longint; {mounter uid}
    ftype          : longint;
    fflags         : longint; {copy of mount flags}
    spare          : array [0..1] of longint; { For later use }
    fstypename     : array[0..15] of char;
    mountpoint     : array[0..89] of char;
    mnfromname     : array[0..89] of char;
  end;
  PStatFS=^TStatFS;

  fdSet=array[0..7] of longint;{=256 bits}
  pfdset=^fdset;
  TFDSet=fdset;

  timeval = packed record
    sec,usec:longint;
  end;
  ptimeval=^timeval;
  TTimeVal=timeval;

  timespec = packed record
    tv_sec,tv_nsec:{longint or int64 ?}longint;
  end;

  timezone = packed record
    minuteswest,dsttime:longint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

  utsname = packed record
    sysname,
    nodename,
    release,
    version,
    machine,
    domainname : Array[0..64] of char;
  end;
  PUTSName=^UTSName;
  TUTSName=UTSName;

  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

{
  $Log$
  Revision 1.9  2002-09-07 16:01:18  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/05/06 10:49:59  marco
   * setitimer for 1.1

}
