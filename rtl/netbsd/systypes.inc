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
    ino    : cardinal;		{ This is not inode number, but "a number
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
            len   : longint;    { size of data buffer 	}
            seek,               { magic cookie returned by getdirentries}
            rewind: cardinal;   { magic cookie for rewinding}
            flags : longint;    { flags for readdir }
            end;

(* Linux kernel record
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
            end; *)

  PDir =^TDir;

{$packrecords C}
type
 dev_t	= cardinal; // taken from sysunix.getdir to avoid ifdef's.

 Stat =record {BSD version}
        dev,                            { inode's device }
        ino        :  cardinal;         { inode's number }
        mode, 				{ inode protection mode }
        nlink	   :  longint;             { number of hard links }
        uid,				{ user ID of the file's owner }
        gid,				{ group ID of the file's group }
        dev_t	   :  cardinal;		{ device type }
	atime,                          { time of last access }
	atime_nsec,         		{ nsec of last access }
	mtime,	           		{ time of last data modification }
	mtime_nsec,         		{ nsec of last data modification }
	ctime,            		{ time of last file status change }
	ctime_nsec : longint;      	{ nsec of last file status change }
	size,           		{ file size, in bytes }
	blocks     : Int64;		{ blocks allocated for file }
	blksze,         		{ optimal blocksize for I/O }
	flags,          		{ user defined flags for file }
	filegen    : cardinal;		{ file generation number }
//	lspare	   : longint;
	qspare     : array[0..1] of int64;
       end;

  PStat=^Stat;
  TStat=Stat;

  TStatfs = packed record
    spare2,	       { place holder}
    bsize,	       { fundamental block size}
    iosize,            { optimal block size }
    blocks,            {  total blocks}
    bfree,             { blocks free}
    bavail,            { block available for mortal users}
    files,	       { Total file nodes}
    ffree	   : longint;    { file nodes free}
    fsid	   : array[0..1] of longint;
    fowner	   : longint; {mounter uid}
    ftype	   : longint;
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
    tv_sec,tv_nsec:longint;
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

{
  $Log$
  Revision 1.3  2003-01-21 15:39:45  marco
   * NetBSD first rtl. Still not 100%, but close

  Revision 1.2  2003/01/17 22:13:47  marco
   * some updates

  Revision 1.1.2.2  2001/08/30 23:00:18  marco
   * Fix for stat record.

  Revision 1.1.2.1  2001/08/10 11:07:17  pierre
   New NetBSD files taken and adapted from FreeBSD

  Revision 1.1.2.5  2001/03/12 15:01:39  marco
   * [Solaris] introduced dev_t

  Revision 1.1.2.4  2001/01/20 16:54:51  marco
   * Fixed timeval. Must have thought that long=int64 when I did initial port?

  Revision 1.1.2.3  2000/10/30 14:36:07  marco
   * timespec from int64 to longint.

  Revision 1.1.2.2  2000/10/24 12:14:37  pierre
   + timespec type for syscall_nr_nanosleep

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1.2.1  2000/09/10 16:12:14  marco
  Initial signals, sockets and clone

  Revision 1.1  2000/07/13 06:30:33  michael
  + Initial import

  Revision 1.8  2000/04/16 16:08:30  marco
   * Updated PDir to BSD libc layout. (which is totally different from Linux)

  Revision 1.7  2000/04/10 15:46:52  marco
   * worked all day. probably a lot changed

  Revision 1.5  2000/03/17 12:58:57  marco
   * some changes to ftruncate based procs. Added a "0" as extra parameter

  Revision 1.4  2000/02/04 16:55:47  marco
   * Fixed tdir, some params need to be 32-bit

  Revision 1.3  2000/02/03 17:05:55  marco

   * Some types fixed/ported.

  Revision 1.2  2000/02/02 16:45:38  marco
   * Typo in STAT record

  Revision 1.1  2000/02/02 16:36:09  marco
   * Initial version. Copy of linux version, with BSD stat.

}
