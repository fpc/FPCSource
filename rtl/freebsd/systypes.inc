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

  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

{
  $Log$
  Revision 1.10  2003-09-14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.9  2002/09/07 16:01:18  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/05/06 10:49:59  marco
   * setitimer for 1.1

}
