{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Dummy multithreading support for DOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
const
   threadvarblocksize : dword = 0;

type
   pd = ^dword;

var
   mainprogramthreadblock : pointer;

procedure init_threadvar(offset : pdword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];

  begin
     offset^:=threadvarblocksize;
     inc(threadblocksize,size);
  end;


function relocate_threadvar(offset : dword) : pointer;[public,alias: 'FPC_RELOCATE_THREADVAR'];

  asm
     movl mainprogramthreadblock,%eax
     addl offset,%eax
  end;

{
  $Log$
  Revision 1.3  2000-01-07 16:41:32  daniel
    * copyright 2000

  Revision 1.2  1999/04/28 06:01:26  florian
    * define MT for multithreading introduced

  Revision 1.1  1999/04/27 19:41:06  florian
    + first implementation for DOS, only a dummy
}
