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
  Revision 1.3  2002-09-07 16:01:19  peter
    * old logs removed and tabs fixed

}
