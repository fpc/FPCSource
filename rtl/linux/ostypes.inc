{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.
    
    OS dependant types internal to the Linux RTL.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type

{
 Linux system calls take arguments as follows :

   i386    m68k    ppc
   %eax    %d0     r0      System call number
   %ebx    %d1     r3      first argument
   %ecx    %d2     r4      second argument
   %edx    %d3     r5      third argumens
   %esi    %d3     r6      fourth argument
   %edi    %d4     r7      fifth argument

  That is why we define a special type, with only these arguments
  To make it processor independent, we don't give any system dependent
  names, but the rather abstract reg1,reg2 etc;
}

  SysCallRegs = record
    		 reg1,
	 	 reg2,
		 reg3,
		 reg4,
		 reg5,
		 reg6   : longint;
  		end;
  PSysCallRegs= ^SysCallRegs;
  TSysCallRegs= SysCallRegs;


  ptimespec   = ^timespec;
  timespec    = packed record
    		 tv_sec   : time_t;
    		 tv_nsec  : clong;
 		end;

  timeval     = packed record
    		 tv_sec,
		 tv_usec:clong;
  		end;
  ptimeval    = ^timeval;
  TTimeVal    = timeval;

CONST 
       _S_IFDIR = $4000;
       _S_IFCHR = $2000;
       _S_IFBLK = $6000;
       _S_IFREG = $8000;
       _S_IFMT  = $f000;
       _S_IFIFO = $1000;

{
 $Log$
 Revision 1.2  2002-12-18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.1  2002/11/12 14:37:59  marco
  * Parts of new unix rtl

}
