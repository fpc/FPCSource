{
    $Id$
}


{$PACKRECORDS C}

  {
     Copyright (c) 2000 Apple Computer, Inc. All rights reserved.
    
     @APPLE_LICENSE_HEADER_START@
     
     The contents of this file constitute Original Code as defined in and
     are subject to the Apple Public Source License Version 1.1 (the
     "License").  You may not use this file except in compliance with the
     License.  Please obtain a copy of the License at
     http://www.apple.com/publicsource and read it before using this file.
     
     This Original Code and all software distributed under the License are
     distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
     EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
     INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
     FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
     License for the specific language governing rights and limitations
     under the License.
     
     @APPLE_LICENSE_HEADER_END@
    }
  { Copyright (c) 1997 Apple Computer, Inc. All Rights Reserved  }
  {
     Copyright (c) 1988, 1989, 1993, 1994
    	The Regents of the University of California.  All rights reserved.
    
     Redistribution and use in source and binary forms, with or without
     modification, are permitted provided that the following conditions
     are met:
     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
     3. All advertising materials mentioning features or use of this software
        must display the following acknowledgement:
          This product includes software developed by the University of
          California, Berkeley and its contributors.
     4. Neither the name of the University nor the names of its contributors
        may be used to endorse or promote products derived from this software
        without specific prior written permission.
    
     THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
     ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
     IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
     ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
     FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
     OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
     LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
     OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
     SUCH DAMAGE.
    
    	@(#)termios.h	8.3 (Berkeley) 3/28/94
    }
{$ifndef _SYS_TERMIOS_H_}
{$define _SYS_TERMIOS_H_}
  {
     Special Control Characters
    
     Index into c_cc[] character array.
    
    	Name	     Subscript	Enabled by
    }
  { ICANON  }

  const
     VEOF = 0;
  { ICANON  }
     VEOL = 1;
{$ifndef _POSIX_SOURCE}
  { ICANON together with IEXTEN  }

  const
     VEOL2 = 2;
{$endif}
  { ICANON  }

  const
     VERASE = 3;
{$ifndef _POSIX_SOURCE}
  { ICANON together with IEXTEN  }

  const
     VWERASE = 4;
{$endif}
  { ICANON  }

  const
     VKILL = 5;
{$ifndef _POSIX_SOURCE}
  { ICANON together with IEXTEN  }

  const
     VREPRINT = 6;
{$endif}
  {			7	   spare 1  }
  { ISIG  }

  const
     VINTR = 8;
  { ISIG  }
     VQUIT = 9;
  { ISIG  }
     VSUSP = 10;
{$ifndef _POSIX_SOURCE}
  { ISIG together with IEXTEN  }

  const
     VDSUSP = 11;
{$endif}
  { IXON, IXOFF  }

  const
     VSTART = 12;
  { IXON, IXOFF  }
     VSTOP = 13;
{$ifndef _POSIX_SOURCE}
  { IEXTEN  }

  const
     VLNEXT = 14;
  { IEXTEN  }
     VDISCARD = 15;
{$endif}
  { !ICANON  }

  const
     VMIN = 16;
  { !ICANON  }
     VTIME = 17;
{$ifndef _POSIX_SOURCE}
  { ICANON together with IEXTEN  }

  const
     VSTATUS = 18;
  {			19	   spare 2  }
{$endif}

  const
     NCCS = 20;
{$ifndef _POSIX_VDISABLE}

  const
     _POSIX_VDISABLE = $ff;
{$endif}
{$ifndef _POSIX_SOURCE}
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function CCEQ(val,c : longint) : longint;

{$endif}
  {
     Input flags - software input processing
    }
  { ignore BREAK condition  }

  const
     IGNBRK = $00000001;
  { map BREAK to SIGINTR  }
     BRKINT = $00000002;
  { ignore (discard) parity errors  }
     IGNPAR = $00000004;
  { mark parity and framing errors  }
     PARMRK = $00000008;
  { enable checking of parity errors  }
     INPCK = $00000010;
  { strip 8th bit off chars  }
     ISTRIP = $00000020;
  { map NL into CR  }
     INLCR = $00000040;
  { ignore CR  }
     IGNCR = $00000080;
  { map CR to NL (ala CRMOD)  }
     ICRNL = $00000100;
  { enable output flow control  }
     IXON = $00000200;
  { enable input flow control  }
     IXOFF = $00000400;
{$ifndef _POSIX_SOURCE}
  { any char will restart after stop  }

  const
     IXANY = $00000800;
  { ring bell on input queue full  }
     IMAXBEL = $00002000;
{$endif}
  {_POSIX_SOURCE  }
  {
     Output flags - software output processing
    }
  { enable following output processing  }

  const
     OPOST = $00000001;
{$ifndef _POSIX_SOURCE}
  { map NL to CR-NL (ala CRMOD)  }

  const
     ONLCR = $00000002;
  { expand tabs to spaces  }
     OXTABS = $00000004;
  { discard EOT's (^D) on output)  }
     ONOEOT = $00000008;
{$endif}
  {_POSIX_SOURCE  }
  {
     Control flags - hardware control of terminal
    }
{$ifndef _POSIX_SOURCE}
  { ignore control flags  }

  const
     CIGNORE = $00000001;
{$endif}
  { character size mask  }

  const
     CSIZE = $00000300;
  { 5 bits (pseudo)  }
     CS5 = $00000000;
  { 6 bits  }
     CS6 = $00000100;
  { 7 bits  }
     CS7 = $00000200;
  { 8 bits  }
     CS8 = $00000300;
  { send 2 stop bits  }
     CSTOPB = $00000400;
  { enable receiver  }
     CREAD = $00000800;
  { parity enable  }
     PARENB = $00001000;
  { odd parity, else even  }
     PARODD = $00002000;
  { hang up on last close  }
     HUPCL = $00004000;
  { ignore modem status lines  }
     CLOCAL = $00008000;
{$ifndef _POSIX_SOURCE}
  { CTS flow control of output  }

  const
     CCTS_OFLOW = $00010000;
  { RTS flow control of input  }
     CRTS_IFLOW = $00020000;
     CRTSCTS = CCTS_OFLOW or CRTS_IFLOW;
  { DTR flow control of input  }
     CDTR_IFLOW = $00040000;
  { DSR flow control of output  }
     CDSR_OFLOW = $00080000;
  { DCD flow control of output  }
     CCAR_OFLOW = $00100000;
  { old name for CCAR_OFLOW  }
     MDMBUF = $00100000;
{$endif}
  {
     "Local" flags - dumping ground for other state
    
     Warning: some flags in this structure begin with
     the letter "I" and look like they belong in the
     input flag.
    }
{$ifndef _POSIX_SOURCE}
  { visual erase for line kill  }

  const
     ECHOKE = $00000001;
{$endif}
  {_POSIX_SOURCE  }
  { visually erase chars  }

  const
     ECHOE = $00000002;
  { echo NL after line kill  }
     ECHOK = $00000004;
  { enable echoing  }
     ECHO = $00000008;
  { echo NL even if ECHO is off  }
     ECHONL = $00000010;
{$ifndef _POSIX_SOURCE}
  { visual erase mode for hardcopy  }

  const
     ECHOPRT = $00000020;
  { echo control chars as ^(Char)  }
     ECHOCTL = $00000040;
{$endif}
  {_POSIX_SOURCE  }
  { enable signals INTR, QUIT, [D]SUSP  }

  const
     ISIG = $00000080;
  { canonicalize input lines  }
     ICANON = $00000100;
{$ifndef _POSIX_SOURCE}
  { use alternate WERASE algorithm  }

  const
     ALTWERASE = $00000200;
{$endif}
  {_POSIX_SOURCE  }
  { enable DISCARD and LNEXT  }

  const
     IEXTEN = $00000400;
  { external processing  }
     EXTPROC = $00000800;
  { stop background jobs from output  }
     TOSTOP = $00400000;
{$ifndef _POSIX_SOURCE}
  { output being flushed (state)  }

  const
     FLUSHO = $00800000;
  { no kernel output from VSTATUS  }
     NOKERNINFO = $02000000;
  { XXX retype pending input (state)  }
     PENDIN = $20000000;
{$endif}
  {_POSIX_SOURCE  }
  { don't flush after interrupt  }

  const
     NOFLSH = $80000000;

  type

     tcflag_t = dword;

     cc_t = byte;

     speed_t = longint;
  { XXX should be unsigned long  }
  { input flags  }
  { output flags  }
  { control flags  }
  { local flags  }
  { control chars  }
  { input speed  }
  { output speed  }
     termios = record
          c_iflag : tcflag_t;
          c_oflag : tcflag_t;
          c_cflag : tcflag_t;
          c_lflag : tcflag_t;
          c_cc : array[0..(NCCS)-1] of cc_t;
          c_ispeed : speed_t;
          c_ospeed : speed_t;
       end;

  {
     Commands passed to tcsetattr() for setting the termios structure.
    }
  { make change immediate  }

  const
     TCSANOW = 0;
  { drain output, then change  }
     TCSADRAIN = 1;
  { drain output, flush input  }
     TCSAFLUSH = 2;
{$ifndef _POSIX_SOURCE}
  { flag - don't alter h.w. state  }

  const
     TCSASOFT = $10;
{$endif}
  {
     Standard speeds
    }

  const
     B0 = 0;
     B50 = 50;
     B75 = 75;
     B110 = 110;
     B134 = 134;
     B150 = 150;
     B200 = 200;
     B300 = 300;
     B600 = 600;
     B1200 = 1200;
     B1800 = 1800;
     B2400 = 2400;
     B4800 = 4800;
     B9600 = 9600;
     B19200 = 19200;
     B38400 = 38400;
{$ifndef _POSIX_SOURCE}

  const
     B7200 = 7200;
     B14400 = 14400;
     B28800 = 28800;
     B57600 = 57600;
     B76800 = 76800;
     B115200 = 115200;
     B230400 = 230400;
     EXTA = 19200;
     EXTB = 38400;
{$endif}
  { !_POSIX_SOURCE  }

  const
     TCIFLUSH = 1;
     TCOFLUSH = 2;
     TCIOFLUSH = 3;
     TCOOFF = 1;
     TCOON = 2;
     TCIOFF = 3;
     TCION = 4;

{
 * Control Character Defaults
 }
        CtrlMask        = $1f;  {\037}
        CEOF            =chr( ORD('d') and CtrlMask);
        CEOL            =chr( $ff and CtrlMask);{ XXX avoid _POSIX_VDISABLE }
        CERASE          =chr( $7F and CtrlMask);
        CINTR           =chr(ORD('c') and CtrlMask);
        CSTATUS         =chr(ORD('t') and CtrlMask);
        CKILL           =chr(ORD('u') and CtrlMask);
        CMIN            =chr(1);
        CQUIT           =chr(034  and CtrlMask);        { FS, ^\ }
        CSUSP           =chr(ORD('z') and CtrlMask);
        CTIME           =chr(0);
        CDSUSP          =chr(ORD('y') and CtrlMask);
        CSTART          =chr(ORD('q') and CtrlMask);
        CSTOP           =chr(ORD('s') and CtrlMask);
        CLNEXT          =chr(ORD('v') and CtrlMask);
        CDISCARD        =chr(ORD('o') and CtrlMask);
        CWERASE         =chr(ORD('w') and CtrlMask);
        CREPRINT        =chr(ORD('r') and CtrlMask);
        CEOT            =CEOF;
{ compat }
        CBRK            =CEOL;
        CRPRNT          =CREPRINT;
        CFLUSH          =CDISCARD;


{
 *        TTYDEFCHARS to include an array of default control characters.
}
    ttydefchars : array[0..NCCS-1] OF char =(
        CEOF,   CEOL,   CEOL,   CERASE, CWERASE, CKILL, CREPRINT,
        chr(_POSIX_VDISABLE), CINTR,  CQUIT,  CSUSP,  CDSUSP, CSTART, CSTOP,  CLNEXT,
        CDISCARD, CMIN, CTIME,  CSTATUS, chr(_POSIX_VDISABLE));

{
  $Log$
  Revision 1.1  2004-01-04 20:05:38  jonas
    * first working version of the Darwin/Mac OS X (for PowerPC) RTL
      Several non-essential units are still missing, but make cycle works

}
