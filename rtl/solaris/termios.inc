

  const
  { Disable special character functions  }
     _POSIX_VDISABLE = 0;
  { was #define dname(params) para_def_expr }
  { argument types are unknown }


  const
     IBSHIFT = 16;
  { required by termio.h and VCEOF/VCEOL  }

  const
     _NCC = 8;

  const
     NCC = _NCC;
  { some defines required by POSIX  }

  const
     NCCS = 19;
  {
   * types defined by POSIX. These are better off in types.h, but
   * the standard says that they have to be in termios.h.
    }

  type

     tcflag_t = dword;

     cc_t = byte;

     speed_t = dword;
  {
   * Ioctl control packet
    }
     ptermios = ^termios;
     termios = record
  { input modes  }
          c_iflag : tcflag_t;
  { output modes  }
          c_oflag : tcflag_t;
  { control modes  }
          c_cflag : tcflag_t;
  { line discipline modes  }
          c_lflag : tcflag_t;
  { control chars  }
          c_cc : array[0..(NCCS)-1] of cc_t;
       end;
     ttermios = termios;

  {
   * POSIX termios functions
   * These functions get mapped into ioctls.
    }
(* Const before type ignored *)

  function cfgetospeed(_para1:Ptermios):speed_t;cdecl;external;

  function cfsetospeed(_para1:Ptermios; _para2:speed_t):cint;cdecl;external;

(* Const before type ignored *)
  function cfgetispeed(_para1:Ptermios):speed_t;cdecl;external;

  function cfsetispeed(_para1:Ptermios; _para2:speed_t):cint;cdecl;external;

  function tcgetattr(_para1:cint; var _para2:termios):cint;cdecl;external;

  function tcgetattr(_para1:cint; _para2:Ptermios):cint;cdecl;external;

(* Const before type ignored *)
  function tcsetattr(_para1:cint; _para2:cint; _para3:Ptermios):cint;cdecl;external;
(* Const before type ignored *)
  function tcsetattr(_para1:cint; _para2:cint; const _para3:termios):cint;cdecl;


//  function tcsendbreak(_para1:cint; _para2:cint):cint;cdecl;

//  function tcdrain(_para1:cint):cint;cdecl;

//  function tcflush(_para1:cint; _para2:cint):cint;cdecl;

//  function tcflow(_para1:cint; _para2:cint):cint;cdecl;


  function tcgetsid(_para1:cint):pid_t;cdecl;external;

  function tcgetsid:pid_t;cdecl;external;

  { control characters  }
  const
     VINTR = 0;
     VQUIT = 1;
     VERASE = 2;
     VKILL = 3;
     VEOF = 4;
     VEOL = 5;

  const
     VEOL2 = 6;

  const
     VMIN = 4;
     VTIME = 5;

  const
     VSWTCH = 7;

  const
     VSTART = 8;
     VSTOP = 9;
     VSUSP = 10;

  const
     VDSUSP = 11;
     VREPRINT = 12;
     VDISCARD = 13;
     VWERASE = 14;
     VLNEXT = 15;
  { 16 thru 19 reserved for future use  }
  {
   * control characters form Xenix termio.h
    }
  { RESERVED true EOF char (V7 compatability)  }
     VCEOF = NCC;
  { RESERVED true EOL char  }
     VCEOL = NCC+1;
     CNUL = 0;
     CDEL = 0177;
  { S5 default control chars  }
  { CINTR, CERASE and CKILL modified to SunOS traditional values  }
     CESC = '\\';
      { return type might be wrong }

  { FS, cntl |  }

  const
     CQUIT = 034;
  { DEL  }
     CERASE = 0177;
  { was #define dname def_expr }

  const
     CEOT = 04;
     CEOL = 0;
     CEOL2 = 0;
  { cntl d  }
     CEOF = 04;
  { cntl q  }
     CSTART = 021;
  { cntl s  }
     CSTOP = 023;
  { cntl z  }
     CSWTCH = 032;
     CNSWTCH = 0;
{
  { was #define dname def_expr }
  function CSUSP : longint;
      { return type might be wrong }

  { was #define dname def_expr }
  function CDSUSP : longint;
      { return type might be wrong }

  { was #define dname def_expr }
  function CRPRNT : longint;
      { return type might be wrong }

  { was #define dname def_expr }
  function CFLUSH : longint;
      { return type might be wrong }

  { was #define dname def_expr }
  function CWERASE : longint;
      { return type might be wrong }

  { was #define dname def_expr }
  function CLNEXT : longint;
      { return type might be wrong }
}
  { input modes  }

  const
     IGNBRK = &0000001;
     BRKINT = &0000002;
     IGNPAR = &0000004;
     PARMRK = &0000010;
     INPCK = &0000020;
     ISTRIP = &0000040;
     INLCR = &0000100;
     IGNCR = &0000200;
     ICRNL = &0000400;
     IUCLC = &0001000;
     IXON = &0002000;
     IXANY = &0004000;
     IXOFF = &0010000;
     IMAXBEL = &0020000;
  { for 386 compatibility  }
     DOSMODE = 0100000;
  { output modes  }
     OPOST = &0000001;
     OLCUC = &0000002;
     ONLCR = &0000004;
     OCRNL = &0000010;
     ONOCR = &0000020;
     ONLRET = &0000040;
     OFILL = &0000100;
     OFDEL = &0000200;
     NLDLY = &0000400;
     NL0 = 0;
     NL1 = &0000400;
     CRDLY = &0003000;
     CR0 = 0;
     CR1 = &0001000;
     CR2 = &0002000;
     CR3 = &0003000;
     TABDLY = &0014000;
     TAB0 = 0;
     TAB1 = &0004000;
     TAB2 = &0010000;
     TAB3 = &0014000;
     XTABS = &0014000;
     BSDLY = &0020000;
     BS0 = 0;
     BS1 = &0020000;
     VTDLY = &0040000;
     VT0 = 0;
     VT1 = &0040000;
     FFDLY = 0100000;
     FF0 = 0;
     FF1 = 0100000;
     PAGEOUT = 0200000;
     WRAP = 0400000;
  { control modes  }
     CBAUD = &0000017;
     CSIZE = &0000060;
     CS5 = 0;
     CS6 = &0000020;
     CS7 = &0000040;
     CS8 = &0000060;
     CSTOPB = &0000100;
     CREAD = &0000200;
     PARENB = &0000400;
     PARODD = &0001000;
     HUPCL = &0002000;
     CLOCAL = &0004000;
     RCV1EN = &0010000;
     XMT1EN = &0020000;
     LOBLK = &0040000;
  { *V7* exclusive use coming fron XENIX  }
     XCLUDE = 0100000;
     CRTSXOFF = 010000000000;
     CRTSCTS = 020000000000;
     CIBAUD = 03600000;
     PAREXT = 04000000;
     CBAUDEXT = 010000000;
     CIBAUDEXT = 020000000;
  {
   * 4.4BSD hardware flow control flags
    }
     CRTS_IFLOW = 010000000000;
     CCTS_OFLOW = 020000000000;
  { line discipline 0 modes  }
     ISIG = &0000001;
     ICANON = &0000002;
     XCASE = &0000004;
     ECHO = &0000010;
     ECHOE = &0000020;
     ECHOK = &0000040;
     ECHONL = &0000100;
     NOFLSH = &0000200;
     TOSTOP = &0000400;
     ECHOCTL = &0001000;
     ECHOPRT = &0002000;
     ECHOKE = &0004000;
     DEFECHO = &0010000;
     FLUSHO = &0020000;
     PENDIN = &0040000;
  { POSIX flag - enable POSIX extensions  }
     IEXTEN = &0100000;
     _TIOC = ord('T') shl 8;
     TIOC = _TIOC;
     TCGETA = _TIOC or 1;
     TCSETA = _TIOC or 2;
     TCSETAW = _TIOC or 3;
     TCSETAF = _TIOC or 4;
     TCSBRK = _TIOC or 5;
     TCXONC = _TIOC or 6;
     TCFLSH = _TIOC or 7;
  { Slots reserved for 386/XENIX compatibility - keyboard control  }
     TIOCKBON = _TIOC or 8;
     TIOCKBOF = _TIOC or 9;
     KBENABLED = _TIOC or 10;
     IOCTYPE = $ff00;
     TCDSET = _TIOC or 32;
  { 386 - "RTS" toggle define 8A1 protocol  }
     RTS_TOG = _TIOC or 33;
     TIOCGWINSZ = _TIOC or 104;
     TIOCSWINSZ = _TIOC or 103;
  {
   * Softcarrier ioctls
    }
     TIOCGSOFTCAR = _TIOC or 105;
     TIOCSSOFTCAR = _TIOC or 106;
  { termios ioctls  }
     TCGETS = _TIOC or 13;
     TCSETS = _TIOC or 14;
  { same as TCSETS  }
     TCSANOW = _TIOC or 14;
     TCSETSW = _TIOC or 15;
  { same as TCSETSW  }
     TCSADRAIN = _TIOC or 15;
     TCSETSF = _TIOC or 16;
  {
   * NTP PPS ioctls
    }
     TIOCGPPS = _TIOC or 125;
     TIOCSPPS = _TIOC or 126;
     TIOCGPPSEV = _TIOC or 127;
  { Argument filled in by TIOCGPPSEV  }

  type
     ppsclockev = record
          tv : timeval;
          serial : cuint;
       end;

     ppsclockev32 = record
          tv : timeval32;
          serial : cuint32;
       end;

  { same as TCSETSF  }

  const
     TCSAFLUSH = _TIOC or 16;
  { termios option flags  }
  { flush data received but not read  }
     TCIFLUSH = 0;
  { flush data written but not transmitted  }
     TCOFLUSH = 1;
  { flush both data both input and output queues  }
     TCIOFLUSH = 2;
  { suspend output  }
     TCOOFF = 0;
  { restart suspended output  }
     TCOON = 1;
  { suspend input  }
     TCIOFF = 2;
  { restart suspended input  }
     TCION = 3;
  { Slots for 386/XENIX compatibility  }
  { BSD includes these ioctls in ttold.h  }

  const
     TIOCGETD = tIOC or 0;
     TIOCSETD = tIOC or 1;
     TIOCHPCL = tIOC or 2;
     TIOCGETP = tIOC or 8;
     TIOCSETP = tIOC or 9;
     TIOCSETN = tIOC or 10;
     TIOCEXCL = tIOC or 13;
     TIOCNXCL = tIOC or 14;
     TIOCFLUSH = tIOC or 16;
     TIOCSETC = tIOC or 17;
     TIOCGETC = tIOC or 18;
  {
   * BSD ioctls that are not the same as XENIX are included here.
   * There are also some relevant ioctls from SUN/BSD sys/ttycom.h
   * BSD pty ioctls like TIOCPKT are not supported in SVR4.
    }
  { bis local mode bits  }
     TIOCLBIS = tIOC or 127;
  { bic local mode bits  }
     TIOCLBIC = tIOC or 126;
  { set entire local mode word  }
     TIOCLSET = tIOC or 125;
  { get local modes  }
     TIOCLGET = tIOC or 124;
  { set break bit  }
     TIOCSBRK = tIOC or 123;
  { clear break bit  }
     TIOCCBRK = tIOC or 122;
  { set data terminal ready  }
     TIOCSDTR = tIOC or 121;
  { clear data terminal ready  }
     TIOCCDTR = tIOC or 120;
  { set local special chars  }
     TIOCSLTC = tIOC or 117;
  { get local special chars  }
     TIOCGLTC = tIOC or 116;
  { driver output queue size  }
     TIOCOUTQ = tIOC or 115;
  { void tty association  }
     TIOCNOTTY = tIOC or 113;
  { stop output, like ^S  }
     TIOCSTOP = tIOC or 111;
  { start output, like ^Q  }
     TIOCSTART = tIOC or 110;
  { private to Sun; do not use  }
     TIOCSILOOP = tIOC or 109;
  { end _SYS_TTOLD_H  }
  { POSIX job control ioctls  }

  { get pgrp of tty  }
  const
     TIOCGPGRP = tIOC or 20;
  { set pgrp of tty  }
     TIOCSPGRP = tIOC or 21;
  { get session id on ctty  }
     TIOCGSID = tIOC or 22;
  { Miscellaneous  }
  { simulate terminal input  }
     TIOCSTI = tIOC or 23;
  { Modem control  }
  { set all modem bits  }
     TIOCMSET = tIOC or 26;
  { bis modem bits  }
     TIOCMBIS = tIOC or 27;
  { bic modem bits  }
     TIOCMBIC = tIOC or 28;
  { get all modem bits  }
     TIOCMGET = tIOC or 29;
  { line enable  }
     TIOCM_LE = &0001;
  { data terminal ready  }
     TIOCM_DTR = &0002;
  { request to send  }
     TIOCM_RTS = &0004;
  { secondary transmit  }
     TIOCM_ST = &0010;
  { secondary receive  }
     TIOCM_SR = &0020;
  { clear to send  }
     TIOCM_CTS = &0040;
  { carrier detect  }
     TIOCM_CAR = &0100;
     TIOCM_CD = TIOCM_CAR;
  { ring  }
     TIOCM_RNG = &0200;
     TIOCM_RI = TIOCM_RNG;
  { data set ready  }
     TIOCM_DSR = &0400;
  { pseudo-tty  }
  { remote input editing  }
     TIOCREMOTE = tIOC or 30;
  { pty: send signal to slave  }
     TIOCSIGNAL = tIOC or 31;
  { Some more 386 xenix stuff  }
     LDIOC = ord('D') shl 8;
     LDOPEN = LDIOC or 0;
     LDCLOSE = LDIOC or 1;
     LDCHG = LDIOC or 2;
     LDGETT = LDIOC or 8;
     LDSETT = LDIOC or 9;
  { Slots for 386 compatibility  }
     LDSMAP = LDIOC or 110;
     LDGMAP = LDIOC or 111;
     LDNMAP = LDIOC or 112;
     LDEMAP = LDIOC or 113;
     LDDMAP = LDIOC or 114;
  {
   * These are retained for 386/XENIX compatibility.
    }
     DIOC = ord('d') shl 8;
  { V7  }
     DIOCGETP = DIOC or 8;
  { V7  }
     DIOCSETP = DIOC or 9;
  {
   * Returns a non-zero value if there
   * are characters in the input queue.
   *
   * XXX - somebody is confused here.  V7 had no such "ioctl", although XENIX may
   * have added it; 4BSD had FIONREAD, which returned the number of characters
   * waiting, and was supposed to work on all descriptors (i.e., every driver
   * should make a stab at implementing it).
    }
  { V7  }
     FIORDCHK = (ord('f') shl 8) or 3;
  {
   * Speeds
    }

  { parameter length, at most 255 bytes }
  IOCPARM_MASK = $ff;

  {  max size of ioctl args  }
  IOCPARM_MAX = IOCPARM_MASK + 1;

  { no parameters }
  IOC_VOID = culong($20000000);

  { copy parameters out }
  IOC_OUT = culong($40000000);

  { copy parameters in }
  IOC_IN = culong($80000000);

  { copy paramters in and out }
  IOC_INOUT = (IOC_IN or IOC_OUT);

  { mask for IN/OUT/VOID }
  IOC_DIRMASK = culong($e0000000);

  FIOCLEX = (IOC_VOID or (0 and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 1);
  FIONCLEX = (IOC_VOID or (0 and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 2);
  FIONREAD = (IOC_OUT or (sizeof(cint) and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 127);
  FIONBIO = (IOC_IN or (sizeof(cint) and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 126);
  FIOASYNC = (IOC_IN or (sizeof(cint) and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 125);
  FIOSETOWN = (IOC_IN or (sizeof(cint) and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 124);
  FIOGETOWN = (IOC_OUT or (sizeof(cint) and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 123);
  FIODTYPE = (IOC_OUT or (sizeof(cint) and IOCPARM_MASK) << 16) or ((ord('f') << 8) or 122);
  
  const
     B0 = 0;
     B50 = 1;
     B75 = 2;
     B110 = 3;
     B134 = 4;
     B150 = 5;
     B200 = 6;
     B300 = 7;
     B600 = 8;
     B1200 = 9;
     B1800 = 10;
     B2400 = 11;
     B4800 = 12;
     B9600 = 13;
     B19200 = 14;
     B38400 = 15;
     B57600 = 16;
     B76800 = 17;
     B115200 = 18;
     B153600 = 19;
     B230400 = 20;
     B307200 = 21;
     B460800 = 22;
  { Windowing structure to support JWINSIZE/TIOCSWINSZ/TIOCGWINSZ  }
  { rows, in characters  }
  { columns, in character  }
  { horizontal size, pixels  }
  { vertical size, pixels  }

  type
     winsize = record
          ws_row : word;
          ws_col : word;
          ws_xpixel : word;
          ws_ypixel : word;
       end;
     twinsize = winsize;
     pwinsize = ^twinsize;

  const
    { alias for FPC rtl }
     TIOCGETA = _TIOC or 1;
     TIOCSETA = _TIOC or 2;
     TIOCSETAW = _TIOC or 3;
     TIOCSETAF = _TIOC or 4;

  { same as TCSETSW  }
     TIOCDRAIN = _TIOC or 15;
