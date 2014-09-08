{$PACKRECORDS C}

const
  CBELL = #7;
{ delete  }
  CDEL = #127;
{ ^D  }
  CEOF = #4;
{ ^H  }
  CERASE = #8;
{ back-slash  }
  CESC = '\';
{ ^L  }
  CFORM = #12;
{ ^C  }
  CINTR = #3;
{ ^U  }
  CKILL = #21;
{ null char  }
  CNUL = #0;
{ ^\  }
  CQUIT = #28;
{ ^Q  }
  CSTART = #17;
{ ^S  }
  CSTOP = #19;
{ ^K  }
  CVT = #11;
{
 * Ioctl control packet
  }
  NCC = 8;

{
 *  Structure, defines for setting page length
  }
{ Set Page Length (Ioctl TCSLEN)  }

const
  PAGE_SETL = 04;
{ Paging Ioctl Command Mask (TCSLEN)  }
  PAGE_MSK = 03;
{ Enable Paging (TCSLEN)  }
  PAGE_ON = 01;
{ Disable Paging (TCSLEN)  }
  PAGE_OFF = 02;
{$ifndef PAGE_ENB}
{ For compatibility with older version  }

const
  PAGE_ENB = PAGE_ON;
{$endif}
type
  Ptty_page = ^tty_page;
  tty_page = record
      tp_flags : char;
      tp_slen : byte;
    end;

  twinsize = record
    ws_row, ws_col, ws_xpixel, ws_ypixel: cushort;
  end;


const
  IOC_VOID = $20000000;
  IOC_OUT = $40000000;
  IOC_IN  = $40000000 shl 1;
  IOCPARM_MASK = $7f;

  TIOCGETD = 	IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 0;	{ get line discipline }
  TIOCSETD = 	IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 1;	{ set line discipline }
  TIOCHPCL = 	IOC_VOID or (ord('t') shl 8) or 2;		{ hang up on last close }
  TIOCMODG = 	IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 3;	{ get modem control state }
  TIOCMODS = 	IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 4;	{ set modem control state }
  TIOCM_LE = 	0001;		{ line enable }
  TIOCM_DTR = 	0002;		{ data terminal ready }
  TIOCM_RTS = 	0004;		{ request to send }
  TIOCM_ST = 	0008;		{ secondary transmit }
  TIOCM_SR = 	0016;		{ secondary receive }
  TIOCM_CTS = 	0032;		{ clear to send }
  TIOCM_CAR = 	0064;		{ carrier detect }
  TIOCM_CD = 	TIOCM_CAR;
  TIOCM_RNG = 	0128;		{ ring }
  TIOCM_RI = 	TIOCM_RNG;
  TIOCM_DSR = 	0256;		{ data set ready }
//  TIOCGETP = 	IOC_OUT or ((sizeof(struct sgttyb) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 8{ get parameters -- gtty }
//  TIOCSETP = 	IOC_IN or ((sizeof(struct sgttyb) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 9{ set parameters -- stty }
//  TIOCSETN = 	IOC_IN or ((sizeof(struct sgttyb) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 10{ as above, but no flushtty }
  TIOCEXCL = 	IOC_VOID or (ord('t') shl 8) or 13;		{ set exclusive use of tty }
  TIOCNXCL = 	IOC_VOID or (ord('t') shl 8) or 14;		{ reset exclusive use of tty }
  TIOCFLUSH = 	IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 16;	{ flush buffers }
//  TIOCSETC = 	IOC_IN or ((sizeof(struct tchars) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 17{ set special characters }
//  TIOCGETC = 	IOC_OUT or ((sizeof(struct tchars) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 18{ get special characters }
  TANDEM = 		$00000001;	{ send stopc on out q full }
  CBREAK = 		$00000002;	{ half-cooked mode }
  LCASE = 		$00000004;	{ simulate lower case }

  ECHO = 		$00000008;	{ echo input }

  CRMOD = 		$00000010;	{ map \r to \r\n on output }
  RAW = 		$00000020;	{ no i/o processing }
  ODDP = 		$00000040;	{ get/send odd parity }
  EVENP = 		$00000080;	{ get/send even parity }
  ANYP = 		$000000c0;	{ get any parity/send none }
  CRDELAY = 		$00000300;	{ \r delay }
  CR0 = 	$00000000;
  CR1 = 	$00000100;	{ tn 300 }
  CR2 = 	$00000200;	{ tty 37 }
  CR3 = 	$00000300;	{ concept 100 }
  TBDELAY = 		$00000c00;	{ horizontal tab delay }
  TAB0 = 	$00000000;

  TAB1 = 	$00000400;	{ tty 37 }
  TAB2 = 	$00000800;

  XTABS = 		$00000c00;	{ expand tabs on output }
  BSDELAY = 		$00001000;	{ \b delay }
  BS0 = 	$00000000;

  BS1 = 	$00001000;

  VTDELAY = 		$00002000;	{ vertical tab delay }
  FF0 = 	$00000000;

  FF1 = 	$00002000;	{ tty 37 }

  NLDELAY = 		$0000c000;	{ \n delay }

  NL0 = 	$00000000;
  NL1 = 	$00004000;	{ tty 37 }
  NL2 = 	$00008000;	{ vt05 }
  NL3 = 	$0000c000;

  ALLDELAY = 	(NLDELAY or TBDELAY or CRDELAY or VTDELAY or BSDELAY);

  TOSTOP = 		$00010000;	{ SIGSTOP on bckgnd output }

  PRTERA = 		$00020000;	{ \ ... / erase }
  CRTERA = 		$00040000;	{ " \b " to wipe out char }
  TILDE = 		$00080000;	{ hazeltine tilde kludge }

  FLUSHO = 		$00100000;	{ flush output to terminal }

  LITOUT = 		$00200000;	{ literal output }
  CRTBS = 		$00400000;	{ do backspacing for crt }
  MDMBUF = 		$00800000;	{ dtr pacing }
  NOHANG = 		$01000000;	{ no SIGHUP on carrier drop }
  L001000 = 		$02000000;
  CRTKIL = 		$04000000;	{ kill line with " \b " }
  PASS8 = 		$08000000;
  CTLECH = 		$10000000;	{ echo control chars as ^X }

  PENDIN = 		$20000000;	{ tp->t_rawq needs reread }

  DECCTQ = 		$40000000;	{ only ^Q starts after ^S }
  NOFLUSH = 		$80000000;	{ no output flush on signal }


{ SYS V REL. 4 PTY IOCTLs    }
  UNLKPT =           IOC_VOID or (ord('t') shl 8) or 70;             { unlock slave pty }
  ISPTM =            IOC_VOID or (ord('t') shl 8) or 71;             { ret. maj+min of pty master }
  ISPTS =            IOC_VOID or (ord('t') shl 8) or 73;             { return maj+min of slave }
  GRTPT =            IOC_VOID or (ord('t') shl 8) or 74;             { grantpt slave pty}
  RLOGIND =          IOC_VOID or (ord('t') shl 8) or 75;             { for rlogind protocol in ptydd }
  TELNETDP =         IOC_VOID or (ord('t') shl 8) or 76;             { for telnetd protocol in ptydd }

  TIOCCONS = 	IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 98;	{ become virtual console }
  TIOCGSID = 	IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 72;	{ get the tty session id }


  TIOCLBIS =         IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 127;     { bis local mode bits }
  TIOCLBIC =         IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 126;     { bic local mode bits }
  TIOCLSET =         IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 125;     { set entire mode word }
  TIOCLGET =         IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 124;     { get local modes }
  LCRTBS =           (CRTBS>>16);
  LPRTERA =          (PRTERA>>16);
  LCRTERA =          (CRTERA>>16);
  LTILDE =           (TILDE>>16);
  LMDMBUF =          (MDMBUF>>16);
  LLITOUT =          (LITOUT>>16);
  LTOSTOP =          (TOSTOP>>16);
  LFLUSHO =          (FLUSHO>>16);
  LNOHANG =          (NOHANG>>16);
  LCRTKIL =          (CRTKIL>>16);
  LPASS8 =           (PASS8>>16);
  LCTLECH =          (CTLECH>>16);
  LPENDIN =          (PENDIN>>16);
  LDECCTQ =          (DECCTQ>>16);
  LNOFLSH =          (NOFLUSH>>16);
  TIOCSBRK =         IOC_VOID or (ord('t') shl 8) or 123;           { set break bit }
  TIOCCBRK =         IOC_VOID or (ord('t') shl 8) or 122;           { clear break bit }
  TIOCSDTR =         IOC_VOID or (ord('t') shl 8) or 121;           { set data terminal ready }
  TIOCCDTR =         IOC_VOID or (ord('t') shl 8) or 120;           { clear data terminal ready }
  TIOCGPGRP =        IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 119;     { get process group }
  TIOCSPGRP =        cint(IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 118);      { set process group }
//  TIOCSLTC =         IOC_IN or ((sizeof(struct ltchars) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 117{ set local special chars }
//  TIOCGLTC =         IOC_OUT or ((sizeof(struct ltchars) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 116{ get local special chars }
  TIOCOUTQ =         IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 115;     { output queue size }
  TIOCSTI =          IOC_IN or ((sizeof(char) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 114;    { simulate terminal input }
  TIOCNOTTY =        IOC_VOID or (ord('t') shl 8) or 113;           { void tty association }
  TIOCPKT =          IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 112;     { pty: set/clear packet mode }
  TIOCPKT_DATA =             $00;    { data packet }
  TIOCPKT_FLUSHREAD =        $01;    { flush packet }
  TIOCPKT_FLUSHWRITE =       $02;    { flush packet }
  TIOCPKT_STOP =             $04;    { stop output }
  TIOCPKT_START =            $08;    { start output }
  TIOCPKT_NOSTOP =           $10;    { no more ^S, ^Q }
  TIOCPKT_DOSTOP =           $20;    { now do ^S ^Q }
  TIOCSTOP =         IOC_VOID or (ord('t') shl 8) or 111;           { stop output, like ^S }
  TIOCSTART =        IOC_VOID or (ord('t') shl 8) or 110;           { start output, like ^Q }
  TIOCMSET =         IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 109;     { set all modem bits }
  TIOCMBIS =         IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 108;     { bis modem bits }
  TIOCMGET =         IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 106;     { get all modem bits }
  TIOCREMOTE =       IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 105;     { remote input editing }
  TIOCGWINSZ =       IOC_OUT or ((sizeof(twinsize) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 104;  { get window size }
  TIOCSWINSZ =       IOC_IN or ((sizeof(twinsize) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 103;  { set window size }
  TIOCUCNTL =        IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 102;     { pty: set/clr usr cntl mode }
  TIOCMIWAIT =       IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 100;     { wait for modem stat change }
{ SLIP (Serial Line IP) ioctl's }
  SLIOCGUNIT =       IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 101;     { get slip unit number }
  SLIOCSFLAGS =      IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 89;      { set configuration flags }
  SLIOCGFLAGS =      IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 90;      { get configuration flags }
  SLIOCSATTACH =     IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('t') shl 8) or 91;     { Attach slip i.f. to tty  }
//  UIOCCMD(n) =       IOC_VOID or (ord('u') shl 8) or n;             { usr cntl op "n" }

  OTTYDISC =         0;               { old, v7 std tty driver }
  NETLDISC =         1;               { line discip for berk net }
  NTTYDISC =         2;               { new tty discipline }
  TABLDISC =         3;               { tablet discipline }
  SLIPDISC =         4;               { serial IP discipline }

  FIOCLEX =          IOC_VOID or (ord('f') shl 8) or 1;             { set close on exec    }
  FIONCLEX =         IOC_VOID or (ord('f') shl 8) or 2;             { clear close on exec  }
{ another local }

  FIONREAD =         IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('f') shl 8) or 127;     { get # bytes to read }
  FIONBIO =          IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('f') shl 8) or 126;     { set/clear non-blocking i/o }
  FIOASYNC =         IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('f') shl 8) or 125;     { set/clear async i/o }

  FIOSETOWN =        IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('f') shl 8) or 124;     { set owner }
  FIOGETOWN =        IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('f') shl 8) or 123;     { get owner }
  FIOASYNCQX =       IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('f') shl 8) or 122;     { set/clear async queueing }

{ socket i/o controls }
  SIOCSHIWAT =       IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 0;              { set high watermark }
  SIOCGHIWAT =       IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 1;              { get high watermark }
  SIOCSLOWAT =       IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 2;              { set low watermark }
  SIOCGLOWAT =       IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 3;              { get low watermark }
  SIOCATMARK =       IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 7;              { at oob mark? }
  SIOCSPGRP =        IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 8;              { set process group }
  SIOCGPGRP =        IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('s') shl 8) or 9;              { get process group }

//  SIOCADDRT =        cint(IOC_IN or ((sizeof(struct ortentry) and IOCPARM_MASK) shl 16) or (ord('r') shl 8) or 10);     { add route }
//  SIOCDELRT =        cint(IOC_IN or ((sizeof(struct ortentry) and IOCPARM_MASK) shl 16) or (ord('r') shl 8) or 11);     { delete route }

//  SIOCSIFADDR =      cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 12);       { set ifnet address }
//  OSIOCGIFADDR =     (int)_IOWR('i',13, struct oifreq);       { get ifnet address }
//  SIOCGIFADDR =      (int)_IOWR('i',33, struct oifreq);       { get ifnet address }
  SIOCGIFADDRS =     cint(IOC_VOID or (ord('i') shl 8) or 140);       { get ifnet addresses for an if}
//  SIOCSIFDSTADDR =   cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 14);       { set p-p address }
//  OSIOCGIFDSTADDR =  (int)_IOWR('i',15, struct oifreq);       { get p-p address }
//  SIOCGIFDSTADDR =   (int)_IOWR('i',34, struct oifreq);       { get p-p address }
//  SIOCSIFFLAGS =     cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 16);       { set ifnet flags }
//  SIOCGIFFLAGS =     (int)_IOWR('i',17, struct oifreq);       { get ifnet flags }
//  OSIOCGIFBRDADDR =  (int)_IOWR('i',18, struct oifreq);       { get broadcast addr }
//  SIOCGIFBRDADDR =   (int)_IOWR('i',35, struct oifreq);       { get broadcast addr }
//  SIOCSIFBRDADDR =   cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 19);        { set broadcast addr }
//  OSIOCGIFCONF =     (int)_IOWR('i',20, struct ifconf);       { get ifnet list }
//  CSIOCGIFCONF =     (int)_IOWR('i',36, struct ifconf);       { get ifnet list }
//  SIOCGIFCONF =      (int)_IOWR('i',69, struct ifconf);       { get ifnet list }
//  OSIOCGIFNETMASK =  (int)_IOWR('i',21, struct oifreq);       { get net addr mask }
//  SIOCGIFNETMASK =   (int)_IOWR('i',37, struct oifreq);       { get net addr mask }
//  SIOCSIFNETMASK =   cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 22);        { set net addr mask }
//  SIOCGIFMETRIC =    (int)_IOWR('i',23, struct oifreq);       { get IF metric }
//  SIOCSIFMETRIC =    cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 24);        { set IF metric }
//  SIOCDIFADDR =      cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 25);        { delete IF addr }
//  SIOCAIFADDR =      cint(IOC_IN or ((sizeof(struct ifaliasreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 26);    { add/chg IF alias }
//  SIOCSIFSUBCHAN =   cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 27);        { set subchannel adr.}
//  SIOCSIFNETDUMP =   cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 28);        { set netdump fastwrt}

//  SIOUPDROUTE =      cint(IOC_IN or ((sizeof(struct ifaliasreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 29);    { Update Routing table }
//  SIOCSARP =         cint(IOC_IN or ((sizeof(struct arpreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 30);       { set arp entry }
//  OSIOCGARP =        (int)_IOWR('i',31, struct arpreq);       { get arp entry }
//  SIOCGARP =         (int)_IOWR('i',38, struct arpreq);       { get arp entry }
//  SIOCDARP =         cint(IOC_IN or ((sizeof(struct arpreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 32);       { delete arp entry }

//  SIOCSIFOPTIONS =   cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 41);       { set ifnet options }
//  SIOCGIFOPTIONS =   (int)_IOWR('i',42, struct oifreq);       { get ifnet options }
//  SIOCADDMULTI =     cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 49);        { add multicast addr }
//  SIOCDELMULTI =     cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 50);        { del multicast addr }
//  SIOCGETVIFCNT =    (int)_IOWR('u', 51, struct sioc_vif_req){ vif pkt cnt }
//  SIOCGETSGCNT =     (int)_IOWR('u', 52, struct sioc_sg_req); { s,g pkt cnt }

//  SIOCADDNETID =     cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 87);        { set netids }
//  SIOCSIFMTU =       cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 88);        { set mtu }
//  SIOCGIFMTU =       (int)_IOWR('i',86, struct oifreq);       { get mtu }

//  SIOCSNETOPT =      cint(IOC_IN or ((sizeof(struct optreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 90); { set network option }
//  SIOCGNETOPT =      (int)_IOWR('i', 91, struct optreq); { get network option }
//  SIOCDNETOPT =      (int)_IOWR('i', 128, struct optreq); { set default }
//  SIOCGNETOPT1 =     (int)_IOWR('i', 129, struct optreq1); { get network option }
//  SIOCGLOADF =       (int)_IOWR('i', 130, int); { get flag: if loadtime passed }
//  SIOCSLOADF =       (int)_IOWR('i', 131, int); { set flag: if loadtime passed }
//  SIOCGTUNEPHASE =   (int)_IOWR('i', 138, int); { get tuning_phase }

//  SIOCSX25XLATE =    cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 99);       { set xlate tab }
//  SIOCGX25XLATE =    (int)_IOWR('i',100, struct oifreq);      { get xlate tab }
//  SIOCDX25XLATE =    cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 101);      { delete xlate tab }

//  SIOCIFDETACH =     cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 102);       { detach an ifnet }
//  SIOCIFATTACH =     cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 103);       { attach an ifnet }

  SIOCGNMTUS =       cint(IOC_VOID or (ord('i') shl 8) or 110); { get NMTUs }
  SIOCGETMTUS =      cint(IOC_VOID or (ord('i') shl 8) or 111); { get common_mtus }
  SIOCADDMTU =       cint(IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 112); { add mtu  }
  SIOCDELMTU =       cint(IOC_IN or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 113); { delete mtu }

  SIOCGIFGIDLIST =   cint(IOC_VOID or (ord('i') shl 8) or 104);                   { get gidlist }
  SIOCSIFGIDLIST =   cint(IOC_VOID or (ord('i') shl 8) or 105);                   { set gidlist }

  SIOCGSIZIFCONF =   cint(IOC_OUT or ((sizeof(cint) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 106); { get size for SIOCGIFCONF }

//  SIOCIF_ATM_UBR =           cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 120);  { set ubr rate }
//  SIOCIF_ATM_SNMPARP =       cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 121);  { atm snmp arp }
//  SIOCIF_ATM_IDLE =          cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 122);  { set idle time }
//  SIOCIF_ATM_DUMPARP =       cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 123);  { atm dump arp }
//  SIOCIF_ATM_SVC =           cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 124);  { atmif init }
//  SIOCIF_ATM_DARP =          cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 125);  { del atmarp }
//  SIOCIF_ATM_GARP =          cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 126);  { get atmarp }
//  SIOCIF_ATM_SARP =          cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 127);  { set atmarp };

{ For IP over Infiniband }

//  SIOCIF_IB_DUMP_ARP =       cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 128); { ib dump arp }
//  SIOCIF_IB_DEL_ARP =        cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 129); { if_ib del  ibarp }
//  SIOCIF_IB_GET_ARP =        cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 130); { get ibarp }
//  SIOCIF_IB_SET_ARP =        cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 131); { set ibarp }
//  SIOCIF_IB_SET_PKEY =       cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 132); { set ib port  pkey value }
//  SIOCIFGETPKEY =            cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 101); { set ubr rate }
//  SIOCIF_IB_SET_PORT =       cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 133); { set ubr rate }
//  SIOCIF_IB_SET_QSIZE =      cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 134); { set ib qsize  }
//  SIOCIF_IB_RESIZE_CQ =      cint(IOC_IN or ((sizeof(struct ifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 135); { set resize IB CQ  }




//  SIOCGISNO =        (int)_IOWR('i',107, struct oifreq);      { get IF network options }
//  SIOCSISNO =        cint(IOC_IN or ((sizeof(struct oifreq) and IOCPARM_MASK) shl 16) or (ord('i') shl 8) or 108);      { set IF network options }
//  SIOCGIFBAUDRATE =  (int)_IOWR('i', 109, struct oifreq);     { get ifnet's if_baudrate }

  SIOCADDIFVIPA =    cint(IOC_VOID or (ord('i') shl 8) or 66);       { specify interfaces that should use this vipa }
  SIOCDELIFVIPA =    cint(IOC_VOID or (ord('i') shl 8) or 67);       { unconfigure interfaces using this vipa       }
  SIOCLISTIFVIPA =   cint(IOC_VOID or (ord('i') shl 8) or 68);       { list interfaces using this vipa              }

  TIOC = ord('T') shl 8;
  TCGETS = TIOC or 1;
  TCSETS = TIOC or 2;
  TCSETSW = TIOC or 3;
  TCSETSF = TIOC or 4;
  TCGETA = TIOC or 5;
  TCSETA = TIOC or 6;
  TCSETAW = TIOC or 7;
  TCSETAF = TIOC or 8;
{ SVID interface  }
  TCSBRK = TIOC or 9;
{ 0->.25 seconds else <arg>ms  }
  TCSBREAK = TIOC or 10;
  TCXONC = TIOC or 11;
  TCFLSH = TIOC or 12;
  TCGLEN = TIOC or 13;
  TCSLEN = TIOC or 14;
  TCSAK = TIOC or 15;
  TCQSAK = TIOC or 16;
  TCTRUST = TIOC or 17;
  TCQTRUST = TIOC or 18;
  TCSMAP = TIOC or 19;
  TCGMAP = TIOC or 20;
  TCKEP = TIOC or 21;
  TCGSAK = TIOC or 22;
  TCLOOP = TIOC or 23;
  TCVPD = TIOC or 24;
  TCREG = TIOC or 25;
  TCGSTATUS = TIOC or 26;
  TCSCONTROL = TIOC or 27;
  TCSCSMAP = TIOC or 28;
  TCGCSMAP = TIOC or 29;
  TCMGR = TCSAK;
  TCQMGR = TCQSAK;
  TIONREAD = FIONREAD;
{ used with TCSAK and TCQSAK  }
  TCSAKOFF = 0;
  TCSAKON = 1;
{ used with TCTRUST and TCQTRUCT  }
  TCUNTRUSTED = 0;
  TCTRUSTED = 1;


type
  Ptcflag_t = ^tcflag_t;
  tcflag_t = dword;

  Pcc_t = ^cc_t;
  cc_t = byte;

const
  NCCS = 16;  
type
  Pspeed_t = ^speed_t;
  speed_t = dword;
{
 * Ioctl control packet
  }
{ input modes  }
{ output modes  }
{ control modes  }
{ line discipline modes  }
{ control chars  }
  Ptermios = ^termios;
  termios = record
      c_iflag : tcflag_t;
      c_oflag : tcflag_t;
      c_cflag : tcflag_t;
      c_lflag : tcflag_t;
      c_cc : array[0..(NCCS)-1] of cc_t;
    end;

{ use POSIX required prototypes  }
function tcgetattr(_para1:longint; var _para2:termios):longint;cdecl;external;
function tcgetsid(_para1:longint):pid_t;cdecl;external;
(* Const before type ignored *)

function tcsetattr(_para1:longint; _para2:longint; constref _para3:termios):longint;cdecl;external;
//function tcsendbreak(_para1:longint; _para2:longint):longint;cdecl;external;
//function tcdrain(_para1:longint):longint;cdecl;external;
//function tcflush(_para1:longint; _para2:longint):longint;cdecl;external;
///function tcflow(_para1:longint; _para2:longint):longint;cdecl;external;
(* Const before type ignored *)
function cfgetospeed(var _para1:termios):speed_t;cdecl;external;
(* Const before type ignored *)
function cfgetispeed(var _para1:termios):speed_t;cdecl;external;
function cfsetospeed(constref _para1:termios; _para2:speed_t):longint;cdecl;external;
function cfsetispeed(var _para1:termios; _para2:speed_t):longint;cdecl;external;

{ mask name symbols for c_lflag  }
{ values for optional_actions arguments to tcsetattr()  }

const
  TCSANOW = 0;  
  TCSADRAIN = 1;  
  TCSAFLUSH = 2;  
{ values for the queue_selector argument to tcflush()  }
  TCIFLUSH = 0;  
  TCOFLUSH = 1;  
  TCIOFLUSH = 2;  
{ values for the action argument to tcflow()  }
  TCOOFF = 0;  
  TCOON = 1;  
  TCIOFF = 2;  
  TCION = 3;  
{ control characters  }
  VINTR = 0;  
  VQUIT = 1;  
  VERASE = 2;  
  VKILL = 3;  
  VEOF = 4;  
  VEOL = 5;  
  VSTART = 7;  
  VSTOP = 8;  
  VSUSP = 9;  
  VMIN = 4;  
  VTIME = 5;  
  VEOL2 = 6;  
  VDSUSP = 10;  
  VREPRINT = 11;  
  VDISCRD = 12;  
  VWERSE = 13;  
  VLNEXT = 14;  
{ 5.4 compatability  }
  VSTRT = VSTART;  

const
  B0 = $00000000;  
  B50 = $00000001;  
  B75 = $00000002;  
  B110 = $00000003;  
  B134 = $00000004;  
  B150 = $00000005;  
  B200 = $00000006;  
  B300 = $00000007;  
  B600 = $00000008;  
  B1200 = $00000009;  
  B1800 = $0000000a;  
  B2400 = $0000000b;  
  B4800 = $0000000c;  
  B9600 = $0000000d;  
  B19200 = $0000000e;  
  B38400 = $0000000f;  
  EXTA = B19200;  
  EXTB = B38400;  
{ _ALL_SOURCE  }
{ c_iflag bits  }

const
  IGNBRK = $00000001;  
  BRKINT = $00000002;  
  IGNPAR = $00000004;  
  PARMRK = $00000008;  
  INPCK = $00000010;  
  ISTRIP = $00000020;  
  INLCR = $00000040;  
  IGNCR = $00000080;  
  ICRNL = $00000100;  
  IXON = $00000200;  
  IXOFF = $00000400;  
const
  IUCLC = $00000800;  

const
  IXANY = $00001000;  
  IMAXBEL = $00010000;  
{ c_oflag bits  }

const
  OPOST = $00000001;  
const
  OLCUC = $00000002;  

const
  ONLCR = $00000004;  
  OCRNL = $00000008;  
  ONOCR = $00000010;  
  ONLRET = $00000020;  
  OFILL = $00000040;  
  OFDEL = $00000080;  
  CRDLY = $00000300;  
//  CR0 = $00000000;
//  CR1 = $00000100;
//  CR2 = $00000200;
//  CR3 = $00000300;
  TABDLY = $00000c00;  
//  TAB0 = $00000000;
//  TAB1 = $00000400;
//  TAB2 = $00000800;
//  TAB3 = $00000c00;
  BSDLY = $00001000;  
//  BS0 = $00000000;
//  BS1 = $00001000;
  FFDLY = $00002000;  
//  FF0 = $00000000;
//  FF1 = $00002000;
  NLDLY = $00004000;  
//  NL0 = $00000000;
//  NL1 = $00004000;
  VTDLY = $00008000;  
  VT0 = $00000000;  
  VT1 = $00008000;  
  DLY_MASK = ((((NLDLY or CRDLY) or TABDLY) or BSDLY) or VTDLY) or FFDLY;  
{ expand tabs to spaces added     	 }
  OXTABS = $00040000;  
{ on 08/05/92.                          }
{ discard EOT's (^D) on output    	 }
  ONOEOT = $00080000;  
{ added on 08/05/92.                    }
{ c_cflag bits  }

const
  _CBAUD = $0000000f;  
  CBAUD = _CBAUD;
  CSIZE = $00000030;
  CS5 = $00000000;  
  CS6 = $00000010;  
  CS7 = $00000020;  
  CS8 = $00000030;  
  CSTOPB = $00000040;  
  CREAD = $00000080;  
  PARENB = $00000100;  
  PARODD = $00000200;  
  HUPCL = $00000400;  
  CLOCAL = $00000800;  
  _CIBAUD = $000f0000;  
  _IBSHIFT = 16;  
  CIBAUD = _CIBAUD;  
  IBSHIFT = _IBSHIFT;  
  PAREXT = $00100000;  
{ c_lflag bits  }

const
  ISIG = $00000001;  
  ICANON = $00000002;  

const
  XCASE = $00000004;  

const
//  ECHO = $00000008;
  ECHOE = $00000010;  
  ECHOK = $00000020;  
  ECHONL = $00000040;  
  NOFLSH = $00000080;  
//  TOSTOP = $00010000;
  ECHOCTL = $00020000;  
  ECHOPRT = $00040000;  
  ECHOKE = $00080000;  
//  FLUSHO = $00100000;
{ use alternate WERASE    	 }
  ALTWERASE = $00400000;  
{ algorithm, added ALTWERASE    }
{ on 08/05/92.                  }
//  PENDIN = $20000000;
{ ALL_SOURCE  }

const
  IEXTEN = $00200000;  

