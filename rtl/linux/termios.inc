{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by Marco van de Voort
    member of the Free Pascal development team

    ioctls constants for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{********************
   IOCtl(TermIOS)
********************}

Const
  { Amount of Control Chars }
  NCCS = 32;
  NCC = 8;

{$ifdef cpupowerpc}
  TCGETS            = $402c7413;
  TCSETS            = $802c7414;
  TCSETSW           = $802c7415;
  TCSETSF           = $802c7416;
  TCGETA            = $40147417;
  TCSETA            = $80147418;
  TCSETAW           = $80147419;
  TCSETAF           = $8014741c;
  TCSBRK            = $2000741d;
  TCXONC            = $2000741e;
  TCFLSH            = $2000741f;
  TIOCEXCL          = $540c;
  TIOCNXCL          = $540d;
  TIOCSCTTY         = $540e;
  TIOCGPGRP         = $40047477;
  TIOCSPGRP         = $80047476;
  TIOCOUTQ          = $40047473;
  TIOCSTI           = $5412;
  TIOCGWINSZ        = $40087468;
  TIOCSWINSZ        = $80087467;
  TIOCMGET          = $5415;
  TIOCMBIS          = $5416;
  TIOCMBIC          = $5417;
  TIOCMSET          = $5418;
  TIOCGSOFTCAR      = $5419;
  TIOCSSOFTCAR      = $541a;
  FIONREAD          = $4004667f;
  TIOCINQ           = $4004667f;
  TIOCLINUX         = $541c;
  TIOCCONS          = $541d;
  TIOCGSERIAL       = $541e;
  TIOCSSERIAL       = $541f;
  TIOCPKT           = $5420;
  FIONBIO           = $8004667e;
  TIOCNOTTY         = $5422;
  TIOCSETD          = $5423;
  TIOCGETD          = $5424;
  TCSBRKP           = $5425;
  TIOCTTYGSTRUCT    = $5426;
  FIONCLEX          = $20006602;
  FIOCLEX           = $20006601;
  FIOASYNC          = $8004667d;
  TIOCSERCONFIG     = $5453;
  TIOCSERGWILD      = $5454;
  TIOCSERSWILD      = $5455;
  TIOCGLCKTRMIOS    = $5456;
  TIOCSLCKTRMIOS    = $5457;
  TIOCSERGSTRUCT    = $5458;
  TIOCSERGETLSR     = $5459;
  TIOCSERGETMULTI   = $545a;
  TIOCSERSETMULTI   = $545b;
  TIOCMIWAIT        = $545c;
  TIOCGICOUNT       = $545d;

{c_cc characters}
  VINTR = 0;
  VQUIT = 1;
  VERASE = 2;
  VKILL = 3;
  VEOF = 4;
  VMIN = 5;
  VEOL = 6;
  VTIME = 7;
  VEOL2 = 8;
  VSWTC = 9;
  VWERASE = 10;
  VREPRINT = 11;
  VSUSP = 12;
  VSTART = 13;
  VSTOP = 14;
  VLNEXT = 15;
  VDISCARD = 16;

{ c_iflag bits }
	 IGNBRK	= &0000001;
	 BRKINT	= &0000002;
	 IGNPAR	= &0000004;
	 PARMRK	= &0000010;
	 INPCK	= &0000020;
	 ISTRIP	= &0000040;
	 INLCR	= &0000100;
	 IGNCR	= &0000200;
	 ICRNL	= &0000400;
	 IXON	= &0001000;
	 IXOFF	= &0002000;
	 IXANY	= &0004000;
	 IUCLC	= &0010000;
	 IMAXBEL= &0020000;

{ c_oflag bits }
	 OPOST	= &0000001;
	 ONLCR	= &0000002;
	 OLCUC	= &0000004;
	 OCRNL	= &0000010;
	 ONOCR	= &0000020;
	 ONLRET	= &0000040;
                
	 OFILL	= &00000100;
	 OFDEL	= &00000200;
	 NLDLY	= &00001400;
	   NL0	= &00000000;
	   NL1	= &00000400;
	   NL2	= &00001000;
	   NL3	= &00001400;
	 TABDLY	= &00006000;
	   TAB0	= &00000000;
	   TAB1	= &00002000;
	   TAB2	= &00004000;
	   TAB3	= &00006000;
	 CRDLY	= &00030000;
	   CR0	= &00000000;
	   CR1	= &00010000;
	   CR2	= &00020000;
	   CR3	= &00030000;
	 FFDLY	= &00040000;
	   FF0	= &00000000;
	   FF1	= &00040000;
	 BSDLY	= &00100000;
	   BS0	= &00000000;
	   BS1	= &00100000;
	 VTDLY	= &00200000;
	   VT0	= &00000000;
	   VT1	= &00200000;
	 XTABS	= &01000000; { Hmm.. Linux/i386 considers this part of TABDLY.. }
                           
{ c_cflag bit meaning }
	 CBAUD	= &0000377;
	  B0	= &0000000;		{ hang up }
	  B50	= &0000001;
	  B75	= &0000002;
	  B110	= &0000003;
	  B134	= &0000004;
	  B150	= &0000005;
	  B200	= &0000006;
	  B300	= &0000007;
	  B600	= &0000010;
	  B1200	= &0000011;
	  B1800	= &0000012;
	  B2400	= &0000013;
	  B4800	= &0000014;
	  B9600	= &0000015;
	  B19200= &0000016;
	  B38400= &0000017;
	  EXTA  = B19200;
	  EXTB  = B38400;
	  CBAUDEX  = &0000020;
	  B57600   = &00020;
	  B115200  = &00021;
	  B230400  = &00022;
	  B460800  = &00023;
	  B500000  = &00024;
	  B576000  = &00025;
	  B921600  = &00026;
	  B1000000 = &00027;
	  B1152000 = &00030;
	  B1500000 = &00031;
	  B2000000 = &00032;
	  B2500000 = &00033;
	  B3000000 = &00034;
	  B3500000 = &00035;
	  B4000000 = &00036;
	 __MAX_BAUD=  B4000000;

	 CSIZE	= &00001400;
	   CS5	= &00000000;
	   CS6	= &00000400;
	   CS7	= &00001000;
	   CS8	= &00001400;

	 CSTOPB	= &00002000;
	 CREAD	= &00004000;
	 PARENB	= &00010000;
	 PARODD	= &00020000;
	 HUPCL	= &00040000;
	 CLOCAL	= &00100000;
	 CRTSCTS= &020000000000;		{ flow control }

{ c_lflag bits }
	 ISIG	= $00000080;
	 ICANON	= $00000100;
	 XCASE	= $00004000;
	 ECHO	= $00000008;
	 ECHOE	= $00000002;
	 ECHOK	= $00000004;
	 ECHONL	= $00000010;
	 NOFLSH	= $80000000;
	 TOSTOP	= $00400000;
	 ECHOCTL= $00000040;
	 ECHOPRT= $00000020;
	 ECHOKE	= $00000001;
	 FLUSHO	= $00800000;
	 PENDIN	= $20000000;
	 IEXTEN	= $00000400;

{ Values for the ACTION argument to `tcflow'.  }
		TCOOFF		= 0;
		TCOON		= 1;
		TCIOFF		= 2;
		TCION		= 3;

{ Values for the QUEUE_SELECTOR argument to `tcflush'.  }
		TCIFLUSH	= 0;
		TCOFLUSH	= 1;
		TCIOFLUSH	= 2;

{ Values for the OPTIONAL_ACTIONS argument to `tcsetattr'.  }
		TCSANOW		= 0;
		TCSADRAIN	= 1;
		TCSAFLUSH	= 2;

{ Used for packet mode }
	 TIOCPKT_DATA		=  0;
	 TIOCPKT_FLUSHREAD	=  1;
	 TIOCPKT_FLUSHWRITE	=  2;
	 TIOCPKT_STOP		=  4;
	 TIOCPKT_START		=  8;
	 TIOCPKT_NOSTOP		= 16;
	 TIOCPKT_DOSTOP		= 32;

{ c_cc characters }
	 _VINTR	= 0;
	 _VQUIT	= 1;
	 _VERASE= 2;
	 _VKILL	= 3;
	 _VEOF	= 4;
	 _VMIN	= 5;
	 _VEOL	= 6;
	 _VTIME	= 7;
	 _VEOL2	= 8;
	 _VSWTC	= 9;

{ modem lines }
	 TIOCM_LE	= $001;
	 TIOCM_DTR	= $002;
	 TIOCM_RTS	= $004;
	 TIOCM_ST	= $008;
	 TIOCM_SR	= $010;
	 TIOCM_CTS	= $020;
	 TIOCM_CAR	= $040;
	 TIOCM_RNG	= $080;
	 TIOCM_DSR	= $100;
	 TIOCM_CD	= TIOCM_CAR;
	 TIOCM_RI	= TIOCM_RNG;

{ ioctl (fd, TIOCSERGETLSR, &result) where result may be as below }
	 TIOCSER_TEMT    = $01;	{ Transmitter physically empty }

{ line disciplines }
	 N_TTY		= 0;
	 N_SLIP		= 1;
	 N_MOUSE	= 2;
	 N_PPP		= 3;
	 N_STRIP	= 4;
	 N_AX25		= 5;
	 N_X25		= 6;	{ X.25 async  }
	 N_6PACK	= 7;
	 N_MASC		= 8;	{ Mobitex module  }
	 N_R3964	= 9;	{ Simatic R3964 module  }
	 N_PROFIBUS_FDL	= 10;	{ Profibus  }
	 N_IRDA		= 11;	{ Linux IR  }
	 N_SMSBLOCK	= 12;	{ SMS block mode  }
	 N_HDLC		= 13;	{ synchronous HDLC  }
	 N_SYNC_PPP	= 14;	{ synchronous PPP  }
	 N_HCI		= 15;	{ Bluetooth HCI UART  }


{$endif cpupowerpc}

{$ifdef cpui386}
  { For Terminal handling }
  TCGETS          = $5401;
  TCSETS          = $5402;
  TCSETSW         = $5403;
  TCSETSF         = $5404;
  TCGETA          = $5405;
  TCSETA          = $5406;
  TCSETAW         = $5407;
  TCSETAF         = $5408;
  TCSBRK          = $5409;
  TCXONC          = $540A;
  TCFLSH          = $540B;
  TIOCEXCL        = $540C;
  TIOCNXCL        = $540D;
  TIOCSCTTY       = $540E;
  TIOCGPGRP       = $540F;
  TIOCSPGRP       = $5410;
  TIOCOUTQ        = $5411;
  TIOCSTI         = $5412;
  TIOCGWINSZ      = $5413;
  TIOCSWINSZ      = $5414;
  TIOCMGET        = $5415;
  TIOCMBIS        = $5416;
  TIOCMBIC        = $5417;
  TIOCMSET        = $5418;
  TIOCGSOFTCAR    = $5419;
  TIOCSSOFTCAR    = $541A;
  FIONREAD        = $541B;
  TIOCINQ         = FIONREAD;
  TIOCLINUX       = $541C;
  TIOCCONS        = $541D;
  TIOCGSERIAL     = $541E;
  TIOCSSERIAL     = $541F;
  TIOCPKT         = $5420;
  FIONBIO         = $5421;
  TIOCNOTTY       = $5422;
  TIOCSETD        = $5423;
  TIOCGETD        = $5424;
  TCSBRKP         = $5425;
  TIOCTTYGSTRUCT  = $5426;
  FIONCLEX        = $5450;
  FIOCLEX         = $5451;
  FIOASYNC        = $5452;
  TIOCSERCONFIG   = $5453;
  TIOCSERGWILD    = $5454;
  TIOCSERSWILD    = $5455;
  TIOCGLCKTRMIOS  = $5456;
  TIOCSLCKTRMIOS  = $5457;
  TIOCSERGSTRUCT  = $5458;
  TIOCSERGETLSR   = $5459;
  TIOCSERGETMULTI = $545A;
  TIOCSERSETMULTI = $545B;

  TIOCMIWAIT      = $545C;
  TIOCGICOUNT     = $545D;
  TIOCGHAYESESP   = $545E;
  TIOCSHAYESESP   = $545F;
  FIOQSIZE        = $5460;


  TIOCPKT_DATA       = 0;
  TIOCPKT_FLUSHREAD  = 1;
  TIOCPKT_FLUSHWRITE = 2;
  TIOCPKT_STOP       = 4;
  TIOCPKT_START      = 8;
  TIOCPKT_NOSTOP     = 16;
  TIOCPKT_DOSTOP     = 32;

{c_cc characters}
  VINTR    = 0;
  VQUIT    = 1;
  VERASE   = 2;
  VKILL    = 3;
  VEOF     = 4;
  VTIME    = 5;
  VMIN     = 6;
  VSWTC    = 7;
  VSTART   = 8;
  VSTOP    = 9;
  VSUSP    = 10;
  VEOL     = 11;
  VREPRINT = 12;
  VDISCARD = 13;
  VWERASE  = 14;
  VLNEXT   = 15;
  VEOL2    = 16;

{c_iflag bits}
   IGNBRK  = $0000001;
   BRKINT  = $0000002;
   IGNPAR  = $0000004;
   PARMRK  = $0000008;
   INPCK   = $0000010;
   ISTRIP  = $0000020;
   INLCR   = $0000040;
   IGNCR   = $0000080;
   ICRNL   = $0000100;
   IUCLC   = $0000200;
   IXON    = $0000400;
   IXANY   = $0000800;
   IXOFF   = $0001000;
   IMAXBEL = $0002000;
   IUTF8   = $0004000;

{c_oflag bits}
   OPOST  = $0000001;
   OLCUC  = $0000002;
   ONLCR  = $0000004;
   OCRNL  = $0000008;
   ONOCR  = $0000010;
   ONLRET = $0000020;
   OFILL  = $0000040;
   OFDEL  = $0000080;
   NLDLY  = $0000100;
     NL0  = $0000000;
     NL1  = $0000100;
   CRDLY  = $0000600;
     CR0  = $0000000;
     CR1  = $0000200;
     CR2  = $0000400;
     CR3  = $0000600;
   TABDLY = $0001800;
     TAB0 = $0000000;
     TAB1 = $0000800;
     TAB2 = $0001000;
     TAB3 = $0001800;
    XTABS = $0001800;
   BSDLY  = $0002000;
     BS0  = $0000000;
     BS1  = $0002000;
   VTDLY  = $0004000;
     VT0  = $0000000;
     VT1  = $0004000;
   FFDLY  = $0008000;
     FF0  = $0000000;
     FF1  = $0008000;

{c_cflag bits}
   CBAUD   = $000100F;
   B0      = $0000000;
   B50     = $0000001;
   B75     = $0000002;
   B110    = $0000003;
   B134    = $0000004;
   B150    = $0000005;
   B200    = $0000006;
   B300    = $0000007;
   B600    = $0000008;
   B1200   = $0000009;
   B1800   = $000000A;
   B2400   = $000000B;
   B4800   = $000000C;
   B9600   = $000000D;
   B19200  = $000000E;
   B38400  = $000000F;
   EXTA    = B19200;
   EXTB    = B38400;
   CSIZE   = $0000030;
     CS5   = $0000000;
     CS6   = $0000010;
     CS7   = $0000020;
     CS8   = $0000030;
   CSTOPB  = $0000040;
   CREAD   = $0000080;
   PARENB  = $0000100;
   PARODD  = $0000200;
   HUPCL   = $0000400;
   CLOCAL  = $0000800;
   CBAUDEX = $0001000;
   B57600  = $0001001;
   B115200 = $0001002;
   B230400 = $0001003;
   B460800 = $0001004;
   B500000 = $0001005;
   B576000 = $0001006;
   B921600 = $0001007;
   B1000000= $0001008;
   B1152000= $0001009;
   B1500000= $000100A;
   B2000000= $000100B;
   B2500000= $000100C;
   B3000000= $000100D;
   B3500000= $000100E;
   B4000000= $000100F;

   CIBAUD  = $100F0000;
   CMSPAR  = $40000000;
   CRTSCTS = $80000000;

{c_lflag bits}
   ISIG    = $0000001;
   ICANON  = $0000002;
   XCASE   = $0000004;
   ECHO    = $0000008;
   ECHOE   = $0000010;
   ECHOK   = $0000020;
   ECHONL  = $0000040;
   NOFLSH  = $0000080;
   TOSTOP  = $0000100;
   ECHOCTL = $0000200;
   ECHOPRT = $0000400;
   ECHOKE  = $0000800;
   FLUSHO  = $0001000;
   PENDIN  = $0004000;
   IEXTEN  = $0008000;

{c_line bits}
   TIOCM_LE   = $001;
   TIOCM_DTR  = $002;
   TIOCM_RTS  = $004;
   TIOCM_ST   = $008;
   TIOCM_SR   = $010;
   TIOCM_CTS  = $020;
   TIOCM_CAR  = $040;
   TIOCM_RNG  = $080;
   TIOCM_DSR  = $100;
   TIOCM_CD   = TIOCM_CAR;
   TIOCM_RI   = TIOCM_RNG;
   TIOCM_OUT1 = $2000;
   TIOCM_OUT2 = $4000;

{TCSetAttr}
   TCSANOW   = 0;
   TCSADRAIN = 1;
   TCSAFLUSH = 2;

{TCFlow}
   TCOOFF = 0;
   TCOON  = 1;
   TCIOFF = 2;
   TCION  = 3;

{TCFlush}
   TCIFLUSH  = 0;
   TCOFLUSH  = 1;
   TCIOFLUSH = 2;

{$endif cpui386}

{$ifdef cpusparc}
  TCGETA = $40125401;
  TCSETA = $80125402;
  TCSETAW = $80125403;
  TCSETAF = $80125404;
  TCSBRK = $20005405;
  TCXONC = $20005406;
  TCFLSH = $20005407;
  TCGETS = $40245408;
  TCSETS = $80245409;
  TCSETSW = $8024540a;
  TCSETSF = $8024540b;
  TIOCGETD = $40047400;
  TIOCSETD = $80047401;
  TIOCEXCL = $2000740d;
  TIOCNXCL = $2000740e;
  TIOCCONS = $20007424;
  TIOCGSOFTCAR = $40047464;
  TIOCSSOFTCAR = $80047465;
  TIOCSWINSZ = $80087467;
  TIOCGWINSZ = $40087468;
  TIOCMGET = $4004746a;
  TIOCMBIC = $8004746b;
  TIOCMBIS = $8004746c;
  TIOCMSET = $8004746d;
  TIOCSTART = $2000746e;
  TIOCSTOP = $2000746f;
  TIOCPKT = $80047470;
  TIOCNOTTY = $20007471;
  TIOCSTI = $80017472;
  TIOCOUTQ = $40047473;
  TIOCCBRK = $2000747a;
  TIOCSBRK = $2000747b;
  TIOCSPGRP = $80047482;
  TIOCGPGRP = $40047483;
  TIOCSCTTY = $20007484;
  TIOCGSID = $40047485;
  TIOCGPTN = $40047486;
  TIOCSPTLCK = $80047487;
  FIOCLEX = $20006601;
  FIONCLEX = $20006602;
  FIOASYNC = $8004667d;
  FIONBIO = $8004667e;
  FIONREAD = $4004667f;
  TIOCINQ = $4004667f;
  TIOCLINUX = $541c;
  TIOCGSERIAL = $541e;
  TIOCSSERIAL = $541f;
  TCSBRKP = $5425;
  TIOCTTYGSTRUCT = $5426;
  TIOCSERCONFIG = $5453;
  TIOCSERGWILD = $5454;
  TIOCSERSWILD = $5455;
  TIOCGLCKTRMIOS = $5456;
  TIOCSLCKTRMIOS = $5457;
  TIOCSERGSTRUCT = $5458;
  TIOCSERGETLSR = $5459;
  TIOCSERGETMULTI = $545a;
  TIOCSERSETMULTI = $545b;
  TIOCMIWAIT = $545c;
  TIOCGICOUNT = $545d;
  TIOCPKT_DATA = $0;
  TIOCPKT_FLUSHREAD = $1;
  TIOCPKT_FLUSHWRITE = $2;
  TIOCPKT_STOP = $4;
  TIOCPKT_START = $8;
  TIOCPKT_NOSTOP = $10;
  TIOCPKT_DOSTOP = $20;

{c_cc characters}
  VINTR = 0;
  VQUIT = 1;
  VERASE = 2;
  VKILL = 3;
  VEOF = 4;
  VEOL = 5;
  VEOL2 = 6;
  VSWTC = 7;
  VSTART = 8;
  VSTOP = 9;
  VSUSP = 10;
  VDSUSP = 11;
  VREPRINT = 12;
  VDISCARD = 13;
  VWERASE = 14;
  VLNEXT = 15;
  VMIN = 4;
  VTIME = 5;

{c_iflag bits}
   IGNBRK  = $0000001;
   BRKINT  = $0000002;
   IGNPAR  = $0000004;
   PARMRK  = $0000008;
   INPCK   = $0000010;
   ISTRIP  = $0000020;
   INLCR   = $0000040;
   IGNCR   = $0000080;
   ICRNL   = $0000100;
   IUCLC   = $0000200;
   IXON    = $0000400;
   IXANY   = $0000800;
   IXOFF   = $0001000;
   IMAXBEL = $0002000;

{c_oflag bits}
   OPOST  = $0000001;
   OLCUC  = $0000002;
   ONLCR  = $0000004;
   OCRNL  = $0000008;
   ONOCR  = $0000010;
   ONLRET = $0000020;
   OFILL  = $0000040;
   OFDEL  = $0000080;
   NLDLY  = $0000100;
     NL0  = $0000000;
     NL1  = $0000100;
   CRDLY  = $0000600;
     CR0  = $0000000;
     CR1  = $0000200;
     CR2  = $0000400;
     CR3  = $0000600;
   TABDLY = $0001800;
     TAB0 = $0000000;
     TAB1 = $0000800;
     TAB2 = $0001000;
     TAB3 = $0001800;
    XTABS = $0001800;
   BSDLY  = $0002000;
     BS0  = $0000000;
     BS1  = $0002000;
   VTDLY  = $0004000;
     VT0  = $0000000;
     VT1  = $0004000;
   FFDLY  = $0008000;
     FF0  = $0000000;
     FF1  = $0008000;

{c_cflag bits}
   CBAUD   = $000100F;
   B0      = $0000000;
   B50     = $0000001;
   B75     = $0000002;
   B110    = $0000003;
   B134    = $0000004;
   B150    = $0000005;
   B200    = $0000006;
   B300    = $0000007;
   B600    = $0000008;
   B1200   = $0000009;
   B1800   = $000000A;
   B2400   = $000000B;
   B4800   = $000000C;
   B9600   = $000000D;
   B19200  = $000000E;
   B38400  = $000000F;
   EXTA    = B19200;
   EXTB    = B38400;
   CSIZE   = $0000030;
     CS5   = $0000000;
     CS6   = $0000010;
     CS7   = $0000020;
     CS8   = $0000030;
   CSTOPB  = $0000040;
   CREAD   = $0000080;
   PARENB  = $0000100;
   PARODD  = $0000200;
   HUPCL   = $0000400;
   CLOCAL  = $0000800;
   CBAUDEX = $0001000;
   B57600  = $0001001;
   B115200 = $0001002;
   B230400 = $0001003;
   B460800 = $0001004;
   CIBAUD  = $100F0000;
   CMSPAR  = $40000000;
   CRTSCTS = $80000000;

{c_lflag bits}
   ISIG    = $0000001;
   ICANON  = $0000002;
   XCASE   = $0000004;
   ECHO    = $0000008;
   ECHOE   = $0000010;
   ECHOK   = $0000020;
   ECHONL  = $0000040;
   NOFLSH  = $0000080;
   TOSTOP  = $0000100;
   ECHOCTL = $0000200;
   ECHOPRT = $0000400;
   ECHOKE  = $0000800;
   FLUSHO  = $0001000;
   PENDIN  = $0004000;
   IEXTEN  = $0008000;

{c_line bits}
   TIOCM_LE   = $001;
   TIOCM_DTR  = $002;
   TIOCM_RTS  = $004;
   TIOCM_ST   = $008;
   TIOCM_SR   = $010;
   TIOCM_CTS  = $020;
   TIOCM_CAR  = $040;
   TIOCM_RNG  = $080;
   TIOCM_DSR  = $100;
   TIOCM_CD   = TIOCM_CAR;
   TIOCM_RI   = TIOCM_RNG;
   TIOCM_OUT1 = $2000;
   TIOCM_OUT2 = $4000;

{TCSetAttr}
   TCSANOW   = 0;
   TCSADRAIN = 1;
   TCSAFLUSH = 2;

{TCFlow}
   TCOOFF = 0;
   TCOON  = 1;
   TCIOFF = 2;
   TCION  = 3;

{TCFlush}
   TCIFLUSH  = 0;
   TCOFLUSH  = 1;
   TCIOFLUSH = 2;

{$endif cpusparc}

{$ifdef cpux86_64}
  TCGETS =              $5401;
  TCSETS =              $5402;
  TCSETSW =             $5403;
  TCSETSF =             $5404;
  TCGETA =              $5405;
  TCSETA =              $5406;
  TCSETAW =             $5407;
  TCSETAF =             $5408;
  TCSBRK =              $5409;
  TCXONC =              $540A;
  TCFLSH =              $540B;
  TIOCEXCL =    $540C;
  TIOCNXCL =    $540D;
  TIOCSCTTY =   $540E;
  TIOCGPGRP =   $540F;
  TIOCSPGRP =   $5410;
  TIOCOUTQ =    $5411;
  TIOCSTI =             $5412;
  TIOCGWINSZ =  $5413;
  TIOCSWINSZ =  $5414;
  TIOCMGET =    $5415;
  TIOCMBIS =    $5416;
  TIOCMBIC =    $5417;
  TIOCMSET =    $5418;
  TIOCGSOFTCAR =        $5419;
  TIOCSSOFTCAR =        $541A;
  FIONREAD =    $541B;
  TIOCINQ =             FIONREAD;
  TIOCLINUX =   $541C;
  TIOCCONS =    $541D;
  TIOCGSERIAL = $541E;
  TIOCSSERIAL = $541F;
  TIOCPKT =             $5420;
  FIONBIO =             $5421;
  TIOCNOTTY =   $5422;
  TIOCSETD =    $5423;
  TIOCGETD =    $5424;
  TCSBRKP =             $5425;
  TIOCSBRK =    $5427;
  TIOCCBRK =    $5428;
  TIOCGSID =    $5429;
  FIONCLEX =    $5450;
  FIOCLEX =             $5451;
  FIOASYNC =    $5452;
  TIOCSERCONFIG =       $5453;
  TIOCSERGWILD =        $5454;
  TIOCSERSWILD =        $5455;
  TIOCGLCKTRMIOS =      $5456;
  TIOCSLCKTRMIOS =      $5457;
  TIOCSERGSTRUCT =      $5458;
  TIOCSERGETLSR =   $5459;
  TIOCSERGETMULTI = $545A;
  TIOCSERSETMULTI = $545B;
  TIOCMIWAIT =  $545C;
  TIOCGICOUNT = $545D;
  TIOCGHAYESESP =   $545E;
  TIOCSHAYESESP =   $545F;
  FIOQSIZE =       $5460;
  TIOCPKT_DATA =                 0;
  TIOCPKT_FLUSHREAD =    1;
  TIOCPKT_FLUSHWRITE =   2;
  TIOCPKT_STOP =                 4;
  TIOCPKT_START =                8;
  TIOCPKT_NOSTOP =              16;
  TIOCPKT_DOSTOP =              32;
  TIOCSER_TEMT =    $01;

{c_cc characters}
  VINTR    = 0;
  VQUIT    = 1;
  VERASE   = 2;
  VKILL    = 3;
  VEOF     = 4;
  VTIME    = 5;
  VMIN     = 6;
  VSWTC    = 7;
  VSTART   = 8;
  VSTOP    = 9;
  VSUSP    = 10;
  VEOL     = 11;
  VREPRINT = 12;
  VDISCARD = 13;
  VWERASE  = 14;
  VLNEXT   = 15;
  VEOL2    = 16;

{c_iflag bits}
   IGNBRK  = $0000001;
   BRKINT  = $0000002;
   IGNPAR  = $0000004;
   PARMRK  = $0000008;
   INPCK   = $0000010;
   ISTRIP  = $0000020;
   INLCR   = $0000040;
   IGNCR   = $0000080;
   ICRNL   = $0000100;
   IUCLC   = $0000200;
   IXON    = $0000400;
   IXANY   = $0000800;
   IXOFF   = $0001000;
   IMAXBEL = $0002000;

{c_oflag bits}
   OPOST  = $0000001;
   OLCUC  = $0000002;
   ONLCR  = $0000004;
   OCRNL  = $0000008;
   ONOCR  = $0000010;
   ONLRET = $0000020;
   OFILL  = $0000040;
   OFDEL  = $0000080;
   NLDLY  = $0000100;
     NL0  = $0000000;
     NL1  = $0000100;
   CRDLY  = $0000600;
     CR0  = $0000000;
     CR1  = $0000200;
     CR2  = $0000400;
     CR3  = $0000600;
   TABDLY = $0001800;
     TAB0 = $0000000;
     TAB1 = $0000800;
     TAB2 = $0001000;
     TAB3 = $0001800;
    XTABS = $0001800;
   BSDLY  = $0002000;
     BS0  = $0000000;
     BS1  = $0002000;
   VTDLY  = $0004000;
     VT0  = $0000000;
     VT1  = $0004000;
   FFDLY  = $0008000;
     FF0  = $0000000;
     FF1  = $0008000;

{c_cflag bits}
   CBAUD   = $000100F;
   B0      = $0000000;
   B50     = $0000001;
   B75     = $0000002;
   B110    = $0000003;
   B134    = $0000004;
   B150    = $0000005;
   B200    = $0000006;
   B300    = $0000007;
   B600    = $0000008;
   B1200   = $0000009;
   B1800   = $000000A;
   B2400   = $000000B;
   B4800   = $000000C;
   B9600   = $000000D;
   B19200  = $000000E;
   B38400  = $000000F;
   EXTA    = B19200;
   EXTB    = B38400;
   CSIZE   = $0000030;
     CS5   = $0000000;
     CS6   = $0000010;
     CS7   = $0000020;
     CS8   = $0000030;
   CSTOPB  = $0000040;
   CREAD   = $0000080;
   PARENB  = $0000100;
   PARODD  = $0000200;
   HUPCL   = $0000400;
   CLOCAL  = $0000800;
   CBAUDEX = $0001000;
   B57600  = $0001001;
   B115200 = $0001002;
   B230400 = $0001003;
   B460800 = $0001004;
   CIBAUD  = $100F0000;
   CMSPAR  = $40000000;
   CRTSCTS = $80000000;

{c_lflag bits}
   ISIG    = $0000001;
   ICANON  = $0000002;
   XCASE   = $0000004;
   ECHO    = $0000008;
   ECHOE   = $0000010;
   ECHOK   = $0000020;
   ECHONL  = $0000040;
   NOFLSH  = $0000080;
   TOSTOP  = $0000100;
   ECHOCTL = $0000200;
   ECHOPRT = $0000400;
   ECHOKE  = $0000800;
   FLUSHO  = $0001000;
   PENDIN  = $0004000;
   IEXTEN  = $0008000;

{c_line bits}
   TIOCM_LE   = $001;
   TIOCM_DTR  = $002;
   TIOCM_RTS  = $004;
   TIOCM_ST   = $008;
   TIOCM_SR   = $010;
   TIOCM_CTS  = $020;
   TIOCM_CAR  = $040;
   TIOCM_RNG  = $080;
   TIOCM_DSR  = $100;
   TIOCM_CD   = TIOCM_CAR;
   TIOCM_RI   = TIOCM_RNG;
   TIOCM_OUT1 = $2000;
   TIOCM_OUT2 = $4000;

{TCSetAttr}
   TCSANOW   = 0;
   TCSADRAIN = 1;
   TCSAFLUSH = 2;

{TCFlow}
   TCOOFF = 0;
   TCOON  = 1;
   TCIOFF = 2;
   TCION  = 3;

{TCFlush}
   TCIFLUSH  = 0;
   TCOFLUSH  = 1;
   TCIOFLUSH = 2;

{$endif cpux86_64}

{$ifdef cpuarm}
  { For Terminal handling }
  TCGETS          = $5401;
  TCSETS          = $5402;
  TCSETSW         = $5403;
  TCSETSF         = $5404;
  TCGETA          = $5405;
  TCSETA          = $5406;
  TCSETAW         = $5407;
  TCSETAF         = $5408;
  TCSBRK          = $5409;
  TCXONC          = $540A;
  TCFLSH          = $540B;
  TIOCEXCL        = $540C;
  TIOCNXCL        = $540D;
  TIOCSCTTY       = $540E;
  TIOCGPGRP       = $540F;
  TIOCSPGRP       = $5410;
  TIOCOUTQ        = $5411;
  TIOCSTI         = $5412;
  TIOCGWINSZ      = $5413;
  TIOCSWINSZ      = $5414;
  TIOCMGET        = $5415;
  TIOCMBIS        = $5416;
  TIOCMBIC        = $5417;
  TIOCMSET        = $5418;
  TIOCGSOFTCAR    = $5419;
  TIOCSSOFTCAR    = $541A;
  FIONREAD        = $541B;
  TIOCINQ         = FIONREAD;
  TIOCLINUX       = $541C;
  TIOCCONS        = $541D;
  TIOCGSERIAL     = $541E;
  TIOCSSERIAL     = $541F;
  TIOCPKT         = $5420;
  FIONBIO         = $5421;
  TIOCNOTTY       = $5422;
  TIOCSETD        = $5423;
  TIOCGETD        = $5424;
  TCSBRKP         = $5425;
  TIOCTTYGSTRUCT  = $5426;
  FIONCLEX        = $5450;
  FIOCLEX         = $5451;
  FIOASYNC        = $5452;
  TIOCSERCONFIG   = $5453;
  TIOCSERGWILD    = $5454;
  TIOCSERSWILD    = $5455;
  TIOCGLCKTRMIOS  = $5456;
  TIOCSLCKTRMIOS  = $5457;
  TIOCSERGSTRUCT  = $5458;
  TIOCSERGETLSR   = $5459;
  TIOCSERGETMULTI = $545A;
  TIOCSERSETMULTI = $545B;

  TIOCMIWAIT      = $545C;
  TIOCGICOUNT     = $545D;
  FIOQSIZE        = $545E;

  TIOCPKT_DATA       = 0;
  TIOCPKT_FLUSHREAD  = 1;
  TIOCPKT_FLUSHWRITE = 2;
  TIOCPKT_STOP       = 4;
  TIOCPKT_START      = 8;
  TIOCPKT_NOSTOP     = 16;
  TIOCPKT_DOSTOP     = 32;

{c_cc characters}
  VINTR    = 0;
  VQUIT    = 1;
  VERASE   = 2;
  VKILL    = 3;
  VEOF     = 4;
  VTIME    = 5;
  VMIN     = 6;
  VSWTC    = 7;
  VSTART   = 8;
  VSTOP    = 9;
  VSUSP    = 10;
  VEOL     = 11;
  VREPRINT = 12;
  VDISCARD = 13;
  VWERASE  = 14;
  VLNEXT   = 15;
  VEOL2    = 16;

{c_iflag bits}
   IGNBRK  = $0000001;
   BRKINT  = $0000002;
   IGNPAR  = $0000004;
   PARMRK  = $0000008;
   INPCK   = $0000010;
   ISTRIP  = $0000020;
   INLCR   = $0000040;
   IGNCR   = $0000080;
   ICRNL   = $0000100;
   IUCLC   = $0000200;
   IXON    = $0000400;
   IXANY   = $0000800;
   IXOFF   = $0001000;
   IMAXBEL = $0002000;

{c_oflag bits}
   OPOST  = $0000001;
   OLCUC  = $0000002;
   ONLCR  = $0000004;
   OCRNL  = $0000008;
   ONOCR  = $0000010;
   ONLRET = $0000020;
   OFILL  = $0000040;
   OFDEL  = $0000080;
   NLDLY  = $0000100;
     NL0  = $0000000;
     NL1  = $0000100;
   CRDLY  = $0000600;
     CR0  = $0000000;
     CR1  = $0000200;
     CR2  = $0000400;
     CR3  = $0000600;
   TABDLY = $0001800;
     TAB0 = $0000000;
     TAB1 = $0000800;
     TAB2 = $0001000;
     TAB3 = $0001800;
    XTABS = $0001800;
   BSDLY  = $0002000;
     BS0  = $0000000;
     BS1  = $0002000;
   VTDLY  = $0004000;
     VT0  = $0000000;
     VT1  = $0004000;
   FFDLY  = $0008000;
     FF0  = $0000000;
     FF1  = $0008000;

{c_cflag bits}
   CBAUD   = $000100F;
   B0      = $0000000;
   B50     = $0000001;
   B75     = $0000002;
   B110    = $0000003;
   B134    = $0000004;
   B150    = $0000005;
   B200    = $0000006;
   B300    = $0000007;
   B600    = $0000008;
   B1200   = $0000009;
   B1800   = $000000A;
   B2400   = $000000B;
   B4800   = $000000C;
   B9600   = $000000D;
   B19200  = $000000E;
   B38400  = $000000F;
   EXTA    = B19200;
   EXTB    = B38400;
   CSIZE   = $0000030;
     CS5   = $0000000;
     CS6   = $0000010;
     CS7   = $0000020;
     CS8   = $0000030;
   CSTOPB  = $0000040;
   CREAD   = $0000080;
   PARENB  = $0000100;
   PARODD  = $0000200;
   HUPCL   = $0000400;
   CLOCAL  = $0000800;
   CBAUDEX = $0001000;
   B57600  = $0001001;
   B115200 = $0001002;
   B230400 = $0001003;
   B460800 = $0001004;
   B500000 = $0001005;
   B576000 = $0001006;
   B921600 = $0001007;
   B1000000= $0001008;
   B1152000= $0001009;
   B1500000= $000100A;
   B2000000= $000100B;
   B2500000= $000100C;
   B3000000= $000100D;
   B3500000= $000100E;
   B4000000= $000100F;
   __MAX_BAUD=  B4000000;
   
   CIBAUD  = $100F0000;
   CMSPAR  = $40000000;
   CRTSCTS = $80000000;

{c_lflag bits}
   ISIG    = $0000001;
   ICANON  = $0000002;
   XCASE   = $0000004;
   ECHO    = $0000008;
   ECHOE   = $0000010;
   ECHOK   = $0000020;
   ECHONL  = $0000040;
   NOFLSH  = $0000080;
   TOSTOP  = $0000100;
   ECHOCTL = $0000200;
   ECHOPRT = $0000400;
   ECHOKE  = $0000800;
   FLUSHO  = $0001000;
   PENDIN  = $0004000;
   IEXTEN  = $0008000;

{c_line bits}
   TIOCM_LE   = $001;
   TIOCM_DTR  = $002;
   TIOCM_RTS  = $004;
   TIOCM_ST   = $008;
   TIOCM_SR   = $010;
   TIOCM_CTS  = $020;
   TIOCM_CAR  = $040;
   TIOCM_RNG  = $080;
   TIOCM_DSR  = $100;
   TIOCM_CD   = TIOCM_CAR;
   TIOCM_RI   = TIOCM_RNG;
   TIOCM_OUT1 = $2000;
   TIOCM_OUT2 = $4000;

{TCSetAttr}
   TCSANOW   = 0;
   TCSADRAIN = 1;
   TCSAFLUSH = 2;

{TCFlow}
   TCOOFF = 0;
   TCOON  = 1;
   TCIOFF = 2;
   TCION  = 3;

{TCFlush}
   TCIFLUSH  = 0;
   TCOFLUSH  = 1;
   TCIOFLUSH = 2;

{$endif cpuarm}

Type
  winsize = packed record
    ws_row,
    ws_col,
    ws_xpixel,
    ws_ypixel : word;
  end;
  TWinSize=winsize;

{$PACKRECORDS C}
  Termios = record
    c_iflag,
    c_oflag,
    c_cflag,
    c_lflag  : cardinal;
    c_line   : char;
    c_cc     : array[0..NCCS-1] of byte;
    c_ispeed,
    c_ospeed : cardinal;
  end;
  TTermios=Termios;
{$PACKRECORDS Default}

