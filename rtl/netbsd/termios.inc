{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Termios header for FreeBSD

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

CONST

{
 * Special Control Characters
 *
 * Index into c_cc[] character array.
 *
 *      Name         Subscript  Enabled by
 }
 VEOF           =0;
 VEOL           =1;
 VEOL2          =2;
 VERASE         =3;
 VWERASE        =4;
 VKILL          =5;
 VREPRINT       =6;
{               =7;        spare 1 }
 VINTR          =8;
 VQUIT          =9;
 VSUSP          =10;
 VDSUSP         =11;
 VSTART         =12;
 VSTOP          =13;
 VLNEXT         =14;
 VDISCARD       =15;
 VMIN           =16;
 VTIME          =17;
 VSTATUS        =18;
{               =19        spare 2 }
 NCCS           =20;

Type
  winsize = packed record
    ws_row,
    ws_col,
    ws_xpixel,
    ws_ypixel : word;
  end;
  TWinSize=winsize;


type
  Termios = packed record
    c_iflag,
    c_oflag,
    c_cflag,
    c_lflag  : longint;
    c_line   : char;
    c_cc     : array[0..NCCS-1] of byte;
   {$IFDEF BSD}
    c_ispeed,
    c_ospeed : longint;
   {$endif}
  end;
  TTermios=Termios;

CONST


 POSIX_VDISABLE=Chr($ff);
{

#define CCEQ(val, c)    ((c) == (val) ? (val) != _POSIX_VDISABLE : 0)
}

{ * Input flags - software input processing}

        IGNBRK          =       $1;     { ignore BREAK condition }
        BRKINT          =       $2;     { map BREAK to SIGINTR }
        IGNPAR          =       $4;     { ignore (discard) parity errors }
        PARMRK          =       $8;     { mark parity and framing errors }
        INPCK           =      $10;     { enable checking of parity errors }
        ISTRIP          =      $20;     { strip 8th bit off chars }
        INLCR           =      $40;     { map NL into CR }
        IGNCR           =      $80;     { ignore CR }
        ICRNL           =     $100;     { map CR to NL (ala CRMOD) }
        IXON            =     $200;     { enable output flow control }
        IXOFF           =     $400;     { enable input flow control }
        IXANY           =     $800;     { any char will restart after stop }
        IMAXBEL         =    $2000;     { ring bell on input queue full }

{
 * Output flags - software output processing
}
        OPOST           =       $1;     { enable following output processing }
        ONLCR           =       $2;     { map NL to CR-NL (ala CRMOD) }
        OXTABS          =       $4;     { expand tabs to spaces }
        ONOEOT          =       $8;     { discard EOT's (^D) on output) }

{
 * Control flags - hardware control of terminal
}
        CIGNORE         =       $1;     { ignore control flags }
        CSIZE           =     $300;     { character size mask }
        CS5             =       $0;         { 5 bits (pseudo) }
        CS6             =     $100;         { 6 bits }
        CS7             =     $200;         { 7 bits }
        CS8             =     $300;         { 8 bits }
        CSTOPB          =     $400;     { send 2 stop bits }
        CREAD           =     $800;     { enable receiver }
        PARENB          =    $1000;     { parity enable }
        PARODD          =    $2000;     { odd parity, else even }
        HUPCL           =    $4000;     { hang up on last close }
        CLOCAL          =    $8000;     { ignore modem status lines }
        CCTS_OFLOW      =   $10000;     { CTS flow control of output }
        CRTS_IFLOW      =   $20000;     { RTS flow control of input }
        CRTSCTS         =   (CCTS_OFLOW or CRTS_IFLOW);
        CDTR_IFLOW      =   $40000;     { DTR flow control of input }
        CDSR_OFLOW      =   $80000;     { DSR flow control of output }
        CCAR_OFLOW      =  $100000;     { DCD flow control of output }
        MDMBUF          =  $100000;     { old name for CCAR_OFLOW }

{
 * "Local" flags - dumping ground for other state
 *
 * Warning: some flags in this structure begin with
 * the letter "I" and look like they belong in the
 * input flag.
 }

        ECHOKE          =       $1;     { visual erase for line kill }
        ECHOE           =       $2;     { visually erase chars }
        ECHOK           =       $4;     { echo NL after line kill }
        ECHO            =       $8;     { enable echoing }
        ECHONL          =      $10;     { echo NL even if ECHO is off }
        ECHOPRT         =      $20;     { visual erase mode for hardcopy }
        ECHOCTL         =      $40;     { echo control chars as ^(Char) }
        ISIG            =      $80;     { enable signals INTR, QUIT, [D]SUSP }
        ICANON          =     $100;     { canonicalize input lines }
        ALTWERASE       =     $200;     { use alternate WERASE algorithm }
        IEXTEN          =     $400;     { enable DISCARD and LNEXT }
        EXTPROC         =     $800;      { external processing }
        TOSTOP          =  $400000;     { stop background jobs from output }
        FLUSHO          =  $800000;     { output being flushed (state) }
        NOKERNINFO      = $2000000;     { no kernel output from VSTATUS }
        PENDIN          =$20000000;     { XXX retype pending input (state) }
        NOFLSH          =$80000000;     { don't flush after interrupt }



{
 * Commands passed to tcsetattr() for setting the termios structure.
}

CONST

        TCSANOW         =0;             { make change immediate }
        TCSADRAIN       =1;             { drain output, then change }
        TCSAFLUSH       =2;             { drain output, flush input }
        TCSASOFT        =$10;           { flag - don't alter h.w. state }

{
 * Standard speeds
}
        B0      =     0;
        B50     =    50;
        B75     =    75;
        B110    =   110;
        B134    =   134;
        B150    =   150;
        B200    =   200;
        B300    =   300;
        B600    =   600;
        B1200   =  1200;
        B1800   =  1800;
        B2400   =  2400;
        B4800   =  4800;
        B9600   =  9600;
        B19200  = 19200;
        B38400  = 38400;
        B7200   =  7200;
        B14400  = 14400;
        B28800  = 28800;
        B57600  = 57600;
        B76800  = 76800;
        B115200 =115200;
        B230400 =230400;
        EXTA    = 19200;
        EXTB    = 38400;

        TCIFLUSH        =1;
        TCOFLUSH        =2;
        TCIOFLUSH       =3;
        TCOOFF          =1;
        TCOON           =2;
        TCIOFF          =3;
        TCION           =4;

{
#include <sys/cdefs.h>

__BEGIN_DECLS
speed_t cfgetispeed __P((const struct termios *));
speed_t cfgetospeed __P((const struct termios *));
int     cfsetispeed __P((struct termios *, speed_t));
int     cfsetospeed __P((struct termios *, speed_t));
int     tcgetattr __P((int, struct termios *));
int     tcsetattr __P((int, int, const struct termios *));
int     tcdrain __P((int));
int     tcflow __P((int, int));
int     tcflush __P((int, int));
int     tcsendbreak __P((int, int));

#ifndef _POSIX_SOURCE
void    cfmakeraw __P((struct termios *));
int     cfsetspeed __P((struct termios *, speed_t));
#endif { !_POSIX_SOURCE }
__END_DECLS

#endif { !_KERNEL }



struct winsize {
        unsigned short  ws_row;         { rows, in characters }
        unsigned short  ws_col;         { columns, in characters }
        unsigned short  ws_xpixel;      { horizontal size, pixels }
        unsigned short  ws_ypixel;      { vertical size, pixels }
};

}
       IOCTLREAD        = $40000000;
       IOCTLWRITE       = $80000000;
       IOCTLVOID        = $20000000;

        TIOCMODG        = IOCTLREAD+$47400+ 3;  { get modem control state }
        TIOCMODS        = IOCTLWRITE+$47400+ 4; { set modem control state }
                TIOCM_LE        =$0001;         { line enable }
                TIOCM_DTR       =$0002;         { data terminal ready }
                TIOCM_RTS       =$0004;         { request to send }
                TIOCM_ST        =$0010;         { secondary transmit }
                TIOCM_SR        =$0020;         { secondary receive }
                TIOCM_CTS       =$0040;         { clear to send }
                TIOCM_CAR       =$0100;         { carrier detect }
                TIOCM_CD        =TIOCM_CAR;
                TIOCM_RNG       =$0200;         { ring }
                TIOCM_RI        =TIOCM_RNG;
                TIOCM_DSR       =$0400;         { data set ready }
                                                { 8-10 compat }
        TIOCEXCL         =IOCTLVOID+$7400+ 13;          { set exclusive use of tty }
        TIOCNXCL         =IOCTLVOID+$7400+ 14;          { reset exclusive use of tty }
                                                { 15 unused }
        TIOCFLUSH        =IOCTLWRITE+$47400+ 16;        { flush buffers }
                                                { 17-18 compat }
        TIOCGETA         =IOCTLREAD+$2C7400+ 19; { get termios struct }
        TIOCSETA         =IOCTLWRITE+$2C7400+ 20; { set termios struct }
        TIOCSETAW        =IOCTLWRITE+$2C7400+ 21; { drain output, set }
        TIOCSETAF        =IOCTLWRITE+$2C7400+ 22; { drn out, fls in, set }
        TIOCGETD         =IOCTLREAD+$47400+ 26; { get line discipline }
        TIOCSETD         =IOCTLWRITE+$47400+ 27;        { set line discipline }
                                                { 127-124 compat }
        TIOCSBRK         =IOCTLVOID+$7400+ 123;         { set break bit }
        TIOCCBRK         =IOCTLVOID+$7400+ 122;         { clear break bit }
        TIOCSDTR         =IOCTLVOID+$7400+ 121;         { set data terminal ready }
        TIOCCDTR         =IOCTLVOID+$7400+ 120;         { clear data terminal ready }
        TIOCGPGRP        =IOCTLREAD+$47400+ 119;        { get pgrp of tty }
        TIOCSPGRP        =IOCTLWRITE+$47400+ 118;       { set pgrp of tty }
                                                { 117-116 compat }
        TIOCOUTQ         =IOCTLREAD+$47400+ 115;        { output queue size }
        TIOCSTI          =IOCTLWRITE+$17400+ 114;       { simulate terminal input }
        TIOCNOTTY        =IOCTLVOID+$7400+ 113;         { void tty association }
        TIOCPKT          =IOCTLWRITE+$47400+ 112;       { pty: set/clear packet mode }
                TIOCPKT_DATA            =$00;   { data packet }
                TIOCPKT_FLUSHREAD       =$01;   { flush packet }
                TIOCPKT_FLUSHWRITE      =$02;   { flush packet }
                TIOCPKT_STOP            =$04;   { stop output }
                TIOCPKT_START           =$08;   { start output }
                TIOCPKT_NOSTOP          =$10;   { no more ^S, ^Q }
                TIOCPKT_DOSTOP          =$20;   { now do ^S ^Q }
                TIOCPKT_IOCTL           =$40;   { state change of pty driver }
        TIOCSTOP         =IOCTLVOID+$7400+ 111;         { stop output, like ^S }
        TIOCSTART        =IOCTLVOID+$7400+ 110;         { start output, like ^Q }
        TIOCMSET         =IOCTLWRITE+$47400+ 109;       { set all modem bits }
        TIOCMBIS         =IOCTLWRITE+$47400+ 108;       { bis modem bits }
        TIOCMBIC         =IOCTLWRITE+$47400+ 107;       { bic modem bits }
        TIOCMGET         =IOCTLREAD+$47400+ 106;        { get all modem bits }
        TIOCREMOTE       =IOCTLWRITE+$47400+ 105;       { remote input editing }
        TIOCGWINSZ       =IOCTLREAD+$87400+ 104;        { get window size }
        TIOCSWINSZ       =IOCTLWRITE+$87400+ 103;       { set window size }
        TIOCUCNTL        =IOCTLWRITE+$47400+ 102;       { pty: set/clr usr cntl mode }
        TIOCSTAT         =IOCTLVOID+$7400+ 101;         { simulate ^T status message }
  //                       UIOCCMD(n)   _IO('u', n)     { usr cntl op "n" }
        TIOCCONS         =IOCTLWRITE+$47400+ 98;        { become virtual console }
        TIOCSCTTY        =IOCTLVOID+$7400+ 97;          { become controlling tty }
        TIOCEXT          =IOCTLWRITE+$47400+ 96;        { pty: external processing }
        TIOCSIG          =IOCTLVOID+$7400+ 95;          { pty: generate signal }
        TIOCDRAIN        =IOCTLVOID+$7400+ 94;          { wait till output drained }
        TIOCMSDTRWAIT    =IOCTLWRITE+$47400+ 91;        { modem: set wait on close }
        TIOCMGDTRWAIT    =IOCTLREAD+$47400+ 90; { modem: get wait on close }
        TIOCTIMESTAMP    =IOCTLREAD+$87400+ 89;         { enable/get timestamp
                                                 * of last input event }
        TIOCDCDTIMESTAMP =IOCTLREAD+$87400+ 88; { enable/get timestamp
                                                 * of last DCd rise }
        TIOCSDRAINWAIT   =IOCTLWRITE+$47400+ 87;        { set ttywait timeout }
        TIOCGDRAINWAIT   =IOCTLREAD+$47400+ 86; { get ttywait timeout }

        TTYDISC          =0;            { termios tty line discipline }
        SLIPDISC         =4;            { serial IP discipline }
        PPPDISC          =5;            { PPP discipline }
        NETGRAPHDISC     =6;            { Netgraph tty node discipline }


{
 * Defaults on "first" open.
 }
        TTYDEF_IFLAG     =(BRKINT       or ICRNL        or IMAXBEL or IXON or IXANY);
       TTYDEF_OFLAG      =(OPOST or ONLCR);
       TTYDEF_LFLAG      =(ECHO or ICANON or ISIG or IEXTEN or ECHOE or ECHOKE or ECHOCTL);
        TTYDEF_CFLAG     =(CREAD or CS8 or HUPCL);
       TTYDEF_SPEED      =(B9600);



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
        POSIX_VDISABLE, CINTR,  CQUIT,  CSUSP,  CDSUSP, CSTART, CSTOP,  CLNEXT,
        CDISCARD, CMIN, CTIME,  CSTATUS, POSIX_VDISABLE);

