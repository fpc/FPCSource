program Terminal_test;
{******************************************************************************
 * Really really budget attempt at Serial IO with Linux and FPC.
 * My first FPC program. Re-built and refined on 12/6/99
 * Written under X windows with nedit 5.0.2 (Not a bad editor)
 * This SHOULD work without including the CRT Unit, However it has problems
 * With reading from the keyboard unless the CRT unit is included ?!?
 *
 * Designed to talk to an RS485 Buss, using RTS as the Tx/Rx Select Pin
 * No Copyrights or warrantys.
 * Let me know if it's of some use to you.
 * Brad Campbell (bcampbel@omen.net.au)
 ******************************************************************************}
uses oldlinux, Crt;

Const DTR : Cardinal = TIOCM_DTR;
Const RTS : Cardinal = TIOCM_RTS;

Var     FD              : Longint;
        InChr           : String[1];
        InStr           : String[80];
        Quit            : Boolean;
        InLen, Loop     : Integer;
        tios            : Termios;
        fds             : FDSet;


Procedure DumpFlags;
begin
IOCtl(FD,TIOCMGET,@tios);
Writeln('Input   Flags    : $',hexstr(tios.c_iflag,8));
Writeln('Output  Flags    : $',hexstr(tios.c_oflag,8));
Writeln('Local   Flags    : $',hexstr(tios.c_lflag,8));
Writeln('Control Flags    : $',hexstr(tios.c_cflag,8));
End;


Procedure RS485RX;
Begin
IOCtl(FD,TIOCMBIS,@RTS);
End;

Procedure RS485TX;
Begin
IOCtl(FD,TIOCMBIC,@RTS);
End;


Procedure DtrOn;
Begin
IOCtl(FD,TIOCMBIS,@DTR);
End;

Procedure DtrOff;
Begin
IOCtl(FD,TIOCMBIC,@DTR);
End;

Procedure SendToRemote(OutString : String);
Begin
Rs485TX;        {Switch Buss to Transmit}
if fdWrite(FD,OutString[1],Length(OutString)) <> Length(OutString) then
        Writeln('Write Error');
{Write(OutString);} {Uncomment for Local Echo}
TCDrain(FD);    {Block Program until all data sent out port has left UART}
RS485RX;        {Switch Buss back to Recieve}
End;


{ Not limited to baud selection I have here, it's just all I use }
Procedure SetBaudrate;
Var     NewBaud : LongInt;
Begin
Writeln;
Writeln('New Baud Rate (300,1200,2400,4800, 9600,19200,38400) ? ');
Readln(NewBaud);
Case NewBaud of
   300 : NewBaud := B300;
  1200 : NewBaud := B1200;
  2400 : NewBaud := B2400;
  4800 : NewBaud := B4800;
  9600 : NewBaud := B9600;
 19200 : NewBaud := B19200;
 38400 : NewBaud := B38400;
Else
        Begin
        Writeln('Invalid Baud Rate. Baud not Changed');
        Writeln;
        NewBaud := 0;
        End;
End;

{ Sets Baud Rate Here }
If NewBaud <> 0 then
        Begin
                IOCtl(FD,TCGETS,@tios);         {Get IOCTL TermIOS Settings}
                CFSetOSpeed(tios,NewBaud);      {Set Relevant Bits}
                IOCtl(FD,TCSETS,@tios);         {Put them back with IOCTL}
                Writeln('New Baudrate ',HexStr(NewBaud,2),' Set');
                {This line just prints what the constant equates to for
                 Information Only}
        End;
End;


Begin
Quit := False;
Writeln('Brad''s Dumb Terminal Test prog v0.2');
Writeln('Ctrl-C to exit program');
Writeln('Ctrl-D to set Baud Rate');
Writeln('Uses /dev/ttyS0 (Com 1)');
Writeln;

FD:=fdOpen('/dev/ttyS0',Open_RdWr or Open_NonBlock or Open_Excl);
{Open Port Read/Write, Not Blocking and Exclusive}

if FD > 0 then Begin

Writeln('Port Open');

FLock(FD,LOCK_EX);
{Attempt to Lock the port, I'm not sure this is strictly nessecary}

Writeln('Port Locked');

{Set Comms Parms, 9600 Baud, 8 Data Bits, Reciever Enabled,
 Modem Control Lines Ignored}
{Read man 3 termios for More options}

IOCtl(FD,TCGETS,@tios);
tios.c_cflag := B9600 Or CS8 Or CREAD Or CLOCAL;
tios.c_lflag := 0;
tios.c_oflag := 0;
tios.c_iflag := 0;
IOCtl(FD,TCSETS,@tios);

DumpFlags;      {This is for information only and dumps the contents of
                 the Termios registers}

Repeat
FD_Zero (FDS);          {Clear File Descriptors Array}
FD_Set (0,FDS);         {Input from Keyboard}
FD_SET (FD,FDS);        {Input from Serial Port}

Select(FD+1,@FDS,nil,nil,nil);  {Will Wait for input from above}

If FD_ISSET(0,FDS) then         {Has there been a key pressed ?}
        If fdRead(0,InChr[1],80) <> 0 then
                Begin
                if InChr[1] = Chr(3) then Quit := True;
                if InChr[1] = Chr(4) then SetBaudRate;
                SendToRemote(InChr[1]);
                End;

If FD_ISSET(FD,FDS) then        {Have we data waiting in UART ? }
        Begin
                InLen := fdRead(FD,InStr[1],80);
                If InLen > 0 then
                For Loop := 1 to Inlen do
                Write(InStr[Loop]);
        End;
Until Quit = True;      {Were Outa Here}
FLock(FD,LOCK_UN);      {Unlock Port}
fdClose(FD);            {Close Port}
End
Else Writeln('Open Port Error');        {We failed to Open/Lock the UART}
End.
