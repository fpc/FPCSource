Program Socket_Comms_Test;
{***************************************************************************
 TCP/IP Streaming Socket Server Example program.
 NumberofConnections is the number of consecutive active connections I will
 allow. I have not hit this limit yet.
 This defaults to port 5000
 The MOST Important thing to look at when doing socket calls of any kind
 is the byte order in the structure. Got caught big time with this in reference
 to the port number.
 This program runs as-is, just telnet localhost 5000 to connect to it.
 No warranty at all, I will not be responsible if this sets fire to your dog!

 This is exactly as I use it, I have just put the references to my db unit
 in curly brackets. It just echoes back what you type on a line by line basis
 Run it in X or on a seperate virtual console to the one you are telneting from
 as it prints a LOT of info to the console about what it is doing and such.
 I'm not a pretty coder at all, so please, no complaints about the lack of
 comments or coding style, unless they are very contructive ;p)

 type 'quit', minus the quotes and in lower case on the console to exit the
 program. The only problem I can see with this, is if you exit it, it does
 not shut down the connections to the telnet sessions cleanly, and therefore
 it leaves port 5000 in a TIME_WAIT state for a couple of minutes. This prevents
 you re-running the program immediately as it will not bind to the port.
 (Bind Error 98).
 If you know how to fix this, please let me know and I'll update the code.
 If you exit all your telnet sessions before shutting the server down, it
 works fine.

 Hope some of you find this usefull. I wrote it, purely because there is a
 big lack of examples of linux port use in FPC. And I know NO C, therefore
 the examples on the net meant nothing to me.

 All I ask is :-
 If you like it, use it or want to change it, please drop me an E-mail.

 Regards Brad Campbell
 bcampbel@omen.net.au
  ***************************************************************************}

{$mode ObjFPC}

Uses Linux, Sockets, Sysutils{, dbu};

Const
 NumberofConnections = 5;

Type ConnectionType = Record
                       IP : Cardinal;
                       Port : Word;
                       Handle : Integer;
                       Connected : Boolean;
                       IdleTimer : Integer;
                      End;


Var
 Connection : Array[1..NumberofConnections] Of ConnectionType;
 FDS        : FDSet;
 S          : LongInt;
 PortNumber : Word;
 GreatestHandle : Integer;
 Quit       : Boolean;
 Command    : String;

Procedure ZeroConnection;

Var Loop : Integer;
Begin
 For Loop := 1 To NumberOfConnections Do
  Connection[Loop].Connected := False;
End;


Function FreeConnections : Integer;

Var Loop : Integer;

Begin
 Result := 0;
 For Loop := 1 To NumberOfConnections Do
  If Not Connection[Loop].Connected Then Inc(Result);
 FreeConnections := Result;
End;

Function GetFreeConnection : Integer;

Var Loop : Integer;
   Found : Boolean;
Begin
 Result := 0;
 Loop := 1;
 Found := False;
 While (Loop < NumberOfConnections + 1) and (Not Found) Do
 Begin
  If Not Connection[Loop].Connected Then
   Begin
    Found := True;
    Result := Loop;
   End;
  Inc(Loop);
  GetFreeConnection := Result;
 End;
End;

Procedure PError(S : String);
Begin
 Writeln(S,SocketError);
 Halt(100);
End;

Procedure PDebug(S : String);
Begin
 Writeln(S);
End;

Procedure PDebugNOLF(S: String);
Begin
 Write(S);
End;

Function SockAddrtoString(InAddr : LongWord) : String;

Var
 P1,P2,P3,P4 : Byte;
 S1,S2,S3,S4 : String;

Begin
 P1 := (InAddr And $ff000000) Shr 24;
 P2 := (InAddr And $ff0000) Shr 16;
 P3 := (InAddr And $ff00) Shr 8;
 P4 := InAddr And $FF;
 Str(P1,S1);
 Str(P2,S2);
 Str(P3,S3);
 Str(P4,S4);
 SockAddrtoString := S4+'.'+S3+'.'+S2+'.'+S1;
End;

Procedure WelcomeHandle(Handle, ConnNum : Integer);

Var Buffer : String;
 Sent : Integer;
Begin
 Buffer := 'Welcome to Brads Server 1.0'+#10+#13+'You Are Connection '+
           InttoStr(ConnNum)+' Of '+InttoStr(NumberofConnections)+
           ', With '+InttoStr(FreeConnections)+' Connections Free'#13+#10;
 Sent := Send(Handle,Buffer[1],Length(Buffer),0);
 If Sent <> Length(Buffer) Then
  PDebug('Wanted to Send : ' +InttoStr(Length(Buffer))+' Sent Only : '
          +InttoStr(Sent)+' to Connection : '+InttoStr(ConnNum));
End;

Procedure AcceptNewConnection;

Var ConnectionNumber : Integer;
    Handle           : LongInt;
    FromAddrSize     : LongInt;
    FromAddr         : TInetSockAddr;

Begin
 FromAddrSize := Sizeof(FromAddr);
 If FreeConnections > 0 Then
  Begin
   ConnectionNumber := GetFreeConnection;
   PDebug('Accepting New Connection Number : '+InttoStr(ConnectionNumber));
   Handle := Accept(S,FromAddr,FromAddrSize);
   If Handle < 0 Then PError('Accept Error!!');
   PDebug('Accepted From : '+SockAddrtoString(FromAddr.Addr)+' Port : '
   +Inttostr(Swap(FromAddr.Port)));
   Connection[ConnectionNumber].Handle := Handle;
   Connection[ConnectionNumber].IP := FromAddr.Addr;
   Connection[ConnectionNumber].Port := FromAddr.Port;
   Connection[ConnectionNumber].Connected := True;
   Connection[ConnectionNumber].IdleTimer := 0;
   WelcomeHandle(Handle,ConnectionNumber);
  End;
End;

Procedure SetUpSocket;

Var
 SockAddr : TInetSockAddr;
 yes  : longint;

Begin
 SockAddr.Family := AF_INET;
 SockAddr.Port := Swap(PortNumber);
 SockAddr.Addr := 0;
 S := Socket(AF_INET,SOCK_STREAM,0);
 If SocketError <> 0 Then PError('Socket Error : ');
 yes := $1010101;  {Copied this from existing code. Value is empiric,
                    but works. (yes=true<>0) }
 SetSocketOptions(s, SOL_SOCKET, SO_REUSEADDR,yes,sizeof(yes));
 If Not Bind(S,SockAddr,SizeOf(SockAddr)) Then PError('Bind Error : ');
 If Not Listen(S,5) Then PError('Listen Error : ');
End;

Procedure LoadConnectedFDS;

Var Loop : Integer;
Begin
 For Loop := 1 To NumberOfConnections Do
  If Connection[Loop].Connected Then
   Begin
    FD_SET(Connection[Loop].Handle,FDS);
    If Connection[Loop].Handle > GreatestHandle Then
     GreatestHandle := Connection[Loop].Handle;
   End;
End;

Procedure ServiceHandle(Handle, ConnectionNum : Integer);

Var Buffer : String;
 Sent, BufferLength : Integer;

Begin
 Writeln('Service Handle : ',Handle);
 BufferLength := Recv(Handle,Buffer[1],200,0);
 Setlength(Buffer,BufferLength);
 If SocketError <> 0 Then
  PDebug('Reciceved Socket Error : '
  +InttoStr(SocketError)+' OnHandle '+InttoStr(Handle));

 If BufferLength = 0 Then  {It's EOF, Socket has been closed}
  Begin
   PDebug('Socket Handle '+InttoStr(Handle)+' Closed');
   Connection[ConnectionNum].Connected := False;
   Shutdown(Handle,2);
   fdClose(Handle);
  End

 Else
  Begin
   PDebug(InttoStr(BufferLength)+' Bytes Recieved');
  {Buffer := Db_Query(Buffer);}
   Sent := Send(Handle,Buffer[1],Length(Buffer),0);
   If Sent <> Bufferlength Then
    PDebug('Wanted to Send : '+InttoStr(Length(Buffer))+' Only Sent : '+InttoStr(Sent));
  End;
End;

Procedure ServiceSockets;

Var Loop : Integer;

Begin
 For Loop := 1 To NumberOfConnections Do
  If Connection[Loop].Connected Then
   If FD_ISSET(Connection[Loop].Handle,FDS) Then
    ServiceHandle(Connection[Loop].Handle,Loop);

 If FD_ISSET(S,FDS) Then AcceptNewConnection;
End;

Procedure CloseAllOpen;

Var Loop : Integer;
Begin
 For Loop := 1 To NumberOfConnections Do
  Begin
   If Connection[Loop].Connected = True Then
    Begin
     Shutdown(Connection[Loop].Handle,1);
{ fdClose(Connection[Loop].Handle);}
 {Connection[Loop].Connected := False;}
    End;
  End;
End;

Begin
 ZeroConnection;  {Clear Connected Array}
 Quit := False;
 PortNumber := 5000;
 SetupSocket;
 Repeat
  FD_ZERO(FDS);
  FD_SET(S,FDS); { Socket Looking for new connections }
  FD_SET(1,FDS); { Terminal }
  GreatestHandle := S;
  LoadConnectedFDS;
  If Select(GreatestHandle+1,@FDS,Nil,Nil,1000) > 0 Then
   Begin
    ServiceSockets;
    If FD_ISSET(1,FDS) Then
     Begin
      PDebug('Reading Console');
      Readln(Command);
      If Command='quit' Then quit := True;
{       Else Writeln(DB_Query(Command));}
      Command := '';
     End;
   End;
{DB_Tic;} {Updates Database Internals, Needs at Least 1 run per second}
 Until Quit = True;
 CloseAllOpen;

End.