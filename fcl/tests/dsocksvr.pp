Program server;
{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Dual server program. This will listen on port 4100 till 
  a client connects if '-i' is on the command-line. 
  Otherwise it will open a unix socket. You can connect by 
  running the 'sockcli' or 'dsockcli' programs in another 
  terminal.
} 

uses ssockets;

const
  ThePort=4100;
  TheSocket = 'ServerSoc';
  
Type
  TServerApp = Class(TObject)
  Private 
    FServer : TSocketServer;
  Public
    Constructor Create(Port : longint);
    Constructor Create(Socket : String);
    Destructor Destroy;override;
    Procedure OnConnect (Sender : TObject; Data : TSocketStream);
    Procedure Run;
  end;
    
Constructor TServerApp.Create(Port : longint);

begin
  FServer:=TINetServer.Create(Port);  
  FServer.OnConnect:=@OnConnect;
end;

Constructor TServerApp.Create(Socket : String);

begin
  FServer:=TUnixServer.Create(Socket);  
  FServer.OnConnect:=@OnConnect;
end;

Destructor TServerApp.Destroy;

begin
  FServer.Free;
end;

Procedure TServerApp.OnConnect (Sender : TObject; Data : TSocketStream);


Var Buf : ShortString;
    Count : longint;
    
begin
  Repeat 
    Count:=Data.Read(Buf[1],255);
    SetLength(Buf,Count);
    Write('Server got : ',Buf);
  Until (Count=0) or (Pos('QUIT',Buf)<>0);    
  Data.Free;
  FServer.StopAccepting;
end;

Procedure TServerApp.Run;

begin
  Write ('Listening on ');
  if FServer is TUnixServer Then
    Writeln ('socket : ',(FServer as TUnixServer).Filename)
  else If FServer is TINetServer Then
    Writeln ('port : ',(FServer as TInetServer).port);
  FServer.StartAccepting;
end;
      
Var 
  Application : TServerApp;
  
begin
  If (ParamCount=1) and (paramstr(1)='-i') then
    Application:=TServerApp.Create(ThePort)
  else
    Application:=TServerApp.Create(TheSocket);  
  Application.Run;
  Application.Free;
end.

{
  $Log$
  Revision 1.1  2000-03-22 20:21:18  michael
  + Added ssockets examples

}
