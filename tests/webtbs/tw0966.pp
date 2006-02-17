{ %INTERACTIVE }
{ %TARGET=win32,linux,wince }

{ Source provided for Free Pascal Bug Report 966 }
{$i-}
uses
{$ifdef Unix}
  linux,
{$else}
  crt,
{$endif}
  Sockets;
Var
 S : Longint ; Sin,Sout: Text;
 Temp, Temp2 : Char;
 i : longint;

const
 isocket: TInetSockAddr= (
    Family:AF_INET;
    Port:$1500;
    Addr:((93*256+36)*256+161)*256+130);
    {*** ftp 130.161.36.93 i.e. ftp.freepascal.org }
    { FIXME: it would be much better to have the number
    through a name server but I don't know how to do this ! PM }

        procedure perror(const S: string);
        begin
        writeln(S,SocketError);
        halt(100) ;
        end;

  procedure read_to_eof;
    var
      temp2 : char;
    begin
{$ifdef Unix}
      while selecttext(sin,1)>0 do
       begin
         read(Sin,Temp2);
         write(Temp2);
       end;
{$else}
      repeat until not eof(sin);
      while not eof(sin) do
        begin
          read(Sin,Temp2);
          write(Temp2);
          delay(1);
        end;
{$endif}
    end;

begin
  S:=Socket(AF_INET,SOCK_STREAM,0);
  if SocketError<>0 then Perror('Client : Socket : ');
  WriteLn('*1');
  if not Connect(s,isocket,sin,sout)then Perror('Client : Socket : ');
  WriteLn('*2');
  ReWrite(Sout); Reset(Sin);
  WriteLn('*3');
  read_to_eof;
  Writeln('Sending "USER anonymous#10"');
  Write(Sout,'USER anonymous'#10);
  read_to_eof;
  Writeln('Sending "PASS core@freepascal.org#10"');
  Write(Sout,'PASS core@freepascal.org'#10);
  read_to_eof;
  Writeln('Sending "QUIT#10"');
  Write(Sout,'QUIT'#10);
  read_to_eof;
  shutdown(s,2); close(sin); close(sout);
end.
