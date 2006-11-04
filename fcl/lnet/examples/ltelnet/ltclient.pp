program ltclient;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, lTelnet, lNet;
  
{ This is a rather simple Telnet client,
  it accepts IP/url and port as arguments
  See file ltelnet.pas if you want to know
  how it works. }
  
type

  { TLTelnetTest }

  TLTelnetTest = class
   private
    FCon: TLTelnetClient;
    FQuit: Boolean;
    procedure OnError(const msg: string; aSocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TLTelnetTest }

constructor TLTelnetTest.Create;
begin
  FCon:=TLTelnetClient.Create(nil);
end;

destructor TLTelnetTest.Destroy;
begin
  FCon.Free;
  inherited Destroy;
end;

procedure TLTelnetTest.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
  FQuit:=True;
end;
  
procedure TLTelnetTest.Run;
var
  s: string;
  c: Char;
  l: Longint; // length of line currently written
  AD: string;
  PORT: Word;
begin
  if ParamCount > 1 then
    try
      AD:=Paramstr(1);
      PORT:=Word(StrToInt(Paramstr(2)));
    except
      Writeln('Invalid command line parameters');
      Exit;
    end else begin
      Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' IP PORT');
      Exit;
    end;
  FQuit:=False;
  l:=0;
  if FCon.Connect(AD, PORT) then begin
    Writeln('Connecting... press any key to cancel');
    repeat
      FCon.CallAction;
      Sleep(1);
      if KeyPressed then
        Halt;
    until FCon.Connected; // wait until timeout or we actualy connected
    
    while not FQuit do begin // if we connected, do main loop
      if KeyPressed then begin
        c:=ReadKey;
        case c of
          #27: FQuit:=True;
           #8: if l > 0 then
                 begin
                   GotoXY(WhereX-1, WhereY);
                   Write(' ');
                   Dec(l);
                   FCon.SendMessage(c);
                 end;
        else begin
               Inc(l);
               if c = #13 then begin
                 Writeln;
               l:=0;
               end;
               FCon.SendMessage(c);
             end;
        end;
      end;
      if FCon.GetMessage(s) > 0 then
        Write(s);
      FCon.CallAction;
      Delay(1);
    end;
  end;
  
  if FCon.GetMessage(s) > 0 then
    Write(s);
end;

var
  Telnet: TLTelnetTest;
begin
  Telnet:=TLTelnetTest.Create;
  Telnet.Run;
  Telnet.Free;
end.

