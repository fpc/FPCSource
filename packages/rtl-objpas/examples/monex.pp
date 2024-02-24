program TMonitorTest;
 
{$APPTYPE CONSOLE}
{$mode objfpc} 
{$h+}
uses
{$ifdef unix}
  cthreads,
{$endif}  
 
  SysUtils, Classes, fpMonitor;
 
type
  Drop = class(TObject)
  private
    // Message sent from producer to consumer.
    Msg: string;
    // True if consumer should wait for producer to send message, false
    // if producer should wait for consumer to retrieve message.
    Empty: Boolean;
  public
    constructor Create;
    function Take: string;
    procedure Put(AMessage: string);
  end;
 
  Producer = class(TThread)
  private
    FDrop: Drop;
  public
    constructor Create(ADrop: Drop);
    procedure Execute; override;
  end;
 
  Consumer = class(TThread)
  private
    FDrop: Drop;
  public
    constructor Create(ADrop: Drop);
    procedure Execute; override;
  end;
 
{ Drop }
 
constructor Drop.Create;
begin
  Empty := True;
end;
 
function Drop.Take: string;
begin
  TMonitor.Enter(Self);
  try
    // Wait until message is available.
    while Empty do
    begin
      TMonitor.Wait(Self, INFINITE);
    end;
    // Toggle status.
    Empty := True;
    // Notify producer that status has changed.
    TMonitor.PulseAll(Self);
    Result := Msg;
  finally
     TMonitor.Exit(Self);
  end;
end;
 
procedure Drop.Put(AMessage: string);
begin
  TMonitor.Enter(Self);
  try
    // Wait until message has been retrieved.
    while not Empty do
    begin
      TMonitor.Wait(Self, INFINITE);
    end;
    // Toggle status.
    Empty := False;
    // Store message.
    Msg := AMessage;
    // Notify consumer that status has changed.
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;
 
{ Producer }
 
constructor Producer.Create(ADrop: Drop);
begin
  FDrop := ADrop;
  inherited Create(False);
end;
 
procedure Producer.Execute;
var
  Msgs: array of string;
  I: Integer;
begin
  SetLength(Msgs, 4);
  Msgs[0] := 'Mares eat oats';
  Msgs[1] := 'Does eat oats';
  Msgs[2] := 'Little lambs eat ivy';
  Msgs[3] := 'A kid will eat ivy too';
  for I := 0 to Length(Msgs) - 1 do
  begin
    FDrop.Put(Msgs[I]);
    Sleep(Random(50{00}));
  end;
  FDrop.Put('DONE');
end;
 
{ Consumer }
 
constructor Consumer.Create(ADrop: Drop);
begin
  FDrop := ADrop;
  inherited Create(False);
end;
 
procedure Consumer.Execute;
var
  Msg: string;
begin
  repeat
    Msg := FDrop.Take;
    WriteLn('Received: ' + Msg);
    Sleep(Random(50{00}));
  until Msg = 'DONE';
end;
 
var
  ADrop: Drop;
 
begin
  Randomize;
  ADrop := Drop.Create;
  Producer.Create(ADrop);
  Consumer.Create(ADrop).WaitFor;
{$IFDEF WINDOWS}  
  ReadLn;
{$ENDIF}
end.