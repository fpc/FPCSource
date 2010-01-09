{$ifdef fpc}
{$mode objfpc}
{$h+}
{$endif}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils, Classes;

var
  lock: TMultiReadExclusiveWriteSynchronizer;
  gcount: longint;

type
  tcounter = class(tthread)
   private
    flock: TMultiReadExclusiveWriteSynchronizer;
    flocalcount: longint;
   public
    constructor create;
    property localcount: longint read flocalcount;
  end;

  treadcounter = class(tcounter)
    procedure execute; override;
  end;
  
  twritecounter = class(tcounter)
    procedure execute; override;
  end;
  
constructor tcounter.create;
  begin
    { create suspended }
    inherited create(true);
    freeonterminate:=false;
    flock:=lock;
    flocalcount:=0;
  end;
  
procedure treadcounter.execute;
  var
    i: longint;
    l: longint;
    r: longint;
  begin
    for i:=1 to 100000 do
      begin
        lock.beginread;
        inc(flocalcount);
        l:=gcount;
        if (random(10000)=0) then
          sleep(20);
        { this must cause data races/loss at some point }
        gcount:=l+1;
        lock.endread;
        r:=random(30000);
        if (r=0) then
          sleep(30);
      end;
  end;


procedure twritecounter.execute;
  var
    i: longint;
    l: longint;
    r: longint;
  begin
    for i:=1 to 500 do
      begin
        lock.beginwrite;
        inc(flocalcount);
        l:=gcount;
        if (random(100)=0) then
          sleep(20);
        { we must be exclusive }
        if gcount<>l then
          halt(1);
        gcount:=l+1;
        lock.endwrite;
        r:=random(30);
        if (r>28) then
          sleep(r);
      end;
  end;
  
var
  r1,r2,r3,r4,r5,r6: treadcounter;
  w1,w2,w3,w4: twritecounter;
begin
  randomize;
  lock:=TMultiReadExclusiveWriteSynchronizer.create;
  { first try some writers }
  w1:=twritecounter.create;
  w2:=twritecounter.create;
  w3:=twritecounter.create;
  w4:=twritecounter.create;
  w1.resume;
  w2.resume;
  w3.resume;
  w4.resume;
  w1.waitfor;
  w2.waitfor;
  w3.waitfor;
  w4.waitfor;
  
  { must not have caused any data races }
  if (gcount<>w1.localcount+w2.localcount+w3.localcount+w4.localcount) then
    halt(1);

  { now try some mixed readers/writers }
  gcount:=0;
  r1:=treadcounter.create;
  r2:=treadcounter.create;
  r3:=treadcounter.create;
  r4:=treadcounter.create;
  r5:=treadcounter.create;
  r6:=treadcounter.create;
  w1:=twritecounter.create;
  w2:=twritecounter.create;
  
  r1.resume;
  r2.resume;
  r3.resume;
  r4.resume;
  r5.resume;
  r6.resume;
  w1.resume;
  w2.resume;
  
  r1.waitfor;
  r2.waitfor;
  r3.waitfor;
  r4.waitfor;
  r5.waitfor;
  r6.waitfor;
  w1.waitfor;
  w2.waitfor;
  
  { updating via the readcount must have caused data races }
  if (gcount>=r1.localcount+r2.localcount+r3.localcount+r4.localcount+r5.localcount+r6.localcount+w1.localcount+w2.localcount) then
    halt(2);
end.
