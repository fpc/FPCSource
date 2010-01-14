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
  waiting: boolean;

type
  terrorcheck = class(tthread)
    procedure execute; override;
  end;

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
        { guarantee at least one sleep }
        if i=50000 then
          sleep(20+random(30))
        else if (random(10000)=0) then
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
        { guarantee at least one sleep }
        if i=250 then
          sleep(20+random(30))
        else if (random(100)=0) then
          sleep(20);
        { we must be exclusive }
        if gcount<>l then
          begin
            writeln('error 1');
            halt(1);
          end;
        gcount:=l+1;
        lock.endwrite;
        r:=random(30);
        if (r>28) then
          sleep(r);
      end;
  end;
  


procedure terrorcheck.execute;
begin
  { make sure we don't exit before this thread has initialised, since    }
  { it can allocate memory in its initialisation, which would cause      }
  { problems for heaptrc as it goes over the memory map in its exit code }
  waiting:=true;
  { avoid deadlocks/bugs from causing this test to never quit }
  sleep(1000*15);
  writeln('error 4');
  halt(4);
end;


var
  r1,r2,r3,r4,r5,r6: treadcounter;
  w1,w2,w3,w4: twritecounter;
begin
  waiting:=false;
  terrorcheck.create(false);
  randomize;
  lock:=TMultiReadExclusiveWriteSynchronizer.create;
  { verify that the lock is recursive }
  lock.beginwrite;
  lock.beginwrite;
  lock.endwrite;
  lock.endwrite;

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
    begin
      writeln('error 2');
      halt(2);
    end;

  w1.free;
  w2.free;
  w3.free;
  w4.free;

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
    begin
      writeln('error 3');
      halt(3);
    end;

  r1.free;
  r2.free;
  r3.free;
  r4.free;
  r5.free;
  r6.free;
  w1.free;
  w2.free;

  lock.free;

  while not waiting do
    sleep(20);
end.
