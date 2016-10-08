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
  event1, event2: prtlevent;
  gcount: longint;
  gotdeadlockexception,
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
  
  treadwritecounter = class(tcounter)
   private
    ftrywriteupgrade: boolean;
   public
    constructor create(trywriteupgrade: boolean);
    procedure execute; override;
  end;

  tdeadlock1 = class(tthread)
    procedure execute; override;
  end;

  tdeadlock2 = class(tthread)
    procedure execute; override;
  end;

  tdoublereadonewrite1 = class(tthread)
    procedure execute; override;
  end;

  tdoublereadonewrite2 = class(tthread)
    procedure execute; override;
  end;

  twrongthreadendacquire = class(tthread)
    ftestwrongreadrelease: boolean;
    constructor create(testwrongreadrelease: boolean);
    procedure execute; override;
  end;

  twrongthreadendrelease = class(tthread)
    ftestwrongreadrelease: boolean;
    constructor create(testwrongreadrelease: boolean);
    procedure execute; override;
  end;

  tdoublewrite = class(tthread)
    fsecondwritethread: boolean;
    constructor create(secondwritethread: boolean);
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


constructor treadwritecounter.create(trywriteupgrade: boolean);
  begin
    ftrywriteupgrade:=trywriteupgrade;
    inherited create;
  end;


procedure treadwritecounter.execute;
  var
    i: longint;
    l: longint;
    r: longint;
  begin
    for i:=1 to 100000 do
      begin
        lock.beginread;
        if ftrywriteupgrade and
           ((i=50000) or
            (random(10000)=0)) then
          begin
            inc(flocalcount);
            lock.beginwrite;
            l:=gcount;
            { guarantee at least one sleep }
            if i=50000 then
              sleep(20+random(30))
            else if (random(5)=0) then
              sleep(20);
            lock.beginwrite;
            gcount:=l+1;
            lock.endwrite;
            lock.endwrite;
          end;
        lock.endread;
        r:=random(30000);
        if (r=0) then
          sleep(30);
      end;
  end;


procedure tdeadlock1.execute;
  var
    localgotdeadlockexception: boolean;
  begin
    localgotdeadlockexception:=false;
    lock.beginread;
    RTLEventSetEvent(event2);
    RTLEventWaitFor(event1);
    try
      lock.beginwrite;
    except
      localgotdeadlockexception:=true;
      gotdeadlockexception:=true;
    end;
    if not localgotdeadlockexception then
      lock.endwrite;
    lock.endread;
  end;


procedure tdeadlock2.execute;
  var
    localgotdeadlockexception: boolean;
  begin
    localgotdeadlockexception:=false;
    lock.beginread;
    RTLEventSetEvent(event1);
    RTLEventWaitFor(event2);
    try
      lock.beginwrite;
    except
      localgotdeadlockexception:=true;
      gotdeadlockexception:=true;
    end;
    if not localgotdeadlockexception then
      lock.endwrite;
    lock.endread;
  end;


procedure tdoublereadonewrite1.execute;
  begin
    // 1)
    lock.beginread;
    // 2)
    RTLEventSetEvent(event2);
    // 5)
    RTLEventWaitFor(event1);
    { ensure tdoublereadonewrite2 has time to get stuck in beginwrite }
    sleep(500);
    // 6)
    lock.beginread;
    // 7)
    lock.endread;
    // 8)
    lock.endread;
  end;


procedure tdoublereadonewrite2.execute;
  begin
    // 3)
    RTLEventWaitFor(event2);
    // 4)
    RTLEventSetEvent(event1);
    // 4a -- block until after 8)
    lock.beginwrite;
    // 9)
    lock.endwrite;
  end;


constructor twrongthreadendacquire.create(testwrongreadrelease: boolean);
  begin
    ftestwrongreadrelease:=testwrongreadrelease;
    inherited create(false);
  end;


procedure twrongthreadendacquire.execute;
  begin
    if ftestwrongreadrelease then
      lock.beginread
    else
      lock.beginwrite;
    RTLEventSetEvent(event1);
    RTLEventWaitFor(event2);
    try
      if ftestwrongreadrelease then
        lock.endread
      else
        lock.endwrite;
    except
      halt(30);
    end;
  end;


constructor twrongthreadendrelease.create(testwrongreadrelease: boolean);
  begin
    ftestwrongreadrelease:=testwrongreadrelease;
    inherited create(false);
  end;


procedure twrongthreadendrelease.execute;
  var
    caught: boolean;
  begin
    RTLEventWaitFor(event1);
    caught:=false;
    try
      if ftestwrongreadrelease then
        lock.endread
      else
        lock.endwrite;
    except
      caught:=true;
    end;
    RTLEventSetEvent(event2);
    if not caught then
      halt(40);
  end;


constructor tdoublewrite.create(secondwritethread: boolean);
  begin
    fsecondwritethread:=secondwritethread;
    inherited create(false);
  end;


procedure tdoublewrite.execute;
  begin
    if fsecondwritethread then
      begin
        RTLEventWaitFor(event1);
        if lock.beginwrite then
          halt(50);
      end
    else
      begin
        if not lock.beginwrite then
          halt(51);
        RTLEventSetEvent(event1);
        // give the other thread the time to get to its beginwrite call
        Sleep(500);
      end;
    lock.endwrite;
  end;


procedure terrorcheck.execute;
begin
  { make sure we don't exit before this thread has initialised, since    }
  { it can allocate memory in its initialisation, which would cause      }
  { problems for heaptrc as it goes over the memory map in its exit code }
  waiting:=true;
  { avoid deadlocks/bugs from causing this test to never quit }
  sleep(1000*60);
  writeln('error 4');
  halt(4);
end;


var
  r1,r2,r3,r4,r5,r6: treadcounter;
  w1,w2,w3,w4: twritecounter;
  rw1,rw2,rw3: treadwritecounter;
  d1: tdeadlock1;
  d2: tdeadlock2;
  dr1: tdoublereadonewrite1;
  dr2: tdoublereadonewrite2;
  wr1: twrongthreadendacquire;
  wr2: twrongthreadendrelease;
  dw1, dw2: tdoublewrite;
  caught: boolean;
begin
  waiting:=false;
  terrorcheck.create(false);
  randomize;
  lock:=TMultiReadExclusiveWriteSynchronizer.create;
  event1:=RTLEventCreate;
  event2:=RTLEventCreate;

  { verify that the lock is recursive }
  if not lock.beginwrite then
    halt(10);
  if not lock.beginwrite then
    halt(11);
  lock.endwrite;
  lock.endwrite;

  { verify that we can upgrade a read lock to a write lock }
  lock.beginread;
  if not lock.beginwrite then
    halt(12);
  lock.endwrite;
  lock.endread;

  { verify that owning a write lock does not prevent getting a read lock }
  if not lock.beginwrite then
    halt(13);
  lock.beginread;
  lock.endread;
  lock.endwrite;

  { verify that calling endread without beginread throws an exception }
  caught:=false;
  try
    lock.endread;
  except
    caught:=true;
  end;
  if not caught then
    halt(14);

  { verify that calling endwrite without beginwrite throws an exception }
  caught:=false;
  try
    lock.endwrite;
  except
    caught:=true;
  end;
  if not caught then
    halt(15);


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

  { mixed readers and writers without proper synchronisation }
  gcount:=0;
  rw1:=treadwritecounter.create(true);
  rw2:=treadwritecounter.create(false);
  rw3:=treadwritecounter.create(false);

  rw1.resume;
  rw2.resume;
  rw3.resume;

  rw1.waitfor;
  rw2.waitfor;
  rw3.waitfor;

  { must not have caused any data races }
  if (gcount<>rw1.localcount+rw2.localcount+rw3.localcount) then
    begin
      writeln('error 5');
      halt(5);
    end;

  RTLEventResetEvent(event1);
  RTLEventResetEvent(event2);

  { check deadlock detection }
  d1:=tdeadlock1.create(false);
  d2:=tdeadlock2.create(false);

  d1.waitfor;
  d2.waitfor;
  if not gotdeadlockexception then
    halt(6);

  d1.free;
  d2.free;


  { check that a waiting writer does not block a reader trying to get
    a recursive read lock it already holds }
  dr1:=tdoublereadonewrite1.create(false);
  dr2:=tdoublereadonewrite2.create(false);

  dr1.waitfor;
  dr2.waitfor;

  dr1.free;
  dr2.free;

  { check that releasing a lock in another thread compared to where it
    was acquired causes an exception }
  wr1:=twrongthreadendacquire.create(true);
  wr2:=twrongthreadendrelease.create(true);
  wr1.waitfor;
  wr2.waitfor;
  wr1.free;
  wr2.free;

  wr1:=twrongthreadendacquire.create(false);
  wr2:=twrongthreadendrelease.create(false);
  wr1.waitfor;
  wr2.waitfor;
  wr1.free;
  wr2.free;

  dw1:=tdoublewrite.create(false);
  dw2:=tdoublewrite.create(true);
  dw1.waitfor;
  dw2.waitfor;
  dw1.free;
  dw2.free;

  RTLEventDestroy(event1);
  RTLEventDestroy(event2);

  lock.free;

  while not waiting do
    sleep(20);
end.
