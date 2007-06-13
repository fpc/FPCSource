{$mode objfpc}{$h+}

uses
{$ifdef UNIX}
  cthreads,
{$endif}
  sysutils,
  classes;

type
  tproducethread = class(tthread)
    procedure execute; override;
  end;

  tconsumethread = class(tthread)
    procedure execute; override;
  end;

const
  fifolength = 1024;
var
  readindex: integer;
  writeindex: integer;
  fifo: array[0..fifolength-1] of pointer;
  shared: pointer;
  done: boolean;
  freefifolock: trtlcriticalsection;

type
  ttestarray = array[0..31] of pointer;

procedure exercise_heap(var p: ttestarray; var i, j: integer);
begin
  if p[i] = nil then
    p[i] := getmem(((j*11) mod 532)+8)
  else begin
    freemem(p[i]);
    p[i] := nil;
  end;
  inc(i);
  if i >= 32 then
    dec(i, 32);
  inc(j, 13);
  if j >= 256 then
    dec(j, 256);
end;

procedure freearray(p: ppointer; count: integer);
var
  i: integer;
begin
  for i := 0 to count-1 do
  begin
    freemem(p[i]);
    p[i] := nil;
  end;
end;

procedure producer;
var
  p: ttestarray;
  i, j, k: longint;
begin
  filldword(p, sizeof(p) div sizeof(dword), 0);
  i := 0;
  j := 0;
  k := 0;
  while not done do
  begin
    if ((writeindex+1) mod fifolength) <> readindex then
    begin
      freemem(fifo[writeindex]);
      fifo[writeindex] := getmem(((writeindex*17) mod 520)+8);
      writeindex := (writeindex + 1) mod 1024;
    end else begin
      exercise_heap(p,i,j);
      inc(k);
      if k = 100 then
      begin
        k := 0;
        ThreadSwitch;
      end;
    end;
  end;
  freearray(p, sizeof(p) div sizeof(pointer));
  sleep(200);
  entercriticalsection(freefifolock);
  freearray(fifo, sizeof(fifo) div sizeof(pointer));
  freemem(shared);
  leavecriticalsection(freefifolock);
end;

procedure consumer;
var
  p: ttestarray;
  i, j, k: longint;
begin
  filldword(p, sizeof(p) div sizeof(dword), 0);
  i := 0;
  j := 0;
  k := 0;
  entercriticalsection(freefifolock);
  while not done do
  begin
    if readindex <> writeindex then
    begin
      freemem(fifo[readindex]);
      fifo[readindex] := getmem(((writeindex*17) mod 520)+8);
      readindex := (readindex + 1) mod fifolength;
    end else begin
      exercise_heap(p,i,j);
      inc(k);
      if k = 100 then
      begin
        k := 0;
        ThreadSwitch;
      end;
    end;
  end;
  shared := getmem(12);
  leavecriticalsection(freefifolock);
  freearray(p, sizeof(p) div sizeof(pointer));
end;

procedure tproducethread.execute;
begin
  producer;
end;

procedure tconsumethread.execute;
begin
  consumer;
end;

var
  produce_thread: tproducethread;
  consume_thread: tconsumethread;
begin
  initcriticalsection(freefifolock);
  done := false;
  filldword(fifo, sizeof(fifo) div sizeof(dword), 0);
  readindex := 0;
  writeindex := 0;
  produce_thread := tproducethread.create(false);
  consume_thread := tconsumethread.create(false);
  sleep(3000);
  done := true;
  produce_thread.waitfor;
  consume_thread.waitfor;
  produce_thread.free;
  consume_thread.free;
  donecriticalsection(freefifolock);
end.
