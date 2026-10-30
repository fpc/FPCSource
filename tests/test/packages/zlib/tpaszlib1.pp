{ %skiptarget=embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,ps1,wasip2 }
{ %skipcpu=jvm }
{ deflateInit, deflateCopy and inflateInit leave nothing allocated when one of
  their allocations fails, whether the memory manager answers nil or raises;
  a failed deflateCopy, and one onto its own source, leave the source able to
  finish what it had begun }
program tpaszlib1;

{$mode objfpc}

uses
  sysutils, zbase, zdeflate, zinflate;

const
  MaxLive = 64;
  MaxRequests = 16;

type
  TFailMode = (fmNil, fmRaise);
  TRoutine = (rDeflateInit, rDeflateCopy, rInflateInit);

const
  ModeName: array[TFailMode] of string = ('nil', 'raise');
  RoutineName: array[TRoutine] of string = ('deflateInit', 'deflateCopy', 'inflateInit');

var
  OldMM, TestMM: TMemoryManager;
  Armed: boolean;
  FailAt, Requests: longint;
  FailMode: TFailMode;
  Live: array[0..MaxLive-1] of pointer;
  LiveCount: longint;
  Overflow: boolean;
  ForeignFrees: longint;
  Errors: longint;

procedure Track(p: pointer);
begin
  if LiveCount < MaxLive then
    begin
      Live[LiveCount] := p;
      Inc(LiveCount);
    end
  else
    Overflow := true;
end;

{ false when p was not allocated while the manager was armed }
function Untrack(p: pointer): boolean;
var
  i: longint;
begin
  Result := true;
  for i := 0 to LiveCount - 1 do
    if Live[i] = p then
      begin
        Live[i] := Live[LiveCount - 1];
        Dec(LiveCount);
        exit;
      end;
  Result := false;
end;

{ a free, while armed, of a block the call did not allocate is counted and not
  carried out, so that its owner can still be checked afterwards }
function ForeignFree(p: pointer): boolean;
begin
  Result := Armed and Assigned(p) and not Untrack(p);
  if Result then
    Inc(ForeignFrees);
end;

{ true when this request is the one to refuse }
function Refuse: boolean;
begin
  Result := false;
  if Armed then
    begin
      Result := Requests = FailAt;
      Inc(Requests);
    end;
end;

function TestGetMem(size: ptruint): pointer;
begin
  if Refuse then
    begin
      if FailMode = fmRaise then
        OutOfMemoryError;
      Result := nil;
      exit;
    end;
  Result := OldMM.GetMem(size);
  if Armed and Assigned(Result) then
    Track(Result);
end;

function TestAllocMem(size: ptruint): pointer;
begin
  if Refuse then
    begin
      if FailMode = fmRaise then
        OutOfMemoryError;
      Result := nil;
      exit;
    end;
  Result := OldMM.AllocMem(size);
  if Armed and Assigned(Result) then
    Track(Result);
end;

function TestFreeMem(p: pointer): ptruint;
begin
  if ForeignFree(p) then
    exit(0);
  Result := OldMM.FreeMem(p);
end;

function TestFreeMemSize(p: pointer; size: ptruint): ptruint;
begin
  if ForeignFree(p) then
    exit(0);
  Result := OldMM.FreeMemSize(p, size);
end;

function TestReAllocMem(var p: pointer; size: ptruint): pointer;
var
  old: pointer;
begin
  old := p;
  Result := OldMM.ReAllocMem(p, size);
  if Armed then
    begin
      Untrack(old);
      if Assigned(Result) then
        Track(Result);
    end;
end;

procedure Fail(const what: string);
begin
  writeln('FAIL ', what);
  Inc(Errors);
end;

const
  { a state the caller left in the stream, never dereferenced }
  Sentinel = pInternal_state(ptruint($5A5A5A50));

const
  HalfSize = 600;

type
  TPackBuf = array[0..4095] of byte;

var
  Plain: array[0..2 * HalfSize - 1] of byte;

function CompressWorks(var s: z_stream): boolean;
var
  scratch: TPackBuf;
begin
  s.next_in := @Plain[0];
  s.avail_in := SizeOf(Plain);
  s.next_out := @scratch[0];
  s.avail_out := SizeOf(scratch);
  Result := deflate(s, Z_FINISH) = Z_STREAM_END;
end;

{ a source that has compressed the first half of Plain without finishing }
function StartSource(var s: z_stream; var outbuf: TPackBuf): boolean;
begin
  FillChar(s, SizeOf(s), 0);
  Result := false;
  if deflateInit(s, Z_DEFAULT_COMPRESSION) <> Z_OK then
    exit;
  s.next_in := @Plain[0];
  s.avail_in := HalfSize;
  s.next_out := @outbuf[0];
  s.avail_out := SizeOf(outbuf);
  Result := (deflate(s, Z_NO_FLUSH) = Z_OK) and (s.avail_in = 0);
end;

{ compresses the second half and finishes; true when outbuf then inflates to
  the whole of Plain }
function FinishAndVerify(var s: z_stream; var outbuf: TPackBuf): boolean;
var
  z: z_stream;
  back: array[0..2 * HalfSize - 1] of byte;
  produced: ptruint;
begin
  Result := false;
  s.next_in := @Plain[HalfSize];
  s.avail_in := HalfSize;
  s.next_out := @outbuf[s.total_out];
  s.avail_out := SizeOf(outbuf) - s.total_out;
  if deflate(s, Z_FINISH) <> Z_STREAM_END then
    exit;
  produced := s.total_out;
  FillChar(z, SizeOf(z), 0);
  if inflateInit(z) <> Z_OK then
    exit;
  z.next_in := @outbuf[0];
  z.avail_in := produced;
  z.next_out := @back[0];
  z.avail_out := SizeOf(back);
  Result := (inflate(z, Z_FINISH) = Z_STREAM_END) and (z.total_out = SizeOf(back))
    and CompareMem(@back[0], @Plain[0], SizeOf(back));
  inflateEnd(z);
end;

procedure CheckSourceUsable(var source: z_stream; var outbuf: TPackBuf; const prefix: string);
begin
  if not FinishAndVerify(source, outbuf) then
    Fail(prefix + 'the source no longer finishes a stream that inflates to its input');
  if deflateEnd(source) <> Z_OK then
    Fail(prefix + 'the source no longer ends cleanly');
end;

{ a stream copied onto itself is already its own copy: Z_OK, no request }
procedure SelfCopyCase;
var
  src: z_stream;
  srcOut: TPackBuf;
  rc: integer;
begin
  if not StartSource(src, srcOut) then
    begin
      Fail('self copy: the source stream could not be made');
      exit;
    end;
  FailAt := -1;
  Requests := 0;
  LiveCount := 0;
  Overflow := false;
  ForeignFrees := 0;
  Armed := true;
  rc := deflateCopy(@src, @src);
  Armed := false;
  if ForeignFrees <> 0 then
    Fail('self copy: freed ' + IntToStr(ForeignFrees) + ' block(s) it did not allocate');
  if rc <> Z_OK then
    Fail('self copy: answered ' + IntToStr(rc) + ', expected Z_OK');
  if (Requests <> 0) or (LiveCount <> 0) then
    Fail('self copy: ' + IntToStr(Requests) + ' request(s), ' + IntToStr(LiveCount) + ' block(s) left');
  CheckSourceUsable(src, srcOut, 'self copy: ');
  writeln('deflateCopy onto itself: checked');
end;

{ runs one routine with request FailAt refused; true once the routine made
  fewer requests than that, i.e. nothing was refused }
function RunCase(routine: TRoutine; mode: TFailMode; k: longint): boolean;
var
  src, dest: z_stream;
  srcOut, copyOut: TPackBuf;
  rc: integer;
  raisedType: TClass;
  raisedClass: string;
  tag: string;
begin
  tag := RoutineName[routine] + ' ' + ModeName[mode] + ' #' + IntToStr(k) + ': ';
  FillChar(src, SizeOf(src), 0);
  FillChar(dest, SizeOf(dest), 0);
  dest.state := Sentinel;
  if routine = rDeflateCopy then
    if not StartSource(src, srcOut) then
      begin
        Fail(tag + 'the source stream could not be made');
        exit(true);
      end;
  rc := Z_OK;
  raisedType := nil;
  FailMode := mode;
  FailAt := k;
  Requests := 0;
  LiveCount := 0;
  Overflow := false;
  ForeignFrees := 0;
  Armed := true;
  try
    case routine of
      rDeflateInit:
        rc := deflateInit(dest, Z_DEFAULT_COMPRESSION);
      rDeflateCopy:
        rc := deflateCopy(@dest, @src);
      rInflateInit:
        rc := inflateInit(dest);
    end;
  except
    { nothing here may allocate: the manager is still counting }
    on e: Exception do
      raisedType := e.ClassType;
  end;
  Armed := false;
  if raisedType = nil then
    raisedClass := ''
  else
    raisedClass := raisedType.ClassName;
  if ForeignFrees <> 0 then
    Fail(tag + 'freed ' + IntToStr(ForeignFrees) + ' block(s) it did not allocate');
  Result := Requests <= k;
  if Result then
    begin
      { nothing refused: the routine must have succeeded }
      if (rc <> Z_OK) or (raisedClass <> '') then
        Fail(tag + 'unrefused call answered ' + IntToStr(rc) + ' ' + raisedClass)
      else if routine = rInflateInit then
        inflateEnd(dest)
      else if routine = rDeflateCopy then
        begin
          { the copy goes on from where the source was, into its own buffer }
          copyOut := srcOut;
          if not FinishAndVerify(dest, copyOut) then
            Fail(tag + 'the copy does not finish a stream that inflates to the input');
          deflateEnd(dest);
        end
      else
        begin
          if not CompressWorks(dest) then
            Fail(tag + 'the new stream does not compress');
          deflateEnd(dest);
        end;
    end
  else
    begin
      if mode = fmNil then
        begin
          if raisedClass <> '' then
            Fail(tag + 'raised ' + raisedClass + ', expected Z_MEM_ERROR')
          else if rc <> Z_MEM_ERROR then
            Fail(tag + 'answered ' + IntToStr(rc) + ', expected Z_MEM_ERROR');
        end
      else if raisedClass <> 'EOutOfMemory' then
        Fail(tag + 'raised "' + raisedClass + '", expected EOutOfMemory');
      if Overflow then
        Fail(tag + 'more live blocks than the test tracks')
      else if LiveCount <> 0 then
        Fail(tag + IntToStr(LiveCount) + ' block(s) left allocated');
      { neither a state the call made nor the one it was given is left }
      if dest.state = Sentinel then
        Fail(tag + 'the stream kept the state it was given')
      else if dest.state <> nil then
        Fail(tag + 'the stream still has a state');
    end;
  if routine = rDeflateCopy then
    CheckSourceUsable(src, srcOut, tag);
end;

var
  r: TRoutine;
  m: TFailMode;
  refused, i: longint;
begin
  GetMemoryManager(OldMM);
  TestMM := OldMM;
  TestMM.GetMem := @TestGetMem;
  TestMM.AllocMem := @TestAllocMem;
  TestMM.FreeMem := @TestFreeMem;
  TestMM.FreeMemSize := @TestFreeMemSize;
  TestMM.ReAllocMem := @TestReAllocMem;
  for i := 0 to High(Plain) do
    Plain[i] := byte((i * 7) xor (i shr 3));
  SetMemoryManager(TestMM);
  Errors := 0;
  for r := Low(TRoutine) to High(TRoutine) do
    for m := Low(TFailMode) to High(TFailMode) do
      begin
        refused := 0;
        while not RunCase(r, m, refused) do
          begin
            Inc(refused);
            if refused > MaxRequests then
              begin
                Fail(RoutineName[r] + ' ' + ModeName[m] + ': never completed');
                break;
              end;
          end;
        writeln(RoutineName[r], ' ', ModeName[m], ': ', refused, ' request(s) refused in turn');
      end;
  SelfCopyCase;
  SetMemoryManager(OldMM);
  if Errors <> 0 then
    begin
      writeln(Errors, ' failure(s)');
      halt(1);
    end;
  writeln('ok');
end.
