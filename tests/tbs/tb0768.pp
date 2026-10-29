{ Assigning a double copies its bits: a signalling NaN stays signalling, with
  invalid operations masked or unmasked, and no exception is raised (on i386
  loading the value into the x87 as a double quiets the NaN, or raises the
  exception when invalid operations are unmasked); on x86_64, and on i386 from
  the Pentium on, a copy whose destination overlaps its source keeps the value,
  as a single load and store through the x87 does }
{ %skipcpu=jvm }
program tb0768;

{$mode objfpc}
{$ifdef cpui386}
{$fputype x87}
{$endif cpui386}

uses
  sysutils, math;

const
  SNaNBits = int64($7FF4000012345678);

type
  TRec = record
    d: double;
  end;

{$ifndef cpujvm}
{ a managed VM cannot assign through a typecast of an untyped parameter }
procedure CopyUntyped(const src; var dst);
begin
  double(dst):=double(src);
end;
{$endif cpujvm}

procedure CopyConst(const src: double; var dst: double);
begin
  dst:=src;
end;

procedure CopyVar(var src, dst: double);
begin
  dst:=src;
end;

procedure Check(bits: int64; code: longint);
begin
  if bits<>SNaNBits then
    begin
      writeln('error ',code,': bits $',IntToHex(bits,16));
      halt(code);
    end;
end;

var
  words: array[0..2] of longword;

procedure CopyAll(base: longint);
var
  s, d: double;
  r1, r2: TRec;
  p: ^double;
begin
  PInt64(@s)^:=SNaNBits;
  try
{$ifndef cpujvm}
    d:=0;
    CopyUntyped(s,d);
    Check(PInt64(@d)^,base+1);
{$endif cpujvm}
    d:=0;
    CopyConst(s,d);
    Check(PInt64(@d)^,base+2);
    d:=0;
    CopyVar(s,d);
    Check(PInt64(@d)^,base+3);
    d:=0;
    d:=s;
    Check(PInt64(@d)^,base+4);
    r1.d:=s;
    Check(PInt64(@r1.d)^,base+5);
    r2:=r1;
    Check(PInt64(@r2.d)^,base+6);
    new(p);
    p^:=s;
    Check(PInt64(p)^,base+7);
    dispose(p);
  except
    on e: Exception do
      begin
        writeln('error ',base+9,': ',e.ClassName,' raised by a copy');
        halt(base+9);
      end;
  end;
end;

begin
  { masked: a copy through the x87 as a double would store a quiet NaN }
  SetExceptionMask(GetExceptionMask+[exInvalidOp]);
  CopyAll(10);
  { unmasked: a copy through the x87 as a double would raise }
  SetExceptionMask(GetExceptionMask-[exInvalidOp]);
  CopyAll(20);
{$if defined(cpux86_64) or (defined(cpui386) and not (defined(cpu) or defined(cpu80386) or defined(cpu80486)))}
  { the destination starts four bytes after the source; below the Pentium
    (an 80386, an 80486, or no CPU selected, which defines the macro CPU) a
    double is copied as two 32-bit words, the first half first }
  words[0]:=$12345678;
  words[1]:=$3FF00000;
  words[2]:=0;
  CopyVar(PDouble(@words[0])^,PDouble(@words[1])^);
  if (words[1]<>$12345678) or (words[2]<>$3FF00000) then
    begin
      writeln('error 30: an overlapping copy lost the value');
      halt(30);
    end;
{$endif}
  writeln('ok');
end.
