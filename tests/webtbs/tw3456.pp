{ Source provided for Free Pascal Bug Report 3456 }
{ Submitted by "Ales Katona (Almindor)" on  2004-12-18 }
{ e-mail: ales@chello.sk }
program objtest;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$ifdef win32}
  {$apptype console}
{$endif}

uses
  SysUtils;

const
{$if defined(cpusparc) or defined(cpuarm) or defined(go32v2)}
  loopcnt = 10000;
{$else}
  loopcnt = 1000000;
{$endif}

type TClassRoot = class
      public
       function Make: TClassRoot; virtual; abstract;
     end;

     TClassB = class;

     TClassA = class(TClassRoot)
      private
       x: longint;
      public
       constructor Create;
       destructor Destroy; override;
       function Make: TClassRoot; override;
     end;

     TClassB = class(TClassRoot)
      private
       x: longint;
      public
       constructor Create;
       destructor Destroy; override;
       function Make: TClassRoot; override;
     end;

constructor TClassA.Create;
begin
  x:=1;
end;

destructor TClassA.Destroy;
begin
  x:=0;
end;

function TClassA.Make: TClassRoot;
begin
  result:=TClassB.Create;
end;

constructor TClassB.Create;
begin
  x:=2;
end;

destructor TClassB.Destroy;
begin
  x:=0;
end;

function TClassB.Make: TClassRoot;
begin
  result:=TClassA.Create;
end;

procedure procb;
var i: longint;
    ar: array of TClassRoot;
    time: double;
begin
  writeln('Array test');
  time:=now;
  setlength(ar, loopcnt+1);
  ar[0]:=TClassA.Create;
  for i:=1 to loopcnt do
    ar[i]:=ar[i-1].Make;
  for i:=0 to loopcnt do
    ar[i].free;
  time:=now-time;
  writeln(time);
end;

var
  p : pointer;
begin
  { Add a big memory block to the free osblocks list }
  getmem(p,1024*1024);
  freemem(p);
  { The small fixed size blocks shall not reuse the big memory block }
  procb;
end.
