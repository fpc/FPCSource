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
// Cmem, // comment out to get the crash
     SysUtils, Contnrs;

const
{$ifdef cpusparc}
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

procedure proca;
var i: longint;
    list: TObjectList;
    time: double;
begin
  writeln('List test');
  time:=now;
  list:=TObjectList.Create(true);
  list.add(TClassA.Create);
  for i:=1 to loopcnt do
    list.add(TClassRoot(list.last).Make);
  list.free;
  time:=now-time;
  writeln(time);
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
   begin
    if i=65535 then
      time:=time;
    ar[i]:=ar[i-1].Make;
   end;
  for i:=0 to loopcnt do
   begin
    if i=65535 then
      time:=time;
    ar[i].free;
    end;
  time:=now-time;
  writeln(time);
end;

begin
  proca;
  procb;
end.

