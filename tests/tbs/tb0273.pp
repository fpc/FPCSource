{ Old file: tbs0319.pp }
{  }

{$ifdef fpc}{$mode delphi}{$endif}

function a:longint;
var
  a : longint;
begin
  a:=1;
end;

type
  cl=class
    k : longint;
    procedure p1;
    procedure p2;
  end;

 o = class
       nonsense  :string;
       procedure flup(nonsense:string);
     end;

 o2 = class
       nonsense  :string;
       procedure flop;
       procedure flup(nonsense:longint);
       procedure flup2(flop:longint);
     end;

procedure o.flup(nonsense:string);
begin
end;

procedure o2.flop;
begin
end;

procedure o2.flup(nonsense:longint);
var
  l : longint;
begin
  l:=nonsense;
end;

procedure o2.flup2(flop:longint);
var
  l : longint;
begin
  l:=flop;
  flup(flop);
end;


procedure cl.p1;
var
  k : longint;
begin
end;

procedure cl.p2;
var
  p1 : longint;
begin
end;

begin
end.
