{ %target=darwin,iphonesim }
{ %skipcpu=powerpc,powerpc64 }

program tanonfunc64;

{$mode delphi}
{$modeswitch cblocks}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test using anonymous functions and C blocks together 

  same as tanonfunc26 but mode delphi
}

type
  TAnon = reference to function(l: longint): longint;
  TBlock = reference to function(l: longint): longint; cdecl; cblock;

function TestBlock(b: TBlock; l: longint): longint;
begin
  Result := b(l);
end;

function GlobalProc(l: longint): longint;
begin
  Result := l + 2;
end;

function TestAnonFunc: longint;
var
  a: TAnon;
begin
  a := function(l: longint): longint
    begin
      Result := l + 1;
    end;
  TestAnonFunc := a(10);
end;

var
  Block: TBlock;
begin
  Block := GlobalProc;
  if TestBlock(Block, 10) <> 12 then
    halt(1);
  if TestAnonFunc <> 11 then
    halt(2);
end.
