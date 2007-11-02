{$mode objfpc}
{$inline on}

type
  pshortstring=^shortstring;

  tr = record
    a,b,c,d,e: shortstring;
  end;

  ta = array[0..5] of shortstring;

  tc = record
    p: pointer;
  end;

var
  p,p2,p3: pointer;
  inlined, failed: boolean;

procedure error(err: longint);
begin
  writeln('error near ',err, ' (inlined: ',inlined,')');
  failed:=true;
end;

function f1(p: pchar): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (p^<>'x') then
    error(1);
  f1.a:=p^;
end;


function f2(var s: shortstring): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (s<>'x') then
    error(2);
  f2.a:=s;
end;


function f3(const s: shortstring): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (s<>'x') then
    error(3);
  f3.a:=s;
end;


function f4(const t: tr): tr;
begin
  fillchar(result,sizeof(tr),0);
  if (t.a<>'x') then
    error(4);
  f4:=t;
end;



function f5(p: pchar): ta;
begin
  fillchar(result,sizeof(result),0);
  if (p^<>'x') then
    error(5);
  result[3]:=p^;
end;


function f6(var s: shortstring): ta;
begin
  fillchar(result,sizeof(result),0);
  if (s<>'x') then
    error(6);
  result[3]:=s;
end;


function f7(const s: shortstring): ta;
begin
  fillchar(result,sizeof(result),0);
  if (s<>'x') then
    error(7);
  result[3]:=s;
end;


function f8(const t: ta): ta;
begin
  fillchar(result,sizeof(result),0);
  if (t[3]<>'x') then
    error(8);
  result:=t;
end;


procedure temp;
begin
  if (pshortstring(p)^<>'x') then
    error(9);
end;

function f9: tr;
begin
  fillchar(result,sizeof(result),0);
  temp;
  result.a:='x';
end;

procedure temp2(var a);
begin
  p2:=@a;
end;

function f10: tr;
begin
  fillchar(result,sizeof(result),0);
  if (pshortstring(p2)^<>'x') then
    error(10);
  result.a:='x';
end;

procedure testrec1;
var
  t: tr;
begin
  t.a:='x';
  t:=f1(@t.a[1]);
end;


procedure testrec2;
var
  t: tr;
begin
  t.a:='x';
  t:=f2(t.a);
end;


procedure testrec3;
var
  t: tr;
begin
  t.a:='x';
  t:=f3(t.a);
end;


procedure testrec4;
var
  t: tr;
begin
  t.a:='x';
  t:=f4(t);
end;


procedure testrec5;
var
  t: tr;
begin
  t.a:='x';
  p:=@t.a;
  t:=f9;
end;


procedure testrecinl1; inline;
var
  t: tr;
begin
  inlined:=true;
  t.a:='x';
  t:=f1(@t.a[1]);
end;


procedure testrecinl2; inline;
var
  t: tr;
begin
  inlined:=true;
  t.a:='x';
  t:=f2(t.a);
end;


procedure testrecinl3; inline;
var
  t: tr;
begin
  inlined:=true;
  t.a:='x';
  t:=f3(t.a);
end;


procedure testrecinl4; inline;
var
  t: tr;
begin
  inlined:=true;
  t.a:='x';
  t:=f4(t);
end;


procedure testrecinl5; inline;
var
  t: tr;
begin
  inlined:=true;
  t.a:='x';
  p:=@t.a;
  t:=f9;
  inlined:=false;
end;


procedure testrec2a;
var
  t: tr;
begin
  t.a:='x';
  temp2(t.a);
  t:=f10;
end;


procedure testrec2ainl; inline;
var
  t: tr;
begin
  inlined:=true;
  t.a:='x';
  temp2(t.a);
  t:=f10;
  inlined:=false;
end;


{$if defined(cpupowerpc32) or defined(cpupowerpc64) or defined(cpui386)}
function f11: tr;
begin
  fillchar(result,sizeof(result),0);
  if (pshortstring(p3)^<>'x') then
    error(11);
  result.a:='x';
end;

procedure testrec3a;
var
  t: tr;
begin
  asm
{$ifdef cpupowerpc32}
    la  r3,t
  {$ifndef macos}
    lis  r4,p3@ha
    addi r4,r4,p3@l
  {$else}
    lwz  r4,p3(r2)
  {$endif}
    stw  r3,0(r4)
{$endif}
{$ifdef cpupowerpc64}
    la  r3,t
{$ifndef darwin}
    lis  r4, p3@highesta
    ori  r4, r4, p3@highera
    sldi r4, r4, 32
    oris r4, r4, p3@ha
{$else darwin}
    lis  r4, p3@ha
{$endif darwin}
    std  r3,p3@l(r4)
{$endif}
{$ifdef cpui386}
    leal t,%eax
    movl %eax,p3
{$endif}
  end;

  t.a:='x';
  t:=f11;
end;


procedure testrec3ainl; inline;
var
  t: tr;
begin
  inlined:=true;
  asm
{$ifdef cpupowerpc32}
    la  r3,t
  {$ifndef macos}
    lis  r4,p3@ha
    addi r4,r4,p3@l
  {$else}
    lwz  r4,p3(r2)
  {$endif}
    stw  r3,0(r4)
{$endif}
{$ifdef cpupowerpc64}
    la  r3,t
{$ifndef darwin}
    lis  r4, p3@highesta
    ori  r4, r4, p3@highera
    sldi r4, r4, 32
    oris r4, r4, p3@ha
{$else darwin}
    lis  r4, p3@ha
{$endif darwin}
    std  r3,p3@l(r4)
{$endif}
{$ifdef cpui386}
    leal t,%eax
    movl %eax,p3
{$endif}
  end;

  t.a:='x';
  t:=f11;
  inlined:=false;
end;

{$endif}



procedure testarr1;
var
  t: ta;
begin
  t[3]:='x';
  t:=f5(@t[3][1]);
end;


procedure testarr2;
var
  t: ta;
begin
  t[3]:='x';
  t:=f6(t[3]);
end;


procedure testarr3;
var
  t: ta;
begin
  t[3]:='x';
  t:=f7(t[3]);
end;


procedure testarr4;
var
  t: ta;
begin
  t[3]:='x';
  t:=f8(t);
end;


procedure testarrinl1; inline;
var
  t: ta;
begin
  inlined:=true;
  t[3]:='x';
  t:=f5(@t[3][1]);
end;


procedure testarrinl2; inline;
var
  t: ta;
begin
  inlined:=true;
  t[3]:='x';
  t:=f6(t[3]);
end;


procedure testarrinl3; inline;
var
  t: ta;
begin
  inlined:=true;
  t[3]:='x';
  t:=f7(t[3]);
end;


procedure testarrinl4; inline;
var
  t: ta;
begin
  inlined:=true;
  t[3]:='x';
  t:=f8(t);
  inlined:=false;
end;


begin
  testrec1;
  testrec2;
  testrec3;
  testrec4;
  testrec5;
  testrecinl1;
  testrecinl2;
  testrecinl3;
  testrecinl4;
  testrecinl5;
  testrec2a;
  testrec2ainl;
{$if defined(cpupowerpc32) or defined(cpui386) or defined(cpupowerpc64)}
  testrec3a;
  testrec3ainl;
{$endif}
  testarr1;
  testarr2;
  testarr3;
  testarr4;
  testarrinl1;
  testarrinl2;
  testarrinl3;
  testarrinl4;
  if failed then
    halt(1);
end.
