program TestStr;
{$ifdef timer}
uses Timer;
{$else}
type
  TTimer = Object
    TotalMSec,
    StartMSec : longint;
    constructor init;
    procedure reset;
    procedure start;
    procedure stop;
    Function MSec:longint;
  end;

procedure TTimer.Reset;
begin
end;

procedure TTimer.Start;
begin
end;


procedure TTimer.Stop;
begin
end;


Function TTimer.MSec:longint;
begin
  MSec:=0;
end;

Constructor TTimer.Init;
begin
end;

{$endif}

const
  TestSize=10; {Use at least 10 for reasonable results}
type
  BenType=array[1..8] of longint;
var
  Total      : longint;
  headBen,
  LoadBen,
  ConcatBen,
  DelBen,
  InsBen,
  CopyBen,
  CmpBen,
  MixBen     : BenType;
  t          : TTimer;

function TestOK:boolean;
Const
  TestStr: string[22]='HELLO, THIS IS A TEST ';
var
  I : INTEGER;
  U : STRING[1];
  Q : STRING[100];
  S : STRING[55];
  T : STRING[60];
  V : STRING;
begin
  TestOk:=false;
  T:='THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 1234567890';
  Insert (T, T, 1);
{Writeln(T);}
  Delete (T, 55, 54);
  S:=Copy (T, -5, 2);     {'TH'}
  U:=Copy (T, 7, 4);      {'I'}
  S:=S + U;               {'THI'}
  Q:=Copy (T, 32, 70);    {'THE LAZY DOG 1234567890'}
  Delete (Q, 2, 1);         {'TE LAZY DOG 1234567890'}
  Delete (Q, 100, 2);       {'TE LAZY DOG 1234567890'}
  Delete (Q, 3, -4);        {'TE LAZY DOG 1234567890'}
  Delete (Q, 3, 10);        {'TE1234567890'}
{  writeln('TE1234567890 - ',Q);}
  I:=Pos ('S', T);        {25}
  Insert(Copy(T,I,200),Q,3);{'TES OVER THE LAZY DOG 12345678901234567890'}
  Delete (Q, 4, 6);         {'TESTHE LAZY DOG 12345678901234567890}
  S:=S + T [25];          {'THIS'}
  S:=S + Copy (S, 3, -5) + Copy (S, 3, 2);  {'THISIS'}
  V:=T;                   {'THE QUICK BROWN FOX JUMPS OVER THE LAZY ..'}
  Delete (V, -10, 47);      {'AZY DOG 1234567890'}
  if (Copy (V, -7, -1)='') and (Pos ('DOG', V)=5) then {TRUE}
   Insert (V, S, 200);    {'THISISAZY DOG 1234567890'}
  U:=Copy (T, 44, 40);    {' '}
  Insert (U, S, 5);         {'THIS ISAZY DOG 1234567890'}
  I:=Pos ('ZY', S);       {9}
  Delete (S, I, -5);        {'THIS ISAZY DOG 1234567890'}
  Insert (Copy(S,5,1),S,8); {'THIS IS AZY DOG 1234567890'}
  Delete (S, 10, 16);       {'THIS IS A0'}
  if S [Length (S)]='0' then {TRUE}
   S:=S + Q;            {'THIS IS A0TESTHE LAZY DOG 123456789012345...'}
  V:=Copy (S, Length (S) - 19, 10); {'1234567890'}
  if V=Copy (S, Length (S) - 9, 10) then {TRUE}
   Delete (S, 15, 3 * Length (V)+2); {'THIS IS A0TEST'}
  Insert ('', S, 0);        {'THIS IS A0TEST'}
  Insert(Copy(S,5,1),S,11); {'THIS IS A0 TEST'}
  Insert ('HELLO', S, -4);  {'HELLOTHIS IS A0 TEST'}
  Insert (',', S, 6);       {'HELLO,THIS IS A0 TEST'}
  Delete (S, Pos ('TEST', S) - 2, 1); {'HELLO,THIS IS A TEST'}
  Delete (Q, 0, 32767);     {''}
  Q:=Q + ' ';             {' '}
  Insert (Q, S, 7);         {'HELLO, THIS IS A TEST'}
  Insert (Q, S, 255);       {'HELLO, THIS IS A TEST '}
  if (S=TestStr) and (Q=' ') and (V='1234567890') and
     (T='THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 1234567890') then
   TestOK:=true;
end;


procedure TestSpeed(Row,Len:byte);
var
  l      : longint;
  hstr,
  OrgStr : string;
begin
  HeadBen[Row]:=Len;
  OrgStr:='';
  while Length(OrgStr)<Len do
   OrgStr:=OrgStr+'aaaaaaaaaa';
  OrgStr:=Copy(OrgStr,1,Len);
  OrgStr[Len]:='b';
{Load/Store}
  t.Reset;
  t.Start;
  for l:=1to 5000*TestSize do
   HSTr:=OrgStr;
  t.Stop;
  inc(Total,t.MSec);
  LoadBen[Row]:=t.MSec;
{Concat}
  t.Reset;
  t.Start;
  for l:=1to 2000*TestSize do
   begin
     Hstr:='aaa';
     Hstr:=Hstr+OrgStr;
   end;
  t.Stop;
  inc(Total,t.MSec);
  ConcatBen[Row]:=t.MSec;
{Copy}
  t.Reset;
  t.Start;
  for l:=1to 2000*TestSize do
   HSTr:=Copy(OrgStr,1,Len);
  t.Stop;
  inc(Total,t.MSec);
  CopyBen[Row]:=t.MSec;
{Delete}
  t.Reset;
  t.Start;
  for l:=1to 2000*TestSize do
   begin
     Hstr:=OrgStr;
     Delete(HStr,1,9);
   end;
  t.Stop;
  inc(Total,t.MSec);
  DelBen[Row]:=t.MSec;
{Insert}
  t.Reset;
  t.Start;
  for l:=1to 1000*TestSize do
   begin
     Hstr:='aaa';
     Insert(OrgStr,hstr,2);
     Hstr:=OrgStr;
     Insert('aaaaaaaaaaaaa',hstr,9);
   end;
  t.Stop;
  inc(Total,t.MSec);
  InsBen[Row]:=t.MSec;
{Compare}
  t.Reset;
  t.Start;
  Hstr:='aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  for l:=1to 5000*TestSize do
   if OrgStr=Hstr then;
  t.Stop;
  inc(Total,t.MSec);
  CmpBen[Row]:=t.MSec;
{Mixed}
  t.Reset;
  t.Start;
  for l:=1 to 400*TestSize do
   begin
     hstr:=OrgStr;
     hstr:=Copy(hstr,1,30);
     Delete(hstr,5,40);
     hstr:=Copy(hstr,1,length(hstr));
     hstr:=hstr+'  ';
     Delete(hstr,length(hstr)-2,2);
     Insert('aaaaaaaaaaaaaaaaaaaaaaaaaaaa',hstr,10);
     Insert('aaaaaaaaaaaaaaaaaaaaaaaaaaaa',hstr,20);
     hstr:=Copy(hstr,1,length(hstr));
     hstr:=Copy(hstr,1,80)+'aaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbb';
     hstr:=hstr+OrgStr;
   end;
  t.Stop;
  inc(Total,t.MSec);
  MixBen[Row]:=t.MSec;
end;


procedure PutBen(const r:BenType);
var
  i : byte;
begin
  for i:=1to 8 do
   Write(r[i]:6);
  Writeln;
end;



begin
  t.Init;
  WriteLn ('String Function Compatibility and Speed Test');
  WriteLn;

  if TestOK then
   WriteLn('Test OK')
  else
   WriteLn('Test Failure!');

  if paramstr(1)='t' then
   halt;

  WriteLn;
  TestSpeed(1,10);
  TestSpeed(2,30);
  TestSpeed(3,50);
  TestSpeed(4,70);
  TestSpeed(5,100);
  TestSpeed(6,150);
  TestSpeed(7,200);
  TestSpeed(8,250);

  Write('Length      ');
  PutBen(HeadBen);
  WriteLn('------------------------------------------------------------------------------');
  Write('Load/Store  ');
  PutBen(LoadBen);
  Write('Concat      ');
  PutBen(ConcatBen);
  Write('Copy        ');
  PutBen(CopyBen);
  Write('Delete      ');
  PutBen(DelBen);
  Write('Insert      ');
  PutBen(InsBen);
  Write('Compare     ');
  PutBen(CmpBen);
  Write('Mixed       ');
  PutBen(MixBen);
  WriteLn('String-Benchmark avarage ',Total div 8,' ms');
end.
