program TestShortStr;
uses timer;

const
  TestSize=1000; {Use at least 10 for reasonable results}
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

procedure TestSpeed(Row,Len:byte);
var
  l      : longint;
  hstr,
  OrgStr : shortstring;
begin
  HeadBen[Row]:=Len;
  OrgStr:='';
  while Length(OrgStr)<Len do
   OrgStr:=OrgStr+'aaaaaaaaaa';
  OrgStr:=Copy(OrgStr,1,Len);
  OrgStr[Len]:='b';
{Load/Store}
  Start;
  for l:=1to 5000*TestSize do
   HSTr:=OrgStr;
  Stop;
  inc(Total,MSec);
  LoadBen[Row]:=MSec;
{Concat}
  Start;
  for l:=1to 2000*TestSize do
   begin
     Hstr:='aaa';
     Hstr:=Hstr+OrgStr;
   end;
  Stop;
  inc(Total,MSec);
  ConcatBen[Row]:=MSec;
{Copy}
  Start;
  for l:=1to 2000*TestSize do
   HSTr:=Copy(OrgStr,1,Len);
  Stop;
  inc(Total,MSec);
  CopyBen[Row]:=MSec;
{Delete}
  Start;
  for l:=1to 2000*TestSize do
   begin
     Hstr:=OrgStr;
     Delete(HStr,1,9);
   end;
  Stop;
  inc(Total,MSec);
  DelBen[Row]:=MSec;
{Insert}
  Start;
  for l:=1to 1000*TestSize do
   begin
     Hstr:='aaa';
     Insert(OrgStr,hstr,2);
     Hstr:=OrgStr;
     Insert('aaaaaaaaaaaaa',hstr,9);
   end;
  Stop;
  inc(Total,MSec);
  InsBen[Row]:=MSec;
{Compare}
  Start;
  Hstr:='aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'+
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  for l:=1to 5000*TestSize do
   if OrgStr=Hstr then;
  Stop;
  inc(Total,MSec);
  CmpBen[Row]:=MSec;
{Mixed}
  Start;
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
  Stop;
  inc(Total,MSec);
  MixBen[Row]:=MSec;
end;


procedure PutBen(const r:BenType);
var
  i : byte;
  rtot : cardinal;
begin
  rtot:=0;
  for i:=1to 8 do
    begin
      inc(rtot,r[i]);
      Write(r[i]:6);
    end;
  Write('':4);  
  Write('avg=',rtot div 8);    
  Writeln;
end;



begin
  VerboseTimer:=false;
  WriteLn ('Shortstring Speed Test');

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
  WriteLn('Shortstring-Benchmark avarage ',Total div 8,' ms');
end.
