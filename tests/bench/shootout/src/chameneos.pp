{ The Computer Language Shootout
  http://shootout.alioth.debian.org
  contributed by Marc Weustink
}
program chameneos;
{$mode objfpc}{$h-}
uses
  PThreads;

type
  TColor = (Blue, Red, Yellow, Faded);

var
  waitfirst,
  waitsecond   : TSemaphore;
  first,second : TColor;
  MeetingsLeft : Integer;
  ThreadInfo   : array[0..3] of record
    Id: TThreadId;
    StartColor: TColor;
    Count: Integer;
  end;


function Complement(c1,c2:TColor):TColor;
begin
  if c2=Faded then
    begin
      result:=Faded;
      exit;
    end;
  if c1=c2 then
    begin
      result:=c1;
      exit;
    end;
  case c1 of
    Blue :
      if c2=Red then
        result:=Yellow
      else
        result:=Red;
    Red :
      if c2=Blue then
        result:=Yellow
      else
        result:=Blue;
    Yellow :
      if c2=Blue then
        result:=Red
      else
        result:=Blue;
    else
      result:=c1;
  end;
end;


function OtherCreaturesColor(me:TColor):TColor;
const
  firstcall : boolean = true;
begin
  result:=Faded;
  sem_wait(waitfirst);

  if firstCall then
    begin
      if MeetingsLeft>0 then
        begin
          first:=me;
          firstcall:=false;
          sem_post(waitfirst);
          sem_wait(waitsecond);
          result:=second;
          dec(MeetingsLeft);
        end;
      sem_post(waitfirst);
    end
  else
    begin
      firstcall:=true;
      second:=me;
      result:=first;
      sem_post(waitsecond);
    end;
end;


function ThreadFunc(AIndex: PtrInt): Pointer; cdecl;
var
  Meetings : Integer;
  me,other : TColor;
begin
  me := ThreadInfo[AIndex].StartColor;
  Meetings := 0;

  while (me<>Faded) do
    begin
      other:=OtherCreaturesColor(me);
      if other=Faded then
        break;
      inc(Meetings);
      me:=Complement(me,other);
    end;

  ThreadInfo[AIndex].Count := Meetings;
  result:=nil;
end;


const
  COLOR: array[0..3] of TColor = (Blue, Red, Yellow, Blue);

var
  n: Integer;
  Attr: TThreadAttr;
  p: Pointer;
begin
  Val(paramstr(1), MeetingsLeft, n);
  if n <> 0 then exit;

  sem_init(waitfirst,0,1);
  sem_init(waitsecond,0,0);

  pthread_attr_init(Attr);
  pthread_attr_setdetachstate(Attr, 0);
  pthread_attr_setstacksize(Attr, 1024 * 16);

  for n := 0 to 3 do begin
    ThreadInfo[n].Count := 0;
    ThreadInfo[n].StartColor := COLOR[n];
    pthread_create(ThreadInfo[n].Id, Attr, TStartRoutine(@ThreadFunc), Pointer(n));
  end;

  for n := 0 to 3 do
    pthread_join(ThreadInfo[n].Id, p);

  WriteLN(ThreadInfo[0].Count + ThreadInfo[1].Count + ThreadInfo[2].Count + ThreadInfo[3].Count);
end.
