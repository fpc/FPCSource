{ %Fail }
{ new with type  of size > 2Gb }

{ Source provided for Free Pascal Bug Report 2053 }
{ Submitted by "Luis Castedo" on  2002-07-24 }
{ e-mail: castedo@elai.upm.es }
program tb2;

{$MODE TP}
{$C+}

type

  PosInteger = 0 .. high(integer);
  Posptrint = 0 .. high(ptrint);

  TMyRecord = record
    mr_sglDummy1: array[0..3] of Single;
    mr_lDummy2  : ptrint;
    mr_iDummy3  : Integer;
    mr_iDummy4  : Integer;
  end;

  TMyRecordArray = array[PosInteger] of TMyRecord;
  TMyLongRecordArray = array[ptrint] of TMyRecord;
  PMyRecordArray = ^TMyRecordArray;
  PMyLongRecordArray = ^TMyLongRecordArray;

var

  pArray: PMyRecordArray;
  pLongArray : PMyLongRecordArray;
  size : ptrint;

begin

  new(parray);
  new(pLongArray);
  size:= 50 * SizeOf(TMyRecord);
  GetMem(pArray, size);
  FillChar(pArray^,size,#0);
  GetMem(pLongArray, 50 * SizeOf(TMyRecord));
  FillChar(pLongArray^,size,#0);

  Assert(Assigned(pArray));

  WriteLn('pArray = ', ptrint(pArray));
  WriteLn('@(pArray^[0]) = ', ptrint(@(pArray^[0])));
  pArray^[0].mr_lDummy2 := 24;

  if (pArray^[0].mr_lDummy2<>24) then
    begin
      Halt(1);
    end;

  WriteLn('pLongArray = ', ptrint(pLongArray));
  WriteLn('@(pLongArray^[0]) = ', ptrint(@(pLongArray^[0])));
  pLongArray^[0].mr_lDummy2 := 25;

  if (pLongArray^[0].mr_lDummy2<>25) then
    begin
      Halt(1);
    end;

  FreeMem(pArray, size);
  FreeMem(pLongArray, size);

end.
