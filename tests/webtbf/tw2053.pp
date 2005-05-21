{ %FAIL }
{ Source provided for Free Pascal Bug Report 2053 }
{ Submitted by "Luis Castedo" on  2002-07-24 }
{ e-mail: castedo@elai.upm.es }
program tb2;

{$MODE TP}
{$C+}

type

  TMyRecord = record
    mr_sglDummy1: array[0..3] of Single;
    mr_lDummy2  : ptrint;
    mr_iDummy3  : Integer;
    mr_iDummy4  : Integer;
  end;

{  TMyRecordArray = array[Integer] of TMyRecord;} { Error }
  TMyRecordArray = array[Ptrint] of TMyRecord; { OK }
  PMyRecordArray = ^TMyRecordArray;

var

  pArray: PMyRecordArray;

begin
  GetMem(pArray, 50 * SizeOf(TMyRecord));
  Assert(Assigned(pArray));

  WriteLn('pArray = ', ptrint(pArray));
  WriteLn('@(pArray^[0]) = ', ptrint(@(pArray^[0])));
  pArray^[0].mr_lDummy2 := 0;

  FreeMem(pArray, 50 * SizeOf(TMyRecord));

end.
