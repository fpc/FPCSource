{ %opt=-Sa }

{$mode objfpc}

program write_read_date;


// Problem - when write date and read date using binary object reader/writer,
//readed value is not same as writed

uses
  classes, sysutils, dateutils;


type
  // just for access protected ReadQWord
  TBinaryObjectReaderFake = class(TBinaryObjectReader)
  end;

procedure test;
var
  mS: TMemoryStream;
  mW: TBinaryObjectWriter;
  mR: TBinaryObjectReaderFake;
  mDateIn, mDateOut, mDateOut2: TDateTime;
  mQW: QWord;
begin
  // for date 41488.5270635417 is content of stream [$11, $50, $5C, $B4, $DD, $10, $42, $E4, $40]
  // which is OK, so with write is probably no problem
  mDateIn := 41488.5270635417;
  mS := TMemoryStream.Create;
  try
    //
    mW := TBinaryObjectWriter.Create(mS, 100);
    try
      mW.WriteDate(mDateIn);
    finally
      mW.Free
    end;
    // this will read bad date
    mS.Position := 0;
    mR := TBinaryObjectReaderFake.Create(mS, 100);
    try
      Assert(mR.ReadValue = vaDate);
      mDateOut := mR.ReadDate;
    finally
      mR.Free
    end;
    // when use ReadQWord, date is readed correctly
    mS.Position := 0;
    mR := TBinaryObjectReaderFake.Create(mS, 100);
    try
      Assert(mR.ReadValue = vaDate);
      mQW := mR.ReadQWord;
      // typecast will not help
      //mDateOut2 := TDateTime(mQW);
      Move(mQW, mDateOut2, SizeOf(mQW));
    finally
      mR.Free
    end;
  finally
    mS.Free;
  end;
  if CompareDateTime(mDateIn, mDateOut) <> 0 then
    begin
      writeln(qword(mDateIn),' <> ',qword(mDateOut));
      Writeln('read date is different from written date');
      halt(1);
    end
  else
    Writeln('read date is same as written date');
  if CompareDateTime(mDateIn, mDateOut2) <> 0 then
    begin
      Writeln('this situation should not happen');
      halt(2);
    end
  else
    Writeln('read date as QWord is same as written date')
end;

begin
  test;
end.

