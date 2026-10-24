{ out of range date/time values raise EConvertError, issue #34214 }
program tdtrange;

{$mode objfpc}{$h+}

uses
  SysUtils, DateUtils;

var
  lCode: Integer;

// Halt with the next code and write aMsg when aOk is false
procedure Check(aOk: Boolean; const aMsg: string);

begin
  Inc(lCode);
  if not aOk then
    begin
    writeln('check ',lCode,' failed: ',aMsg);
    halt(lCode);
    end;
end;


// Return true when DateTimeToStr of aValue raises EConvertError
function StrRaisesConvertError(aValue: TDateTime): Boolean;

begin
  Result:=False;
  try
    DateTimeToStr(aValue);
  except
    on EConvertError do
      Result:=True;
  end;
end;


// Return true when DateTimeToTimeStamp of aValue raises EConvertError
function StampRaisesConvertError(aValue: TDateTime): Boolean;

begin
  Result:=False;
  try
    DateTimeToTimeStamp(aValue);
  except
    on EConvertError do
      Result:=True;
  end;
end;


var
  lDT: TDateTime;
  lStamp: TTimeStamp;

begin
  lCode:=0;
  lDT:=IncHour(EncodeDateTime(2000,12,31,23,59,59,0),High(Int64));
  Check(StrRaisesConvertError(lDT),'DateTimeToStr of IncHour by High(Int64)');
  Check(StampRaisesConvertError(lDT),'DateTimeToTimeStamp of IncHour by High(Int64)');
  Check(StampRaisesConvertError(-lDT),'DateTimeToTimeStamp of a large negative value');
  Check(StampRaisesConvertError(MaxDateTime+1),'DateTimeToTimeStamp of MaxDateTime+1');
  Check(StampRaisesConvertError(MinDateTime-1),'DateTimeToTimeStamp of MinDateTime-1');

  lStamp:=DateTimeToTimeStamp(EncodeDateTime(9999,12,31,23,59,59,999));
  Check((lStamp.Date=3652059) and (lStamp.Time=86399999),'DateTimeToTimeStamp of 9999-12-31 23:59:59.999');
  lStamp:=DateTimeToTimeStamp(EncodeDateTime(1,1,1,0,0,0,0));
  Check((lStamp.Date=1) and (lStamp.Time=0),'DateTimeToTimeStamp of 0001-01-01');
  lStamp:=DateTimeToTimeStamp(EncodeDateTime(1,1,1,12,0,0,0));
  Check((lStamp.Date=1) and (lStamp.Time=43200000),'DateTimeToTimeStamp of 0001-01-01 12:00');
  Check(not StrRaisesConvertError(EncodeDateTime(2000,12,31,23,59,59,0)),'DateTimeToStr of a valid value');
  writeln('ok');
end.
