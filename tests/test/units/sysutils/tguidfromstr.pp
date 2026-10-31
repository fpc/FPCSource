{ TGUID.FromString and StringToGUID, also with the unicode RTL, issue #41971 }
program tguidfromstr;

{$mode objfpc}{$h+}

uses
  SysUtils;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


const
  cGUID = '{01234567-89AB-CDEF-0123-456789ABCDEF}';

var
  lGUID, lNew: TGUID;

begin
  Check(1,TGUID.FromString(cGUID,lGUID),'TGUID.FromString accepts a valid GUID');
  Check(2,GUIDToString(lGUID)=cGUID,'GUIDToString(TGUID.FromString) gives the original string');
  Check(3,lGUID.D1=$01234567,'D1 of the parsed GUID');
  Check(4,(lGUID.D4[0]=$01) and (lGUID.D4[7]=$EF),'D4 of the parsed GUID');
  Check(5,TGUID.FromString('{01234567-89ab-cdef-0123-456789abcdef}',lNew) and IsEqualGUID(lGUID,lNew),
    'TGUID.FromString accepts lower case digits');
  Check(6,not TGUID.FromString('{0123456G-89AB-CDEF-0123-456789ABCDEF}',lNew),'TGUID.FromString rejects a non-hex digit');
  Check(7,not TGUID.FromString('{01234567+89AB-CDEF-0123-456789ABCDEF}',lNew),'TGUID.FromString rejects a missing dash');
  Check(8,not TGUID.FromString('{01234567-89AB-CDEF-0123-456789ABCDE}',lNew),'TGUID.FromString rejects a short string');
  Check(10,IsEqualGUID(StringToGUID(cGUID),lGUID),'StringToGUID parses a valid GUID');
  Check(11,TryStringToGUID(cGUID,lNew) and IsEqualGUID(lNew,lGUID),'TryStringToGUID parses a valid GUID');
  Check(12,not TryStringToGUID('{XYZ}',lNew),'TryStringToGUID rejects an invalid GUID');
  Check(20,CreateGUID(lNew)=0,'CreateGUID succeeds');
  Check(21,IsEqualGUID(StringToGUID(GUIDToString(lNew)),lNew),'a created GUID survives a round trip');
  writeln('ok');
end.
