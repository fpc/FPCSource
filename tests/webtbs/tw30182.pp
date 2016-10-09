program SmallTestInterfaceRTTI;

{$mode objfpc}{$H+}

uses
  classes, typinfo;

type
  IMyNewMPInterface = interface(IInvokable)
    ['{AA503475-0187-4108-8E27-41475F4EF818}']
    procedure TestStdCall(LongParaName: TObject; const B: string; var C: integer; out D: byte); stdcall;
  end;

var
  ti:PTypeInfo;
  td : PTypeData;
begin
  ti:=TypeInfo(IMyNewMPInterface);

  td := GetTypeData(ti);

  // this gives an error (e.g. wrong data) on aarch64.
  // after patch of ncgrtti.pas, data is correct (unit name)
  if ti^.Kind = tkInterface then
    begin
      writeln('IntfUnit: ',td^.IntfUnit);
      if td^.IntfUnit<>'SmallTestInterfaceRTTI' then
        halt(1);
    end
  else
    halt(2);
end.

