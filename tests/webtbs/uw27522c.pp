{$MODE OBJFPC} { -*- delphi -*- }
{$CODEPAGE UTF8}
{$MODESWITCH ADVANCEDRECORDS+}
unit uw27522c;

interface

type
   TTest3 = record
     Value: Integer;
     function TestMethod(): UTF8String;
   end;

implementation

uses uw27522b, sysutils, uw27522a;

function TTest3.TestMethod(): UTF8String;
begin
    Result := '' + IntToStr(Value);
end;

end.