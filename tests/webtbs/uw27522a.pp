{$MODE OBJFPC} { -*- delphi -*- }
{$CODEPAGE UTF8}
unit uw27522a;

interface

uses
   uw27522c;

type
   Test1Record = record
     case Integer of
        0: (Integer1, Integer2, Integer3: Integer);
        1: (AsString: ShortString);
   end;
   operator + (const Op1: UTF8String; const Op2: Test1Record): UTF8String;

implementation

uses uw27522b, sysutils;

operator + (const Op1: UTF8String; const Op2: Test1Record): UTF8String;
begin
   Result := '';
end;

function Test1A(const Value: TTest3): Test1Record;
begin
end;

function Test1B(const Value: TTest3): Integer;
begin
   Writeln('' + IntToStr(Value.Value));
end;

end.