{ %TARGET=linux,android }
{ %OPT=-Cg }
{ This test uncovered code generation bug on MIPS when addressing a large global record
  in position-independent way, but of course it cannot hurt to test other CPUs. }
unit tw30552;

interface

type
  TMyInnerRecord = packed record
                          end;
  PMyInnerRecord = ^TMyInnerRecord;
  TMyRecord = record
    ALotOfData: array[0..10000] of Cardinal;
    MyPointer: PMyInnerRecord;
  end;
var
  MyGlobalVariable: TMyRecord;

implementation

procedure TestProcedure;
begin
  with MyGlobalVariable.MyPointer^ do begin
    end;
end;

begin {main}
end.
