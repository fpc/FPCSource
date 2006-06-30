unit uw6203;

{$mode delphi}

interface

const
  CM_TEST = $B000 + 18;

type
  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0: (
        WParam: Longint;
        LParam: Longint;
        Result: Longint);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
  end;

  TTest = class
  private
    procedure CMTest(var Msg: TMessage); message CM_TEST;
  end;

var
  Err : boolean;
  
implementation

procedure TTest.CMTest(var Msg: TMessage);
begin
  WriteLn('TTest.CMTest');
  err:=false;
end;

end.

