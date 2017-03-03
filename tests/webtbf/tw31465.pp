{ %FAIL }

program tw31465;

{$MODE DELPHI}

uses
  FGL;

type
  THWAddr = record
  public type
    THWBytes = array [0..5] of Byte;
  private
    FValue: THWBytes;
  public
    class var Empty: THWAddr;
    class var Broadcast: THWAddr;
  end;

  TGWCache = class(TFPGList<THWAddr>);

begin

end.
