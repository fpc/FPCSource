{ This tests that class variables for the various helper kinds work correctly }

program thlp47;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

type
  TObjectHelper = class helper for TObject
  public
    class procedure Init;
  public class var
    Value: LongInt;
  end;

  TGuidHelper = record helper for TGuid
  public
    class procedure Init; static;
  public class var
    Value: LongInt;
  end;

  TLongIntHelper = type helper for LongInt
  public
    class procedure Init; static;
  public class var
    Value: LongInt;
  end;

class procedure TObjectHelper.Init;
begin
  Value := 42;
end;

class procedure TGuidHelper.Init;
begin
  Value := 21;
end;

class procedure TLongIntHelper.Init;
begin
  Value := 84;
end;

begin
  TObject.Init;
  if TObject.Value <> 42 then
    Halt(1);
  TGuid.Init;
  if TGuid.Value <> 21 then
    Halt(2);
  LongInt.Init;
  if LongInt.Value <> 84 then
    Halt(3);
end.
