unit uverylongunitname2_123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345;
{$mode delphi}
(* This is a copy - This is deliberate
   This is used by "tlongname2" compiled with -gw3
   The other copy is compiled with -gw / Both tests need their own unit, to avoid re-using ppu
*)
interface
type
  AVeryLongTypeNameThatIsJustOwerAOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TFoo = class
  public type    AVeryLongTypeNameThatIsJustOwerAOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TFoo = class
    x: integer;
    end;
  end;
  // nested to trigger make_mangledname's loop over objectdef/recorddef
  AVeryLongUnitNameThatIsJustAboutOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TBar = class helper for AVeryLongTypeNameThatIsJustOwerAOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TFoo.AVeryLongTypeNameThatIsJustOwerAOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TFoo
    procedure p;
  end;

var v: AVeryLongTypeNameThatIsJustOwerAOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TFoo.AVeryLongTypeNameThatIsJustOwerAOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TFoo;


implementation

procedure AVeryLongUnitNameThatIsJustAboutOneHundredCharactersLongToReproduceTheDuplicateDebugLabelsInFPCTrunk11111111111111111111123_TBar.p;
begin
  //
end;

initialization
  // use it, to ensure the code isn't omitted
  v.p;
end.
