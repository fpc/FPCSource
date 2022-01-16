{ this program just links all externals, declared in the xtestext1 unit }
program xtestext1_linktest;
uses
  xtestext1;
begin
  halt(0);
  XTestFakeInput(nil,nil,0,0);
  XTestGetInput(nil,0);
  XTestQueryInputSize(nil,nil);
  XTestPressKey(nil,0,0,0,0);
  XTestPressButton(nil,0,0,0,0);
  XTestMovePointer(nil,0,nil,nil,nil,0);
  XTestFlush(nil);
  XTestStopInput(nil);
  XTestReset(nil);
end.
