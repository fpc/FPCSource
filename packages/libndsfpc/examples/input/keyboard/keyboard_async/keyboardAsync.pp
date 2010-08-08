program keyboardAsync;

{$mode objfpc}

uses
  ctypes, nds9;
  
var
  key: integer;

begin
  consoleDemoInit();  //setup the sub screen for printing
  
  keyboardDemoInit();
  
  keyboardShow();
  
  while true do
  begin
  
    key := keyboardUpdate();
  
    if (key > 0) then
      iprintf('%c', key);
  
    swiWaitForVBlank();
  end;

end.
