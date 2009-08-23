program keyboardStdin;

{$mode objfpc}

uses
  ctypes, nds9;
  
procedure OnKeyPressed(key: cint);
begin
  if (key > 0) then
    iprintf('%c', key);
end;

var
  kbd: pKeyboard;
  myName: array [0..255] of char;
begin
  consoleDemoInit();

  kbd :=  keyboardDemoInit();

  kbd^.OnKeyPressed := @OnKeyPressed;

  while true do
  begin
    iprintf('What is your name?'#10);
    
    scanf('%s', myName);
    
    iprintf(#10'Hello %s', myName);
    
    scanKeys();
    while (keysDown() = 0)do 
      scanKeys();
    
    swiWaitForVBlank();
    consoleClear();
   end;

end.
