uses
  Keyboard;

function hexstr(val : longint;cnt : byte) : string;
const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
var
  i : longint;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;

var
  Key: TKeyEvent;
  Chr: Char;

begin
  InitKeyboard;
  Chr := #0;
  while Chr <> #27 do begin
    Key := GetKeyEvent;
    writeln('KeyEvent: ',hexstr(key,8));
    Key:=translatekeyevent(key);
    if IsFunctionKey(Key) then begin
      WriteLn('Function key was pressed, Code: ', GetKeyEventCode(Key));
     end
     else begin
      Chr := GetKeyEventChar(Key);
      WriteLn('Normal key was pressed, character: ', Chr, ' (', Ord(Chr), ')');
    end;
  end;
  DoneKeyboard;
end.
