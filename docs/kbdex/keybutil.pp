unit keybutil;

Interface

Uses keyboard;

Type
  TKeyRecord = packed record
    KeyCode : Word;
    ShiftState, Flags : Byte;
  end;
 
Const 
  // Only use these strings. Should be used to localize key names.
  SShift       : Array [1..3] of string[5] = ('SHIFT','CTRL','ALT');
  SLeftRight   : Array [1..2] of string[5] = ('LEFT','RIGHT');
  SUnicodeChar : String = 'Unicode character ';
  SScanCode    : String = 'Key with scancode ';
  SUnknownFunctionKey : String = 'Unknown function key : ';
  SAnd         : String = 'AND';
  SKeyPad      : Array [0..($FF2F-kbdHome)] of string[6] = 
                 ('Home','Up','PgUp','Left',
                  'Middle','Right','End','Down',
                  'PgDn','Insert','Delete','',
                  '','','','');

Function ShiftStateToString(KeyEvent : TKeyEvent; UseLeftRight : Boolean) : String;
Function FunctionKeyName (KeyCode : Word) : String;
Function KeyEventToString(KeyEvent : TKeyEvent) : String;


Implementation

Procedure AddToString (Var S : String; Const A : String);

begin
  If Length(S)=0 then
    S:=A
  else
    S:=S+' '+A;  
end;

Function IntToStr(Int : Longint) : String;

begin
  Str(Int,IntToStr);  
end;
  
Function ShiftStateToString(KeyEvent : TKeyEvent; UseLeftRight : Boolean) : String;

Var
  S : Integer;
  T : String;
  
begin
  S:=GetKeyEventShiftState(KeyEvent);
  T:='';
  If (S and kbShift)<>0 then
    begin
    if UseLeftRight then
      case (S and kbShift) of
        kbShift      : AddToString(T,SLeftRight[1]+' '+SAnd+' '+SLeftRight[2]);
        kbLeftShift  : AddToString(T,SLeftRight[1]);
        kbRightShift : AddToString(T,SLeftRight[2]);
      end;
    AddToString(T,SShift[1]);
    end;
  If (S and kbCtrl)<>0 Then
    AddToString(T,SShift[2]);
  If (S and kbAlt)<>0 Then  
    AddToString(T,SShift[3]);
  ShiftStateToString:=T;  
end;

Function FunctionKeyName (KeyCode : Word) : String;

begin
  If ((KeyCode-KbdF1)<$1F) Then 
    FunctionKeyName:='F'+IntToStr((KeyCode-KbdF1+1))
  else
    begin
    If (KeyCode-kbdHome)<($2F-$1F) then
      FunctionKeyName:=SKeyPad[(KeyCode-kbdHome)]
    else
      FunctionKeyName:=SUnknownFunctionKey + IntToStr(KeyCode);
    end;  
end;

Function KeyEventToString(KeyEvent : TKeyEvent) : String;

Var
  T : String;

begin
  T:=ShiftStateToString(KeyEvent,False);
  Case GetKeyEventFlags(KeyEvent) of
    kbASCII   : AddToString(T,GetKeyEventChar(KeyEvent));
    kbUniCode : AddToString(T,SUniCodeChar+IntToStr(GetKeyEventUniCode(Keyevent)));
    kbFnKey   : AddToString(T,FunctionKeyName(GetKeyEventCode(KeyEvent)));
                // Not good, we need a GetKeyEventScanCode function !!
    kbPhys    : AddToString(T,SScanCode+IntToStr(KeyEvent and $ffff));
  end;
  KeyEventToString:=T;
end;
  
end. 
