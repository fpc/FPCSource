{
  System independent keyboard interface for tp7

  $Id$
}

procedure InitKeyboard;
begin
end;

procedure DoneKeyboard;
begin
end;

function GetKeyEvent: TKeyEvent; assembler;
asm
        mov     ax,[word ptr PendingKeyEvent+0]
        or      ax,[word ptr PendingKeyEvent+2]
        je      @@1
        mov     ax,[word ptr PendingKeyEvent+0]
        mov     dx,[word ptr PendingKeyEvent+2]
        mov     [word ptr PendingKeyEvent+0],0
        mov     [word ptr PendingKeyEvent+2],0
        jmp     @@99
@@1:
        mov     ax,40h
        mov     es,ax
        mov     ah, 10h         { get extended key }
        int     16h
        mov     dh,3
        mov     dl,[byte ptr es:17h] { shift state }
        and     dl,0fh
        cmp     al,0e0h
        jne     @@2
        or      ah,ah
        jz      @@2
        mov     al,0
@@2:
@@99:
end;


function PollKeyEvent: TKeyEvent; assembler;
asm
        mov     ax,[word ptr PendingKeyEvent+0]
        or      ax,[word ptr PendingKeyEvent+2]
        je      @@1
        mov     ax,[word ptr PendingKeyEvent+0]
        mov     dx,[word ptr PendingKeyEvent+2]
        jmp     @@2
@@1:
        mov     ax,40h
        mov     es,ax
        mov     ah,11h
        int     16h
        jnz     @@2
        xor     ax,ax
        xor     dx,dx
        jmp     @@99
@@2:    mov     dh,3
        mov     dl,[byte ptr es:17h] { shift state }
        and     dl,0fh
        cmp     al,0e0h
        jne     @@3
        or      ah,ah
        jz      @@3
        mov     al,0
@@3:
@@99:
end;


function PollShiftStateEvent: TKeyEvent;assembler;
asm
        mov     ax,40h
        mov     es,ax
        xor     dx,dx
        xor     ax,ax
        mov     dl,byte ptr es:[17h] { shift state}
        and     dl,0fh
end;


{ Function key translation }
type
  TTranslationEntry = record
    Min, Max: Byte;
    Offset: Word;
  end;

const
  TranslationTableEntries = 12;
  TranslationTable: array [1..TranslationTableEntries] of TTranslationEntry =
    ((Min: $3B; Max: $44; Offset: kbdF1),   { function keys F1-F10 }
     (Min: $54; Max: $5D; Offset: kbdF1),   { Shift fn keys F1-F10 }
     (Min: $5E; Max: $67; Offset: kbdF1),   { Ctrl fn keys F1-F10 }
     (Min: $68; Max: $71; Offset: kbdF1),   { Alt fn keys F1-F10 }
     (Min: $85; Max: $86; Offset: kbdF11),  { function keys F11-F12 }
     (Min: $87; Max: $88; Offset: kbdF11),  { Shift+function keys F11-F12 }
     (Min: $89; Max: $8A; Offset: kbdF11),  { Ctrl+function keys F11-F12 }
     (Min: $8B; Max: $8C; Offset: kbdF11),  { Alt+function keys F11-F12 }
     (Min:  71; Max:  73; Offset: kbdHome), { Keypad keys kbdHome-kbdPgUp }
     (Min:  75; Max:  77; Offset: kbdLeft), { Keypad keys kbdLeft-kbdRight }
     (Min:  79; Max:  81; Offset: kbdEnd),  { Keypad keys kbdEnd-kbdPgDn }
     (Min: $52; Max: $53; Offset: kbdInsert));


function TranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
var
  I: Integer;
  ScanCode: Byte;
begin
  if KeyEvent and $03000000 = $03000000 then
   begin
     if KeyEvent and $000000FF <> 0 then
      begin
        TranslateKeyEvent := KeyEvent and $00FFFFFF;
        exit;
      end
     else
      begin
        { This is a function key }
        ScanCode := (KeyEvent and $0000FF00) shr 8;
        for I := 1 to TranslationTableEntries do
         begin
           if (TranslationTable[I].Min <= ScanCode) and (ScanCode <= TranslationTable[I].Max) then
            begin
              TranslateKeyEvent := $02000000 + (KeyEvent and $00FF0000) +
                (ScanCode - TranslationTable[I].Min) + TranslationTable[I].Offset;
              exit;
            end;
         end;
      end;
   end;
  TranslateKeyEvent := KeyEvent;
end;

function TranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;
begin
  TranslateKeyEventUniCode := KeyEvent;
  ErrorHandler(errKbdNotImplemented, nil);
end;

{
  $Log$
  Revision 1.1  2000-07-13 06:29:41  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.2  1998/12/12 19:13:13  peter
    * keyboard updates
    * make test target, make all only makes units

  Revision 1.1  1998/12/04 12:48:57  peter
    * moved some dirs

  Revision 1.3  1998/11/01 20:28:25  peter
    * fixed strange al=$e0 after int $16 call

  Revision 1.2  1998/10/28 21:18:27  peter
    * more fixes

  Revision 1.1  1998/10/26 11:31:49  peter
    + inital include files

}
