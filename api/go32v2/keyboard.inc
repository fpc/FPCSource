{
  System independent keyboard interface for go32v2

<<<<<<< keyboard.inc
  $Id$

  WARNING this code needs %fs to contain the DOS memory selector
  don't forget to reload it after calling C functions
  that could change it PM
=======
  $Id$
>>>>>>> 1.3
}
uses
  go32;

procedure InitKeyboard;
begin
end;

procedure DoneKeyboard;
begin
end;

function GetKeyEvent: TKeyEvent;
var
  regs : trealregs;
begin
  if PendingKeyEvent<>0 then
   begin
     GetKeyEvent:=PendingKeyEvent;
     PendingKeyEvent:=0;
     exit;
   end;
  regs.ah:=$10;
  realintr($16,regs);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  GetKeyEvent:=regs.ax or ((mem[$40:$17] and $f) shl 16);
end;


function PollKeyEvent: TKeyEvent;
var
  regs : trealregs;
begin
  if PendingKeyEvent<>0 then
   exit(PendingKeyEvent);
  regs.ah:=$11;
  realintr($16,regs);
  if (regs.realflags and zeroflag<>0) then
   exit(0);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  PollKeyEvent:=regs.ax or ((mem[$40:$17] and $f) shl 16);
end;


function PollShiftStateEvent: TKeyEvent;
begin
  PollShiftStateEvent:=((mem[$40:$17] and $f) shl 16);
end;


{ Function key translation }
type
  TTranslationEntry = packed record
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
  Revision 1.1  2000-01-06 01:20:30  peter
    * moved out of packages/ back to topdir

  Revision 1.2  1999/12/10 12:42:26  pierre
   * several mods to handle different keyboard layouts

  Revision 1.3  1999/11/24 23:36:56  peter
    * moved to packages dir

  Revision 1.2  1998/12/12 19:13:00  peter
    * keyboard updates
    * make test target, make all only makes units

  Revision 1.1  1998/12/04 12:48:27  peter
    * moved some dirs

  Revision 1.3  1998/11/01 20:28:26  peter
    * fixed strange al=$e0 after int $16 call

  Revision 1.2  1998/10/28 21:18:24  peter
    * more fixes

  Revision 1.1  1998/10/26 11:31:47  peter
    + inital include files

}
