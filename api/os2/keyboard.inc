{
  System independent keyboard interface for os2

  $Id$
}
uses
{$IFDEF PPC_FPC}
 KbdCalls, DosCalls;
{$ELSE}
 {$IFDEF PPC_VIRTUAL}
 OS2Base;
 {$ELSE}
  {$IFDEF PPC_BPOS2}
 Os2Subs, DosProcs;
  {$ELSE}
   {$IFDEF PPC_SPEED}
 BseSub, BseDos;
   {$ENDIF}
  {$ENDIF}
 {$ENDIF} 
{$ENDIF}

type
{$IFDEF PPC_VIRTUAL}
 TKbdKeyInfo = KbdKeyInfo;
 TKbdInfo = KbdInfo;
{$ELSE}
 {$IFDEF PPC_SPEED}
 TKbdKeyInfo = KbdKeyInfo;
 TKbdInfo = KbdInfo;
 {$ENDIF}
{$ENDIF}

const
 DefaultKeyboard = 0;
 Handle: word = DefaultKeyboard;

procedure InitKeyboard;
var
 K: TKbdInfo;
begin
 if KbdGetFocus (IO_Wait, DefaultKeyboard) = No_Error then
 begin
  if KbdOpen (Handle) <> No_Error then Handle := DefaultKeyboard;
  KbdFlushBuffer (Handle);
  KbdFreeFocus (DefaultKeyboard);
  KbdGetFocus (IO_Wait, Handle);
  K.cb := 10;
  KbdGetStatus (K, Handle);
  K.fsMask := $14;
  KbdSetStatus (K, Handle);
 end;
end;

procedure DoneKeyboard;
begin
 KbdFreeFocus (Handle);
 if KbdGetFocus (IO_Wait, DefaultKeyboard) = No_Error then
 begin
  KbdClose (Handle);
  Handle := DefaultKeyboard;
  KbdFreeFocus (DefaultKeyboard);
 end;
end;

function GetKeyEvent: TKeyEvent;
var
 K: TKbdKeyInfo;
 RC: word;
begin
 if PendingKeyEvent <> 0 then
 begin
  GetKeyEvent := PendingKeyEvent;
  PendingKeyEvent := 0;
 end else
 begin
  KbdGetFocus (IO_Wait, Handle);
  while (KbdCharIn (K, IO_Wait, Handle) <> No_Error)
                                   or (K.fbStatus and $40 = 0) do DosSleep (5);
  with K do GetKeyEvent := cardinal (fsState or $F) shl 16 or
                               cardinal (byte (chScan)) shl 8 or byte (chChar);
 end;
end;

function PollKeyEvent: TKeyEvent;
begin
 if PendingKeyEvent <> 0 then PollKeyEvent := PendingKeyEvent else
 begin
{  regs.ah:=$11;
  realintr($16,regs);
  if (regs.realflags and zeroflag<>0) then
   exit(0);
  if (regs.al=$e0) and (regs.ah<>0) then
   regs.al:=0;
  PollKeyEvent:=regs.ax or ((mem[$40:$17] and $f) shl 16);
}
 end;
end;

function PollShiftStateEvent: TKeyEvent;
begin
end;

function TranslateKeyEvent(KeyEvent: TKeyEvent): TKeyEvent;
begin
end;

function TranslateKeyEventUniCode(KeyEvent: TKeyEvent): TKeyEvent;
begin
end;

{
  $Log$
  Revision 1.2  2000-01-09 20:42:05  hajny
    + the first part of implementation

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:32  peter
    * moved some dirs

  Revision 1.1  1998/10/26 11:31:49  peter
    + inital include files

}
