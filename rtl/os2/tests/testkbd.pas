program TestKBD;
{$X+}

{$IFNDEF OS2}
 Sorry, this code is for OS/2 only...
{$ENDIF}

uses
{$IFDEF FPC}
 KbdCalls;
{$ELSE}
 {$IFDEF SPEED}
 BseSub;
 {$ELSE}
 Os2Subs;
 {$ENDIF}
{$ENDIF}

function ExtKeyPressed: boolean;       (* 'key' is here as well e.g. a shift *)
var
{$IFNDEF VER70} (* patched Borland Pascal *)
 KI: KbdKeyInfo;
 K: KbdInfo;
{$ELSE}
 KI: TKbdKeyInfo;
 K: TKbdInfo;
{$ENDIF}
 B: boolean;
begin
 B := false;
 K.cb := SizeOf (K);
 KbdGetStatus (K, 0);
 KbdPeek (KI, 0);
 if (KI.fbStatus and $FE <> 0) or (K.fsState and $FF0F <> 0) then
 begin
  ExtKeyPressed := true;
  if KI.fbStatus and $FE <> 0 then KbdCharIn (KI, IO_NOWAIT, 0);
 end else ExtKeyPressed := false;
end;

begin
 repeat until not (ExtKeyPressed);
 WriteLn (#13#10'Press _any_ key to continue (including shifts etc.) ...');
 repeat until ExtKeyPressed;
end.
