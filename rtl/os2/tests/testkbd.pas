program TestKBD;
{$X+}

uses
{$IFDEF FPK}
 KbdCalls;
{$ELSE}
 Os2Base, Os2Def;
{$ENDIF}

function ExtKeyPressed: boolean;       (* 'key' is here as well e.g. a shift *)
var
 C: char;
{$IFDEF VIRTUALPASCAL}
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
{ FillChar (KI, SizeOf (KI), 0);
 KbdCharIn (KI, IO_NOWAIT, 0);}
 ExtKeyPressed :=
{ (KI.chScan <> 0) and (KI.chScan and $80 = 0) or }
                                                    (K.fsState and $FF0F <> 0);
end;

begin
 WriteLn ('Press any _shift_ (or Alt, Ctrl etc.) key to continue ...');
 repeat until ExtKeyPressed;
end.
