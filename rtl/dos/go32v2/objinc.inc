{---------------------------------------------------------------------------}
{  FileClose -> Platforms DOS              - Not checked                    }
{---------------------------------------------------------------------------}
FUNCTION FileClose (Handle: THandle): Word;
var
  regs : trealregs;
begin
  regs.realebx:=handle;
  regs.realeax:=$3e00;
  sysrealintr($21,regs);
  FileClose := 0;
end;

{---------------------------------------------------------------------------}
{  FileOpen -> Platforms DOS              - Checked 05May1998 CEC           }
{  Returns 0 on failure                                                     }
{---------------------------------------------------------------------------}
FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;
Var
var regs : trealregs;
BEGIN
         DosStreamError:=0;
         syscopytodos(longint(@FileName),256);
         { get linear address from system unit }
         regs.realedx:=tb mod 16;
         regs.realds:=tb div 16;
         regs.realeax := Mode;
         regs.realecx:=0;
         sysrealintr($21,regs);
         if (regs.realflags and 1) <> 0 then
           begin
             InOutRes:=lo(regs.realeax);
             FileOpen:=$0;
             exit;
           end
         else
            { word handle (under DOS) }
            FileOpen:=regs.realeax and $ffff;
END;

{---------------------------------------------------------------------------}
{  FileWrite -> Platforms DOS              - Checked 05May1998 CEC          }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
BEGIN
 system.do_write(longint(Handle),longint(@Buf),Count);
 Actual:=Count;
 FileWrite:=InOutRes;
End;


{---------------------------------------------------------------------------}
{  SetFileSize -> Platforms DOS          - Not Checked                      }
{---------------------------------------------------------------------------}
FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
VAR Actual, Buf: LongInt;
BEGIN
   If (Actual = FileSize) Then Begin                  { No position error }
     Actual := FileWrite(Handle, Pointer(@Buf), 0,Actual);   { Truncate the file }
     If (Actual <> -1) Then SetFileSize := 0 Else     { No truncate error }
       SetFileSize := 103;                            { File truncate error }
   End Else SetFileSize := 103;                       { File truncate error }
END;


{---------------------------------------------------------------------------}
{  FileRead -> Platforms DOS              - Checked 05May1998 CEC           }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
BEGIN
  Actual:=system.do_read(longint(Handle),longint(@Buf),Count);
  FileRead:=InOutRes;
End;



{***************************************************************************}
{  DosSetFilePtr -> Platforms DOS          - Checked 05May1998 CEC          }
{***************************************************************************}
{=DosSetFilePtr======================================================
Calls the operating system to move the file denoted by the handle to
to the requested position. The move method can be: 0 = absolute offset;
1 = offset from present location; 2 = offset from end of file;
Any error is held in DosErrorStream and returned from the call.
If the return is zero (ie no error) NewPos contains the new absolute
file position.
-> Platforms DOS/DPMI/WIN - Checked 16May96 LdB}

FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
Var
 regs: Trealregs;
const
 CarryFlag = $001;
BEGIN
  regs.realeax := ($42 shl 8) + Byte(MoveType);
{  regs.realah := $42;
  regs.realal := Byte(MoveType); }
  regs.realedx := pos and $ffff;   { keep low word }
  regs.realecx := pos shr 16;
  regs.realebx := longint(Handle);
  sysrealintr($21,regs);
  if (regs.RealFlags and CarryFlag = 0) then { no error }
     Actual:=(regs.realeax and $ffff) + ((regs.realedx and $ffff) shl 16)
  else
     DosStreamError:=word(regs.realeax);

  SetFilePos := DosStreamError;                   { Return any error }
END;

