{---------------------------------------------------------------------------}
{  FileClose -> Platforms DOS              - Not checked                    }
{---------------------------------------------------------------------------}
FUNCTION FileClose(Handle: THandle): word;
begin
   asm
      xor  %bx,%bx
      movw handle,%bx
      movb $0x3e,%ah
      pushl %ebp
      intl $0x21
      popl %ebp
   end;
   FileClose := 0;
end;

{---------------------------------------------------------------------------}
{  FileOpen -> Platforms DOS              - Checked 05May1998 CEC           }
{  Returns 0 on failure                                                     }
{---------------------------------------------------------------------------}

FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;
begin
   asm
     xorl  %eax, %eax
     movw  %ax, DosStreamError
     movl  FileName, %edx
     xorw  %cx,%cx
     movw  Mode, %ax
     pushl %ebp
     int   $0x21
     popl  %ebp
     jnc   .Lexit1
     movw  %ax, DosStreamError                        { Hold Error  }
     xorl  %eax, %eax                                 { Open Failed }
   .Lexit1:
     movw  %ax, __RESULT
   END;
end;


{---------------------------------------------------------------------------}
{  FileRead -> Platforms DOS              - Checked 05May1998 CEC           }
{---------------------------------------------------------------------------}
FUNCTION FileRead (Handle: THandle; Var Buf; Count: Sw_Word;
Var Actual: Sw_Word): Word;
BEGIN
  asm
     movl count,%ecx
     movl buf,%edx
     xorl %ebx,%ebx
     movw handle,%bx
     movb $0x3f,%ah
     int $0x21
     jnc .LDOSREAD1
     movw %ax,DosStreamError
     xorl %eax,%eax
  .LDOSREAD1:
  end;
  Actual:=Count;
  FileRead:=DosStreamError;
end;



{---------------------------------------------------------------------------}
{  FileWrite -> Platforms DOS              - Checked 05May1998 CEC          }
{---------------------------------------------------------------------------}
FUNCTION FileWrite (Handle: THandle; Var Buf; Count: Sw_Word; Var Actual: Sw_Word): Word;
BEGIN
  Actual:=0;
  asm
     movl Count,%ecx
     movl buf,%edx
     xorl %ebx,%ebx
     movw Handle,%bx
     movb $0x40,%ah
     pushl %ebp
     int $0x21
     pop   %ebp
     jnc .LDOSWRITE1
     movw %ax,DosStreamError
  .LDOSWRITE1:
  end;
  Actual:=Count;
  FileWrite:=DosStreamError;
end;

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

{***************************************************************************}
{  DosSetFilePtr -> Platforms DOS          - Checked 05May1998 CEC          }
{***************************************************************************}
FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
Var Actual: LongInt): Word;
BEGIN
  asm
     movw MoveType, %ax;                              { Load move type }
     movb $0x42, %ah;
     movl pos, %edx;                              { Load file position }
     andl $0xffff,%edx                            { Only keep low word }
     movl pos, %ecx
     shrl $16,%ecx;
     movw Handle, %bx;                              { Load file handle }
     pushl %ebp;
     int $0x21;                                         { Position the file }
     popl %ebp;
     jc .Lexit4
     movl Actual,%edi                                { New position address }
     movw %ax, %bx;
     movw %dx, %ax;
     shll $0x10, %eax;                                   { Roll to high part }
     movw %bx, %ax;
     movl %eax, (%edi);                               { Update new position }
     xorl %eax, %eax;
   .Lexit4:
     movw %ax, DosStreamError                         { DOS error returned }
   END;
   SetFilePos := DosStreamError;                   { Return any error }
END;



