{
  System independent filecontrol interface for tp7

  $Id$
}

{ no known 16 bit compilers support overriding }

function OpenFileStr(FName: PChar; Flags: Longint): TFileHandle; assembler;
asm
@@Retry:
        push    ds
        lds     dx,FName
        mov     al,byte ptr Flags
        mov     ah,3dh
        int     21h
        pop     ds
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax      { ErrorCode: Longint }
        les     dx,FName
        push    es
        push    dx      { FName as ErrorInfo }
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
        mov     ax,-1
@@1:
end;

function CreateFileStr(FName: PChar): TFileHandle; assembler;
asm
@@Retry:
        push    ds
        lds     dx,FName
        mov     cl,20h
        xor     ch,ch
        mov     ah,3Ch
        int     21h
        pop     ds
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        les     dx,FName
        push    es
        push    dx              { FName as errorinfo }
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
        mov     ax,-1
@@1:
end;

procedure DeleteFileStr(FName: PChar); assembler;
asm
@@Retry:
        push    ds
        lds     dx,FName
        mov     AH,41h
        int     21h
        pop     ds
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        les     dx,FName
        push    es
        push    dx
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
@@1:
end;

procedure CloseFile(Handle: TFileHandle); assembler;
asm
@@Retry:
        mov     bx,Handle
        mov     ah,3eh
        int     21h
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        push    ax
        push    ax
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
@@1:
end;

function SeekFile(Handle: TFileHandle; Pos: TFileInt; SeekType: Word): TFileInt; assembler;
asm
@@Retry:
        mov     ah,42H
        mov     bx,Handle
        mov     dx,word ptr Pos[0]
        mov     cx,word ptr Pos[2]
        mov     al,byte ptr SeekType
        int     21h
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        push    ax
        push    ax
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
        mov     ax,-1
        mov     dx,-1
@@1:
end;

function ReadFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord; assembler;
asm
@@Retry:
        push    ds
        lds     dx,Buff
        mov     cx,Count
        mov     bx,Handle
        mov     ah,3fh
        int     21h
        pop     ds
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        push    ax
        push    ax
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
        xor     ax,ax
@@1:
end;

function WriteFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord; assembler;
asm
@@Retry:
        push    ds
        lds     dx,Buff
        mov     cx,Count
        mov     bx,Handle
        mov     ah,40h
        int     21h
        pop     ds
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        push    ax
        push    ax
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
        xor     ax,ax
@@1:
end;

procedure FlushFile(Handle: TFileHandle); assembler;
asm
@@Retry:
        mov     bx,Handle
        mov     ah,68H
        int     21h
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        push    ax
        push    ax
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
@@1:
end;

procedure TruncateFile(Handle: TFileHandle);
begin
  WriteFile(Handle, Handle, 0);
end;

function EndOfFile(Handle: TFileHandle): Boolean; assembler;
asm
@@Retry:
        mov     ax,4400h
        mov     bx,Handle
        int     21h
        jnc     @@1
        push    ax
        mov     ax,0
        push    ax
        push    ax
        push    ax
        call    [ErrorHandler]
        cmp     ax,errRetry
        je      @@Retry
        jmp     @@3      { Set result to 1, though an error has happened }
@@1:
        test    ax,40h
        jz      @@3
        xor     al,al
        jmp     @@2
@@3:
        mov     al,1
@@2:
end;

function FilePos(Handle: TFileHandle): TFileInt;
begin
  FilePos := SeekFile(Handle, 0, skCur);
end;

function FileSize(Handle: TFileHandle): TFileInt;
var
  L: Longint;
begin
  L := FilePos(Handle);
  FileSize := SeekFile(Handle, 0, skEnd);
  SeekFile(Handle, L, skBeg);
end;

{
  $Log$
  Revision 1.1  2000-01-06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:57  peter
    * moved some dirs

  Revision 1.1  1998/10/26 11:31:49  peter
    + inital include files

}
