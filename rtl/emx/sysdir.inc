{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for EMX.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure DosDir (Func: byte; S: rawbytestring);

begin
  DoDirSeparators (S);
  asm
    movl S, %edx
    movb Func, %ah
    call SysCall
    jnc .LDOS_DIRS1
    movw %ax, InOutRes
    .LDOS_DIRS1:
  end ['eax', 'edx'];
end;

procedure do_MkDir (S: rawbytestring);
var 
  RC: cardinal;
begin
  if os_mode = osOs2 then
   begin
    DoDirSeparators (S);
    RC := DosCreateDir (PAnsiChar(S), nil);
    if RC <> 0 then
     begin
      InOutRes := RC;
      Errno2InOutRes;
     end;
   end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
    DosDir ($39, S);
   end;
end;


procedure do_RmDir (S: rawbytestring);
var
  RC: cardinal;
begin
  if S = '.' then
   InOutRes := 16
  else
   if os_mode = osOs2 then
    begin
     DoDirSeparators (S);
     RC := DosDeleteDir (PAnsiChar(S));
     if RC <> 0 then
      begin
       InOutRes := RC;
       Errno2InOutRes;
      end;
    end
   else
   { Under EMX 0.9d DOS this routine call may sometimes fail   }
   { The syscall documentation indicates clearly that this     }
   { routine was NOT tested.                                   }
    DosDir ($3A, S);
end;


{$ASMMODE INTEL}

procedure do_ChDir (S: rawbytestring);
var
  RC: cardinal;
  Len: longint;
begin
(* According to EMX documentation, EMX has only one current directory
   for all processes, so we'll use native calls under OS/2. *)
  Len := Length (S);
  if os_Mode = osOS2 then
   begin
    if (Len >= 2) and (S [2] = ':') then
     begin
      RC := DosSetDefaultDisk ((Ord (S[1]) and not ($20)) - $40);
      if RC <> 0 then
       begin
        InOutRes := RC;
        Errno2InOutRes;
       end
      else
       if Len > 2 then
        begin
         DoDirSeparators (S);
         if (S [Len] = DirectorySeparator) and (Len <> 3) then
          S [Len] := #0;
         RC := DosSetCurrentDir (PAnsiChar(S));
         if RC <> 0 then
          begin
           InOutRes := RC;
           Errno2InOutRes;
          end;
        end;
     end
    else
     begin
      DoDirSeparators (S);
      if (Len > 1) and (S [Len] = DirectorySeparator) then
       S [Len] := #0;
      RC := DosSetCurrentDir (PAnsiChar(S));
      if RC <> 0 then
       begin
        InOutRes:= RC;
        Errno2InOutRes;
       end;
     end;
   end
  else
   if (Len >= 2) and (S [2] = ':') then
    begin
     asm
      mov esi, S
      mov al, [esi + 1]
      and al, not (20h)
      sub al, 41h
      mov edx, eax
      mov ah, 0Eh
      call syscall
      mov ah, 19h
      call syscall
      cmp al, dl
      jz @LCHDIR
      mov InOutRes, 15
@LCHDIR:
     end ['eax','edx','esi'];
     if (Len > 2) and (InOutRes <> 0) then
      begin
       if (S [Len] in AllowDirectorySeparators) and (Len <> 3) then
        S [Len] := #0;
      { Under EMX 0.9d DOS this routine may sometime }
      { fail or crash the system.                    }
       DosDir ($3B, S);
      end;
    end
   else
    begin
     if (Len > 1) and (S [Len] in AllowDirectorySeparators) then
      S [Len] := #0;
    { Under EMX 0.9d DOS this routine may sometime }
    { fail or crash the system.                    }
     DosDir ($3B, S);
    end;
end;


{$ASMMODE ATT}

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);

{Written by Michael Van Canneyt.}

var sof:PAnsiChar;
    i:byte;

begin
    SetLength(Dir,260);
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=PAnsiChar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    asm
        movb drivenr,%dl
        movl sof,%esi
        mov  $0x47,%ah
        call syscall
        jnc .LGetDir
        movw %ax, InOutRes
.LGetDir:
    end [ 'eax','edx','esi'];
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
			     if dir[i] in AllowDirectorySeparators then
			       dir[i]:=DirectorySeparator;
            inc(i);
        end;
    SetLength(dir,i-1);
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=chr(64+drivenr)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            asm
                movb $0x19,%ah
                call syscall
                addb $65,%al
                movb %al,i
            end ['eax'];
            dir[1]:=AnsiChar(i);
        end;
    SetCodePage(dir,DefaultFileSystemCodePage,false);
    { upcase the string (FPC function) }
    if not (FileNameCasePreserving) then dir:=upcase(dir);
end;
