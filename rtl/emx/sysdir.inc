{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for the Win32 API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}


procedure dosdir(func:byte;const s:string);

var buffer:array[0..255] of char;

begin
    move(s[1],buffer,length(s));
    buffer[length(s)]:=#0;
    DoDirSeparators(Pchar(@buffer));
    asm
        leal buffer,%edx
        movb func,%ah
        call syscall
        jnc  .LDOS_DIRS1
        movw %ax,inoutres
    .LDOS_DIRS1:
    end ['eax', 'edx'];
end;


procedure MkDir (const S: string);[IOCHECK];

var buffer:array[0..255] of char;
    Rc : word;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
 if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      DoDirSeparators(Pchar(@buffer));
      Rc := DosCreateDir(buffer,nil);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
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


procedure rmdir(const s : string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  if os_mode = osOs2 then
    begin
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      DoDirSeparators(Pchar(@buffer));
      Rc := DosDeleteDir(buffer);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
    end
  else
   begin
     { Under EMX 0.9d DOS this routine call may sometimes fail   }
     { The syscall documentation indicates clearly that this     }
     { routine was NOT tested.                                   }
        DosDir ($3A, S);
end;
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);[IOCheck];

var RC: cardinal;
    Buffer: array [0..255] of char;

begin
  If (s='') or (InOutRes <> 0) then
   exit;
(* According to EMX documentation, EMX has only one current directory
   for all processes, so we'll use native calls under OS/2. *)
            if os_Mode = osOS2 then
                begin
                    if (Length (S) >= 2) and (S [2] = ':') then
                        begin
                            RC := DosSetDefaultDisk ((Ord (S [1]) and
                                                             not ($20)) - $40);
                            if RC <> 0 then
                                InOutRes := RC
                            else
                                if Length (S) > 2 then
                                    begin
                                        Move (S [1], Buffer, Length (S));
                                        Buffer [Length (S)] := #0;
                                        DoDirSeparators (PChar (@Buffer));
                                        RC := DosSetCurrentDir (@Buffer);
                                        if RC <> 0 then
                                         begin
                                            InOutRes := RC;
                                            Errno2InOutRes;
                                         end;
                                    end;
                        end
                    else
                        begin
                            Move (S [1], Buffer, Length (S));
                            Buffer [Length (S)] := #0;
                            DoDirSeparators (PChar (@Buffer));
                            RC := DosSetCurrentDir (@Buffer);
                            if RC <> 0 then
                             begin
                                  InOutRes:= RC;
                                  Errno2InOutRes;
                             end;
                        end;
                end
            else
                if (Length (S) >= 2) and (S [2] = ':') then
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
                        if (Length (S) > 2) and (InOutRes <> 0) then
                            { Under EMX 0.9d DOS this routine may sometime }
                            { fail or crash the system.                    }
                            DosDir ($3B, S);
                    end
                else
                    { Under EMX 0.9d DOS this routine may sometime }
                    { fail or crash the system.                    }
                    DosDir ($3B, S);
end;

{$ASMMODE ATT}

procedure GetDir (DriveNr: byte; var Dir: ShortString);

{Written by Michael Van Canneyt.}

var sof:Pchar;
    i:byte;

begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
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
    dir[0]:=#3;
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
			     if dir[i] in AllowDirectorySeparators then
			       dir[i]:=DirectorySeparator;
            dir[0]:=char(i);
            inc(i);
        end;
    { upcase the string (FPC function) }
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
            dir[1]:=char(i);
        end;
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
end;



