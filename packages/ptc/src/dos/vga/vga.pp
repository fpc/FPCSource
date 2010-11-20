{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$MODE objfpc}
{$ASMMODE intel}

{$DEFINE X86_ASSEMBLER}

unit vga;

interface

const
{mode types}
  FAKEMODE = 0;
  RGB332 = 1;
  INDEX8 = 2;
{fakemode types}
  FAKEMODE1A = 0;
  FAKEMODE1B = 1;
  FAKEMODE1C = 2;
  FAKEMODE2A = 3;
  FAKEMODE2B = 4;
  FAKEMODE2C = 5;
  FAKEMODE3A = 6;
  FAKEMODE3B = 7;
  FAKEMODE3C = 8;

var
  m_mode_type: Integer;
  m_fake_type: Integer;
  m_dispoffset: Integer;

procedure VGASetMode(xres, yres, modetype, faketype: Integer);
procedure fakemode_load(src: PByte; wvr: Boolean);

implementation

uses
  go32fix;

var
  RealRegs: TRealRegs;

procedure vgamode;

begin
  RealRegs.ax := $13;
  realintr($10, RealRegs);
end;

procedure biostextmode;

begin
  RealRegs.ax := 3;
  realintr($10, RealRegs);
end;

procedure wait_retrace;

begin
  while (inportb($3DA) and 8) <> 0 do;
  while (inportb($3DA) and 8) = 0 do;
end;

procedure clearmem(d: DWord); Assembler; Register;

Asm
  cld
  push es
  mov edi, d
  mov ax, fs
  mov es, ax
  mov ecx, 2048/4
  xor eax, eax
  rep stosd
  pop es
end;

procedure clear_memory;

var
  dest: DWord;
  strip: Integer;

begin
  wait_retrace;
  dest := $A0000;
  for strip := 0 to 31 do
  begin
    outportw($3C4, $102);
    clearmem(dest);
    outportw($3C4, $202);
    clearmem(dest);
    outportw($3C4, $402);
    clearmem(dest);
    outportw($3C4, $802);
    clearmem(dest);
    Inc(dest, 2048);
  end;
end;

procedure palette(data: PDWord);

var
  I: Integer;
  C: DWord;

begin
  outportb($3C8, 0);
  for I := 0 to 255 do
  begin
    C := (data[I] shr 2) and $3F3F3F;
    outportb($3C9, C shr 16);
    outportb($3C9, C shr 8);
    outportb($3C9, C);
  end;
end;

procedure VGASetMode(xres, yres, modetype, faketype: Integer);

var
  pal: array [0..255] of DWord;
  I: Integer;
  r, g, b: Integer;
  z: Integer;

begin
  m_mode_type := modetype;
  { set up display offset to centre image on display }
  m_dispoffset := ((100 - (yres shr 1)) * 320) + (160 - (xres shr 1));
  if (faketype < FAKEMODE1A) or (faketype > FAKEMODE2C) then
    faketype := FAKEMODE2A;
  m_fake_type := faketype;

  vgamode;
  if modetype = FAKEMODE then
  begin
    FillChar(pal, SizeOf(pal), 0);
    palette(@pal);
    m_dispoffset := 0;
    wait_retrace;
    if (faketype >= FAKEMODE1A) and (faketype <= FAKEMODE1C) then
    begin
      {FAKEMODE1x - 320x600}
      outportb($3D4, $11);
      outportb($3D5, inportb($3D5) and $7F);
      outportb($3C2, $E7);
      outportb($3D4, $00); outportb($3D5, $5F);
      outportb($3D4, $01); outportb($3D5, $4F);
      outportb($3D4, $02); outportb($3D5, $50);
      outportb($3D4, $03); outportb($3D5, $82);
      outportb($3D4, $04); outportb($3D5, $54);
      outportb($3D4, $05); outportb($3D5, $80);
      outportb($3D4, $06); outportb($3D5, $70);
      outportb($3D4, $07); outportb($3D5, $F0);
      outportb($3D4, $08); outportb($3D5, $00);
      outportb($3D4, $09); outportb($3D5, $60);
      outportb($3D4, $10); outportb($3D5, $5B);
      outportb($3D4, $11); outportb($3D5, $8C);
      outportb($3D4, $12); outportb($3D5, $57);
      outportb($3D4, $13); outportb($3D5, $28);
      outportb($3D4, $14); outportb($3D5, $00);
      outportb($3D4, $15); outportb($3D5, $58);
      outportb($3D4, $16); outportb($3D5, $70);
      outportb($3D4, $17); outportb($3D5, $E3);
      outportb($3C4, $01); outportb($3C5, $01);
      outportb($3C4, $04); outportb($3C5, $06);
      outportb($3CE, $05); outportb($3CF, $40);
      outportb($3CE, $06); outportb($3CF, $05);
      outportb($3CE, $06); outportb($3CF, $05);
    end
    else
    begin
      outportb($3D4, $11); outportb($3D5, inportb($3D5) and $7F);
      outportb($3C2, $63);
      outportb($3D4, $00); outportb($3D5, $5F);
      outportb($3D4, $01); outportb($3D5, $4F);
      outportb($3D4, $02); outportb($3D5, $50);
      outportb($3D4, $03); outportb($3D5, $82);
      outportb($3D4, $04); outportb($3D5, $54);
      outportb($3D4, $05); outportb($3D5, $80);
      outportb($3D4, $06); outportb($3D5, $BF);
      outportb($3D4, $07); outportb($3D5, $1F);
      outportb($3D4, $08); outportb($3D5, $00);
      outportb($3D4, $09); outportb($3D5, $40);
      outportb($3D4, $10); outportb($3D5, $9C);
      outportb($3D4, $11); outportb($3D5, $8E);
      outportb($3D4, $12); outportb($3D5, $8F);
      outportb($3D4, $13); outportb($3D5, $28);
      outportb($3D4, $14); outportb($3D5, $00);
      outportb($3D4, $15); outportb($3D5, $96);
      outportb($3D4, $16); outportb($3D5, $B9);
      outportb($3D4, $17); outportb($3D5, $E3);
      outportb($3C4, $01); outportb($3C5, $01);
      outportb($3C4, $04); outportb($3C5, $06);
      outportb($3CE, $05); outportb($3CF, $40);
      outportb($3CE, $06); outportb($3CF, $05);
      outportb($3CE, $06); outportb($3CF, $05);
    end;
    clear_memory;
    if (faketype >= FAKEMODE2A) and (faketype <= FAKEMODE2C) then
    begin
      {FAKEMODE2 palette}
      {taken from PTC 0.73}
      for I := 0 to $7F do
      begin
        {bit 7 = 0 (top section)}
        {red (4 bits)}
        r := Round(((I shr 3) * 255) / 15);
        {blue (3 bits)}
        b := Round(((I and 7) * 255) / 7);
        pal[I] := (r shl 16) or b;
      end;
      for I := $80 to $FF do
      begin
        {bit 7 = 1 (bottom section)}
        {green}
        g := Round(((I and $1F) * 255) / 31);
        pal[I] := g shl 8;
      end;
    end
    else
    begin
      for I := 0 to 63 do
      begin
        {FAKEMODE(1,3) palette}
        z := Round((I * 255) / 63);
        pal[I] := z shl 16;
        pal[I + 64] := z shl 8;
        pal[I + 128] := z;
        pal[I + 192] := (z shl 16) or (z shl 8) or z;
      end;
    end;
    palette(@pal);
  end
  else
    if modetype = RGB332 then
    begin
      for I := 0 to 255 do
      begin
        r := Round(((I shr 5) * 255) / 7);
        g := Round((((I and $1C) shr 2) * 255) / 7);
        b := Round(((I and $03) * 255) / 3);
        pal[I] := (r shl 16) or (g shl 8) or b;
      end;
      palette(@pal);
    end;
end;

{$IFDEF X86_ASSEMBLER}

{
  X86 fakemode routines for OpenPTC 1.0 C++ API implementation [DOS extension]
  Copyright (c) 1998 Glenn Fiedler (ptc@gaffer.org)
  Adapted for use in OpenPTC 1.0 C++ API by Jonathan Matthew (jmatthew@uq.net.au)
  Converted to Free Pascal inline assembler by Nikolay Nikolov (nickysn@users.sourceforge.net)
  This source code is licensed under the GNU LGPL
}

{
  Parameters:
  eax - source (moved to esi)
  edx - dest (moved to edi)
  ecx - # rows (moved to edx)
  returns updated dest in eax
}

function PlaneBlt1_RGB(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov     ebp, 20
        push    edx

@L2:        mov     cl, [esi]                   { ECX = greens (low 3 bits) }
            mov     ch, [esi+8]
            shl     ecx, 16
            mov     cl, [esi+8+8]
            mov     ch, [esi+8+8+8]
            mov     ebx, ecx                    { EBX = blues }
            and     ecx, 11100000111000001110000011100000b
            ror     ecx, 16+5

            mov     dl, [esi+1]                 { EDX = greens (high 3 bits) }
            mov     dh, [esi+8+1]
            shl     edx, 16
            mov     dl, [esi+8+8+1]
            mov     dh, [esi+8+8+8+1]
            mov     eax, edx                    { EAX = reds }
            and     edx, 00000111000001110000011100000111b
            ror     edx, 16-3

            and     ebx, 00011111000111110001111100011111b
            ror     ebx, 15
            add     ebx, 80808080h

            and     eax, 11111000111110001111100011111000b
            ror     eax, 16+2

            add     ecx, edx
            add     ecx, 40404040h

            mov     fs:[edi], eax
            mov     fs:[edi+80], ecx
            mov     fs:[edi+160], ebx

            add     edi, 4
            add     esi, 4*4*2

            dec     ebp
            jnz     @L2

        pop     edx
        add     edi, 160

        dec     edx
        jnz     @L1

@L3:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;


function PlaneBlt1_RBG(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov     ebp,20
        push    edx

@L2:        mov     cl,[esi]                                    { ECX = greens (low 3 bits) }
            mov     ch,[esi+8]
            shl     ecx,16
            mov     cl,[esi+8+8]
            mov     ch,[esi+8+8+8]
            mov     ebx,ecx                                     { setup blues in ebx }
            and     ecx,11100000111000001110000011100000b
            ror     ecx,16+5

            mov     dl,[esi+1]                                  { EDX = greens (high 3 bits) }
            mov     dh,[esi+8+1]
            shl     edx,16
            mov     dl,[esi+8+8+1]
            mov     dh,[esi+8+8+8+1]
            mov     eax,edx                                     { setup reds in eax }
            and     edx,00000111000001110000011100000111b
            ror     edx,16-3

            and     ebx,00011111000111110001111100011111b       { EBX = blues }
            ror     ebx,15
            add     ebx,80808080h

            and     eax,11111000111110001111100011111000b       { EAX = reds }
            ror     eax,16+2

            add     ecx,edx                                     { ECX = green(lo)+green(hi) }
            add     ecx,40404040h

            mov     fs:[edi],eax
            mov     fs:[edi+160],ecx
            mov     fs:[edi+80],ebx

            add     edi,4
            add     esi,4*4*2

            dec     ebp
            jnz     @L2

        pop     edx
        add     edi,160

        dec     edx
        jnz     @L1

@L3:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;

function PlaneBlt1_GRB(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov     ebp,20
        push    edx

@L2:        mov     cl,[esi]                                    { ECX = greens (low 3 bits) }
            mov     ch,[esi+8]
            shl     ecx,16
            mov     cl,[esi+8+8]
            mov     ch,[esi+8+8+8]
            mov     ebx,ecx                                     { setup blues in ebx }
            and     ecx,11100000111000001110000011100000b
            ror     ecx,16+5

            mov     dl,[esi+1]                                  { EDX = greens (high 3 bits) }
            mov     dh,[esi+8+1]
            shl     edx,16
            mov     dl,[esi+8+8+1]
            mov     dh,[esi+8+8+8+1]
            mov     eax,edx                                     { setup reds in eax }
            and     edx,00000111000001110000011100000111b
            ror     edx,16-3

            and     ebx,00011111000111110001111100011111b       { EBX = blues }
            ror     ebx,15
            add     ebx,80808080h

            and     eax,11111000111110001111100011111000b       { EAX = reds }
            ror     eax,16+2

            add     ecx,edx                                     { ECX = green(lo)+green(hi) }
            add     ecx,40404040h

            mov     fs:[edi+80],eax
            mov     fs:[edi],ecx
            mov     fs:[edi+160],ebx

            add     edi,4
            add     esi,4*4*2

            dec     ebp
            jnz     @L2

        pop     edx
        add     edi,160

        dec     edx
        jnz     @L1

@L3:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;


function PlaneBlt2_RBG(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov     ebp, 20
        push    edx

@L2:        mov     cl, [esi]
            mov     ch, [esi+8]
            shl     ecx, 16
            mov     cl, [esi+16]
            mov     ch, [esi+24]            { low greens in ecx }
            mov     ebx, ecx                { blues in ebx }
            and     ecx, 0c0c0c0c0h
            ror     ecx, 16+6

            mov     dl, [esi+1]
            mov     dh, [esi+9]
            shl     edx, 16
            mov     dl, [esi+17]
            mov     dh, [esi+25]            { high greens in edx }
            mov     eax, edx                { reds in eax }
            and     edx, 07070707h
            ror     edx, 16-2

            and     ebx, 1c1c1c1ch
            ror     ebx, 16+2

            and     eax, 0f0f0f0f0h
            ror     eax, 16+1

            add     eax, ebx                { eax = red+blue }
            add     ecx, edx                { ecx = green }

            or      ecx, 80808080h

            mov     fs:[edi], eax
            mov     fs:[edi+80], ecx

            add     esi, 4*4*2
            add     edi, 4

            dec     ebp
            jnz     @L2

        pop     edx
        add     edi, 80

        dec     edx
        jnz     @L1

@L3:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;


function PlaneBlt2_GBR(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov     ebp, 20
        push    edx

@L2:        mov     cl, [esi]
            mov     ch, [esi+8]
            shl     ecx, 16
            mov     cl, [esi+16]
            mov     ch, [esi+24]            { low greens in ecx }
            mov     ebx, ecx                { blues in ebx }
            and     ecx, 0c0c0c0c0h
            ror     ecx, 16+6

            mov     dl, [esi+1]
            mov     dh, [esi+9]
            shl     edx, 16
            mov     dl, [esi+17]
            mov     dh, [esi+25]            { high greens in edx }
            mov     eax, edx                { reds in eax }
            and     edx, 07070707h
            ror     edx, 16-2

            and     ebx, 1c1c1c1ch
            ror     ebx, 16+2

            and     eax, 0f0f0f0f0h
            ror     eax, 16+1

            add     eax, ebx                { eax = red+blue }
            add     ecx, edx                { ecx = green }

            or      ecx, 80808080h

            mov     fs:[edi], ecx
            mov     fs:[edi+80], eax

            add     esi, 4*4*2
            add     edi, 4

            dec     ebp
            jnz     @L2

        pop     edx
        add     edi, 80

        dec     edx
        jnz     @L1

@L3:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;

function PlaneBlt3_RGBRGB(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov ebp,20
        push edx

@L2:        { 1st pixel [0r,0g,1b] }

            mov cl,[esi]                                    { ECX = greens (low 3 bits) }
            mov ch,[esi+8]
            shl ecx,16
            mov cl,[esi+8+8]
            mov ch,[esi+8+8+8]
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1]                                  { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1]
            shl edx,16
            mov dl,[esi+8+8+1]
            mov dh,[esi+8+8+8+1]
            mov eax,edx                                     { setup reds in eax }
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            mov bl,[esi+320*2]                              { EBX = blues }
            mov bh,[esi+8+320*2]
            shl ebx,16
            mov bl,[esi+8+8+320*2]
            mov bh,[esi+8+8+8+320*2]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            and eax,11111000111110001111100011111000b       { EAX = reds }
            ror eax,16+2

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi],eax
            mov fs:[edi+80],ecx
            mov fs:[edi+160],ebx

            { 2nd pixel [1r,2g,2b] }

            mov al,[esi+1+320*2]                            { EAX = reds }
            mov ah,[esi+8+1+320*2]
            shl eax,16
            mov al,[esi+8+8+1+320*2]
            mov ah,[esi+8+8+8+1+320*2]
            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov cl,[esi+640*2]                              { ECX = greens (low 3 bits) }
            mov ch,[esi+8+640*2]
            shl ecx,16
            mov cl,[esi+8+8+640*2]
            mov ch,[esi+8+8+8+640*2]
            mov ebx,ecx                                     { setup blues in ebx }
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1+640*2]                            { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1+640*2]
            shl edx,16
            mov dl,[esi+8+8+1+640*2]
            mov dh,[esi+8+8+8+1+640*2]
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            and ebx,00011111000111110001111100011111b       { EBX = blues }
            ror ebx,15
            add ebx,80808080h

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+240],eax
            mov fs:[edi+240+80],ecx
            mov fs:[edi+240+160],ebx

            add edi,4
            add esi,4*4*2

            dec ebp
            jz @L3
            jmp @L2

@L3:    pop edx
        add edi,320+80
        add esi,320*2*2

        dec edx
        jz @L4
        jmp @L1

@L4:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;

function PlaneBlt3_GRBGRB(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov ebp,20
        push edx

@L2:        { 1st pixel [0g,0r,1b] }

            mov cl,[esi]                                    { ECX = greens (low 3 bits) }
            mov ch,[esi+8]
            shl ecx,16
            mov cl,[esi+8+8]
            mov ch,[esi+8+8+8]
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1]                                  { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1]
            shl edx,16
            mov dl,[esi+8+8+1]
            mov dh,[esi+8+8+8+1]
            mov eax,edx                                     { setup reds in eax }
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            mov bl,[esi+320*2]                              { EBX = blues }
            mov bh,[esi+8+320*2]
            shl ebx,16
            mov bl,[esi+8+8+320*2]
            mov bh,[esi+8+8+8+320*2]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            and eax,11111000111110001111100011111000b       { EAX = reds }
            ror eax,16+2

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi],ecx        { G0 }
            mov fs:[edi+80],eax     { R0 }
            mov fs:[edi+160],ebx    { B1 }

            { 2nd pixel [1g,2r,2b] }

            mov al,[esi+1+640*2]                            { EAX = reds }
            mov ah,[esi+8+1+640*2]
            shl eax,16
            mov al,[esi+8+8+1+640*2]
            mov ah,[esi+8+8+8+1+640*2]
            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov cl,[esi+320*2]                              { ECX = greens (low 3 bits) }
            mov ch,[esi+8+320*2]
            shl ecx,16
            mov cl,[esi+8+8+320*2]
            mov ch,[esi+8+8+8+320*2]
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1+320*2]                            { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1+320*2]
            shl edx,16
            mov dl,[esi+8+8+1+320*2]
            mov dh,[esi+8+8+8+1+320*2]
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            mov bl,[esi+640*2]                              { EBX = blues }
            mov bh,[esi+8+640*2]
            shl ebx,16
            mov bl,[esi+8+8+640*2]
            mov bh,[esi+8+8+8+640*2]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+240],ecx      { G1 }
            mov fs:[edi+240+80],eax   { R2 }
            mov fs:[edi+240+160],ebx  { B2 }

            add edi,4
            add esi,4*4*2

            dec ebp
            jz @L3
            jmp @L2

@L3:    pop edx
        add edi,320+80
        add esi,320*2*2

        dec edx
        jz @L4
        jmp @L1

@L4:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;

function PlaneBlt3_RBGRBG(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov ebp,20
        push edx

@L2:        { 1st pixel [0r,0b,1g] }

            mov bl,[esi]                                    { EBX = blues }
            mov bh,[esi+8]
            shl ebx,16
            mov bl,[esi+8+8]
            mov bh,[esi+8+8+8]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            mov al,[esi+1]                                  { EAX = reds }
            mov ah,[esi+8+1]
            shl eax,16
            mov al,[esi+8+8+1]
            mov ah,[esi+8+8+8+1]
            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov cl,[esi+320*2]                              { ECX = greens (low 3 bits) }
            mov ch,[esi+8+320*2]
            shl ecx,16
            mov cl,[esi+8+8+320*2]
            mov ch,[esi+8+8+8+320*2]
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov fs:[edi],eax       { R0 }

            mov dl,[esi+1+320*2]                            { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1+320*2]
            shl edx,16
            mov dl,[esi+8+8+1+320*2]
            mov dh,[esi+8+8+8+1+320*2]
            mov eax,edx                                     { setup eax = r1 }
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+80],ebx    { B0 }
            mov fs:[edi+160],ecx   { G1 }

            { 2nd pixel [1r,2b,2g] }

            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov cl,[esi+640*2]                              { ECX = greens (low 3 bits) }
            mov ch,[esi+8+640*2]
            shl ecx,16
            mov cl,[esi+8+8+640*2]
            mov ch,[esi+8+8+8+640*2]
            mov ebx,ecx                                     { setup blues in ebx }
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1+640*2]                            { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1+640*2]
            shl edx,16
            mov dl,[esi+8+8+1+640*2]
            mov dh,[esi+8+8+8+1+640*2]
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            and ebx,00011111000111110001111100011111b       { EBX = blues }
            ror ebx,15
            add ebx,80808080h

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+240],eax      { R1 }
            mov fs:[edi+240+80],ebx   { B2 }
            mov fs:[edi+240+160],ecx  { G2 }

            add edi,4
            add esi,4*4*2

            dec ebp
            jz @L3
            jmp @L2

@L3:    pop edx
        add edi,320+80
        add esi,320*2*2

        dec edx
        jz @L4
        jmp @L1

@L4:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;


function PlaneBlt3_GRBRBG(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov ebp,20
        push edx

@L2:        { 1st pixel [0g,0r,1b] }

            mov cl,[esi]                                    { ECX = greens (low 3 bits) }
            mov ch,[esi+8]
            shl ecx,16
            mov cl,[esi+8+8]
            mov ch,[esi+8+8+8]
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1]                                  { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1]
            shl edx,16
            mov dl,[esi+8+8+1]
            mov dh,[esi+8+8+8+1]
            mov eax,edx                                     { setup reds in eax }
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            mov bl,[esi+320*2]                              { EBX = blues }
            mov bh,[esi+8+320*2]
            shl ebx,16
            mov bl,[esi+8+8+320*2]
            mov bh,[esi+8+8+8+320*2]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            and eax,11111000111110001111100011111000b       { EAX = reds }
            ror eax,16+2

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi],ecx        { G0 }
            mov fs:[edi+80],eax     { R0 }
            mov fs:[edi+160],ebx    { B1 }

            { 2nd pixel [1r,2b,2g] }

            mov al,[esi+1+320*2]                            { EAX = reds }
            mov ah,[esi+8+1+320*2]
            shl eax,16
            mov al,[esi+8+8+1+320*2]
            mov ah,[esi+8+8+8+1+320*2]
            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov cl,[esi+640*2]                              { ECX = greens (low 3 bits) }
            mov ch,[esi+8+640*2]
            shl ecx,16
            mov cl,[esi+8+8+640*2]
            mov ch,[esi+8+8+8+640*2]
            mov ebx,ecx                                     { setup ebx = blues }
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov dl,[esi+1+640*2]                            { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1+640*2]
            shl edx,16
            mov dl,[esi+8+8+1+640*2]
            mov dh,[esi+8+8+8+1+640*2]
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            and ebx,00011111000111110001111100011111b       { EBX = blues }
            ror ebx,15
            add ebx,80808080h

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+240],eax       { R1 }
            mov fs:[edi+240+80],ebx    { B2 }
            mov fs:[edi+240+160],ecx   { G2 }

            add edi,4
            add esi,4*4*2

            dec ebp
            jz @L3
            jmp @L2

@L3:    pop edx
        add edi,320+80
        add esi,320*2*2

        dec edx
        jz @L4
        jmp @L1

@L4:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;


function PlaneBlt3_RBGGRB(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

@L1:    mov ebp,20
        push edx

@L2:        { 1st pixel [0r,0b,1g] }

            mov bl,[esi]                                    { EBX = blues }
            mov bh,[esi+8]
            shl ebx,16
            mov bl,[esi+8+8]
            mov bh,[esi+8+8+8]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            mov al,[esi+1]                                  { EAX = reds }
            mov ah,[esi+8+1]
            shl eax,16
            mov al,[esi+8+8+1]
            mov ah,[esi+8+8+8+1]
            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov fs:[edi+80],ebx    { B0 }

            mov dl,[esi+1+320*2]                            { EDX = greens (high 3 bits) }
            mov dh,[esi+8+1+320*2]
            shl edx,16
            mov dl,[esi+8+8+1+320*2]
            mov dh,[esi+8+8+8+1+320*2]
            mov ebx,edx                                     { setup ebx = g1 (hi) }
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            mov fs:[edi],eax       { R0 }

            mov cl,[esi+320*2]                              { ECX = greens (low 3 bits) }
            mov ch,[esi+8+320*2]
            shl ecx,16
            mov cl,[esi+8+8+320*2]
            mov ch,[esi+8+8+8+320*2]
            mov eax,ecx                                     { setup eax = g1 (lo) }
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+160],ecx   { G1 }

            { 2nd pixel [1g,2r,2b] }

            mov ecx,eax                                     { ECX = greens (low 3 bits) }
            and ecx,11100000111000001110000011100000b
            ror ecx,16+5

            mov edx,ebx                                     { EDX = greens (hi 3 bits) }
            and edx,00000111000001110000011100000111b
            ror edx,16-3

            mov al,[esi+1+640*2]                            { EAX = reds }
            mov ah,[esi+8+1+640*2]
            shl eax,16
            mov al,[esi+8+8+1+640*2]
            mov ah,[esi+8+8+8+1+640*2]
            and eax,11111000111110001111100011111000b
            ror eax,16+2

            mov bl,[esi+640*2]                              { EBX = blues }
            mov bh,[esi+8+640*2]
            shl ebx,16
            mov bl,[esi+8+8+640*2]
            mov bh,[esi+8+8+8+640*2]
            and ebx,00011111000111110001111100011111b
            ror ebx,15
            add ebx,80808080h

            add ecx,edx                                     { ECX = green(lo)+green(hi) }
            add ecx,40404040h

            mov fs:[edi+240],ecx       { G1 }
            mov fs:[edi+240+80],eax    { R2 }
            mov fs:[edi+240+160],ebx   { B2 }

            add edi,4
            add esi,4*4*2

            dec ebp
            jz @L3
            jmp @L2

@L3:    pop edx
        add edi,320+80
        add esi,320*2*2

        dec edx
        jz @L4
        jmp @L1

@L4:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;


function PlaneBlt3_RGBR(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx

    mov ebp,20

@L1:    mov cl,[esi]                                    { ECX = greens (low 3 bits) }
        mov ch,[esi+8]
        shl ecx,16
        mov cl,[esi+8+8]
        mov ch,[esi+8+8+8]
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        mov dl,[esi+1]                                  { EDX = greens (high 3 bits) }
        mov dh,[esi+8+1]
        shl edx,16
        mov dl,[esi+8+8+1]
        mov dh,[esi+8+8+8+1]
        mov eax,edx                                     { setup reds in eax }
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        mov bl,[esi+320*2]                              { EBX = blues }
        mov bh,[esi+8+320*2]
        shl ebx,16
        mov bl,[esi+8+8+320*2]
        mov bh,[esi+8+8+8+320*2]
        and ebx,00011111000111110001111100011111b
        ror ebx,15
        add ebx,80808080h

        and eax,11111000111110001111100011111000b       { EAX = reds }
        ror eax,16+2

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi],eax
        mov fs:[edi+80],ecx
        mov fs:[edi+160],ebx

        mov al,[esi+1+320*2]                            { EAX = reds }
        mov ah,[esi+8+1+320*2]
        shl eax,16
        mov al,[esi+8+8+1+320*2]
        mov ah,[esi+8+8+8+1+320*2]
        and eax,11111000111110001111100011111000b
        ror eax,16+2

        mov fs:[edi+240],eax

        add edi,4
        add esi,4*4*2

        dec ebp
        jz @L2
        jmp @L1

@L2:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;


function PlaneBlt3_GRBG(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx
    mov ebp,20

@L1:    mov cl,[esi]                                    { ECX = greens (low 3 bits) }
        mov ch,[esi+8]
        shl ecx,16
        mov cl,[esi+8+8]
        mov ch,[esi+8+8+8]
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        mov dl,[esi+1]                                  { EDX = greens (high 3 bits) }
        mov dh,[esi+8+1]
        shl edx,16
        mov dl,[esi+8+8+1]
        mov dh,[esi+8+8+8+1]
        mov eax,edx                                     { setup reds in eax }
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        mov bl,[esi+320*2]                              { EBX = blues }
        mov bh,[esi+8+320*2]
        shl ebx,16
        mov bl,[esi+8+8+320*2]
        mov bh,[esi+8+8+8+320*2]
        and ebx,00011111000111110001111100011111b
        ror ebx,15
        add ebx,80808080h

        and eax,11111000111110001111100011111000b       { EAX = reds }
        ror eax,16+2

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi],ecx        { G0 }
        mov fs:[edi+80],eax     { R0 }
        mov fs:[edi+160],ebx    { B1 }

        mov cl,[esi+320*2]                              { ECX = greens (low 3 bits) }
        mov ch,[esi+8+320*2]
        shl ecx,16
        mov cl,[esi+8+8+320*2]
        mov ch,[esi+8+8+8+320*2]
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        mov dl,[esi+1+320*2]                            { EDX = greens (high 3 bits) }
        mov dh,[esi+8+1+320*2]
        shl edx,16
        mov dl,[esi+8+8+1+320*2]
        mov dh,[esi+8+8+8+1+320*2]
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi+240],ecx      { G1 }

        add edi,4
        add esi,4*4*2

        dec ebp
        jz @L2
        jmp @L1

@L2:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;

function PlaneBlt3_RBGR(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx
    mov ebp,20

@L1:    mov bl,[esi]                                    { EBX = blues }
        mov bh,[esi+8]
        shl ebx,16
        mov bl,[esi+8+8]
        mov bh,[esi+8+8+8]
        and ebx,00011111000111110001111100011111b
        ror ebx,15
        add ebx,80808080h

        mov al,[esi+1]                                  { EAX = reds }
        mov ah,[esi+8+1]
        shl eax,16
        mov al,[esi+8+8+1]
        mov ah,[esi+8+8+8+1]
        and eax,11111000111110001111100011111000b
        ror eax,16+2

        mov cl,[esi+320*2]                              { ECX = greens (low 3 bits) }
        mov ch,[esi+8+320*2]
        shl ecx,16
        mov cl,[esi+8+8+320*2]
        mov ch,[esi+8+8+8+320*2]
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        mov fs:[edi],eax       { R0 }

        mov dl,[esi+1+320*2]                            { EDX = greens (high 3 bits) }
        mov dh,[esi+8+1+320*2]
        shl edx,16
        mov dl,[esi+8+8+1+320*2]
        mov dh,[esi+8+8+8+1+320*2]
        mov eax,edx                                     { setup eax = r1 }
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi+80],ebx    { B0 }
        mov fs:[edi+160],ecx   { G1 }

        and eax,11111000111110001111100011111000b
        ror eax,16+2

        mov fs:[edi+240],eax      { R1 }

        add edi,4
        add esi,4*4*2

        dec ebp
        jz @L2
        jmp @L1

@L2:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;


function PlaneBlt3_GRBR(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx
    mov ebp,20

@L1:    mov cl,[esi]                                    { ECX = greens (low 3 bits) }
        mov ch,[esi+8]
        shl ecx,16
        mov cl,[esi+8+8]
        mov ch,[esi+8+8+8]
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        mov dl,[esi+1]                                  { EDX = greens (high 3 bits) }
        mov dh,[esi+8+1]
        shl edx,16
        mov dl,[esi+8+8+1]
        mov dh,[esi+8+8+8+1]
        mov eax,edx                                     { setup reds in eax }
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        mov bl,[esi+320*2]                              { EBX = blues }
        mov bh,[esi+8+320*2]
        shl ebx,16
        mov bl,[esi+8+8+320*2]
        mov bh,[esi+8+8+8+320*2]
        and ebx,00011111000111110001111100011111b
        ror ebx,15
        add ebx,80808080h

        and eax,11111000111110001111100011111000b       { EAX = reds }
        ror eax,16+2

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi],ecx        { G0 }
        mov fs:[edi+80],eax     { R0 }
        mov fs:[edi+160],ebx    { B1 }

        mov al,[esi+1+320*2]                            { EAX = reds }
        mov ah,[esi+8+1+320*2]
        shl eax,16
        mov al,[esi+8+8+1+320*2]
        mov ah,[esi+8+8+8+1+320*2]
        and eax,11111000111110001111100011111000b
        ror eax,16+2

        mov fs:[edi+240],eax       { R1 }

        add edi,4
        add esi,4*4*2

        dec ebp
        jz @L2
        jmp @L1

@L2:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;


function PlaneBlt3_RBGG(src: PByte; dest: DWord; rows: Integer): DWord; Assembler; Register;

Asm
    push ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax
    mov     edi, edx
    mov     edx, ecx
    mov ebp,20

@L1:    mov bl,[esi]                                    { EBX = blues }
        mov bh,[esi+8]
        shl ebx,16
        mov bl,[esi+8+8]
        mov bh,[esi+8+8+8]
        and ebx,00011111000111110001111100011111b
        ror ebx,15
        add ebx,80808080h

        mov al,[esi+1]                                  { EAX = reds }
        mov ah,[esi+8+1]
        shl eax,16
        mov al,[esi+8+8+1]
        mov ah,[esi+8+8+8+1]
        and eax,11111000111110001111100011111000b
        ror eax,16+2

        mov fs:[edi+80],ebx    { B0 }

        mov dl,[esi+1+320*2]                            { EDX = greens (high 3 bits) }
        mov dh,[esi+8+1+320*2]
        shl edx,16
        mov dl,[esi+8+8+1+320*2]
        mov dh,[esi+8+8+8+1+320*2]
        mov ebx,edx                                     { setup ebx = g1 (hi) }
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        mov fs:[edi],eax       { R0 }

        mov cl,[esi+320*2]                              { ECX = greens (low 3 bits) }
        mov ch,[esi+8+320*2]
        shl ecx,16
        mov cl,[esi+8+8+320*2]
        mov ch,[esi+8+8+8+320*2]
        mov eax,ecx                                     { setup eax = g1 (lo) }
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi+160],ecx   { G1 }

        mov ecx,eax                                     { ECX = greens (low 3 bits) }
        and ecx,11100000111000001110000011100000b
        ror ecx,16+5

        mov edx,ebx                                     { EDX = greens (hi 3 bits) }
        and edx,00000111000001110000011100000111b
        ror edx,16-3

        add ecx,edx                                     { ECX = green(lo)+green(hi) }
        add ecx,40404040h

        mov fs:[edi+240],ecx       { G1 }

        add edi,4
        add esi,4*4*2

        dec ebp
        jz @L2
        jmp @L1

@L2:
    mov     eax, edi
    pop     ebx
    pop     edi
    pop     esi
    pop ebp
end;

{$ELSE X86_ASSEMBLER}

function PlaneBlt1_RGB(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := r;
      MemL[dest + 20*4] := gl;
      MemL[dest + 40*4] := b;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 40 * 4);
  end;
  PlaneBlt1_RGB := dest;
end;

function PlaneBlt1_RBG(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := r;
      MemL[dest + 20*4] := b;
      MemL[dest + 40*4] := gl;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 40 * 4);
  end;
  PlaneBlt1_RBG := dest;
end;

function PlaneBlt1_GRB(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := gl;
      MemL[dest + 20*4] := r;
      MemL[dest + 40*4] := b;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 40 * 4);
  end;
  PlaneBlt1_GRB := dest;
end;

function PlaneBlt2_RBG(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      b := gl;
      gl := gl and $C0C0C0C0;
      gl := gl shr 6;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 2;

      b := b and $1C1C1C1C;
      b := b shr 2;

      r := r and $F0F0F0F0;
      r := r shr 1;

      Inc(r, b);
      Inc(gl, gh);
      gl := gl or $80808080;

      MemL[dest] := r;
      MemL[dest + 20*4] := gl;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 20 * 4);
  end;
  PlaneBlt2_RBG := dest;
end;

function PlaneBlt2_GBR(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      b := gl;
      gl := gl and $C0C0C0C0;
      gl := gl shr 6;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 2;

      b := b and $1C1C1C1C;
      b := b shr 2;

      r := r and $F0F0F0F0;
      r := r shr 1;

      Inc(r, b);
      Inc(gl, gh);
      gl := gl or $80808080;

      MemL[dest] := gl;
      MemL[dest + 20*4] := r;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 20 * 4);
  end;
  PlaneBlt2_GBR := dest;
end;

function PlaneBlt3_RGBRGB(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := ((src[ 0+(320*2)])       ) or ((src[ 8+(320*2)]) shl 8) or
           ((src[16+(320*2)]) shl 16) or ((src[24+(320*2)]) shl 24);
      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := r;
      MemL[dest + 20*4] := gl;
      MemL[dest + 40*4] := b;

      r := ((src[ 1+(320*2)])       ) or ((src[ 9+(320*2)]) shl 8) or
           ((src[17+(320*2)]) shl 16) or ((src[25+(320*2)]) shl 24);
      r := r and $F8F8F8F8;
      r := r shr 2;

      gl := ((src[ 0+(640*2)])       ) or ((src[ 8+(640*2)]) shl 8) or
            ((src[16+(640*2)]) shl 16) or ((src[24+(640*2)]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1+(640*2)])       ) or ((src[ 9+(640*2)]) shl 8) or
            ((src[17+(640*2)]) shl 16) or ((src[25+(640*2)]) shl 24);
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest + 60*4] := r;
      MemL[dest + 80*4] := gl;
      MemL[dest + 100*4] := b;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 100 * 4);
    Inc(src, 320 * 2 * 2);
  end;
  PlaneBlt3_RGBRGB := dest;
end;

function PlaneBlt3_GRBGRB(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := ((src[ 0+(320*2)])       ) or ((src[ 8+(320*2)]) shl 8) or
           ((src[16+(320*2)]) shl 16) or ((src[24+(320*2)]) shl 24);
      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := gl;
      MemL[dest + 20*4] := r;
      MemL[dest + 40*4] := b;

      r := ((src[ 1+(320*2)])       ) or ((src[ 9+(320*2)]) shl 8) or
           ((src[17+(320*2)]) shl 16) or ((src[25+(320*2)]) shl 24);
      r := r and $F8F8F8F8;
      r := r shr 2;

      gl := ((src[ 0+(640*2)])       ) or ((src[ 8+(640*2)]) shl 8) or
            ((src[16+(640*2)]) shl 16) or ((src[24+(640*2)]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1+(640*2)])       ) or ((src[ 9+(640*2)]) shl 8) or
            ((src[17+(640*2)]) shl 16) or ((src[25+(640*2)]) shl 24);
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest + 60*4] := gl;
      MemL[dest + 80*4] := r;
      MemL[dest + 100*4] := b;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 100 * 4);
    Inc(src, 320 * 2 * 2);
  end;
  PlaneBlt3_GRBGRB := dest;
end;

function PlaneBlt3_RBGRBG(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := ((src[ 0+(320*2)])       ) or ((src[ 8+(320*2)]) shl 8) or
           ((src[16+(320*2)]) shl 16) or ((src[24+(320*2)]) shl 24);
      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := r;
      MemL[dest + 20*4] := b;
      MemL[dest + 40*4] := gl;

      r := ((src[ 1+(320*2)])       ) or ((src[ 9+(320*2)]) shl 8) or
           ((src[17+(320*2)]) shl 16) or ((src[25+(320*2)]) shl 24);
      r := r and $F8F8F8F8;
      r := r shr 2;

      gl := ((src[ 0+(640*2)])       ) or ((src[ 8+(640*2)]) shl 8) or
            ((src[16+(640*2)]) shl 16) or ((src[24+(640*2)]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1+(640*2)])       ) or ((src[ 9+(640*2)]) shl 8) or
            ((src[17+(640*2)]) shl 16) or ((src[25+(640*2)]) shl 24);
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest + 60*4] := r;
      MemL[dest + 80*4] := b;
      MemL[dest + 100*4] := gl;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 100 * 4);
    Inc(src, 320 * 2 * 2);
  end;
  PlaneBlt3_RBGRBG := dest;
end;

function PlaneBlt3_GRBRBG(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := ((src[ 0+(320*2)])       ) or ((src[ 8+(320*2)]) shl 8) or
           ((src[16+(320*2)]) shl 16) or ((src[24+(320*2)]) shl 24);
      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := gl;
      MemL[dest + 20*4] := r;
      MemL[dest + 40*4] := b;

      r := ((src[ 1+(320*2)])       ) or ((src[ 9+(320*2)]) shl 8) or
           ((src[17+(320*2)]) shl 16) or ((src[25+(320*2)]) shl 24);
      r := r and $F8F8F8F8;
      r := r shr 2;

      gl := ((src[ 0+(640*2)])       ) or ((src[ 8+(640*2)]) shl 8) or
            ((src[16+(640*2)]) shl 16) or ((src[24+(640*2)]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1+(640*2)])       ) or ((src[ 9+(640*2)]) shl 8) or
            ((src[17+(640*2)]) shl 16) or ((src[25+(640*2)]) shl 24);
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest + 60*4] := r;
      MemL[dest + 80*4] := b;
      MemL[dest + 100*4] := gl;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 100 * 4);
    Inc(src, 320 * 2 * 2);
  end;
  PlaneBlt3_GRBRBG := dest;
end;

function PlaneBlt3_RBGGRB(src: PByte; dest: DWord; rows: Integer): DWord;

var
  row, col: Integer;
  r, gl, gh, b: DWord;

begin
  for row := 1 to rows do
  begin
    for col := 0 to 19 do
    begin
      gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
            ((src[16]) shl 16) or ((src[24]) shl 24);
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
            ((src[17]) shl 16) or ((src[25]) shl 24);
      r := gh;
      gh := gh and $07070707;
      gh := gh shl 3;

      b := ((src[ 0+(320*2)])       ) or ((src[ 8+(320*2)]) shl 8) or
           ((src[16+(320*2)]) shl 16) or ((src[24+(320*2)]) shl 24);
      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      r := r and $F8F8F8F8;
      r := r shr 2;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest] := r;
      MemL[dest + 20*4] := b;
      MemL[dest + 40*4] := gl;

      r := ((src[ 1+(320*2)])       ) or ((src[ 9+(320*2)]) shl 8) or
           ((src[17+(320*2)]) shl 16) or ((src[25+(320*2)]) shl 24);
      r := r and $F8F8F8F8;
      r := r shr 2;

      gl := ((src[ 0+(640*2)])       ) or ((src[ 8+(640*2)]) shl 8) or
            ((src[16+(640*2)]) shl 16) or ((src[24+(640*2)]) shl 24);
      b := gl;
      gl := gl and $E0E0E0E0;
      gl := gl shr 5;

      gh := ((src[ 1+(640*2)])       ) or ((src[ 9+(640*2)]) shl 8) or
            ((src[17+(640*2)]) shl 16) or ((src[25+(640*2)]) shl 24);
      gh := gh and $07070707;
      gh := gh shl 3;

      b := b and $1F1F1F1F;
      b := b shl 1;
      b := b or $80808080;

      Inc(gl, gh);
      gl := gl or $40404040;

      MemL[dest + 60*4] := gl;
      MemL[dest + 80*4] := r;
      MemL[dest + 100*4] := b;

      Inc(dest, 4);
      Inc(src, 4 * 4 * 2);
    end;
    Inc(dest, 100 * 4);
    Inc(src, 320 * 2 * 2);
  end;
  PlaneBlt3_RBGGRB := dest;
end;

function PlaneBlt3_RGBR(src: PByte; dest: DWord; rows: Integer): DWord;

var
  {row,} col: Integer;
  r, gl, gh, b: DWord;

begin
  for col := 0 to 19 do
  begin
    gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
          ((src[16]) shl 16) or ((src[24]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
          ((src[17]) shl 16) or ((src[25]) shl 24);
    r := gh;
    gh := gh and $07070707;
    gh := gh shl 3;

    b := ((src[ 0+320*2])       ) or ((src[ 8+320*2]) shl  8) or
         ((src[16+320*2]) shl 16) or ((src[24+320*2]) shl 24);
    b := b and $1F1F1F1F;
    b := b shl 1;
    b := b or $80808080;

    r := r and $F8F8F8F8;
    r := r shr 2;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest] := r;
    MemL[dest + 20*4] := gl;
    MemL[dest + 40*4] := b;

    r := ((src[ 1+320*2])       ) or ((src[ 9+320*2]) shl  8) or
         ((src[17+320*2]) shl 16) or ((src[25+320*2]) shl 24);
    r := r or $F8F8F8F8;
    r := r shr 2;

    MemL[dest + 60*4] := r;

    Inc(dest, 4);
    Inc(src, 4 * 4 * 2);
  end;
  PlaneBlt3_RGBR := dest;
end;

function PlaneBlt3_GRBG(src: PByte; dest: DWord; rows: Integer): DWord;

var
  {row,} col: Integer;
  r, gl, gh, b: DWord;

begin
  for col := 0 to 19 do
  begin
    gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
          ((src[16]) shl 16) or ((src[24]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
          ((src[17]) shl 16) or ((src[25]) shl 24);
    r := gh;
    gh := gh and $07070707;
    gh := gh shl 3;

    b := ((src[ 0+320*2])       ) or ((src[ 8+320*2]) shl  8) or
         ((src[16+320*2]) shl 16) or ((src[24+320*2]) shl 24);
    b := b and $1F1F1F1F;
    b := b shl 1;
    b := b or $80808080;

    r := r and $F8F8F8F8;
    r := r shr 2;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest] := gl;
    MemL[dest + 20*4] := r;
    MemL[dest + 40*4] := b;

    gl := ((src[ 0+640*2])       ) or ((src[ 8+640*2]) shl  8) or
          ((src[16+640*2]) shl 16) or ((src[24+640*2]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1+640*2])       ) or ((src[ 9+640*2]) shl  8) or
          ((src[17+640*2]) shl 16) or ((src[25+640*2]) shl 24);
    gh := gh and $07070707;
    gh := gh shl 3;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest + 60*4] := gl;

    Inc(dest, 4);
    Inc(src, 4 * 4 * 2);
  end;
  PlaneBlt3_GRBG := dest;
end;

function PlaneBlt3_RBGR(src: PByte; dest: DWord; rows: Integer): DWord;

var
  {row,} col: Integer;
  r, gl, gh, b: DWord;

begin
  for col := 0 to 19 do
  begin
    gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
          ((src[16]) shl 16) or ((src[24]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
          ((src[17]) shl 16) or ((src[25]) shl 24);
    r := gh;
    gh := gh and $07070707;
    gh := gh shl 3;

    b := ((src[ 0+320*2])       ) or ((src[ 8+320*2]) shl  8) or
         ((src[16+320*2]) shl 16) or ((src[24+320*2]) shl 24);
    b := b and $1F1F1F1F;
    b := b shl 1;
    b := b or $80808080;

    r := r and $F8F8F8F8;
    r := r shr 2;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest] := r;
    MemL[dest + 20*4] := b;
    MemL[dest + 40*4] := gl;

    r := ((src[ 1+320*2])       ) or ((src[ 9+320*2]) shl  8) or
         ((src[17+320*2]) shl 16) or ((src[25+320*2]) shl 24);
    r := r or $F8F8F8F8;
    r := r shr 2;

    MemL[dest + 60*4] := r;

    Inc(dest, 4);
    Inc(src, 4 * 4 * 2);
  end;
  PlaneBlt3_RBGR := dest;
end;

function PlaneBlt3_GRBR(src: PByte; dest: DWord; rows: Integer): DWord;

var
  {row,} col: Integer;
  r, gl, gh, b: DWord;

begin
  for col := 0 to 19 do
  begin
    gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
          ((src[16]) shl 16) or ((src[24]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
          ((src[17]) shl 16) or ((src[25]) shl 24);
    r := gh;
    gh := gh and $07070707;
    gh := gh shl 3;

    b := ((src[ 0+320*2])       ) or ((src[ 8+320*2]) shl  8) or
         ((src[16+320*2]) shl 16) or ((src[24+320*2]) shl 24);
    b := b and $1F1F1F1F;
    b := b shl 1;
    b := b or $80808080;

    r := r and $F8F8F8F8;
    r := r shr 2;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest] := gl;
    MemL[dest + 20*4] := r;
    MemL[dest + 40*4] := b;

    r := ((src[ 1+320*2])       ) or ((src[ 9+320*2]) shl  8) or
         ((src[17+320*2]) shl 16) or ((src[25+320*2]) shl 24);
    r := r or $F8F8F8F8;
    r := r shr 2;

    MemL[dest + 60*4] := r;

    Inc(dest, 4);
    Inc(src, 4 * 4 * 2);
  end;
  PlaneBlt3_GRBR := dest;
end;

function PlaneBlt3_RBGG(src: PByte; dest: DWord; rows: Integer): DWord;

var
  {row,} col: Integer;
  r, gl, gh, b: DWord;

begin
  for col := 0 to 19 do
  begin
    gl := ((src[ 0])       ) or ((src[ 8]) shl  8) or
          ((src[16]) shl 16) or ((src[24]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1])       ) or ((src[ 9]) shl 8) or
          ((src[17]) shl 16) or ((src[25]) shl 24);
    r := gh;
    gh := gh and $07070707;
    gh := gh shl 3;

    b := ((src[ 0+320*2])       ) or ((src[ 8+320*2]) shl  8) or
         ((src[16+320*2]) shl 16) or ((src[24+320*2]) shl 24);
    b := b and $1F1F1F1F;
    b := b shl 1;
    b := b or $80808080;

    r := r and $F8F8F8F8;
    r := r shr 2;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest] := r;
    MemL[dest + 20*4] := b;
    MemL[dest + 40*4] := gl;

    gl := ((src[ 0+640*2])       ) or ((src[ 8+640*2]) shl  8) or
          ((src[16+640*2]) shl 16) or ((src[24+640*2]) shl 24);
    gl := gl and $E0E0E0E0;
    gl := gl shr 5;

    gh := ((src[ 1+640*2])       ) or ((src[ 9+640*2]) shl  8) or
          ((src[17+640*2]) shl 16) or ((src[25+640*2]) shl 24);
    gh := gh and $07070707;
    gh := gh shl 3;

    Inc(gl, gh);
    gl := gl or $40404040;

    MemL[dest + 60*4] := gl;

    Inc(dest, 4);
    Inc(src, 4 * 4 * 2);
  end;
  PlaneBlt3_RBGG := dest;
end;

{$ENDIF X86_ASSEMBLER}

procedure fakemode_load(src: PByte; wvr: Boolean);

var
  dest, d: DWord;
  w, s: Integer;

begin
  dest := $A0000;
  case m_fake_type of
    FAKEMODE1A :
      for w := 0 to 24 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt1_RGB(src + 0, dest, 8);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt1_RGB(src + 2, dest, 8);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt1_RGB(src + 4, dest, 8);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt1_RGB(src + 6, dest, 8);
        Inc(src, 320 * 4 * 4);
      end;
    FAKEMODE1B :
      for w := 0 to 24 do
      begin
        {plane 0}
        outportw($3C4, $102);
        d := PlaneBlt1_RBG(src + (4*4*2*20*0), dest, 1);
        d := PlaneBlt1_GRB(src + (4*4*2*20*1), d, 1);
        d := PlaneBlt1_RBG(src + (4*4*2*20*2), d, 1);
        d := PlaneBlt1_GRB(src + (4*4*2*20*3), d, 1);
        d := PlaneBlt1_RBG(src + (4*4*2*20*4), d, 1);
        d := PlaneBlt1_GRB(src + (4*4*2*20*5), d, 1);
        d := PlaneBlt1_RBG(src + (4*4*2*20*6), d, 1);
        d := PlaneBlt1_GRB(src + (4*4*2*20*7), d, 1);

        {plane 1}
        outportw($3C4, $202);
        d := PlaneBlt1_GRB(src + 2 + (4*4*2*20*0), dest, 1);
        d := PlaneBlt1_RBG(src + 2 + (4*4*2*20*1), d, 1);
        d := PlaneBlt1_GRB(src + 2 + (4*4*2*20*2), d, 1);
        d := PlaneBlt1_RBG(src + 2 + (4*4*2*20*3), d, 1);
        d := PlaneBlt1_GRB(src + 2 + (4*4*2*20*4), d, 1);
        d := PlaneBlt1_RBG(src + 2 + (4*4*2*20*5), d, 1);
        d := PlaneBlt1_GRB(src + 2 + (4*4*2*20*6), d, 1);
        d := PlaneBlt1_RBG(src + 2 + (4*4*2*20*7), d, 1);

        {plane 2}
        outportw($3C4, $402);
        d := PlaneBlt1_RBG(src + 4 + (4*4*2*20*0), dest, 1);
        d := PlaneBlt1_GRB(src + 4 + (4*4*2*20*1), d, 1);
        d := PlaneBlt1_RBG(src + 4 + (4*4*2*20*2), d, 1);
        d := PlaneBlt1_GRB(src + 4 + (4*4*2*20*3), d, 1);
        d := PlaneBlt1_RBG(src + 4 + (4*4*2*20*4), d, 1);
        d := PlaneBlt1_GRB(src + 4 + (4*4*2*20*5), d, 1);
        d := PlaneBlt1_RBG(src + 4 + (4*4*2*20*6), d, 1);
        d := PlaneBlt1_GRB(src + 4 + (4*4*2*20*7), d, 1);

        {plane 3}
        outportw($3C4, $802);
        d := PlaneBlt1_GRB(src + 6 + (4*4*2*20*0), dest, 1);
        d := PlaneBlt1_RBG(src + 6 + (4*4*2*20*1), d, 1);
        d := PlaneBlt1_GRB(src + 6 + (4*4*2*20*2), d, 1);
        d := PlaneBlt1_RBG(src + 6 + (4*4*2*20*3), d, 1);
        d := PlaneBlt1_GRB(src + 6 + (4*4*2*20*4), d, 1);
        d := PlaneBlt1_RBG(src + 6 + (4*4*2*20*5), d, 1);
        d := PlaneBlt1_GRB(src + 6 + (4*4*2*20*6), d, 1);
        dest := PlaneBlt1_RBG(src + 6 + (4*4*2*20*7), d, 1);
        Inc(src, 320*4*4);
      end;
    FAKEMODE1C :
      for w := 0 to 24 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt1_RBG(src + 0, dest, 8);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt1_GRB(src + 2, dest, 8);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt1_RBG(src + 4, dest, 8);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt1_GRB(src + 6, dest, 8);
        Inc(src, 320 * 4 * 4);
      end;
    FAKEMODE2A :
      for w := 0 to 24 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt2_RBG(src + 0, dest, 8);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt2_RBG(src + 2, dest, 8);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt2_RBG(src + 4, dest, 8);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt2_RBG(src + 6, dest, 8);
        Inc(src, 320 * 4 * 4);
      end;
    FAKEMODE2B :
      for w := 0 to 24 do
      begin
        {plane 0}
        outportw($3C4, $102);
        d := PlaneBlt2_RBG(src + (4*4*2*20*0), dest, 1);
        d := PlaneBlt2_GBR(src + (4*4*2*20*1), d, 1);
        d := PlaneBlt2_RBG(src + (4*4*2*20*2), d, 1);
        d := PlaneBlt2_GBR(src + (4*4*2*20*3), d, 1);
        d := PlaneBlt2_RBG(src + (4*4*2*20*4), d, 1);
        d := PlaneBlt2_GBR(src + (4*4*2*20*5), d, 1);
        d := PlaneBlt2_RBG(src + (4*4*2*20*6), d, 1);
        d := PlaneBlt2_GBR(src + (4*4*2*20*7), d, 1);

        {plane 1}
        outportw($3C4, $202);
        d := PlaneBlt2_GBR(src + 2 + (4*4*2*20*0), dest, 1);
        d := PlaneBlt2_RBG(src + 2 + (4*4*2*20*1), d, 1);
        d := PlaneBlt2_GBR(src + 2 + (4*4*2*20*2), d, 1);
        d := PlaneBlt2_RBG(src + 2 + (4*4*2*20*3), d, 1);
        d := PlaneBlt2_GBR(src + 2 + (4*4*2*20*4), d, 1);
        d := PlaneBlt2_RBG(src + 2 + (4*4*2*20*5), d, 1);
        d := PlaneBlt2_GBR(src + 2 + (4*4*2*20*6), d, 1);
        d := PlaneBlt2_RBG(src + 2 + (4*4*2*20*7), d, 1);

        {plane 2}
        outportw($3C4, $402);
        d := PlaneBlt2_RBG(src + 4 + (4*4*2*20*0), dest, 1);
        d := PlaneBlt2_GBR(src + 4 + (4*4*2*20*1), d, 1);
        d := PlaneBlt2_RBG(src + 4 + (4*4*2*20*2), d, 1);
        d := PlaneBlt2_GBR(src + 4 + (4*4*2*20*3), d, 1);
        d := PlaneBlt2_RBG(src + 4 + (4*4*2*20*4), d, 1);
        d := PlaneBlt2_GBR(src + 4 + (4*4*2*20*5), d, 1);
        d := PlaneBlt2_RBG(src + 4 + (4*4*2*20*6), d, 1);
        d := PlaneBlt2_GBR(src + 4 + (4*4*2*20*7), d, 1);

        {plane 3}
        outportw($3C4, $802);
        d := PlaneBlt2_GBR(src + 6 + (4*4*2*20*0), dest, 1);
        d := PlaneBlt2_RBG(src + 6 + (4*4*2*20*1), d, 1);
        d := PlaneBlt2_GBR(src + 6 + (4*4*2*20*2), d, 1);
        d := PlaneBlt2_RBG(src + 6 + (4*4*2*20*3), d, 1);
        d := PlaneBlt2_GBR(src + 6 + (4*4*2*20*4), d, 1);
        d := PlaneBlt2_RBG(src + 6 + (4*4*2*20*5), d, 1);
        d := PlaneBlt2_GBR(src + 6 + (4*4*2*20*6), d, 1);
        dest := PlaneBlt2_RBG(src + 6 + (4*4*2*20*7), d, 1);
        Inc(src, 320*4*4);
      end;
    FAKEMODE2C :
      for w := 0 to 24 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt2_RBG(src + 0, dest, 8);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt2_GBR(src + 2, dest, 8);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt2_RBG(src + 4, dest, 8);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt2_GBR(src + 6, dest, 8);
        Inc(src, 320 * 4 * 4);
      end;
    FAKEMODE3A: begin
      for w := 0 to 15 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt3_RGBRGB(src + 0, dest, 4);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt3_RGBRGB(src + 2, dest, 4);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt3_RGBRGB(src + 4, dest, 4);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt3_RGBRGB(src + 6, dest, 4);
        Inc(src, 320 * 4 * 2 * 3);
      end;
      s := (4*4*2*20) + (320*2*2*2);
      outportw($3C4, $102);
      d := PlaneBlt3_RGBRGB(src, dest, 2);
      PlaneBlt3_RGBR(src + s, d, 1);

      outportw($3C4, $202);
      d := PlaneBlt3_RGBRGB(src + 2, dest, 2);
      PlaneBlt3_RGBR(src + s + 2, d, 1);

      outportw($3C4, $402);
      d := PlaneBlt3_RGBRGB(src + 4, dest, 2);
      PlaneBlt3_RGBR(src + s + 4, d, 1);

      outportw($3C4, $802);
      d := PlaneBlt3_RGBRGB(src + 6, dest, 2);
      PlaneBlt3_RGBR(src + s + 6, d, 1);
    end;
    FAKEMODE3B: begin
      for w := 0 to 15 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt3_GRBRBG(src + 0, dest, 4);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt3_RBGGRB(src + 2, dest, 4);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt3_GRBRBG(src + 4, dest, 4);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt3_RBGGRB(src + 6, dest, 4);
        Inc(src, 320 * 4 * 2 * 3);
      end;
      s := (4*4*2*20) + (320*2*2*2);
      outportw($3C4, $102);
      d := PlaneBlt3_GRBRBG(src, dest, 2);
      PlaneBlt3_GRBR(src + s, d, 1);

      outportw($3C4, $202);
      d := PlaneBlt3_RBGGRB(src + 2, dest, 2);
      PlaneBlt3_RBGG(src + s + 2, d, 1);

      outportw($3C4, $402);
      d := PlaneBlt3_GRBRBG(src + 4, dest, 2);
      PlaneBlt3_GRBR(src + s + 4, d, 1);

      outportw($3C4, $802);
      d := PlaneBlt3_RBGGRB(src + 6, dest, 2);
      PlaneBlt3_RBGG(src + s + 6, d, 1);
    end;
    FAKEMODE3C: begin
      for w := 0 to 15 do
      begin
        {plane 0}
        outportw($3C4, $102);
        PlaneBlt3_GRBGRB(src + 0, dest, 4);

        {plane 1}
        outportw($3C4, $202);
        PlaneBlt3_RBGRBG(src + 2, dest, 4);

        {plane 2}
        outportw($3C4, $402);
        PlaneBlt3_GRBGRB(src + 4, dest, 4);

        {plane 3}
        outportw($3C4, $802);
        dest := PlaneBlt3_RBGRBG(src + 6, dest, 4);
        Inc(src, 320 * 4 * 2 * 3);
      end;
      s := (4*4*2*20) + (320*2*2*2);
      outportw($3C4, $102);
      d := PlaneBlt3_GRBGRB(src, dest, 2);
      PlaneBlt3_GRBG(src + s, d, 1);

      outportw($3C4, $202);
      d := PlaneBlt3_RBGRBG(src + 2, dest, 2);
      PlaneBlt3_RBGR(src + s + 2, d, 1);

      outportw($3C4, $402);
      d := PlaneBlt3_GRBGRB(src + 4, dest, 2);
      PlaneBlt3_GRBG(src + s + 4, d, 1);

      outportw($3C4, $802);
      d := PlaneBlt3_RBGRBG(src + 6, dest, 2);
      PlaneBlt3_RBGR(src + s + 6, d, 1);
    end;
  end;
  if wvr then
    wait_retrace;
end;

end.
