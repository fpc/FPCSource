{
     $Id$
}
{ How this works:                                                        }
{    QueryAdapter - Va chercher tout les modes videos et drivers         }
{      disponibles sur cette carte, et les mets dans une linked list     }
{      en ordre de driver number, et a l'interieur de cela, dans un      }
{      ordre croissant de mode number.                                   }
{   DetectGraph - Verifie si la liste chainee de drivers existe, sinon   }
{      apelle QueryAdapter                                               }
{   InitGraph - Appelle DetectGraph, et verifie que le mode demande existe}
{      bel et bien et est disponible sur ce PC                           }
{$ifndef fpc}
  {$ifndef noasmgraph}
    {$define asmgraph}
  {$endif noasmgraph}
  {$i dpmi.inc}
{$else fpc}
  {$asmmode intel}
{$endif fpc}


{ How to access real mode memory }
{ using 32-bit DPMI memory       }
{  1. Allocate a descriptor      }
{  2. Set segment limit          }
{  3. Set base linear address    }
  const
   InternalDriverName = 'DOSGX';
{$ifdef fpc}
 {$ifdef asmgraph}
   VideoOfs : DWord = 0;   { Segment to draw to }
 {$else asmgraph}
   VideoOfs : word = 0;   { Segment to draw to }
 {$endif asmgraph}
{$else fpc}
   VideoOfs : word = 0;   { Segment to draw to }
{$endif fpc}
   FirstPlane = $0102;   (* 02 = Index to Color plane Select, *)
                         (* 01 = Enable color plane 1         *)

{    ; ===== VGA Register Values ===== }

    SCREEN_WIDTH    =     80     ; { MODE-X 320 SCREEN WIDTH         }
                                   { CHANGE THE VALUE IF OTHER MODES }
                                   { OTHER THEN 320 ARE USED.        }
    ATTRIB_Ctrl     =   $03C0    ; { VGA Attribute Controller        }
    GC_Index        =   $03CE    ; { VGA Graphics Controller         }
    SC_Index        =   $03C4    ; { VGA Sequencer Controller        }
    SC_Data         =   $03C5    ; { VGA Sequencer Data Port         }
    CRTC_Index      =   $03D4    ; { VGA CRT Controller              }
    CRTC_Data       =   $03D5    ; { VGA CRT Controller Data         }
    MISC_OUTPUT     =   $03C2    ; { VGA Misc Register               }
    INPUT_1         =   $03DA    ; { Input Status #1 Register        }

    DAC_WRITE_ADDR  =   $03C8    ; { VGA DAC Write Addr Register     }
    DAC_READ_ADDR   =   $03C7    ; { VGA DAC Read Addr Register      }
    PEL_DATA_REG    =   $03C9    ; { VGA DAC/PEL data Register R/W   }

    PIXEL_PAN_REG   =   $033     ; { Attrib Index: Pixel Pan Reg     }
    MAP_MASK        =   $002     ; { S=   $Index: Write Map Mask reg }
    READ_MAP        =   $004     ; { GC Index: Read Map Register     }
    START_DISP_HI   =   $00C     ; { CRTC Index: Display Start Hi    }
    START_DISP_LO   =   $00D     ; { CRTC Index: Display Start Lo    }

    MAP_MASK_PLANE1 =   $00102   ; { Map Register + Plane 1          }
    MAP_MASK_PLANE2 =   $01102   ; { Map Register + Plane 1          }
    ALL_PLANES_ON   =   $00F02   ; { Map Register + All Bit Planes   }

    CHAIN4_OFF      =   $00604   ; { Chain 4 mode Off                }
    ASYNC_RESET     =   $00100   ; { (A)synchronous Reset            }
    SEQU_RESTART    =   $00300   ; { Sequencer Restart               }

    LATCHES_ON      =   $00008   ; { Bit Mask + Data from Latches    }
    LATCHES_OFF     =   $0FF08   ; { Bit Mask + Data from CPU        }

    VERT_RETRACE    =   $08      ; { INPUT_1: Vertical Retrace Bit   }
    PLANE_BITS      =   $03      ; { Bits 0-1 of Xpos = Plane #      }
    ALL_PLANES      =   $0F      ; { All Bit Planes Selected         }
    CHAR_BITS       =   $0F      ; { Bits 0-3 of Character Data      }

    GET_CHAR_PTR    =   $01130   ; { VGA BIOS Func: Get Char Set     }
    ROM_8x8_Lo      =   $03      ; { ROM 8x8 Char Set Lo Pointer     }
    ROM_8x8_Hi      =   $04      ; { ROM 8x8 Char Set Hi Pointer     }

    { Constants Specific for these routines                          }

    NUM_MODES       =   $8       ; { # of Mode X Variations           }

    { in 16 color modes, the actual colors used are no 0..15, but: }
    ToRealCols16: Array[0..15] of word =
      (0,1,2,3,4,5,7,20,56,57,58,59,60,61,62,63);

  var
     ScrWidth : word absolute $40:$4a;

{$ifndef tp}
  procedure seg_bytemove(sseg : word;source : longint;dseg : word;dest : longint;count : longint);

    begin
       asm
          push es
          push ds
          cld
          mov ecx,count
          mov esi,source
          mov edi,dest
          mov ax,dseg
          mov es,ax
          mov ax,sseg
          mov ds,ax
          rep movsb
          pop ds
          pop es
       end ['ESI','EDI','ECX','EAX']
    end;
{$endif tp}

 {************************************************************************}
 {*                     4-bit planar VGA mode routines                   *}
 {************************************************************************}

  Procedure Init640x200x16; {$ifndef fpc}far;{$endif fpc} assembler;
  { must also clear the screen...}
   asm
     mov ax,000Eh
{$ifdef fpc}
     push ebp
{$endif fpc}
     int 10h
{$ifdef fpc}
     pop ebp
{$endif fpc}
   end;


   Procedure Init640x350x16; {$ifndef fpc}far;{$endif fpc} assembler;
  { must also clear the screen...}
    asm
      mov ax,0010h
{$ifdef fpc}
      push ebp
{$endif fpc}
      int 10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
    end;

  procedure Init640x480x16; {$ifndef fpc}far;{$endif fpc} assembler;
  { must also clear the screen...}
    asm
      mov  ax,0012h
{$ifdef fpc}
      push ebp
{$endif fpc}
      int  10h
{$ifdef fpc}
     pop ebp
{$endif fpc}
    end;

 Procedure PutPixel16(X,Y : Integer; Pixel: Word); {$ifndef fpc}far;{$endif fpc}
{$ifndef asmgraph}
 var offset: word;
     dummy: byte;
{$endif asmgraph}
  Begin
    X:= X + StartXViewPort;
    Y:= Y + StartYViewPort;
    { convert to absolute coordinates and then verify clipping...}
    if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
{$ifndef asmgraph}
     offset := y * 80 + (x shr 3) + VideoOfs;
     PortW[$3ce] := $f01;
     PortW[$3ce] := Pixel shl 8;
     PortB[$3ce] := 8;
     PortW[$3cf] := $80 shr (x and $7) + (Pixel shl 8);
     dummy := Mem[$a000: offset];
     Mem[$a000: offset] := dummy;
     PortW[$3ce] := $ff08;
     PortB[$3ce] := 1;
{$else asmgraph}
      asm
 {$ifndef fpc}
        mov  es, [SegA000]
 {$endif fpc}
        { enable the set / reset function and load the color }
        mov  dx, 3ceh
        mov  ax, 0f01h
        out  dx, ax
        { setup set/reset register }
        mov  ax, [Pixel]
        shl  ax, 8
        out  dx, ax
        { setup the bit mask register }
        mov  al, 8
        out  dx, al
        inc  dx
        { load the bitmask register }
        mov  cx, [X]
        and  cx, 0007h
        mov  al, 80h
        shr  al, cl
        out  dx, ax
 {$ifndef fpc}
        { get the x index and divide by 8 for 16-color }
        mov  ax,[X]
        shr  ax,3
        push ax
        { determine the address }
        mov  ax,80
        mov  bx,[Y]
        mul  bx
        pop  cx
        add  ax,cx
        mov  di,ax
        add  di, [VideoOfs]
        { send the data through the display memory through set/reset }
        mov  bl,es:[di]
        mov  es:[di],bl

        { reset for formal vga operation }
        mov  dx,3ceh
        mov  ax,0ff08h
        out  dx,ax

        { restore enable set/reset register }
        mov  ax,0001h
        out  dx,ax
 {$else fpc}
        { get the x index and divide by 8 for 16-color }
        movzx eax,[X]
        shr  eax,3
        push eax
        { determine the address }
        mov  eax,80
        mov  bx,[Y]
        mul  bx
        pop  ecx
        add  eax,ecx
        mov  edi,eax
        add  edi, [VideoOfs]
        { send the data through the display memory through set/reset }
        mov  bl,fs:[edi+$a0000]
        mov  fs:[edi+$a0000],bl

        { reset for formal vga operation }
        mov  dx,3ceh
        mov  ax,0ff08h
        out  dx,ax

        { restore enable set/reset register }
        mov  ax,0001h
        out  dx,ax
 {$endif fpc}
      end;
{$endif asmgraph}
   end;


 Function GetPixel16(X,Y: Integer):word; {$ifndef fpc}far;{$endif fpc}
{$ifndef asmgraph}
 Var dummy, offset: Word;
     shift: byte;
{$endif asmgraph}
  Begin
    X:= X + StartXViewPort;
    Y:= Y + StartYViewPort;
{$ifndef asmgraph}
    offset := Y * 80 + (x shr 3) + VideoOfs;
    PortB[$3ce] := 4;
    shift := 7 - (X and 7);
    PortB[$3cf] := 0;
    dummy := (Mem[$a000:offset] shr shift) and 1;
    PortB[$3cf] := 1;
    dummy := dummy or (((Mem[$a000:offset] shr shift) and 1) shl 1);
    PortB[$3cf] := 2;
    dummy := dummy or (((Mem[$a000:offset] shr shift) and 1) shl 2);
    PortB[$3cf] := 3;
    dummy := dummy or (((Mem[$a000:offset] shr shift) and 1) shl 3);
    GetPixel16 := dummy;
{$else asmgraph}
    asm
  {$ifndef fpc}
      mov   ax, [X]          { Get X address                    }
      push  ax
      shr   ax, 3
      push  ax

      mov   ax,80
      mov   bx,[Y]
      mul   bx
      pop   cx
      add   ax,cx
      mov   si,ax            { SI = correct offset into video segment }

      mov   es,[SegA000]
      add   si,[VideoOfs]    { Point to correct page offset... }

      mov   dx,03ceh
      mov   ax,4
      out   dx,al
      inc   dx

      pop   ax
      and   ax,0007h
      mov   cl,07
      sub   cl,al
      mov   bl,cl

      { read plane 0 }
      mov   al,0             { Select plane to read }
      out   dx,al
      mov   al,es:[si]       { read display memory }
      shr   al,cl
      and   al,01h
      mov   ah,al            { save bit in AH       }

      { read plane 1 }
      mov   al,1             { Select plane to read }
      out   dx,al
      mov   al,es:[si]
      shr   al,cl
      and   al,01h
      shl   al,1
      or    ah,al            { save bit in AH      }

      { read plane 2 }
      mov   al,2             { Select plane to read }
      out   dx,al
      mov   al,es:[si]
      shr   al,cl
      and   al,01h
      shl   al,2
      or    ah,al            { save bit in AH       }

      { read plane 3 }
      mov   al,3             { Select plane to read }
      out   dx,al
      mov   al,es:[si]
      shr   al,cl
      and   al,01h
      shl   al,3
      or    ah,al            { save bit in AH       }

      mov   al,ah            { 16-bit pixel in AX   }
      xor   ah,ah
      mov   @Result, ax
  {$else fpc}
      movzx eax, [X]          { Get X address                    }
      push  eax
      shr   eax, 3
      push  eax

      mov   eax,80
      mov   bx,[Y]
      mul   bx
      pop   ecx
      add   eax,ecx
      mov   esi,eax            { SI = correct offset into video segment }

      add   esi,[VideoOfs]    { Point to correct page offset... }

      mov   dx,03ceh
      mov   ax,4
      out   dx,al
      inc   dx

      pop   eax
      and   eax,0007h
      mov   cl,07
      sub   cl,al
      mov   bl,cl

      { read plane 0 }
      mov   al,0             { Select plane to read }
      out   dx,al
      mov   al,fs:[esi+$a0000]       { read display memory }
      shr   al,cl
      and   al,01h
      mov   ah,al            { save bit in AH       }

      { read plane 1 }
      mov   al,1             { Select plane to read }
      out   dx,al
      mov   al,fs:[esi+$a0000]
      shr   al,cl
      and   al,01h
      shl   al,1
      or    ah,al            { save bit in AH      }

      { read plane 2 }
      mov   al,2             { Select plane to read }
      out   dx,al
      mov   al,fs:[esi+$a0000]
      shr   al,cl
      and   al,01h
      shl   al,2
      or    ah,al            { save bit in AH       }

      { read plane 3 }
      mov   al,3             { Select plane to read }
      out   dx,al
      mov   al,fs:[esi+$a0000]
      shr   al,cl
      and   al,01h
      shl   al,3
      or    ah,al            { save bit in AH       }

      mov   al,ah            { 16-bit pixel in AX   }
      xor   ah,ah
      mov   @Result, ax
  {$endif fpc}
    end;
{$endif asmgraph}
  end;

Procedure GetScanLine16(y: integer; var data);

var dummylong: longint;
    Offset, count, count2, amount, index: word;
    plane: byte;
Begin
{$ifdef logging}
  LogLn('GetScanLine16 start, length to get: '+strf(ViewWidth+1)+' at y = '+strf(y));
{$Endif logging}
  PortB[$3ce] := 4;
  offset := (Y + StartYViewPort) * 80 + (StartXViewPort shr 3) + VideoOfs;
{$ifdef logging}
  LogLn('Offset: '+HexStr(offset,4)+' - ' + strf(offset));
{$Endif logging}
  { first get enough pixels so offset is 32bit aligned }
  amount := 0;
  index := 0;
  If ((StartXViewPort and 31) <> 0) Or
     (ViewWidth < 32) Then
    Begin
      If (ViewWidth >= 32+32-(StartXViewPort and 31)) Then
        amount := 32-(StartXViewPort and 31)
      Else amount := ViewWidth + 1;
{$ifdef logging}
      LogLn('amount to align to 32bits or to get all: ' + strf(amount));
{$Endif logging}
      For count := 0 to amount-1 do
        WordArray(Data)[Count] := getpixel16(Count,y);
      index := count+1;
      Inc(Offset,(amount+7) shr 3);
{$ifdef logging}
      LogLn('offset now: '+HexStr(offset,4)+' - ' + strf(offset));
      LogLn('index now: '+strf(index));
{$Endif logging}
    End;
  amount := ViewWidth + 1 - amount;
{$ifdef logging}
  LogLn('amount left: ' + strf(amount));
{$Endif logging}
  If amount = 0 Then Exit;
  { first get everything from plane 3 (4th plane) }
  PortB[$3cf] := 3;
  Count := 0;
  For Count := 1 to (amount shr 5) Do
    Begin
      dummylong := MemL[$a000:offset+(Count-1)*4];
      dummylong :=
        ((dummylong and $ff) shl 24) or
        ((dummylong and $ff00) shl 8) or
        ((dummylong and $ff0000) shr 8) or
        ((dummylong and $ff000000) shr 24);
      For Count2 := 31 downto 0 Do
        Begin
          WordArray(Data)[index+Count2] := DummyLong and 1;
          DummyLong := DummyLong shr 1;
        End;
      Inc(Index, 32);
    End;
{ Now get the data from the 3 other planes }
  plane := 3;
  Repeat
    Dec(Index,Count*32);
    Dec(plane);
    PortB[$3cf] := plane;
    Count := 0;
    For Count := 1 to (amount shr 5) Do
      Begin
        dummylong := MemL[$a000:offset+(Count-1)*4];
        dummylong :=
          ((dummylong and $ff) shl 24) or
          ((dummylong and $ff00) shl 8) or
          ((dummylong and $ff0000) shr 8) or
          ((dummylong and $ff000000) shr 24);
        For Count2 := 31 downto 0 Do
          Begin
            WordArray(Data)[index+Count2] :=
              (WordArray(Data)[index+Count2] shl 1) + (DummyLong and 1);
            DummyLong := DummyLong shr 1;
          End;
        Inc(Index, 32);
      End;
  Until plane = 0;
  amount := amount and 31;
  Dec(index);
{$ifdef Logging}
  LogLn('Last array index written to: '+strf(index));
  LogLn('amount left: '+strf(amount)+' starting at x = '+strf(index+1));
{$Endif logging}
  For Count := 1 to amount Do
    WordArray(Data)[index+Count] := getpixel16(index+Count,y);
{$ifdef logging}
  LogLn('First 32 bytes gotten with getscanline16: ');
  If ViewWidth + 1 >= 32 Then
    Count2 := 32
  Else Count2 := ViewWidth;
  For Count := 0 to Count2-1 Do
    Log(strf(WordArray(Data)[Count])+' ');
  LogLn('');
  If ViewWidth + 1 >= 32 Then
    Begin
      LogLn('Last 32 bytes gotten with getscanline16: ');
      For Count := 31 downto 0 Do
      Log(strf(WordArray(Data)[ViewWidth-Count])+' ');
    End;
  LogLn('');
  GetScanLineDefault(y,Data);
  LogLn('First 32 bytes gotten with getscanlinedef: ');
  If ViewWidth + 1 >= 32 Then
    Count2 := 32
  Else Count2 := ViewWidth;
  For Count := 0 to Count2-1 Do
    Log(strf(WordArray(Data)[Count])+' ');
  LogLn('');
  If ViewWidth + 1 >= 32 Then
    Begin
      LogLn('Last 32 bytes gotten with getscanlinedef: ');
      For Count := 31 downto 0 Do
      Log(strf(WordArray(Data)[ViewWidth-Count])+' ');
    End;
  LogLn('');
  LogLn('GetScanLine16 end');
{$Endif logging}
End;

 Procedure DirectPutPixel16(X,Y : Integer); {$ifndef fpc}far;{$endif fpc}
 { x,y -> must be in global coordinates. No clipping. }
  var
   color: word;
{$ifndef asmgraph}
  offset: word;
  dummy: byte;
{$endif asmgraph}
 begin
    case CurrentWriteMode of
      XORPut:
        begin
      { getpixel wants local/relative coordinates }
          Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
          Color := CurrentColor Xor Color;
        end;
      OrPut:
        begin
      { getpixel wants local/relative coordinates }
          Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
          Color := CurrentColor Or Color;
        end;
      AndPut:
        begin
      { getpixel wants local/relative coordinates }
          Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
          Color := CurrentColor And Color;
        end;
      NotPut:
        begin
          Color := Not Color;
        end
      else
        Color := CurrentColor;
    end;
{$ifndef asmgraph}
    offset := Y * 80 + (X shr 3) + VideoOfs;
    PortW[$3ce] := $f01;
    PortW[$3ce] := Color shl 8;
    PortB[$3ce] := 8;
    PortW[$3cf] := $80 shr (X and 7) + (Color shl 8);
    dummy := Mem[$a000: offset];
    Mem[$a000: offset] := dummy;
    PortW[$3ce] := $ff08;
    PortB[$3ce] := 1;
{$else asmgraph}
    asm
  {$ifndef fpc}
      mov  es, [SegA000]
      { enable the set / reset function and load the color }
      mov  dx, 3ceh
      mov  ax, 0f01h
      out  dx, ax
      { setup set/reset register }
      mov  ax, [Color]
      shl  ax, 8
      out  dx, ax
      { setup the bit mask register }
      mov  al, 8
      out  dx, al
      inc  dx
      { load the bitmask register }
      mov  cx, [X]
      and  cx, 0007h
      mov  al, 80h
      shr  al, cl
      out  dx, ax
      { get the x index and divide by 8 for 16-color }
      mov  ax,[X]
      shr  ax,3
      push ax
      { determine the address }
      mov  ax,80
      mov  bx,[Y]
      mul  bx
      pop  cx
      add  ax,cx
      mov  di,ax
      { send the data through the display memory through set/reset }
      add  di,[VideoOfs]   { add correct page }
      mov  bl,es:[di]
      mov  es:[di],bl

      { reset for formal vga operation }
      mov  dx,3ceh
      mov  ax,0ff08h
      out  dx,ax

      { restore enable set/reset register }
      mov  ax,0001h
      out  dx,ax
  {$else fpc}
      { enable the set / reset function and load the color }
      mov  dx, 3ceh
      mov  ax, 0f01h
      out  dx, ax
      { setup set/reset register }
      mov  ax, [Color]
      shl  ax, 8
      out  dx, ax
      { setup the bit mask register }
      mov  al, 8
      out  dx, al
      inc  dx
      { load the bitmask register }
      mov  cx, [X]
      and  cx, 0007h
      mov  al, 80h
      shr  al, cl
      out  dx, ax
      { get the x index and divide by 8 for 16-color }
      movzx eax,[X]
      shr  eax,3
      push eax
      { determine the address }
      mov  eax,80
      mov  bx,[Y]
      mul  bx
      pop  ecx
      add  eax,ecx
      mov  edi,eax
      { send the data through the display memory through set/reset }
      add  edi,[VideoOfs]   { add correct page }
      mov  bl,fs:[edi+$a0000]
      mov  fs:[edi+$a0000],bl

      { reset for formal vga operation }
      mov  dx,3ceh
      mov  ax,0ff08h
      out  dx,ax

      { restore enable set/reset register }
      mov  ax,0001h
      out  dx,ax
  {$endif fpc}
    end;
{$endif asmgraph}
 end;

{$ifndef tp}
  procedure HLine16(x,x2,y: integer); {$ifndef fpc}far;{$endif fpc}

   var
      xtmp: integer;
      ScrOfs,HLength : word;
      LMask,RMask : byte;

   Begin

    { must we swap the values? }
    if x > x2 then
      Begin
        xtmp := x2;
        x2 := x;
        x:= xtmp;
      end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    X2  := X2 + StartXViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    ScrOfs:=y*ScrWidth+x div 8;
    HLength:=x2 div 8-x div 8;
    LMask:=$ff shr (x and 7);
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
    RMask:=$ff shl (7-(x2 and 7));
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
    if HLength=0 then
      LMask:=LMask and RMask;
    port[$3ce]:=0;
    If CurrentWriteMode <> NotPut Then
      port[$3cf]:= CurrentColor
    else port[$3cf]:= not CurrentColor;
    port[$3ce]:=1;
    port[$3cf]:=$f;
    port[$3ce]:=3;
    case CurrentWriteMode of
       XORPut:
         port[$3cf]:=3 shl 3;
       ANDPut:
         port[$3cf]:=1 shl 3;
       ORPut:
         port[$3cf]:=2 shl 3;
       NormalPut, NotPut:
         port[$3cf]:=0
       else
         port[$3cf]:=0
    end;

    port[$3ce]:=8;
    port[$3cf]:=LMask;
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
    Mem[$a000:ScrOfs]:=Mem[$a000:ScrOfs]+1;
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
    port[$3ce]:=8;
    if HLength>0 then
      begin
         dec(HLength);
         inc(ScrOfs);
         if HLength>0 then
           begin
              port[$3cf]:=$ff;
              seg_bytemove(dosmemselector,$a0000+ScrOfs,dosmemselector,$a0000+ScrOfs,HLength);
              ScrOfs:=ScrOfs+HLength;
           end;
         port[$3cf]:=RMask;
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
         Mem[$a000:ScrOfs]:=Mem[$a000:ScrOfs]+1;
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
      end;
    // clean up
    port[$3cf]:=0;
    port[$3ce]:=8;
    port[$3cf]:=$ff;
    port[$3ce]:=1;
    port[$3cf]:=0;
    port[$3ce]:=3;
    port[$3cf]:=0;
   end;

  procedure VLine16(x,y,y2: integer); {$ifndef fpc}far;{$endif fpc}

   var
     ytmp: integer;
     ScrOfs,i : longint;
     BitMask : byte;

  Begin
    { must we swap the values? }
    if y > y2 then
     Begin
       ytmp := y2;
       y2 := y;
       y:= ytmp;
     end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    Y2  := Y2 + StartYViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x,y2,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    ScrOfs:=y*ScrWidth+x div 8;
    BitMask:=$80 shr (x and 7);
    port[$3ce]:=0;
    If CurrentWriteMode <> NotPut Then
      port[$3cf]:= CurrentColor
    else port[$3cf]:= not CurrentColor;
    port[$3ce]:=1;
    port[$3cf]:=$f;
    port[$3ce]:=8;
    port[$3cf]:=BitMask;
    port[$3ce]:=3;
    case CurrentWriteMode of
       XORPut:
         port[$3cf]:=3 shl 3;
       ANDPut:
         port[$3cf]:=1 shl 3;
       ORPut:
         port[$3cf]:=2 shl 3;
       NormalPut, NotPut:
         port[$3cf]:=0
       else
         port[$3cf]:=0
    end;
    for i:=y to y2 do
      begin
{$ifopt r+}
{$define rangeOn}
{$r-}
{$endif}
{$ifopt q+}
{$define overflowOn}
{$q-}
{$endif}
         Mem[$a000:ScrOfs]:=Mem[$a000:ScrOfs]+1;
{$ifdef rangeOn}
{$undef rangeOn}
{$r+}
{$endif}
{$ifdef overflowOn}
{$undef overflowOn}
{$q+}
{$endif}
         ScrOfs:=ScrOfs+ScrWidth;
      end;
    // clean up
    port[$3cf]:=0;
    port[$3ce]:=8;
    port[$3cf]:=$ff;
    port[$3ce]:=1;
    port[$3cf]:=0;
    port[$3ce]:=3;
    port[$3cf]:=0;
  End;
{$endif tp}


 procedure SetVisual480(page: word); {$ifndef fpc}far;{$endif fpc}
 { no page flipping support in 640x480 mode }
  begin
    VideoOfs := 0;
  end;

 procedure SetActive480(page: word); {$ifndef fpc}far;{$endif fpc}
 { no page flipping support in 640x480 mode }
  begin
    VideoOfs := 0;
  end;


 procedure SetVisual200(page: word); {$ifndef fpc}far;{$endif fpc}
  { two page support... }
  begin
    if page > HardwarePages then exit;
    asm
      mov ax,[page]    { only lower byte is supported. }
      mov ah,05h
{$ifdef fpc}
      push ebp
{$endif fpc}
      int 10h
{$ifdef fpc}
      pop ebp
{$endif fpc}

      { read start address }
      mov dx,3d4h
      mov al,0ch
      out dx,al
      inc dx
      in  al,dx
      mov ah,al
      dec dx
      mov al,0dh
      out dx,al
      in  al,dx
    end;
  end;

 procedure SetActive200(page: word); {$ifndef fpc}far;{$endif fpc}
  { two page support... }
  begin
    case page of
     0 : VideoOfs := 0;
     1 : VideoOfs := 16384;
     2 : VideoOfs := 32768;
    else
      VideoOfs := 0;
    end;
  end;

 procedure SetVisual350(page: word); {$ifndef fpc}far;{$endif fpc}
  { one page support... }
  begin
    if page > HardwarePages then exit;
    asm
      mov ax,[page]    { only lower byte is supported. }
      mov ah,05h
{$ifdef fpc}
      push ebp
{$endif fpc}
      int 10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
    end;
  end;

 procedure SetActive350(page: word); {$ifndef fpc}far;{$endif fpc}
  { one page support... }
  begin
    case page of
     0 : VideoOfs := 0;
     1 : VideoOfs := 32768;
    else
      VideoOfs := 0;
    end;
  end;





 {************************************************************************}
 {*                     320x200x256c Routines                            *}
 {************************************************************************}

 Procedure Init320; {$ifndef fpc}far;{$endif fpc} assembler;
  asm
    mov ax,0013h
{$ifdef fpc}
    push ebp
{$endif fpc}
    int 10h
{$ifdef fpc}
    pop ebp
{$endif fpc}
  end;

 Procedure PutPixel320(X,Y : Integer; Pixel: Word); {$ifndef fpc}far;{$endif fpc}
 { x,y -> must be in local coordinates. Clipping if required. }
  Begin
    X:= X + StartXViewPort;
    Y:= Y + StartYViewPort;
    { convert to absolute coordinates and then verify clipping...}
    if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
{$ifndef asmgraph}
    Mem[$a000: y * 320 + x + VideoOfs] := Lo(Pixel);
{$else asmgraph}
    asm
  {$ifndef fpc}
      mov    es, [SegA000]
      mov    ax, [Y]
      mov    di, [X]
      xchg   ah, al            { The value of Y must be in AH }
      add    di, ax
      shr    ax, 2
      add    di, ax
      add    di, [VideoOfs]    { point to correct page.. }
      mov    ax, [Pixel]
      mov    es:[di], al
  {$else fpc}
      movzx  edi, x
      movzx  ebx, y
      add    edi, [VideoOfs]
      shl    ebx, 6
      add    edi, ebx
      mov    ax, pixel
      mov    fs:[edi+ebx*4+$a0000], al
  {$endif fpc}
    end;
{$endif asmgraph}
  end;


 Function GetPixel320(X,Y: Integer):word; {$ifndef fpc}far;{$endif fpc}
  Begin
   X:= X + StartXViewPort;
   Y:= Y + StartYViewPort;
{$ifndef asmgraph}
   GetPixel320 := Mem[$a000:y * 320 + x + VideoOfs];
{$else asmgraph}
    asm
  {$ifndef fpc}
      mov    es, [SegA000]
      mov    ax, [Y]
      mov    di, [X]
      xchg   ah, al            { The value of Y must be in AH }
      add    di, ax
      shr    ax, 2
      add    di, ax
      xor    ax, ax
      add    di, [VideoOfs]   { point to correct gfx page ... }
      mov    al,es:[di]
      mov    @Result,ax
  {$else fpc}
      movzx  edi, x
      movzx  ebx, y
      add    edi, [VideoOfs]
      shl    ebx, 6
      add    edi, ebx
      mov    al, fs:[edi+ebx*4+$a0000]
      mov    @Result, al
 {$endif fpc}
    end;
{$endif asmgraph}
  end;


 Procedure DirectPutPixel320(X,Y : Integer); {$ifndef fpc}far;{$endif fpc}
 { x,y -> must be in global coordinates. No clipping. }
{$ifndef asmgraph}
 var offset: word;
     dummy: Byte;
 begin
   dummy := CurrentColor;
   offset := y * 320 + x + VideoOfs;
   case CurrentWriteMode of
     XorPut: dummy := dummy xor Mem[$a000:offset];
     OrPut: dummy := dummy or Mem[$a000:offset];
     AndPut: dummy := dummy and Mem[$a000:offset];
     NotPut: dummy := Not dummy;
   end;
   Mem[$a000:offset] := dummy;
 end;
{$else asmgraph}
  assembler;
    asm
  {$ifndef fpc}
      mov    es, [SegA000]
      mov    ax, [Y]
      mov    di, [X]
      xchg   ah, al            { The value of Y must be in AH }
      add    di, ax
      shr    ax, 2
      add    di, ax
      add    di, [VideoOfs]
      mov    ax, [CurrentColor]
      cmp    [CurrentWriteMode],XORPut   { check write mode   }
      jne    @MOVMode
      mov    ah,es:[di]        { read the byte...             }
      xor    al,ah             { xor it and return value into AL }
    @MovMode:
      mov    es:[di], al
  {$else fpc}
      movzx  edi, y
      shl    edi, 6
      mov    ebx, edx
      add    edi, [VideoOfs]
      mov    ax, [CurrentColor]
      cmp    [CurrentWriteMode],XORPut   { check write mode   }
      jne    @MOVMode
      mov    bl, fs:[edi+ebx*4+$a0000]
      xor    al, bl
    @MovMode:
      mov    fs:[edi+ebx*4+$a0000], al
{$endif fpc}
    end;
{$endif asmgraph}


 procedure SetVisual320(page: word); {$ifndef fpc}far;{$endif fpc}
  { no page support... }
  begin
  end;

 procedure SetActive320(page: word); {$ifndef fpc}far;{$endif fpc}
  { no page support... }
  begin
   VideoOfs := 0;
  end;

 {************************************************************************}
 {*                       Mode-X related routines                        *}
 {************************************************************************}
const CrtAddress: word = 0;

 procedure InitModeX; {$ifndef fpc}far;{$endif fpc}
  begin
   asm
     {see if we are using color-/monochorme display}
     MOV DX,3CCh  {use output register:     }
     IN AL,DX
     TEST AL,1    {is it a color display?    }
     MOV DX,3D4h
     JNZ @L1      {yes  }
     MOV DX,3B4h  {no  }
  @L1:          {DX = 3B4h / 3D4h = CRTAddress-register for monochrome/color}
     MOV CRTAddress,DX

     MOV  AX, 0013h
{$ifdef fpc}
     push ebp
{$EndIf fpc}
     INT  10h
{$ifdef fpc}
    pop ebp
{$EndIf fpc}
     MOV DX,03C4h   {select memory-mode-register at sequencer port    }
     MOV AL,04
     OUT DX,AL
     INC DX         {read in data via the according data register     }
     IN  AL,DX
     AND AL,0F7h    {bit 3 := 0: don't chain the 4 planes}
     OR  AL,04      {bit 2 := 1: no odd/even mechanism }
     OUT DX,AL      {activate new settings    }
     MOV DX,03C4h   {s.a.: address sequencer reg. 2 (=map-mask),...   }
     MOV AL,02
     OUT DX,AL
     INC DX
     MOV AL,0Fh     {...and allow access to all 4 bit maps            }
     OUT DX,AL
{$ifndef fpc}
     MOV AX,[SegA000]  {starting with segment A000h, set 8000h logical     }
     MOV ES,AX      {words = 4*8000h physical words (because of 4     }
     XOR DI,DI      {bitplanes) to 0                                  }
     XOR AX,AX
     MOV CX,8000h
     CLD
     REP STOSW
{$else fpc}
     push es
     push fs
     mov edi, $a0000
     pop es
     xor eax, eax
     mov ecx, 4000h
     cld
     rep stosd
     pop es
{$EndIf fpc}
     MOV DX,CRTAddress  {address the underline-location-register at }
     MOV AL,14h         {the CRT-controller port, read out the according      }
     OUT DX,AL          {data register:                            }
     INC DX
     IN  AL,DX
     AND AL,0BFh    {bit 6:=0: no double word addressing scheme in}
     OUT DX,AL      {video RAM                              }
     DEC DX
     MOV AL,17h     {select mode control register     }
     OUT DX,AL
     INC DX
     IN  AL,DX
     OR  AL,40h     {bit 6 := 1: memory access scheme=linear bit array      }
     OUT DX,AL
  end;
 end;


 Function GetPixelX(X,Y: Integer): word; {$ifndef fpc}far;{$endif fpc}
{$ifndef asmgraph}
 var offset: word;
{$endif asmgraph}
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
{$ifndef asmgraph}
     offset := y * 80 + x shr 2 + VideoOfs;
     PortW[$3c4] := FirstPlane shl (x and 3);
     GetPixelX := Mem[$a000:offset];
{$else asmgraph}
    asm
  {$ifndef fpc}
     mov di,[Y]                   ; (* DI = Y coordinate                 *)
     (* Multiply by 80 start *)
     mov bx, di
     shl di, 6                    ; (* Faster on 286/386/486 machines    *)
     shl bx, 4
     add di, bx                   ;  (* Multiply Value by 80             *)
     (* End multiply by 80  *)
     mov cx, [X]
     mov ax, cx
    {DI = Y * LINESIZE, BX = X, coordinates admissible}
     shr ax, 1                    ; (* Faster on 286/86 machines         *)
     shr ax, 1
     add di, ax                ; {DI = Y * LINESIZE + (X SHR 2) }
     add di, [VideoOfs]  ; (* Pointing at start of Active page *)
    (* Select plane to use *)
    mov dx, 03c4h
    mov ax, FirstPlane        ; (* Map Mask & Plane Select Register *)
    and cl, 03h               ; (* Get Plane Bits                   *)
    shl ah, cl                ; (* Get Plane Select Value           *)
    out dx, ax
   (* End selection of plane *)
    mov es,[SegA000]
    mov al, ES:[DI]
    xor ah, ah
    mov @Result, ax
  {$else fpc}
     movzx edi,[Y]                   ; (* DI = Y coordinate                 *)
     (* Multiply by 80 start *)
     mov ebx, edi
     shl edi, 6                    ; (* Faster on 286/386/486 machines    *)
     shl ebx, 4
     add edi, ebx                   ;  (* Multiply Value by 80             *)
     (* End multiply by 80  *)
     movzx ecx, [X]
     movzx eax, [Y]
    {DI = Y * LINESIZE, BX = X, coordinates admissible}
     shr eax, 2
     add edi, eax                ; {DI = Y * LINESIZE + (X SHR 2) }
     add edi, [VideoOfs]  ; (* Pointing at start of Active page *)
    (* Select plane to use *)
    mov dx, 03c4h
    mov ax, FirstPlane        ; (* Map Mask & Plane Select Register *)
    and cl, 03h               ; (* Get Plane Bits                   *)
    shl ah, cl                ; (* Get Plane Select Value           *)
    out dx, ax
   (* End selection of plane *)
    mov ax, fs:[edi+$a0000]
    mov @Result, ax
  {$endif fpc}
   end;
{$endif asmgraph}
 end;

 procedure SetVisualX(page: word); {$ifndef fpc}far;{$endif fpc}
  { 4 page support... }

   Procedure SetVisibleStart(AOffset: word); Assembler;
   (* Select where the left corner of the screen will be *)
   { By Matt Pritchard }
    asm
      { Wait if we are currently in a Vertical Retrace        }
     MOV     DX, INPUT_1         { Input Status #1 Register       }
   @DP_WAIT0:
     IN      AL, DX              { Get VGA status                 }
     AND     AL, VERT_RETRACE    { In Display mode yet?           }
     JNZ     @DP_WAIT0           { If Not, wait for it            }

    { Set the Start Display Address to the new page         }

     MOV     DX, CRTC_Index      { We Change the VGA Sequencer    }
     MOV     AL, START_DISP_LO   { Display Start Low Register     }
{$ifndef fpc}
     MOV     AH, BYTE PTR [AOffset] { Low 8 Bits of Start Addr    }
     OUT     DX, AX              { Set Display Addr Low           }
     MOV     AL, START_DISP_HI   { Display Start High Register    }
     MOV     AH, BYTE PTR [AOffset+1] { High 8 Bits of Start Addr }
{$else fpc}
    mov ah, byte [AOffset]
    out dx, ax
    mov AL, START_DISP_HI
    mov ah, byte [AOffset+1]
{$endif fpc}
     OUT     DX, AX              { Set Display Addr High          }
     { Wait for a Vertical Retrace to smooth out things      }

     MOV     DX, INPUT_1         { Input Status #1 Register       }

  @DP_WAIT1:
     IN      AL, DX              { Get VGA status                 }
     AND     AL, VERT_RETRACE    { Vertical Retrace Start?        }
     JZ      @DP_WAIT1           { If Not, wait for it            }
    { Now Set Display Starting Address                     }
  end;

{$ifdef fpc}
  {$undef asmgraph}
{$endif fpc}

  begin
    Case page of
      0: SetVisibleStart(0);
      1: SetVisibleStart(16000);
      2: SetVisibleStart(32000);
      3: SetVisibleStart(48000);
    else
      SetVisibleStart(0);
    end;
  end;

 procedure SetActiveX(page: word); {$ifndef fpc}far;{$endif fpc}
  { 4 page support... }
  begin
   case page of
     0: VideoOfs := 0;
     1: VideoOfs := 16000;
     2: VideoOfs := 32000;
     3: VideoOfs := 48000;
   else
     VideoOfs:=0;
   end;
  end;

 Procedure PutPixelX(X,Y: Integer; color:word); {$ifndef fpc}far;{$endif fpc}
{$ifndef asmgraph}
 var offset: word;
     dummy: byte;
{$endif asmgraph}
  begin
    X:= X + StartXViewPort;
    Y:= Y + StartYViewPort;
    { convert to absolute coordinates and then verify clipping...}
    if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
{$ifndef asmgraph}
     Dummy := color;
     offset := y * 80 + x shr 2 + VideoOfs;
     PortW[$3c4] := FirstPlane shl (x and 3);
     If CurrentWriteMode = XorPut Then
       Dummy := Dummy Xor Mem[$a000:offset];
     Mem[$a000:offset] := Dummy;
{$else asmgraph}
     asm
      mov di,[Y]                   ; (* DI = Y coordinate                 *)
      (* Multiply by 80 start *)
      mov bx, di
      shl di, 6                    ; (* Faster on 286/386/486 machines    *)
      shl bx, 4
      add di, bx                   ;  (* Multiply Value by 80             *)
      (* End multiply by 80  *)
      mov cx, [X]
      mov ax, cx
      {DI = Y * LINESIZE, BX = X, coordinates admissible}
      shr ax, 2
      add di, ax                ; {DI = Y * LINESIZE + (X SHR 2) }
      add di, [VideoOfs]        ; (* Pointing at start of Active page *)
      (* Select plane to use *)
      mov dx, 03c4h
      mov ax, FirstPlane        ; (* Map Mask & Plane Select Register *)
      and cl, 03h               ; (* Get Plane Bits                   *)
      shl ah, cl                ; (* Get Plane Select Value           *)
      out dx, ax
      (* End selection of plane *)
      mov es,[SegA000]
      mov ax,[Color]            ; { only lower byte is used. }
      cmp [CurrentWriteMode],XORPut   { check write mode   }
      jne @MOVMode
      mov ah,es:[di]        { read the byte...             }
      xor al,ah             { xor it and return value into AL }
    @MovMode:
      mov es:[di], al
    end;
{$endif asmgraph}
  end;


 Procedure DirectPutPixelX(X,Y: Integer); {$ifndef fpc}far;{$endif fpc}
 { x,y -> must be in global coordinates. No clipping. }
{$ifndef asmgraph}
 Var offset: Word;
     dummy: Byte;
 begin
   dummy := CurrentColor;
   offset := y * 80 + x shr 2 + VideoOfs;
   PortW[$3c4] := FirstPlane shl (x and 3);
   If CurrentWriteMode = XorPut Then
     dummy := dummy xor Mem[$a000: offset];
   Mem[$a000: offset] := Dummy;
 end;
{$else asmgraph}
 Assembler;
 asm
   mov di,[Y]                   ; (* DI = Y coordinate                 *)
 (* Multiply by 80 start *)
   mov bx, di
   shl di, 6                    ; (* Faster on 286/386/486 machines    *)
   shl bx, 4
   add di, bx                   ;  (* Multiply Value by 80             *)
 (* End multiply by 80  *)
   mov cx, [X]
   mov ax, cx
  {DI = Y * LINESIZE, BX = X, coordinates admissible}
   shr ax, 2
   add di, ax                ; {DI = Y * LINESIZE + (X SHR 2) }
   add di, [VideoOfs]        ; (* Pointing at start of Active page *)
 (* Select plane to use *)
   mov dx, 03c4h
   mov ax, FirstPlane        ; (* Map Mask & Plane Select Register *)
   and cl, 03h               ; (* Get Plane Bits                   *)
   shl ah, cl                ; (* Get Plane Select Value           *)
   out dx, ax
 (* End selection of plane *)
   mov es,[SegA000]
   mov ax,[CurrentColor]     ; { only lower byte is used. }
   cmp [CurrentWriteMode],XORPut   { check write mode   }
   jne @MOVMode
   mov ah,es:[di]        { read the byte...             }
   xor al,ah             { xor it and return value into AL }
 @MovMode:
   mov es:[di], al
 end;
{$endif asmgraph}



 {************************************************************************}
 {*                       General routines                               *}
 {************************************************************************}
 var
  SavePtr : pointer;    { pointer to video state                 }
  StateSize: word;      { size in 64 byte blocks for video state }
  VideoMode: byte;      { old video mode before graph mode       }
  SaveSupported : Boolean;    { Save/Restore video state supported? }


      {**************************************************************}
      {*                     DPMI Routines                          *}
      {**************************************************************}

{$IFDEF DPMI}
  RealStateSeg: word;    { Real segment of saved video state }

 Procedure SaveStateVGA; {$ifndef fpc}far;{$endif fpc}
 var
  PtrLong: longint;
  regs: TDPMIRegisters;
  begin
    SaveSupported := FALSE;
    SavePtr := nil;
    { Get the video mode }
    asm
      mov  ah,0fh
      int  10h
      mov  [VideoMode], al
    end;
    { Prepare to save video state...}
    asm
      mov  ax, 1C00h       { get buffer size to save state }
      mov  cx, 00000111b   { Save DAC / Data areas / Hardware states }
{$ifdef fpc}
      push ebp
{$endif fpc}
      int  10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
      mov  [StateSize], bx
      cmp  al,01ch
      jnz  @notok
      mov  [SaveSupported],TRUE
     @notok:
    end;
    if SaveSupported then
      begin

{$ifndef fpc}
        PtrLong:=GlobalDosAlloc(64*StateSize);  { values returned in 64-byte blocks }
{$else fpc}
        PtrLong:=Global_Dos_Alloc(64*StateSize);  { values returned in 64-byte blocks }
{$endif fpc}
        if PtrLong = 0 then
           RunError(203);
        SavePtr := pointer(longint(PtrLong and $0000ffff) shl 16);
{$ifndef fpc}
        { In FPC mode, we can't do anything with this (no far pointers)  }
        { However, we still need to keep it to be able to free the       }
        { memory afterwards. Since this data is not accessed in PM code, }
        { there's no need to save it in a seperate buffer (JM)           }
        if not assigned(SavePtr) then
           RunError(203);
{$endif fpc}
        RealStateSeg := word(PtrLong shr 16);
        FillChar(regs, sizeof(regs), #0);
        { call the real mode interrupt ... }
        regs.eax := $1C01;      { save the state buffer                   }
        regs.ecx := $07;        { Save DAC / Data areas / Hardware states }
        regs.es := RealStateSeg;
        regs.ebx := 0;
        RealIntr($10,regs);
        FillChar(regs, sizeof(regs), #0);
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
        regs.eax := $1C02;      { restore the state buffer                }
        regs.ecx := $07;        { rest DAC / Data areas / Hardware states }
        regs.es := RealStateSeg;
        regs.ebx := 0;
        RealIntr($10,regs);
      end;
  end;

 procedure RestoreStateVGA; {$ifndef fpc}far;{$endif fpc}
  var
   regs:TDPMIRegisters;
  begin
     { go back to the old video mode...}
     asm
      mov  ah,00
      mov  al,[VideoMode]
{$ifdef fpc}
      push ebp
{$endif fpc}
      int  10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
     end;
     { then restore all state information }
{$ifndef fpc}
     if assigned(SavePtr) and (SaveSupported=TRUE) then
{$else fpc}
     { No far pointer support, so it's possible that that assigned(SavePtr) }
     { would return false under FPC. Just check if it's different from nil. }
     if (SavePtr <> nil) and (SaveSupported=TRUE) then
{$endif fpc}
      begin
        FillChar(regs, sizeof(regs), #0);
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
         regs.eax := $1C02;      { restore the state buffer                }
         regs.ecx := $07;        { rest DAC / Data areas / Hardware states }
         regs.es := RealStateSeg;
         regs.ebx := 0;
         RealIntr($10,regs);
{$ifndef fpc}
         if GlobalDosFree(longint(SavePtr) shr 16)<>0 then
{$else fpc}
         if Not Global_Dos_Free(longint(SavePtr) shr 16) then
{$endif fpc}
          RunError(216);

         SavePtr := nil;
       end;
  end;

{$ELSE}

      {**************************************************************}
      {*                     Real mode routines                     *}
      {**************************************************************}


 Procedure SaveStateVGA; far;
  begin
    SavePtr := nil;
    SaveSupported := FALSE;
    { Get the video mode }
    asm
      mov  ah,0fh
      int  10h
      mov  [VideoMode], al
    end;
    { Prepare to save video state...}
    asm
      mov  ax, 1C00h       { get buffer size to save state }
      mov  cx, 00000111b   { Save DAC / Data areas / Hardware states }
      int  10h
      mov  [StateSize], bx
      cmp  al,01ch
      jnz  @notok
      mov  [SaveSupported],TRUE
     @notok:
    end;
    if SaveSupported then
      Begin
        GetMem(SavePtr, 64*StateSize); { values returned in 64-byte blocks }
        if not assigned(SavePtr) then
           RunError(203);
        asm
         mov  ax, 1C01h       { save the state buffer                   }
         mov  cx, 00000111b   { Save DAC / Data areas / Hardware states }
         mov  es, WORD PTR [SavePtr+2]
         mov  bx, WORD PTR [SavePtr]
         int  10h
        end;
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
        asm
         mov  ax, 1C02h       { save the state buffer                   }
         mov  cx, 00000111b   { Save DAC / Data areas / Hardware states }
         mov  es, WORD PTR [SavePtr+2]
         mov  bx, WORD PTR [SavePtr]
         int  10h
        end;
      end;
  end;

 procedure RestoreStateVGA; far;
  begin
     { go back to the old video mode...}
     asm
      mov  ah,00
      mov  al,[VideoMode]
      int  10h
     end;

     { then restore all state information }
     if assigned(SavePtr) and (SaveSupported=TRUE) then
       begin
         { restore state, according to Ralph Brown Interrupt list }
         asm
           mov  ax, 1C02h       { save the state buffer                   }
           mov  cx, 00000111b   { Save DAC / Data areas / Hardware states }
           mov  es, WORD PTR [SavePtr+2]
           mov  bx, WORD PTR [SavePtr]
           int  10h
         end;
         FreeMem(SavePtr, 64*StateSize);
         SavePtr := nil;
       end;
  end;
{$ENDIF DPMI}



   { VGA is never a direct color mode, so no need to check ... }
   Procedure SetVGARGBPalette(ColorNum, RedValue, GreenValue,
      BlueValue : Integer); {$ifndef fpc}far;{$endif fpc}
    begin
      { translate the color number for 16 color mode }
      If MaxColor = 16 Then
        ColorNum := ToRealCols16[ColorNum];
      asm
        { on some hardware - there is a snow like effect       }
        { when changing the palette register directly          }
        { so we wait for a vertical retrace start period.      }
        mov dx, $03da
      @1:
        in    al, dx          { Get input status register    }
        test  al, $08         { check if in vertical retrace }
        jnz   @1              { yes, complete it             }
                              { we have to wait for the next }
                              { retrace to assure ourselves  }
                              { that we have time to complete }
                              { the DAC operation within      }
                              { the vertical retrace period   }
       @2:
        in    al, dx
        test  al, $08
        jz    @2              { repeat until vertical retrace start }

        mov dx, $03c8       { Set color register address to use }
        mov ax, [ColorNum]
        out dx, al
        inc dx              { Point to DAC registers            }
        mov ax, [RedValue]  { Get RedValue                      }
  {      and ax, $ff        } { mask out all upper bits           }
        shr al, 2           { convert to LSB RGB format         }
        out dx, al
        mov ax, [GreenValue]{ Get RedValue                      }
  {      and ax, $ff        } { mask out all upper bits           }
        shr al, 2           { convert to LSB RGB format         }
        out dx, al
        mov ax, [BlueValue] { Get RedValue                      }
  {      and ax, $ff        } { mask out all upper bits           }
        shr al, 2           { convert to LSB RGB format         }
        out dx, al
      end
    End;


   { VGA is never a direct color mode, so no need to check ... }
  Procedure GetVGARGBPalette(ColorNum: integer; Var
      RedValue, GreenValue, BlueValue : integer); {$ifndef fpc}far;{$endif fpc}
   begin
     If MaxColor = 16 Then
       ColorNum := ToRealCols16[ColorNum];
     Port[$03C7] := ColorNum;
     { we must convert to lsb values... because the vga uses the 6 msb bits }
     { which is not compatible with anything.                               }
     RedValue := Integer(Port[$3C9] shl 2);
     GreenValue := Integer(Port[$3C9] shl 2);
     BlueValue := Integer(Port[$3C9] shl 2);
   end;


 {************************************************************************}
 {*                       VESA related routines                          *}
 {************************************************************************}
{$I vesa.inc}

 {************************************************************************}
 {*                       General routines                               *}
 {************************************************************************}
 procedure CloseGraph;
 Begin
    If not isgraphmode then
      begin
        _graphresult := grnoinitgraph;
        exit
      end;
{$ifdef logging}
    LogLn('calling RestoreVideoState at '+strf(longint(RestoreVideoState)));
{$endif logging}
    if not assigned(RestoreVideoState) then
      RunError(216);
{$ifdef logging}
    LogLn('actual call of RestoreVideoState');
{$endif logging}
    RestoreVideoState;
    isgraphmode := false;
 end;

  function QueryAdapterInfo:PModeInfo;
  { This routine returns the head pointer to the list }
  { of supported graphics modes.                      }
  { Returns nil if no graphics mode supported.        }
  { This list is READ ONLY!                           }
   var
    EGADetected : Boolean;
    VGADetected : Boolean;
    mode: TModeInfo;
   begin
     QueryAdapterInfo := ModeList;
     { If the mode listing already exists... }
     { simply return it, without changing    }
     { anything...                           }
     if assigned(ModeList) then
       exit;


     EGADetected := FALSE;
     VGADetected := FALSE;
     { check if Hercules adapter supported ... }
     { check if EGA adapter supported...       }
     asm
       mov ah,12h
       mov bx,0FF10h
{$ifdef fpc}
       push ebp
{$endif fpc}
       int 10h              { get EGA information }
{$ifdef fpc}
       pop ebp
{$endif fpc}
       cmp bh,0ffh
       jz  @noega
       mov [EGADetected],TRUE
     @noega:
     end;
{$ifdef logging}
     LogLn('EGA detected: '+strf(Longint(EGADetected)));
{$endif logging}
     { check if VGA adapter supported...       }
     if EGADetected then
       begin
        asm
         mov ax,1a00h
{$ifdef fpc}
         push ebp
{$endif fpc}
         int 10h            { get display combination code...}
{$ifdef fpc}
         pop ebp
{$endif fpc}
         cmp al,1ah         { check if supported...          }
         jne @novga
         { now check if this is the ATI EGA }
         mov ax,1c00h       { get state size for save...     }
                            { ... all important data         }
         mov cx,07h
{$ifdef fpc}
         push ebp
{$endif fpc}
         int 10h
{$ifdef fpc}
         pop ebp
{$endif fpc}
         cmp al,1ch         { success?                       }
         jne @novga
         mov [VGADetected],TRUE
        @novga:
        end;
       end;
{$ifdef logging}
       LogLn('VGA detected: '+strf(Longint(VGADetected)));
{$endif logging}
     if VGADetected then
       begin
         SaveVideoState := SaveStateVGA;
{$ifdef logging}
         LogLn('Setting VGA SaveVideoState to '+strf(longint(SaveVideoState)));
{$endif logging}
         RestoreVideoState := RestoreStateVGA;
{$ifdef logging}
         LogLn('Setting VGA RestoreVideoState to '+strf(longint(RestoreVideoState)));
{$endif logging}

         InitMode(mode);
         { now add all standard VGA modes...       }
         mode.DriverNumber:= LowRes;
         mode.HardwarePages:= 0;
         mode.ModeNumber:=0;
         mode.ModeName:='320 x 200 VGA';
         mode.MaxColor := 256;
         mode.PaletteSize := mode.MaxColor;
         mode.DirectColor := FALSE;
         mode.MaxX := 319;
         mode.MaxY := 199;
         mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel320;
         mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel320;
         mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel320;
         mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVGARGBPalette;
         mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVGARGBPalette;
         mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisual320;
         mode.SetActivePage := {$ifdef fpc}@{$endif}SetActive320;
         mode.InitMode := {$ifdef fpc}@{$endif}Init320;
         mode.XAspect := 10000;
         mode.YAspect := 10000;
         AddMode(mode);

         { now add all standard VGA modes...       }
         InitMode(mode);
         mode.DriverNumber:= LowRes;
         mode.ModeNumber:=1;
         mode.HardwarePages := 3; { 0..3 }
         mode.ModeName:='320 x 200 ModeX';
         mode.MaxColor := 256;
         mode.DirectColor := FALSE;
         mode.PaletteSize := mode.MaxColor;
         mode.MaxX := 319;
         mode.MaxY := 199;
         mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixelX;
         mode.PutPixel:={$ifdef fpc}@{$endif}PutPixelX;
         mode.GetPixel:={$ifdef fpc}@{$endif}GetPixelX;
         mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVGARGBPalette;
         mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVGARGBPalette;
         mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualX;
         mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveX;
         mode.InitMode := {$ifdef fpc}@{$endif}InitModeX;
         mode.XAspect := 10000;
         mode.YAspect := 10000;
         AddMode(mode);

         InitMode(mode);
         mode.ModeNumber:=VGALo;
         mode.DriverNumber := VGA;
         mode.ModeName:='640 x 200 VGA';
         mode.MaxColor := 16;
         mode.HardwarePages := 2;
         mode.DirectColor := FALSE;
         mode.PaletteSize := mode.MaxColor;
         mode.MaxX := 639;
         mode.MaxY := 199;
         mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16;
         mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16;
         mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16;
         mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVGARGBPalette;
         mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVGARGBPalette;
         mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisual200;
         mode.SetActivePage := {$ifdef fpc}@{$endif}SetActive200;
         mode.InitMode := {$ifdef fpc}@{$endif}Init640x200x16;
         mode.HLine := {$ifdef fpc}@{$endif}HLine16;
         mode.VLine := {$ifdef fpc}@{$endif}VLine16;
         mode.GetScanLine := {$ifdef fpc}@{$endif}GetScanLine16;
         mode.XAspect := 10000;
         mode.YAspect := 10000;
         AddMode(mode);

         InitMode(mode);
         mode.ModeNumber:=VGAMed;
         mode.DriverNumber := VGA;
         mode.ModeName:='640 x 350 VGA';
         mode.HardwarePages := 1;
         mode.MaxColor := 16;
         mode.DirectColor := FALSE;
         mode.PaletteSize := mode.MaxColor;
         mode.MaxX := 639;
         mode.MaxY := 349;
         mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16;
         mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16;
         mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16;
         mode.InitMode := {$ifdef fpc}@{$endif}Init640x350x16;
         mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVGARGBPalette;
         mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVGARGBPalette;
         mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisual350;
         mode.SetActivePage := {$ifdef fpc}@{$endif}SetActive350;
         mode.HLine := {$ifdef fpc}@{$endif}HLine16;
         mode.VLine := {$ifdef fpc}@{$endif}VLine16;
         mode.GetScanLine := {$ifdef fpc}@{$endif}GetScanLine16;
         mode.XAspect := 10000;
         mode.YAspect := 10000;
         AddMode(mode);

         InitMode(mode);
         mode.ModeNumber:=VGAHi;
         mode.DriverNumber := VGA;
         mode.HardwarePages := 0;
         mode.ModeName:='640 x 480 VGA';
         mode.MaxColor := 16;
         mode.DirectColor := FALSE;
         mode.PaletteSize := mode.MaxColor;
         mode.MaxX := 639;
         mode.MaxY := 479;
         mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixel16;
         mode.PutPixel:={$ifdef fpc}@{$endif}PutPixel16;
         mode.GetPixel:={$ifdef fpc}@{$endif}GetPixel16;
         mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVGARGBPalette;
         mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVGARGBPalette;
         mode.InitMode := {$ifdef fpc}@{$endif}Init640x480x16;
         mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisual480;
         mode.SetActivePage := {$ifdef fpc}@{$endif}SetActive480;
         mode.HLine := {$ifdef fpc}@{$endif}HLine16;
         mode.VLine := {$ifdef fpc}@{$endif}VLine16;
         mode.GetScanLine := {$ifdef fpc}@{$endif}GetScanLine16;
         mode.XAspect := 10000;
         mode.YAspect := 10000;
         AddMode(mode);
       end;

     { check if VESA adapter supported...      }
{$ifndef noSupportVESA}
     hasVesa := getVesaInfo(VESAInfo);
{$else noSupportVESA}
     hasVESA := false;
{$endif noSupportVESA}
     if hasVesa then
       begin
         { We have to set and restore the entire VESA state }
         { otherwise, if we use the VGA BIOS only function  }
         { there might be a crash under DPMI, such as in the}
         { ATI Mach64                                       }
         SaveVideoState := SaveStateVESA;
{$ifdef logging}
         LogLn('Setting SaveVideoState to '+strf(longint(SaveVideoState)));
{$endif logging}
         RestoreVideoState := RestoreStateVESA;
{$ifdef logging}
         LogLn('Setting RestoreVideoState to '+strf(longint(RestoreVideoState)));
{$endif logging}
         { now check all supported modes...}
         if SearchVESAModes(m320x200x32k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m320x200x32k;
             mode.DriverNumber := VESA;
             mode.ModeName:='320 x 200 VESA';
             mode.MaxColor := 32768;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 319;
             mode.MaxY := 199;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA32k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA32k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA32k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init320x200x32k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m320x200x64k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m320x200x64k;
             mode.DriverNumber := VESA;
             mode.ModeName:='320 x 200 VESA';
             mode.MaxColor := 65536;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 319;
             mode.MaxY := 199;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA64k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA64k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA64k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init320x200x64k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m640x400x256) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m640x400x256;
             mode.DriverNumber := VESA;
             mode.ModeName:='640 x 400 VESA';
             mode.MaxColor := 256;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 639;
             mode.MaxY := 399;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA256;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA256;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA256;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init640x400x256;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.hline := {$ifdef fpc}@{$endif}HLineVESA256;
             mode.vline := {$ifdef fpc}@{$endif}VLineVESA256;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m640x480x256) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m640x480x256;
             mode.DriverNumber := VESA;
             mode.ModeName:='640 x 480 VESA';
             mode.MaxColor := 256;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 639;
             mode.MaxY := 479;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA256;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA256;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA256;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init640x480x256;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.hline := {$ifdef fpc}@{$endif}HLineVESA256;
             mode.hline := {$ifdef fpc}@{$endif}HLineVESA256;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m640x480x32k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m640x480x32k;
             mode.DriverNumber := VESA;
             mode.ModeName:='640 x 400 VESA';
             mode.MaxColor := 32768;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 639;
             mode.MaxY := 399;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA32k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA32k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA32k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init640x480x32k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m640x480x64k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m640x480x64k;
             mode.DriverNumber := VESA;
             mode.ModeName:='640 x 480 VESA';
             mode.MaxColor := 65536;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 639;
             mode.MaxY := 479;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA64k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA64k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA64k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init640x480x64k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m800x600x16) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m800x600x16;
             mode.DriverNumber := VESA;
             mode.ModeName:='800 x 600 VESA';
             mode.MaxColor := 16;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.DirectColor := FALSE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA16;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA16;
(*             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA16; *)
             mode.InitMode := {$ifdef fpc}@{$endif}Init800x600x16;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m800x600x256) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m800x600x256;
             mode.DriverNumber := VESA;
             mode.ModeName:='800 x 600 VESA';
             mode.MaxColor := 256;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA256;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA256;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA256;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init800x600x256;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.hline := {$ifdef fpc}@{$endif}HLineVESA256;
             mode.vline := {$ifdef fpc}@{$endif}VLineVESA256;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m800x600x32k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m800x600x32k;
             mode.DriverNumber := VESA;
             mode.ModeName:='800 x 600 VESA';
             mode.MaxColor := 32768;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA32k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA32k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA32k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init800x600x32k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m800x600x64k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m800x600x16;
             mode.DriverNumber := VESA;
             mode.ModeName:='800 x 600 VESA';
             mode.MaxColor := 65536;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA64k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA64k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA64k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init800x600x64k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1024x768x16) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1024x768x16;
             mode.DriverNumber := VESA;
             mode.ModeName:='1024 x 768 VESA';
             mode.MaxColor := 16;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA16;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA16;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
(*             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA16; *)
             mode.InitMode := {$ifdef fpc}@{$endif}Init1024x768x16;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1024x768x256) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1024x768x256;
             mode.DriverNumber := VESA;
             mode.ModeName:='1024 x 768 VESA';
             mode.MaxColor := 256;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA256;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA256;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA256;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init1024x768x256;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.vline := {$ifdef fpc}@{$endif}VLineVESA256;
             mode.hline := {$ifdef fpc}@{$endif}HLineVESA256;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1024x768x32k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1024x768x32k;
             mode.DriverNumber := VESA;
             mode.ModeName:='1024 x 768 VESA';
             mode.MaxColor := 32768;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA32k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA32k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA32k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init640x480x32k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1024x768x64k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1024x768x64k;
             mode.DriverNumber := VESA;
             mode.ModeName:='1024 x 768 VESA';
             mode.MaxColor := 65536;
             mode.DirectColor := TRUE;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA64k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA64k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA64k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.InitMode := {$ifdef fpc}@{$endif}Init1024x768x64k;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1280x1024x16) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1280x1024x16;
             mode.DriverNumber := VESA;
             mode.ModeName:='1280 x 1024 VESA';
             mode.MaxColor := 16;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.DirectColor := FALSE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA16;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA16;
(*             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA16; *)
             mode.InitMode := {$ifdef fpc}@{$endif}Init1280x1024x16;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1280x1024x256) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1280x1024x256;
             mode.DriverNumber := VESA;
             mode.ModeName:='1280 x 1024 VESA';
             mode.MaxColor := 256;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.DirectColor := FALSE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA256;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA256;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA256;
             mode.InitMode := {$ifdef fpc}@{$endif}Init1280x1024x256;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.vline := {$ifdef fpc}@{$endif}VLineVESA256;
             mode.hline := {$ifdef fpc}@{$endif}HLineVESA256;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1280x1024x32k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1280x1024x32k;
             mode.DriverNumber := VESA;
             mode.ModeName:='1280 x 1024 VESA';
             mode.MaxColor := 32768;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.DirectColor := TRUE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA32k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA32k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA32k;
             mode.InitMode := {$ifdef fpc}@{$endif}Init1280x1024x32k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
         if SearchVESAModes(m1280x1024x64k) then
           begin
             InitMode(mode);
             mode.ModeNumber:=m1280x1024x64k;
             mode.DriverNumber := VESA;
             mode.ModeName:='1280 x 1024 VESA';
             mode.MaxColor := 65536;
             { the ModeInfo is automatically set if the mode is supported }
             { by the call to SearchVESAMode.                             }
             mode.HardwarePages := VESAModeInfo.NumberOfPages;
             mode.DirectColor := TRUE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:={$ifdef fpc}@{$endif}DirectPutPixVESA64k;
             mode.PutPixel:={$ifdef fpc}@{$endif}PutPixVESA64k;
             mode.GetPixel:={$ifdef fpc}@{$endif}GetPixVESA64k;
             mode.InitMode := {$ifdef fpc}@{$endif}Init1280x1024x64k;
             mode.SetRGBPalette := {$ifdef fpc}@{$endif}SetVESARGBPalette;
             mode.GetRGBPalette := {$ifdef fpc}@{$endif}GetVESARGBPalette;
             mode.SetVisualPage := {$ifdef fpc}@{$endif}SetVisualVESA;
             mode.SetActivePage := {$ifdef fpc}@{$endif}SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;
       end;
   end;

{
$Log$
Revision 1.22  1999-10-08 14:28:18  jonas
  * fixed set/getvgargbpalette for VGA 16 color modes

Revision 1.21  1999/09/27 23:34:40  peter
  * new graph unit is default for go32v2
  * removed warnings/notes

Revision 1.20  1999/09/26 13:31:06  jonas
  * changed name of modeinfo variable to vesamodeinfo and fixed
    associated errors (fillchar(modeinfo,sizeof(tmodeinfo),#0) instead
    of sizeof(TVesamodeinfo) etc)
  * changed several sizeof(type) to sizeof(varname) to avoid similar
    errors in the future

Revision 1.19  1999/09/24 22:52:38  jonas
  * optimized patternline a bit (always use hline when possible)
  * isgraphmode stuff cleanup
  * vesainfo.modelist now gets disposed in cleanmode instead of in
    closegraph (required moving of some declarations from vesa.inc to
    new vesah.inc)
  * queryadapter gets no longer called from initgraph (is called from
    initialization of graph unit)
  * bugfix for notput in 32k and 64k vesa modes
  * a div replaced by / in fillpoly

Revision 1.18  1999/09/24 14:22:38  jonas
  + getscanline16

Revision 1.17  1999/09/24 11:31:38  jonas
  * fixed another typo :(

Revision 1.16  1999/09/23 14:00:41  jonas
  * -dlogging no longer required to fuction correctly
  * some typo's fixed

Revision 1.15  1999/09/22 13:13:34  jonas
  * renamed text.inc -> gtext.inc to avoid conflict with system unit
  * fixed textwidth
  * isgraphmode now gets properly updated, so mode restoring works
    again

Revision 1.14  1999/09/18 22:21:09  jonas
  + hlinevesa256 and vlinevesa256
  + support for not/xor/or/andput in vesamodes with 32k/64k colors
  * lots of changes to avoid warnings under FPC

Revision 1.13  1999/09/18 16:03:36  jonas
  * graph.pp: removed pieslice and sector from ToDo list
  * closegraph: exits now immidiately if isgraphmode = false (caused
    RTE 204 with VESA enabled if you set exitproc to call closegraph
    and also called closegraph explicitely before exit, like bgidemo)

Revision 1.12  1999/09/15 13:37:50  jonas
  * small change to internalellipsedef to be TP compatible
  * fixed directputpixel for vga 320*200*256

Revision 1.11  1999/09/12 17:28:59  jonas
  * several changes to internalellipse to make it faster
    and to make sure it updates the ArcCall correctly
    (not yet done for width = 3)
  * Arc mostly works now, only sometimes an endless loop, don't know
    why

Revision 1.10  1999/09/11 19:43:01  jonas
  * FloodFill: did not take into account current viewport settings
  * GetScanLine: only get line inside viewport, data outside of it
    is not used anyway
  * InternalEllipseDefault: fix for when xradius or yradius = 0 and
    increase xradius and yradius always by one (TP does this too)
  * fixed conlict in vesa.inc from last update
  * some conditionals to avoid range check and overflow errors in
    places where it doesn't matter

Revision 1.9  1999/08/01 14:50:51  jonas
  * fixed hline16 and vline16 for notput (also TP supports copy, and, or, xor and
    notput for lines!!)
  * fixed directputpixel16 to support all the different put types

Revision 1.8  1999/07/18 15:07:19  jonas
  + xor-, and and- orput support for VESA256 modes
  * compile with -dlogging if you want some info to be logged to grlog.txt

Revision 1.7  1999/07/14 18:18:02  florian
  * cosmetic changes

Revision 1.6  1999/07/14 18:16:23  florian
  * HLine16 and VLine16 implemented

Revision 1.5  1999/07/14 14:32:12  florian
  * small VGA detection problem solved

Revision 1.4  1999/07/12 13:27:08  jonas
  + added Log and Id tags
  * added first FPC support, only VGA works to some extend for now
  * use -dasmgraph to use assembler routines, otherwise Pascal
    equivalents are used
  * use -dsupportVESA to support VESA (crashes under FPC for now)
  * only dispose vesainfo at closegrph if a vesa card was detected
  * changed int32 to longint (int32 is not declared under FPC)
  * changed the declaration of almost every procedure in graph.inc to
    "far;" because otherwise you can't assign them to procvars under TP
    real mode (but unexplainable "data segnment too large" errors prevent
    it from working under real mode anyway)

}
