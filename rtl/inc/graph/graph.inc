{ How this works:                                                        }
{    QueryAdapter - Va chercher tout les modes videos et drivers         }
{      disponibles sur cette carte, et les mets dans une linked list     }
{      en ordre de driver number, et a l'interieur de cela, dans un      }
{      ordre croissant de mode number.                                   }
{   DetectGraph - Verifie si la liste chainee de drivers existe, sinon   }
{      apelle QueryAdapter                                               }
{   InitGraph - Appelle DetectGraph, et verifie que le mode demande existe}
{      bel et bien et est disponible sur ce PC                           }

{$i dpmi.inc}

CONST
  { VESA Specific video modes. }
  m320x200x32k      = $10D;
  m320x200x64k      = $10E;

  m640x400x256      = $100;

  m640x480x256      = $101;
  m640x480x32k      = $110;
  m640x480x64k      = $111;

  m800x600x16       = $102;
  m800x600x256      = $103;
  m800x600x32k      = $113;
  m800x600x64k      = $114;

  m1024x768x16      = $104;
  m1024x768x256     = $105;
  m1024x768x32k     = $116;
  m1024x768x64k     = $117;

  m1280x1024x16     = $106;
  m1280x1024x256    = $107;
  m1280x1024x32k    = $119;
  m1280x1024x64k    = $11A;





{ How to access real mode memory }
{ using 32-bit DPMI memory       }
{  1. Allocate a descriptor      }
{  2. Set segment limit          }
{  3. Set base linear address    }
  const
   InternalDriverName = 'DOSGX';
   VideoOfs : word = 0;   { Segment to draw to }

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


 {************************************************************************}
 {*                     4-bit planar VGA mode routines                   *)
 {************************************************************************}

  Procedure Init640x200x16; assembler;
  { must also clear the screen...}
   asm
     mov ax,000Eh
     int 10h
   end;


   Procedure Init640x350x16; assembler;
  { must also clear the screen...}
    asm
      mov ax,0010h
      int 10h
    end;

  procedure Init640x480x16; assembler;
  { must also clear the screen...}
    asm
      mov  ax,0012h
      int  10h
    end;

 Procedure PutPixel16(X,Y : Integer; Pixel: Word);
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
      asm
        mov  es, [SegA000]
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
      end;
   end;


 Function GetPixel16(X,Y: Integer):word;
  Begin
    X:= X + StartXViewPort;
    Y:= Y + StartYViewPort;
    asm
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

    end;
  end;


 Procedure DirectPutPixel16(X,Y : Integer);
 { x,y -> must be in global coordinates. No clipping. }
  var
   color: word;
 begin
    if CurrentWriteMode = XORPut then
     begin
      { getpixel wants local/relative coordinates }
      Color := GetPixel(x-StartXViewPort,y-StartYViewPort);
      Color := CurrentColor XOR Color;
     end
    else
      Color := CurrentColor;
    asm
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
    end;
 end;


 procedure SetVisual480(page: word);
 { no page flipping support in 640x480 mode }
  begin
    VideoOfs := 0;
  end;

 procedure SetActive480(page: word);
 { no page flipping support in 640x480 mode }
  begin
    VideoOfs := 0;
  end;


 procedure SetVisual200(page: word);
  { two page support... }
  begin
    if page > HardwarePages then exit;
    asm
      mov ax,[page]    { only lower byte is supported. }
      mov ah,05h
      int 10h

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

 procedure SetActive200(page: word);
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

 procedure SetVisual350(page: word);
  { one page support... }
  begin
    if page > HardwarePages then exit;
    asm
      mov ax,[page]    { only lower byte is supported. }
      mov ah,05h
      int 10h
    end;
  end;

 procedure SetActive350(page: word);
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
 {*                     320x200x256c Routines                            *)
 {************************************************************************}

 Procedure Init320; assembler;
  asm
    mov ax,0013h
    int 10h
  end;

 Procedure PutPixel320(X,Y : Integer; Pixel: Word);
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
    asm
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
    end;
  end;


 Function GetPixel320(X,Y: Integer):word;
  Begin
   X:= X + StartXViewPort;
   Y:= Y + StartYViewPort;
    asm
      mov    es, [SegA000]
      mov    ax, [Y]
      mov    di, [X]
      xchg   ah, al            { The value of Y must be in AH }
      add    di, ax
      shr    ax, 2
      add    di, ax
      add    di, [VideoOfs]   { point to correct gfx page ... }
      mov    al,es:[di]
      cbw
      mov    @Result,ax
    end;
  end;


 Procedure DirectPutPixel320(X,Y : Integer);assembler;
 { x,y -> must be in global coordinates. No clipping. }
    asm
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
    end;


 procedure SetVisual320(page: word);
  { no page support... }
  begin
  end;

 procedure SetActive320(page: word);
  { no page support... }
  begin
   VideoOfs := 0;
  end;

 {************************************************************************}
 {*                       Mode-X related routines                        *}
 {************************************************************************}
const CrtAddress: word = 0;

 procedure InitModeX;
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
     INT  10h
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
     MOV AX,[SegA000]  {starting with segment A000h, set 8000h logical     }
     MOV ES,AX      {words = 4*8000h physical words (because of 4     }
     SUB DI,DI      {bitplanes) to 0                                  }
     MOV AX,DI
     MOV CX,8000h
     CLD
     REP STOSW

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


 Function  GetPixelX(X,Y: Integer): word;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
    ASM
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
   end;
 end;

 procedure SetVisualX(page: word);
  { 4 page support... }

   Procedure SetVisibleStart(AOffset: word); Assembler;
   (* Select where the left corner of the screen will be *)
   { By Matt Pritchard }
    ASM
      { Wait if we are currently in a Vertical Retrace        }
     MOV     DX, INPUT_1         { Input Status #1 Register       }
   @DP_WAIT0:
     IN      AL, DX              { Get VGA status                 }
     AND     AL, VERT_RETRACE    { In Display mode yet?           }
     JNZ     @DP_WAIT0           { If Not, wait for it            }

    { Set the Start Display Address to the new page         }

     MOV     DX, CRTC_Index      { We Change the VGA Sequencer    }
     MOV     AL, START_DISP_LO   { Display Start Low Register     }
     MOV     AH, BYTE PTR [AOffset] { Low 8 Bits of Start Addr    }
     OUT     DX, AX              { Set Display Addr Low           }
     MOV     AL, START_DISP_HI   { Display Start High Register    }
     MOV     AH, BYTE PTR [AOffset+1] { High 8 Bits of Start Addr }
     OUT     DX, AX              { Set Display Addr High          }
     { Wait for a Vertical Retrace to smooth out things      }

     MOV     DX, INPUT_1         { Input Status #1 Register       }

  @DP_WAIT1:
     IN      AL, DX              { Get VGA status                 }
     AND     AL, VERT_RETRACE    { Vertical Retrace Start?        }
     JZ      @DP_WAIT1           { If Not, wait for it            }
    { Now Set Display Starting Address                     }
  end;

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

 procedure SetActiveX(page: word);
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

 Procedure PutPixelX(X,Y: Integer; color:word);
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
     ASM
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
  end;


 Procedure DirectPutPixelX(X,Y: Integer); Assembler;
 { x,y -> must be in global coordinates. No clipping. }
 ASM
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

 Procedure SaveStateVGA;
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
      int  10h
      mov  [StateSize], bx
      cmp  al,01ch
      jnz  @notok
      mov  [SaveSupported],TRUE
     @notok:
    end;
    if SaveSupported then
      begin
        PtrLong:=GlobalDosAlloc(64*StateSize);  { values returned in 64-byte blocks }
        if PtrLong = 0 then
           RunError(203);
        SavePtr := pointer(longint(PtrLong and $0000ffff) shl 16);
        RealStateSeg := word((PtrLong and $ffff0000) shr 16);
        if not assigned(SavePtr) then
           RunError(203);

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

 procedure RestoreStateVGA;
  var
   regs:TDPMIRegisters;
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
        FillChar(regs, sizeof(regs), #0);
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
         regs.eax := $1C02;      { restore the state buffer                }
         regs.ecx := $07;        { rest DAC / Data areas / Hardware states }
         regs.es := RealStateSeg;
         regs.ebx := 0;
         RealIntr($10,regs);
         if GlobalDosFree(longint(SavePtr) shr 16)<>0 then
          RunError(216);

         SavePtr := nil;
       end;
  end;

{$ELSE}

      {**************************************************************}
      {*                     Real mode routines                     *}
      {**************************************************************}


 Procedure SaveStateVGA;
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

 procedure RestoreStateVGA;
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
      BlueValue : Integer); assembler;
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
      and ax, $ff         { mask out all upper bits           }
      shr al, 2           { convert to LSB RGB format         }
      out dx, al
      mov ax, [GreenValue]{ Get RedValue                      }
      and ax, $ff         { mask out all upper bits           }
      shr al, 2           { convert to LSB RGB format         }
      out dx, al
      mov ax, [BlueValue] { Get RedValue                      }
      and ax, $ff         { mask out all upper bits           }
      shr al, 2           { convert to LSB RGB format         }
      out dx, al
    end;


   { VGA is never a direct color mode, so no need to check ... }
  Procedure GetVGARGBPalette(ColorNum: integer; Var
      RedValue, GreenValue, BlueValue : integer);
   begin
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
    if not assigned(RestoreVideoState) then
      RunError(216);
    RestoreVideoState;
{$IFDEF DPMI}
    { We had copied the buffer of mode information }
    { and allocated it dynamically... now free it  }
    Dispose(VESAInfo.ModeList);
{$ENDIF}
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
       int 10h              { get EGA information }
       cmp bh,0ffh
       jz  @noega
       mov [EGADetected],TRUE
     @noega:
     end;
     { check if VGA adapter supported...       }
     if EGADetected then
       begin
        asm
         mov ax,1a00h
         int 10h            { get display combination code...}
         cmp al,1ah         { check if supported...          }
         jne @novga
         { now check if this is the ATI EGA }
         mov ax,1c00h       { get state size for save...     }
         mov cx,00h
         int 10h
         cmp al,1ch         { success?                       }
         jne @novga
         mov [VGADetected],TRUE
        @novga:
        end;
       end;
     if VGADetected then
       begin

         SaveVideoState := SaveStateVGA;
         RestoreVideoState := RestoreStateVGA;


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
         mode.DirectPutPixel:=DirectPutPixel320;
         mode.PutPixel:=PutPixel320;
         mode.GetPixel:=GetPixel320;
         mode.SetRGBPalette := SetVGARGBPalette;
         mode.GetRGBPalette := GetVGARGBPalette;
         mode.SetVisualPage := SetVisual320;
         mode.SetActivePage := SetActive320;
         mode.InitMode := Init320;
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
         mode.DirectPutPixel:=DirectPutPixelX;
         mode.PutPixel:=PutPixelX;
         mode.GetPixel:=GetPixelX;
         mode.SetRGBPalette := SetVGARGBPalette;
         mode.GetRGBPalette := GetVGARGBPalette;
         mode.SetVisualPage := SetVisualX;
         mode.SetActivePage := SetActiveX;
         mode.InitMode := InitModeX;
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
         mode.DirectPutPixel:=DirectPutPixel16;
         mode.PutPixel:=PutPixel16;
         mode.GetPixel:=GetPixel16;
         mode.SetRGBPalette := SetVGARGBPalette;
         mode.GetRGBPalette := GetVGARGBPalette;
         mode.SetVisualPage := SetVisual200;
         mode.SetActivePage := SetActive200;
         mode.InitMode := Init640x200x16;
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
         mode.DirectPutPixel:=DirectPutPixel16;
         mode.PutPixel:=PutPixel16;
         mode.GetPixel:=GetPixel16;
         mode.InitMode := Init640x350x16;
         mode.SetRGBPalette := SetVGARGBPalette;
         mode.GetRGBPalette := GetVGARGBPalette;
         mode.SetVisualPage := SetVisual350;
         mode.SetActivePage := SetActive350;
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
         mode.DirectPutPixel:=DirectPutPixel16;
         mode.PutPixel:=PutPixel16;
         mode.GetPixel:=GetPixel16;
         mode.SetRGBPalette := SetVGARGBPalette;
         mode.GetRGBPalette := GetVGARGBPalette;
         mode.InitMode := Init640x480x16;
         mode.SetVisualPage := SetVisual480;
         mode.SetActivePage := SetActive480;
         mode.XAspect := 10000;
         mode.YAspect := 10000;
         AddMode(mode);
       end;

     { check if VESA adapter supported...      }
     if getVesaInfo(VESAInfo) then
       begin
         { We have to set and restore the entire VESA state }
         { otherwise, if we use the VGA BIOS only function  }
         { there might be a crash under DPMI, such as in the}
         { ATI Mach64                                       }
         SaveVideoState := SaveStateVESA;
         RestoreVideoState := RestoreStateVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 319;
             mode.MaxY := 199;
             mode.DirectPutPixel:=DirectPutPixVESA32k;
             mode.PutPixel:=PutPixVESA32k;
             mode.GetPixel:=GetPixVESA32k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init320x200x32k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 319;
             mode.MaxY := 199;
             mode.DirectPutPixel:=DirectPutPixVESA64k;
             mode.PutPixel:=PutPixVESA64k;
             mode.GetPixel:=GetPixVESA64k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init320x200x64k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 639;
             mode.MaxY := 399;
             mode.DirectPutPixel:=DirectPutPixVESA256;
             mode.PutPixel:=PutPixVESA256;
             mode.GetPixel:=GetPixVESA256;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init640x400x256;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 639;
             mode.MaxY := 479;
             mode.DirectPutPixel:=DirectPutPixVESA256;
             mode.PutPixel:=PutPixVESA256;
             mode.GetPixel:=GetPixVESA256;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init640x480x256;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 639;
             mode.MaxY := 399;
             mode.DirectPutPixel:=DirectPutPixVESA32k;
             mode.PutPixel:=PutPixVESA32k;
             mode.GetPixel:=GetPixVESA32k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init640x480x32k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 639;
             mode.MaxY := 479;
             mode.DirectPutPixel:=DirectPutPixVESA64k;
             mode.PutPixel:=PutPixVESA64k;
             mode.GetPixel:=GetPixVESA64k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init640x480x64k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.DirectColor := FALSE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:=DirectPutPixVESA16;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.PutPixel:=PutPixVESA16;
{             mode.GetPixel:=GetPixVESA16;}
             mode.InitMode := Init800x600x16;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:=DirectPutPixVESA256;
             mode.PutPixel:=PutPixVESA256;
             mode.GetPixel:=GetPixVESA256;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init800x600x256;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:=DirectPutPixVESA32k;
             mode.PutPixel:=PutPixVESA32k;
             mode.GetPixel:=GetPixVESA32k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init800x600x32k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 799;
             mode.MaxY := 599;
             mode.DirectPutPixel:=DirectPutPixVESA64k;
             mode.PutPixel:=PutPixVESA64k;
             mode.GetPixel:=GetPixVESA64k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init800x600x64k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:=DirectPutPixVESA16;
             mode.PutPixel:=PutPixVESA16;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
{             mode.GetPixel:=GetPixVESA16;}
             mode.InitMode := Init1024x768x16;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := FALSE;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:=DirectPutPixVESA256;
             mode.PutPixel:=PutPixVESA256;
             mode.GetPixel:=GetPixVESA256;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init1024x768x256;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.DirectColor := TRUE;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:=DirectPutPixVESA32k;
             mode.PutPixel:=PutPixVESA32k;
             mode.GetPixel:=GetPixVESA32k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init640x480x32k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1023;
             mode.MaxY := 767;
             mode.DirectPutPixel:=DirectPutPixVESA64k;
             mode.PutPixel:=PutPixVESA64k;
             mode.GetPixel:=GetPixVESA64k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.InitMode := Init1024x768x64k;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.DirectColor := FALSE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:=DirectPutPixVESA16;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.PutPixel:=PutPixVESA16;
{             mode.GetPixel:=GetPixVESA16;}
             mode.InitMode := Init1280x1024x16;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.DirectColor := FALSE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:=DirectPutPixVESA256;
             mode.PutPixel:=PutPixVESA256;
             mode.GetPixel:=GetPixVESA256;
             mode.InitMode := Init1280x1024x256;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.DirectColor := TRUE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:=DirectPutPixVESA32k;
             mode.PutPixel:=PutPixVESA32k;
             mode.GetPixel:=GetPixVESA32k;
             mode.InitMode := Init1280x1024x32k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
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
             mode.HardwarePages := ModeInfo.NumberOfPages;
             mode.DirectColor := TRUE;
             mode.PaletteSize := mode.MaxColor;
             mode.MaxX := 1279;
             mode.MaxY := 1023;
             mode.DirectPutPixel:=DirectPutPixVESA64k;
             mode.PutPixel:=PutPixVESA64k;
             mode.GetPixel:=GetPixVESA64k;
             mode.InitMode := Init1280x1024x64k;
             mode.SetRGBPalette := SetVESARGBPalette;
             mode.GetRGBPalette := GetVESARGBPalette;
             mode.SetVisualPage := SetVisualVESA;
             mode.SetActivePage := SetActiveVESA;
             mode.XAspect := 10000;
             mode.YAspect := 10000;
             AddMode(mode);
           end;

       end;
   end;
