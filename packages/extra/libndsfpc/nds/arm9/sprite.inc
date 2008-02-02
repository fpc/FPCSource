(*
  $Id: sprite.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	sprite.h -- definitions for DS sprites
  
  	Copyright (C) 2007
  		Liran Nuna	(LiraNuna)
  		Dave Murphy (WinterMute)
  
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  
  	1.	The origin of this software must not be misrepresented; you
  	must not claim that you wrote the original software. If you use
  	this software in a product, an acknowledgment in the product
  	documentation would be appreciated but is not required.
  
  	2.	Altered source versions must be plainly marked as such, and
  	must not be misrepresented as being the original software.
  
  	3.	This notice may not be removed or altered from any source
  	distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)

{$ifndef ARM9}
  {$error Sprites are only available on the ARM9}
{$endif ARM9}

// Sprite control defines

{$ifdef NDS_INTERFACE}
// Attribute 0 consists of 8 bits of Y plus the following flags:
const
  ATTR0_NORMAL          = (0 shl 8);
  ATTR0_ROTSCALE        =	(1 shl 8);
  ATTR0_DISABLED        =	(2 shl 8);
  ATTR0_ROTSCALE_DOUBLE	= (3 shl 8);

  ATTR0_TYPE_NORMAL     = (0 shl 10);
  ATTR0_TYPE_BLENDED    =	(1 shl 10);
  ATTR0_TYPE_WINDOWED   = (2 shl 10);
  ATTR0_BMP             =	(3 shl 10);

  ATTR0_MOSAIC          = (1 shl 12);

  ATTR0_COLOR_16        = (0 shl 13); //16 color in tile mode...16 bit in bitmap mode
  ATTR0_COLOR_256       = (1 shl 13);

  ATTR0_SQUARE          = (0 shl 14);
  ATTR0_WIDE            = (1 shl 14);
  ATTR0_TALL            = (2 shl 14);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function OBJ_Y(m: cint): cint; inline;
begin
  OBJ_Y := ((m) and $00ff);
end;

function ATTR1_ROTDATA(n: cint): cint; inline;
begin
  ATTR1_ROTDATA := ((n) shl 9)  // note: overlaps with flip flags
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
const
  ATTR1_FLIP_X          = (1 shl 12);
  ATTR1_FLIP_Y          = (1 shl 13);
  ATTR1_SIZE_8          = (0 shl 14);
  ATTR1_SIZE_16         = (1 shl 14);
  ATTR1_SIZE_32         = (2 shl 14);
  ATTR1_SIZE_64         = (3 shl 14);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function OBJ_X(m: cint): cint; inline;
begin
  OBJ_X := ((m) and $01ff);
end;

// Atribute 2 consists of the following:
function ATTR2_PRIORITY(n: cint): cint; inline;     
begin
  ATTR2_PRIORITY := ((n) shl 10);
end;

function ATTR2_PALETTE(n: cint): cint; inline;      
begin
  ATTR2_PALETTE := ((n) shl 12);
end;

function ATTR2_ALPHA(n: cint): cint; inline;		  
begin
  ATTR2_ALPHA := ((n) shl 12);
end;
{$endif NDS_IMPLEMENTATION}

{$ifdef NDS_INTERFACE}
type 
  TObjMode = (OBJMODE_NORMAL, OBJMODE_BLENDED, OBJMODE_WINDOWED, OBJMODE_BITMAP);
  TObjShape = (OBJSHAPE_SQUARE, OBJSHAPE_WIDE, OBJSHAPE_TALL, OBJSHAPE_FORBIDDEN);
  TObjSize = (OBJSIZE_8, OBJSIZE_16, OBJSIZE_32, OBJSIZE_64);
  TObjColMode = (OBJCOLOR_16, OBJCOLOR_256);
  TObjPriority = (OBJPRIORITY_0, OBJPRIORITY_1, OBJPRIORITY_2, OBJPRIORITY_3);

  sSpriteEntry = packed record
    case integer of
      0: (st1:  packed record
                  st1:  bitpacked record
                          posY: 0..8;
                          case integer of
                            0: (st1:  bitpacked record
                                        __padding1: 0..1;
                                        isHidden: 0..1;
                                        __padding2: 0..6;
                                      end; 
                                );
                            1: (st2:  bitpacked record
                                        isRotoscale: 0..1;
                                        rsDouble: 0..1;
                                        objMode: 0..2;
                                        isMosaic: 0..1;
                                        colMode: 0..1;
                                        objShape: 0..2;
                                      end;
                               );  
                        end;
                  st2:  bitpacked record
                          tileIdx: 0..10;
                          objPriority: 0..2;
                          objPal: 0..4;
                        end; 
                  attribute3: cuint16;
                  case integer of
                    0: (st1:  bitpacked record
                                posX: 0..9;
                                __padding1: 0..7;
                              end; 
                       );
                    1: (st2:  bitpacked record
                                __padding1: 0..8;
                                case integer of
                                  0: (st1:  bitpacked record
                                              __padding1: 0..4;
                                              hFlip: 0..1;
                                              vFlip: 0..1;
                                              __padding2: 0..2;
                                            end;
                                     );
                                  1: (st2:  bitpacked record
                                              __padding1: 0..1;
                                              rsMatrixIdx: 0..5;
                                              objSize: 0..2;
                                            end;
                                     );
                              end;
                       ); 
                end;
         ); 
      1: (st:  packed record
                  attribute: array [0..2] of cuint16;
                  filler: cuint16;    
               end;
        );
  end;
  SpriteEntry = sSpriteEntry;
  TSpriteEntry = sSpriteEntry;
  PSpriteEntry = ^sSpriteEntry;


  sSpriteRotation = record
    filler1: array [0..2] of cuint16;
    hdx: cint16;
    filler2: array [0..2] of cuint16;
    hdy: cint16;
    filler3: array [0..2] of cuint16;
    vdx: cint16;
    filler4: array [0..2] of cuint16;
    vdy: cint16;
  end;
  SpriteRotation = sSpriteRotation;
  TSpriteRotation = sSpriteRotation;
  pSpriteRotation = ^sSpriteRotation;


const
  SPRITE_COUNT = 128;
  MATRIX_COUNT = 32;

type
  TOAM = record
    case integer of
      0: (spriteBuffer: array [0..SPRITE_COUNT-1] of TSpriteEntry;);
      1: (matrixBuffer: array [0..MATRIX_COUNT-1] of TSpriteRotation;);
  end;
{$endif NDS_INTERFACE}
 
{$ifdef NDS_INTERFACE}
function OBJ_Y(m: cint): cint; inline;
function ATTR1_ROTDATA(n: cint): cint; inline;
function OBJ_X(m: cint): cint; inline;
function ATTR2_PRIORITY(n: cint): cint; inline;     
function ATTR2_PALETTE(n: cint): cint; inline;      
function ATTR2_ALPHA(n: cint): cint; inline;		  
{$endif NDS_INTERFACE}
