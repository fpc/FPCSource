(*
  $Id$
  ------------------------------------------------------------------------------
	Header file for libgba DMA definitions

	Copyright 2003-2004 by Dave Murphy.

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Library General Public
	License as published by the Free Software Foundation; either
	version 2 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Library General Public License for more details.

	You should have received a copy of the GNU Library General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
	MA 02110-1301, USA.

	Please report all bugs and problems through the bug tracker at
	"http://sourceforge.net/tracker/?group_id=114505&atid=668551".


  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$




*)

{$ifdef GBA_INTERFACE}
const
  REG_DMA0SAD	: pu32 = pointer(REG_BASE + $0b0);
  REG_DMA0DAD	: pu32 = pointer(REG_BASE + $0b4);
  REG_DMA0CNT	: pu32 = pointer(REG_BASE + $0b8);

  REG_DMA1SAD	: pu32 = pointer(REG_BASE + $0bc);
  REG_DMA1DAD	: pu32 = pointer(REG_BASE + $0c0);
  REG_DMA1CNT	: pu32 = pointer(REG_BASE + $0c4);

  REG_DMA2SAD	: pu32 = pointer(REG_BASE + $0c8);
  REG_DMA2DAD	: pu32 = pointer(REG_BASE + $0cc);
  REG_DMA2CNT	: pu32 = pointer(REG_BASE + $0d0);

  REG_DMA3SAD	: pu32 = pointer(REG_BASE + $0d4);
  REG_DMA3DAD	: pu32 = pointer(REG_BASE + $0d8);
  REG_DMA3CNT	: pu32 = pointer(REG_BASE + $0dc);


  DMA_DST_INC     = (0 shl 21);
  DMA_DST_DEC     = (1 shl 21);
  DMA_DST_FIXED	  = (2 shl 21);
  DMA_DST_RELOAD  = (3 shl 21);

  DMA_SRC_INC     = (0 shl 23);
  DMA_SRC_DEC     = (1 shl 23);
  DMA_SRC_FIXED   = (2 shl 23);

  DMA_REPEAT      = (1 shl 25);

  DMA16           = (0 shl 26);
  DMA32           = (1 shl 26);

  GAMEPAK_DRQ     = (1 shl 27);

  DMA_IMMEDIATE   = (0 shl 28);
  DMA_VBLANK      = (1 shl 28);
  DMA_HBLANK      = (2 shl 28);
  DMA_SPECIAL     = (3 shl 28);

  DMA_IRQ         = (1 shl 30);
  DMA_ENABLE      = (1 shl 31);
  
procedure DMA0COPY(source: pu16; dest: pu16; mode: u32); inline;
procedure DMA1COPY(source: pu16; dest: pu16; mode: u32); inline;
procedure DMA2COPY(source: pu16; dest: pu16; mode: u32); inline;
procedure DMA3COPY(source: pu16; dest: pu16; mode: u32); inline;
procedure DMA_Copy(channel: byte; source, dest: pu16; mode: u32); inline;
procedure dmaCopy(const source: pointer; dest: pointer; size: u32); inline;
{$endif GBA_INTERFACE}



{$ifdef GBA_IMPLEMENTATION}
procedure DMA0COPY(source: pu16; dest: pu16; mode: u32); inline;
begin
  REG_DMA0SAD^ := u32(source);
  REG_DMA0DAD^ := u32(dest);
  REG_DMA0CNT^ := DMA_ENABLE or mode;
end; 

procedure DMA1COPY(source: pu16; dest: pu16; mode: u32); inline;
begin
  REG_DMA1SAD^ := u32(source);
  REG_DMA1DAD^ := u32(dest);
  REG_DMA1CNT^ := DMA_ENABLE or mode;
end; 

procedure DMA2COPY(source: pu16; dest: pu16; mode: u32); inline;
begin
  REG_DMA2SAD^ := u32(source);
  REG_DMA2DAD^ := u32(dest);
  REG_DMA2CNT^ := DMA_ENABLE or mode;
end; 

procedure DMA3COPY(source: pu16; dest: pu16; mode: u32); inline;
begin
  REG_DMA3SAD^ := u32(source);
  REG_DMA3DAD^ := u32(dest);
  REG_DMA3CNT^ := DMA_ENABLE or mode;
end; 



procedure DMA_Copy(channel: byte; source, dest: pu16; mode: u32); inline;
begin
  case channel of
    0: DMA0COPY(source, dest, mode); 
    1: DMA1COPY(source, dest, mode); 
    2: DMA2COPY(source, dest, mode); 
    3: DMA3COPY(source, dest, mode); 
  end;
end;

procedure dmaCopy(const source: pointer; dest: pointer; size: u32); inline;
begin
  DMA_Copy(3, source, dest, DMA16 or (size shr 1));
end;
{$endif GBA_IMPLEMENTATION}


