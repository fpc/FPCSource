(*
  $Id: fat.inc 25 2007-12-10 21:06:46Z p4p3r0 $
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
  		Jason Rogers (dovoto)
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
{$ifdef NDS_INTERFACE}
function fatInit(cacheSize: cuint32; setAsDefaultDevice: cbool): cbool; cdecl; external;
function fatInitDefault(): cbool; cdecl; external;
function fatMountSimple (const name: pcchar; const ainterface: PDISC_INTERFACE): cbool; cdecl; external;
function fatMount (const name: pcchar; const ainterface: PDISC_INTERFACE; startSector, cacheSize, SectorsPerPage: cuint32): cbool; cdecl; external;
function fatUnmount(const name: pcchar): cbool; cdecl; external;

// Get Volume Label
procedure fatGetVolumeLabel(const name: pcchar; labl: pcchar); cdecl; external;

// File attributes
const
  ATTR_ARCHIVE    = $20;  // Archive
  ATTR_DIRECTORY  = $10;  // Directory
  ATTR_VOLUME     = $08;  // Volume
  ATTR_SYSTEM     = $04;  // System
  ATTR_HIDDEN     = $02;  // Hidden
  ATTR_READONLY   = $01;  // Read only


// Methods to modify DOS File Attributes

function FAT_getAttr(const _file: pcchar): cint; cdecl; external;
function FAT_setAttr(const _file: pcchar; attr: cint): cint; cdecl; external;

{$define LIBFAT_FEOS_MULTICWD}

{$endif NDS_INTERFACE}

