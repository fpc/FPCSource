
{%REGION devkitlogo}

//======================================================================
//
//	devkitlogo, 256x192@8, 
//	+ palette 256 entries, not compressed
//	+ 180 tiles (t|f reduced) not compressed
//	+ regular map (flat), not compressed, 32x24 
//	Total size: 512 + 11520 + 1536 = 13568
//
//	Time-stamp: 2014-08-04, 20:56:23
//	Exported by Cearn's GBA Image Transmogrifier, v0.8.10
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================

{$IFNDEF GRIT_DEVKITLOGO_INC}
{$DEFINE GRIT_DEVKITLOGO_INC}

const devkitlogoTilesLen = 11520;
      devkitlogoTilesMax = 2880;
var devkitlogoTiles: array [0..0] of cuint32; cvar; external;

const devkitlogoMapLen = 1536;
      devkitlogoMapMax = 768;
var devkitlogoMap: array [0..0] of cuint16; cvar; external;

const devkitlogoPalLen = 512;
      devkitlogoPalMax = 256;
var devkitlogoPal: array [0..0] of cuint16; cvar; external;

{$ENDIF GRIT_DEVKITLOGO_INC}

{%ENDREGION devkitlogo}
