
{%REGION drunkenlogo}

//======================================================================
//
//	drunkenlogo, 256x192@8, 
//	+ palette 256 entries, not compressed
//	+ 547 tiles (t|f reduced) not compressed
//	+ regular map (flat), not compressed, 32x24 
//	Total size: 512 + 35008 + 1536 = 37056
//
//	Time-stamp: 2014-08-04, 20:56:22
//	Exported by Cearn's GBA Image Transmogrifier, v0.8.10
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================

{$IFNDEF GRIT_DRUNKENLOGO_INC}
{$DEFINE GRIT_DRUNKENLOGO_INC}

const drunkenlogoTilesLen = 35008;
      drunkenlogoTilesMax = 8752;
var drunkenlogoTiles: array [0..0] of cuint32; cvar; external;

const drunkenlogoMapLen = 1536;
      drunkenlogoMapMax = 768;
var drunkenlogoMap: array [0..0] of cuint16; cvar; external;

const drunkenlogoPalLen = 512;
      drunkenlogoPalMax = 256;
var drunkenlogoPal: array [0..0] of cuint16; cvar; external;

{$ENDIF GRIT_DRUNKENLOGO_INC}

{%ENDREGION drunkenlogo}
