unit Multilayer;

interface

uses
  ctypes;
  
//======================================================================
//
//	Layer_1, 256x256@8, 
//	+ regular map (flat), not compressed, 32x32 
//	Total size: 2048 = 2048
//
//	Time-stamp: 2007-12-07, 18:17:34
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer_1MapLen = 2048;
var
  Layer_1Map: array [0..1023] of cushort; cvar; external;

//======================================================================
//
//	Layer_2, 256x256@8, 
//	+ regular map (flat), not compressed, 32x32 
//	Total size: 2048 = 2048
//
//	Time-stamp: 2007-12-07, 18:17:34
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer_2MapLen = 2048;
var
  Layer_2Map: array [0..1023] of cushort; cvar; external;

//======================================================================
//
//	Layer_3, 256x256@8, 
//	+ regular map (flat), not compressed, 32x32 
//	Total size: 2048 = 2048
//
//	Time-stamp: 2007-12-07, 18:17:34
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer_3MapLen = 2048;
var
  Layer_3Map: array [0..1023] of cushort; cvar; external;

//======================================================================
//
//	Multilayer, 8x2504@8, 
//	+ palette 256 entries, not compressed
//	+ 313 tiles not compressed
//	Total size: 512 + 20032 = 20544
//
//	Time-stamp: 2007-12-07, 18:17:35
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  MultilayerPalLen = 512;
  MultilayerTilesLen = 20032;
var
  MultilayerPal: array [0..255] of cushort; cvar; external;
  MultilayerTiles: array [0..5007] of cuint; cvar; external;

implementation
end.
