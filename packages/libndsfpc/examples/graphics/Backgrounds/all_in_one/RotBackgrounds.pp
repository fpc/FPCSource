unit RotBackgrounds;

interface

uses
  ctypes;
//======================================================================
//
//	Layer128x128r, 128x128@8, 
//	+ affine map, not compressed, 16x16 
//	External tile file: (null).
//	Total size: 256 = 256
//
//	Time-stamp: 2008-12-03, 18:15:41
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================
const
  Layer128x128rMapLen = 256;
var
  Layer128x128rMap: array [0..255] of cuchar; cvar; external;

//======================================================================
//
//	Layer256x256r, 256x256@8, 
//	+ affine map, not compressed, 32x32 
//	External tile file: (null).
//	Total size: 1024 = 1024
//
//	Time-stamp: 2008-12-03, 18:15:41
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================
const
  Layer256x256rMapLen = 1024;
var
  Layer256x256rMap: array [0..1023] of cuchar; cvar; external;

//======================================================================
//
//	Layer512x512r, 512x512@8, 
//	+ affine map, not compressed, 64x64 
//	External tile file: (null).
//	Total size: 4096 = 4096
//
//	Time-stamp: 2008-12-03, 18:15:41
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================
const
  Layer512x512rMapLen = 4096;
var
  Layer512x512rMap: array [0..4095] of cuchar; cvar; external;

//======================================================================
//
//	Layer1024x1024r, 1024x1024@8, 
//	+ affine map, not compressed, 128x128 
//	External tile file: (null).
//	Total size: 16384 = 16384
//
//	Time-stamp: 2008-12-03, 18:15:42
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================
const
  Layer1024x1024rMapLen = 16384;
var
  Layer1024x1024rMap: array [0..16383] of cuchar; cvar; external;

//======================================================================
//
//	RotBackgrounds, 8x872@8, 
//	+ palette 256 entries, not compressed
//	+ 109 tiles not compressed
//	Total size: 512 + 6976 = 7488
//
//	Time-stamp: 2008-12-03, 18:15:42
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================
const
  RotBackgroundsTilesLen = 6976;
  RotBackgroundsPalLen = 512;
var
  RotBackgroundsTiles: array [0..1743] of cuint; cvar; external;
  RotBackgroundsPal: array [0..255] of cushort; cvar; external;


implementation

end.
