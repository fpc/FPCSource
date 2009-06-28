unit TextBackgrounds;

interface

uses
  ctypes;

//======================================================================
//
//	Layer128x128, 128x128@8, 
//	+ regular map (flat), not compressed, 16x16 
//	Total size: 512 = 512
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer128x128MapLen = 512;
var
  Layer128x128Map: array [0..255] of cushort; cvar; external;


//======================================================================
//
//	Layer256x256, 256x256@8, 
//	+ regular map (flat), not compressed, 32x32 
//	Total size: 2048 = 2048
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer256x256MapLen = 2048;
var
  Layer256x256Map: array [0..1023] of cushort; cvar; external;


//======================================================================
//
//	Layer_512x512, 512x512@8, 
//	+ regular map (flat), not compressed, 64x64 
//	Total size: 8192 = 8192
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer512x512MapLen = 8192;
var
  Layer512x512Map: array [0..4095] of cushort; cvar; external;


//======================================================================
//
//	Layer1024x1024, 1024x1024@8, 
//	+ regular map (flat), not compressed, 128x128 
//	Total size: 32768 = 32768
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer1024x1024MapLen = 32768;
var
  Layer1024x1024Map: array [0..16383] of cushort; cvar; external;

//======================================================================
//
//	Layer256x512, 256x512@8, 
//	+ regular map (flat), not compressed, 32x64 
//	Total size: 4096 = 4096
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer256x512MapLen = 4096;
var
  Layer256x512Map: array [0..2047] of cushort; cvar; external;


//======================================================================
//
//	Layer512x256, 512x256@8, 
//	+ regular map (flat), not compressed, 64x32 
//	Total size: 4096 = 4096
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  Layer512x256MapLen = 4096;
var
  Layer512x256Map: array [0..2047] of cushort; cvar; external;

//======================================================================
//
//	TextBackgrounds, 8x6512@8, 
//	+ palette 256 entries, not compressed
//	+ 814 tiles not compressed
//	Total size: 512 + 52096 = 52608
//
//	Time-stamp: 2007-12-05, 16:39:14
//	Exported by Cearn's GBA Image Transmogrifier
//	( http://www.coranac.com )
//
//======================================================================
const
  TextBackgroundsPalLen = 512;
  TextBackgroundsTilesLen = 52096;
var
  TextBackgroundsPal: array [0..255] of cushort; cvar; external;
  TextBackgroundsTiles: array [0..13023] of cuint; cvar; external;

implementation

end.
