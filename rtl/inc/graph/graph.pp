{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit Graph2;
{-------------------------------------------------------}
{ Differences with TP Graph unit:                       }
{ -  default putimage and getimage only support a max.  }
{    of 64K colors on screen, because all pixels are    }
{    saved as words.                                    }
{ -  Set RGB Palette is not used, SetPalette must be    }
{    used instead.                                      }
{ -  In the TP graph unit, Clipping is always performed }
{    on strings written with OutText, and this clipping }
{    is done on a character per character basis (for    }
{    example, if ONE part of a character is outside the }
{    viewport , then that character is not written at   }
{    all to the screen. In FPC Pascal, clipping is done }
{    on a PIXEL basis, not a character basis, so part of }
{    characters which are not entirely in the viewport  }
{    may appear on the screen.                          }
{ -  SetTextStyle only conforms to the TP version when  }
{    the correct (and expected) values are used for     }
{    CharSize for stroked fonts (4 = stroked fonts)     }
{ -  InstallUserDriver is not supported, so always      }
{    returns an error.                                  }
{ -  RegisterBGIDriver is not supported, so always      }
{    returns an error.                                  }
{ - DrawPoly XORPut mode is not exactly the same as in  }
{   the TP graph unit.                                  }
{ - FillEllipse does not support XORPut mode with a     }
{   bounded FloodFill. Mode is always CopyPut mode.     }
{ - Imagesize returns a longint instead of a word       }
{ - ImageSize cannot return an error value              }
{-------------------------------------------------------}
{ AUTHORS:                                                                      }
{   Gernot Tenchio      - original version              }
{   Florian Klaempfl    - major updates                 }
{   Pierre Mueller      - major bugfixes                }
{   Carl Eric Codere    - complete rewrite              }
{   Thomas Schatzl      - optimizations,routines and    }
{ Credits (external):       suggestions.                }
{   - Original FloodFill code by                        }
{        Menno Victor van der star                      }
{     (the code has been heavily modified)              }
{-------------------------------------------------------}
{-------------------------------------------------------}
{ For significant speed improvements , is is recommended }
{ that these routines be hooked (otherwise the default,  }
{ slower routines will be used) :                        }
{   HLine()                                              }
{   VLine()                                              }
{   PatternLine()                                        }
{   ClearViewPort()                                      }
{   PutImage()                                           }
{   GetImage()  - ImageSize() should also be changed     }
{   InternalEllipse()                                    }
{   Line()                                               }
{   GetScanLine()                                        }
{--------------------------------------------------------}
{ FPC unit requirements:                                 }
{  All modes should at least have 1 graphics page to     }
{  make it possible to create animation on all supported }
{  systems , this can be done either by double-buffering }
{  yourself in the heap if no hardware is available to do}
{  it.                                                   }
{--------------------------------------------------------}
{ COMPATIBILITY WARNING: Some of the compatibility tests }
{ were done using the CGA and other the VGA drivers.     }
{ Within the BGI drivers themselves the BEHAVIOUR is not }
{ the same, so be warned!!!                              }
{--------------------------------------------------------}
{ History log:                                           }
{   15th February 1999:                                  }
{   + Added support for system font in vertical mode     }
{   + system font is now available for all platforms     }
{   * font support routines now compile                  }
{   * textHeight would not return correct size for system }
{     font                                               }
{   * Alignment of fonts partly fixed                    }
{   17th Feb. 1999:                                      }
{   + First support for stroked fonts                    }
{   18th Feb. 1999:                                      }
{   * bugfix of line drawing which fixes stroked font    }
{     displays.                                          }
{   23rd Feb. 1999:                                      }
{   + Applied Pierre's patches to font                   }
{   + Added scaling of bitmapped fonts                   }
{   + Vertical stroked fonts                             }
{  24th Feb. 1999:                                       }
{   * Scaling of stroked fonts must be done using FPs    }
{     to be 100% compatible with turbo pascal            }
{   + Sped up by 40% stroked font scaling calculations   }
{   + RegisterBGIFont                                    }
{  9th march 1999:                                       }
{   + Starting implementing Fillpoly()                   }
{  15th march 1999:                                      }
{   + SetFillStyle()                                     }
{   + patternLine()                                      }
{   + Bar()                                              }
{   * GraphDefaults would not make the Default color     }
{     of the fill pattern to the Max. Palette entry.     }
{   + SetFillPattern()                                   }
{  20th march 1999:                                      }
{   * GraphDefaults would not reset to the text system   }
{   * DefaultFont would write one character too much to  }
{     the screen                                         }
{   + Sloped thick lines in Line()                       }
{   + Sloped patterned lines in Line()                   }
{   * GraphDefaults would not reset the User Fill pattern}
{     to $ff                                             }
{   + DirectPutPixel takes care of XOR mode writes       }
{     improves speed by about 30% over old method of     }
{     GetPixel XOR CurrentColor                          }
{   * Dashed LineStyle exactly like BP version now       }
{   + Center LineStyle (checked against CGA driver)      }
{   * GraphDefaults() now resets linepattern array       }
{ 1st april  1999:                                       }
{   + First implementation of FillPoly (incomplete)      }
{ 2nd april  1999:                                       }
{   * FillPoly did not Reset PatternLine index           }
{   * FillPoly did not use correct color                 }
{   * PatternLine was writing modes in reverse direction }
{   * PatternLine would not work with non-rectangular    }
{     shapes.                                            }
{   * PatternLine must fill up the ENTIRE pattern,       }
{     with either the foreground or background color.    }
{   * GraphDefaults() would not call SetBkColor()        }
{   * Fixed some memory leaks in FillPoly()              }
{ 11th April  1999:                                      }
{   * PatternLine() was drawing one pixel less then      }
{     requested                                          }
{ 12th April  1999:                                      }
{   + FloodFill - first working implementation           }
{      Horrbly slow even on very fast cpu's              }
{   + Some suggestions of Thomas implemented             }
{ 13th April  1999:                                      }
{   * FloodFill() vertical index was off by one pixel    }
{   * FloodFill() would never draw the last line in the  }
{      list                                              }
{   - Removed ClearViewPort320 which was wrong anyways,  }
{     will need to be implemented later.                 }
{   * PatternLine() would not always restore write mode  }
{   + Circle() uses NormalPut always with NormWidth lines}
{   + FillEllipse() initial version                      }
{   * InternalEllipse() - 0 to 360 now supported as      }
{     angles.                                            }
{ 14th April  1999:                                      }
{   * mod x = and (x-1)(from Thomas Schatzl) gives a     }
{     significant speed improvement.                     }
{ 15th april  1999:                                      }
{   + Arc() ok except for Aspect Ratio, which does not   }
{     give us the correct ratio on a 320x200 screen.     }
{   + Added FillPoly() from Thomas Schatzl               }
{   + More hookable routines                             }
{  16th april  1999:                                     }
{   + Line() checked ok.                                 }
{  17th april  1999:                                     }
{   * GraphDefaults() would not reset CP                 }
{   + GetX(), GetY(), MoveTo() checked for viewports     }
{   * OutTextXY() should not update the CP               }
{   * ClearViewPort() would not update the CP            }
{   * ClearDevice() would not update the CP              }
{   * Sector() would update the CP by calling LineTo     }
{   * Bar3D() would update the CP                        }
{   * PieSlice() would update the CP                     }
{  18th april  1999:                                     }
{   + Clipping algorithm                                 }
{  19th april  1999:                                     }
{   + Adapterinfo structure                              }
{  20th april 1999:                                      }
{   + GetModeName                                        }
{   + GetGraphMode                                       }
{   + GetModeRange                                       }
{--------------------------------------------------------}
{ LEFT TO DO:                                            }
{   - optimize scaling of stroked fonts                  }
{   - optimize InternalEllipse()                         }
{      using linear appx. of sine/cosine tables          }
{   - justification for stroked fonts does not work      }
{   - On Closegraph deallocate all font pointers         }
{--------------------------------------------------------}

{ text.inc will crash on aligned requirement machines.          }
{ (packed record for fontrec)                                   }
{$G+}

Interface


    const
       { error codes }
       grOk =  0;
       grNoInitGraph = -1;
       grNotDetected = -2;
       grFileNotFound = -3;
       grInvalidDriver = -4;
       grNoLoadMem = -5;
       grNoScanMem = -6;
       grNoFloodMem = -7;
       grFontNotFound = -8;
       grNoFontMem = -9;
       grInvalidMode = -10;
       grError = -11;
       grIOerror = -12;
       grInvalidFont = -13;
       grInvalidFontNum = -14;
       grInvalidVersion = -18;


       black = 0;
       blue = 1;
       green = 2;
       cyan = 3;
       red = 4;
       magenta = 5;
       brown = 6;
       lightgray = 7;
       darkgray = 8;
       lightblue = 9;
       lightgreen = 10;
       lightcyan = 11;
       lightred = 12;
       lightmagenta = 13;
       yellow = 14;
       white = 15;

       { Line styles for GetLineStyle/SetLineStyle }
       SolidLn = 0;
       DottedLn = 1;
       CenterLn = 2;
       DashedLn = 3;
       UserBitLn = 4;

       NormWidth = 1;
       ThickWidth = 3;

       { Set/GetTextStyle Konstanten: }
       DefaultFont = 0;
       TriplexFont = 1;
       SmallFont = 2;
       SansSerifFont = 3;
       GothicFont = 4;
       ScriptFont = 5;
       SimpleFont = 6;
       TSCRFont = 7;
       LCOMFont = 8;
       EuroFont = 9;
       BoldFont = 10;

       HorizDir = 0;
       VertDir = 1;

       UserCharSize = 0;

       ClipOn = true;
       ClipOff = false;

       { Bar3D constants }
       TopOn = true;
       TopOff = false;

       { fill pattern for Get/SetFillStyle: }
       EmptyFill      = 0;
       SolidFill      = 1;
       LineFill       = 2;
       LtSlashFill    = 3;
       SlashFill      = 4;
       BkSlashFill    = 5;
       LtBkSlashFill  = 6;
       HatchFill      = 7;
       XHatchFill     = 8;
       InterleaveFill = 9;
       WideDotFill    = 10;
       CloseDotFill   = 11;
       UserFill       = 12;

       { bitblt operators  }
       NormalPut     = 0;
       CopyPut       = 0;
       XORPut        = 1;
       OrPut         = 2;
       AndPut        = 3;
       NotPut        = 4;

       { SetTextJustify constants }
       LeftText   = 0;
       CenterText = 1;
       RightText  = 2;

       BottomText = 0;
       TopText    = 2;

       { graphic drivers }
       CurrentDriver = -128;
       Detect        = 0;
       LowRes        = 1;
       HercMono      = 7;
       VGA           = 9;

       { graph modes }
       Default = 0;

       { VGA Driver modes }
       VGALo   = 0;
       VGAMed  = 1;
       VGAHi   = 2;

       { Hercules mono card }
       HercMonoHi = 0;






    type
       RGBColor = record
	     r,g,b,i : byte;
       end;

       PaletteType = record
	     Size   : integer;
	     Colors : array[0..767]of Byte;
       end;

       LineSettingsType = record
	     linestyle : word;
	     pattern : word;
	     thickness : word;
       end;

       TextSettingsType = record
	     font : word;
	     direction : word;
	     charsize : word;
	     horiz : word;
	     vert : word;
       end;

       FillSettingsType = record
	     pattern : word;
	     color : word;
       end;

       FillPatternType = array[1..8] of byte;

       PointType = record
   	     x,y : integer;
       end;

       ViewPortType = record
	     x1,y1,x2,y2 : integer;
	     Clip : boolean;
       end;

       ArcCoordsType = record
	     x,y : integer;
	     xstart,ystart : integer;
	     xend,yend : integer;
       end;


  const
       fillpatternTable : array[0..12] of FillPatternType = (
	   ($00,$00,$00,$00,$00,$00,$00,$00),     { background color  }
	   ($ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff),     { foreground color  }
	   ($ff,$ff,$00,$00,$ff,$ff,$00,$00),     { horizontal lines  }
	   ($01,$02,$04,$08,$10,$20,$40,$80),     { slashes           }
	   ($07,$0e,$1c,$38,$70,$e0,$c1,$83),     { thick slashes     }
	   ($07,$83,$c1,$e0,$70,$38,$1c,$0e),     { thick backslashes }
	   ($5a,$2d,$96,$4b,$a5,$d2,$69,$b4),     { backslashes       }
	   ($ff,$88,$88,$88,$ff,$88,$88,$88),     { small boxes       }
	   ($18,$24,$42,$81,$81,$42,$24,$18),     { rhombus           }
	   ($cc,$33,$cc,$33,$cc,$33,$cc,$33),     { wall pattern      }
	   ($80,$00,$08,$00,$80,$00,$08,$00),     { wide points       }
	   ($88,$00,$22,$00,$88,$00,$22,$00),     { dense points      }
	   (0,0,0,0,0,0,0,0)                      { user defined line style }
	  );



  { ******************** PROCEDURAL VARIABLES ********************* }
  { * These are hooks which have device specific stuff in them,   * }
  { * therefore to add new modes or to redirect these routines    * }
  { * then declare variables of these types as shown below.       * }
  {-----------------------------------------------------------------}

TYPE


       { This is the standard putpixel routine used by all function }
       { drawing routines, it will use the viewport settings, as    }
       { well as clip, and use the current foreground color to plot }
       { the desired pixel.                                         }
       defpixelproc = procedure(X,Y: Integer);

       { standard plot and get pixel                                }
       getpixelproc = function(X,Y: Integer): word;
       putpixelproc = procedure(X,Y: Integer; Color: Word);

       { clears the viewport, also used to clear the device         }
       clrviewproc  = procedure;

       { putimage procedure, can be hooked to accomplish transparency }
       putimageproc = procedure (X,Y: Integer; var Bitmap; BitBlt: Word);
       getimageproc = procedure(X1,Y1,X2,Y2: Integer; Var Bitmap);
       imagesizeproc= function (X1,Y1,X2,Y2: Integer): longint;

       graphfreememprc = procedure (var P: Pointer; size: word);
       graphgetmemprc  = procedure (var P: pointer; size: word);

       { internal routines -- can be hooked for much faster drawing }

       { draw filled horizontal lines using current color }
       { on entry coordinates are already clipped.        }
       hlineproc = procedure (x, x2,y : integer);
       { on entry coordinates are already clipped.        }
       { draw filled vertical line using current color    }
       vlineproc = procedure (x,y,y2: integer);

       { this routine is used to draw filled patterns for all routines }
       { that require it. (FillPoly, FloodFill, Sector, etc...         }
       { clipping is verified, uses current Fill settings for drawing  }
       patternlineproc = procedure (x1,x2,y: integer);

       { this routine is used to draw all circles/ellipses/sectors     }
       { more info... on this later...                                 }
       ellipseproc = procedure (X,Y: Integer;XRadius: word;
	  YRadius:word; stAngle,EndAngle: word);

       { Line routine - draws lines thick/norm widths with current     }
       { color and line style - LINE must be clipped here.             }
       lineproc = procedure (X1, Y1, X2, Y2 : Integer);

       { this routine is used for FloodFill - it returns an entire      }
       { screen scan line with a word for each pixel in the scanline    }
       getscanlineproc = procedure (Y : integer; var data);

       { this routine actually switches to the desired video mode.     }
       initmodeproc = procedure;

       { this routine is called to save the sate just before a mode set }
       savestateproc = procedure;
       { this routine is called in closegraph to cleanup...             }
       restorestateproc = procedure;

TYPE
    {-----------------------------------}
    { Linked list for mode information  }
    { This list is set up by one of the }
    { following routines:               }
    { It lists all available resolutions}
    { on this display adapter.          }
    {-----------------------------------}
    {   QueryAdapter()                  }
    {   DetectGraph()                   }
    {   InitGraph()                     }
    {-----------------------------------}
    PModeInfo = ^TModeInfo;
    TModeInfo = record
      DriverNumber: Integer;
      ModeNumber: Integer;
      MaxColor: Longint;
      XAspect : Integer;
      YAspect : Integer;
      MaxX: Integer;
      MaxY: Integer;
      ModeName: String[18];
      { necessary hooks ... }
      DirectPutPixel : DefPixelProc;
      GetPixel       : GetPixelProc;
      PutPixel       : PutPixelProc;
      { defaults possible ... }
      ClearViewPort  : ClrViewProc;
      PutImage       : PutImageProc;
      GetImage       : GetImageProc;
      ImageSize      : ImageSizeProc;
      GetScanLine    : GetScanLineProc;
      Line           : LineProc;
      InternalEllipse: EllipseProc;
      PatternLine    : PatternLineProc;
      HLine          : HLineProc;
      VLine          : VLineProc;
      InitMode       : InitModeProc;
      next: PModeInfo;
    end;



VAR
  DirectPutPixel : DefPixelProc;
  ClearViewPort  : ClrViewProc;
  PutPixel       : PutPixelProc;
  PutImage       : PutImageProc;
  GetImage       : GetImageProc;
  ImageSize      : ImageSizeProc;
  GetPixel       : GetPixelProc;

  GraphFreeMemPtr: graphfreememprc;
  GraphGetMemPtr : graphgetmemprc;

  GetScanLine    : GetScanLineProc;
  Line           : LineProc;
  InternalEllipse: EllipseProc;
  PatternLine    : PatternLineProc;
  HLine          : HLineProc;
  VLine          : VLineProc;

  SaveVideoState : SaveStateProc;
  RestoreVideoState: RestoreStateProc;


Procedure Closegraph;
procedure SetLineStyle(LineStyle: word; Pattern: word; Thickness: word);
procedure SetVisualPage(page : word);
procedure SetActivePage(page : word);
function  GraphErrorMsg(ErrorCode: Integer): string;
Function  GetMaxX: Integer;
Function  GetMaxY: Integer;
Procedure SetViewPort(X1, Y1, X2, Y2: Integer; Clip: Boolean);
Function  GraphResult: Integer;
function  GetModeName(ModeNumber: integer): string;
procedure SetGraphMode(Mode: Integer);
function GetGraphMode: Integer;
function GetMaxMode: word;
procedure RestoreCrtMode;
procedure GetModeRange(GraphDriver: Integer; var LoMode, HiMode: Integer);
Function  GetX: Integer;
Function  GetY: Integer;
procedure GraphDefaults;
procedure ClearDevice;
procedure GetViewSettings(var viewport : ViewPortType);
procedure SetWriteMode(WriteMode : integer);
procedure GetFillSettings(var Fillinfo:Fillsettingstype);
procedure GetFillPattern(var FillPattern:FillPatternType);
procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);
procedure InitGraph(var GraphDriver:Integer;var GraphMode:Integer;const PathToDriver:String);
function InstallUserDriver(Name: string; AutoDetectPtr: Pointer): integer;
function RegisterBGIDriver(driver: pointer): integer;
procedure SetFillStyle(Pattern : word; Color: word);
procedure SetFillPattern(Pattern: FillPatternType; Color: word);
 procedure MoveRel(Dx, Dy: Integer);
 procedure MoveTo(X,Y: Integer);
 { -------------------- Color/Palette ------------------------------- }
 procedure SetBkColor(ColorNum: Word);
 function  GetColor: Word;
 function  GetBkColor: Word;
 procedure SetColor(Color: Word);
 function  GetMaxColor: word;

 { -------------------- Shapes/Lines -------------------------------- }
 procedure Rectangle(x1,y1,x2,y2:integer);
 procedure Bar(x1,y1,x2,y2:integer);
 procedure Bar3D(x1, y1, x2, y2 : integer;depth : word;top : boolean);
 procedure FillPoly(NumPoints: word; Var PolyPoints);
 procedure DrawPoly(NumPoints : word;var polypoints);
 procedure LineRel(Dx, Dy: Integer);
 procedure LineTo(X,Y : Integer);
 procedure FloodFill(x : integer; y : integer; Border: word);

 { -------------------- Circle related routines --------------------- }
 procedure GetAspectRatio(var Xasp,Yasp : word);
 procedure SetAspectRatio(Xasp, Yasp : word);


 procedure Arc(X,Y : Integer; StAngle,EndAngle,Radius: word);
 procedure PieSlice(X,Y,stangle,endAngle:integer;Radius: Word);
 procedure FillEllipse(X, Y: Integer; XRadius, YRadius: Word);
 procedure Circle(X, Y: Integer; Radius:Word);
 procedure Sector(x, y: Integer; StAngle,EndAngle, XRadius, YRadius: Word);
 procedure Ellipse(X,Y : Integer; stAngle, EndAngle: word; XRadius,
   YRadius: word);

 { --------------------- Text related routines --------------------- }
 function  InstallUserFont(const FontFileName : string) : integer;
 function  RegisterBGIfont(font : pointer) : integer;
 procedure GetTextSettings(var TextInfo : TextSettingsType);
 function  TextHeight(const TextString : string) : word;
 function  TextWidth(const TextString : string) : word;
 procedure SetTextJustify(horiz,vert : word);
 procedure SetTextStyle(font,direction : word;charsize : word);
 procedure SetUserCharSize(Multx,Divx,Multy,Divy : word);

 procedure OutTextXY(x,y : integer;const TextString : string);
 procedure OutText(const TextString : string);



Implementation

const
   StdBufferSize = 4096;   { Buffer size for FloodFill }


type


  tinttable = array[0..8192] of integer;
  pinttable = ^tinttable;

  WordArray = Array [0..StdbufferSize] Of word;
  PWordArray = ^WordArray;


const
   { Mask for each bit in byte used to determine pattern }
   BitArray: Array[0..7] of byte =
     ($01,$02,$04,$08,$10,$20,$40,$80);
   RevbitArray: Array[0..7] of byte =
     ($80,$40,$20,$10,$08,$04,$02,$01);




    { pre expanded line patterns }
    { 0 = LSB of byte pattern    }
    { 15 = MSB of byte pattern   }
    LinePatterns: Array[0..15] of BOOLEAN =
     (TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
      TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE);

const
  BGIPath : string = '.';


  { Default font 8x8 system from IBM PC }
  {$i fontdata.inc}


var
  CurrentColor:     Word;
  CurrentBkColor: Word;
  CurrentX : Integer;   { viewport relative }
  CurrentY : Integer;   { viewport relative }

  ClipPixels: Boolean;  { Should cliiping be enabled }


  CurrentWriteMode: Integer;


  _GraphResult : Integer;


  LineInfo : LineSettingsType;
  FillSettings: FillSettingsType;

  { information for Text Output routines }
  CurrentTextInfo : TextSettingsType;
  CurrentXRatio: Real;
  CurrentYRatio: Real;
  installedfonts: longint;  { Number of installed fonts }


  StartXViewPort: Integer; { absolute }
  StartYViewPort: Integer; { absolute }
  ViewWidth : Integer;
  ViewHeight: Integer;
  VideoStart: Pointer;     { ADDRESS OF CURRENT ACTIVE PAGE }


  IsGraphMode : Boolean; { Indicates if we are in graph mode or not }


  ArcCall: ArcCoordsType;   { Information on the last call to Arc or Ellipse }


var

  { ******************** HARDWARE INFORMATION ********************* }
  { Should be set in InitGraph once only.                           }
  IntCurrentMode : Integer;
  IntCurrentDriver : Integer;       { Currently loaded driver          }
  XAspect : Integer;
  YAspect : Integer;
  MaxX : Integer;       { Maximum resolution - ABSOLUTE }
  MaxY : Integer;       { Maximum resolution - ABSOLUTE }
  MaxColor : Longint;

  DriverName: String;
  ModeList : PModeInfo;




{--------------------------------------------------------------------------}
{                                                                          }
{                    LINE AND LINE RELATED ROUTINES                        }
{                                                                          }
{--------------------------------------------------------------------------}

  {$i clip.inc}

  procedure HLineDefault(x,x2,y: integer);

   var
    Col: word;
    xtmp: integer;
   Begin

    { must we swap the values? }
    if x >= x2 then
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
    for x:= x to x2 do
      DirectPutPixel(X,Y);
   end;


  procedure VLineDefault(x,y,y2: integer);

   var
    Col: word;
    ytmp: integer;
  Begin
    { must we swap the values? }
    if y >= y2 then
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
    for y := y to y2 do Directputpixel(x,y)
  End;


  procedure LineDefault(X1, Y1, X2, Y2: Integer);

  var X, Y :           Integer;
      deltax, deltay : Integer;
      d, dinc1, dinc2: Integer;
      xinc1          : Integer;
      xinc2          : Integer;
      yinc1          : Integer;
      yinc2          : Integer;
      i, j           : Integer;
      Flag           : Boolean; { determines pixel direction in thick lines }
      NumPixels      : Integer;
      PixelCount     : Integer;
      OldCurrentColor: Word;
      swtmp          : integer;
      TmpNumPixels   : integer;
 begin
{******************************************}
{  SOLID LINES                             }
{******************************************}
  if lineinfo.LineStyle = SolidLn then
    Begin
       { we separate normal and thick width for speed }
       { and because it would not be 100% compatible  }
       { with the TP graph unit otherwise             }
       if y1 = y2 then
	Begin
     {******************************************}
     {  SOLID LINES HORIZONTAL                  }
     {******************************************}
	  if lineinfo.Thickness=NormWidth then
	    hline(x1,x2,y2)
	  else
	    begin
	       { thick width }
	       hline(x1,x2,y2-1);
	       hline(x1,x2,y2);
	       hline(x2,x2,y2+1);
	    end;
	end
    else
    if x1 = x2 then
	Begin
     {******************************************}
     {  SOLID LINES VERTICAL                    }
     {******************************************}
	  if lineinfo.Thickness=NormWidth then
	    vline(x1,y1,y2)
	  else
	    begin
	    { thick width }
	      vline(x1-1,y1,y2);
	      vline(x1,y1,y2);
	      vline(x1+1,y1,y2);
	    end;
	end
    else
    begin
     { Convert to global coordinates. }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y1 := y1 + StartYViewPort;
     y2 := y2 + StartYViewPort;
     { if fully clipped then exit... }
     if ClipPixels then
       begin
       if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
           StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
              exit;
       end;
     {******************************************}
     {  SLOPED SOLID LINES                      }
     {******************************************}
	   oldCurrentColor := CurrentColor;
	   { Calculate deltax and deltay for initialisation }
	   deltax := abs(x2 - x1);
	   deltay := abs(y2 - y1);

	  { Initialize all vars based on which is the independent variable }
	  if deltax >= deltay then
	    begin

	     Flag := FALSE;
	     { x is independent variable }
	     numpixels := deltax + 1;
	     d := (2 * deltay) - deltax;
	     dinc1 := deltay Shl 1;
	     dinc2 := (deltay - deltax) shl 1;
	     xinc1 := 1;
	     xinc2 := 1;
	     yinc1 := 0;
	     yinc2 := 1;
	    end
	  else
	    begin

	     Flag := TRUE;
	     { y is independent variable }
	     numpixels := deltay + 1;
	     d := (2 * deltax) - deltay;
	     dinc1 := deltax Shl 1;
	     dinc2 := (deltax - deltay) shl 1;
	     xinc1 := 0;
	     xinc2 := 1;
	     yinc1 := 1;
	     yinc2 := 1;
	    end;

	 { Make sure x and y move in the right directions }
	 if x1 > x2 then
	   begin
	    xinc1 := - xinc1;
	    xinc2 := - xinc2;
	   end;
	 if y1 > y2 then
	  begin
	   yinc1 := - yinc1;
	   yinc2 := - yinc2;
	  end;

	 { Start drawing at <x1, y1> }
	 x := x1;
	 y := y1;


	 If LineInfo.Thickness=NormWidth then

	  Begin

	    { Draw the pixels }
	    for i := 1 to numpixels do
	      begin
		DirectPutPixel(x, y);
		if d < 0 then
		  begin
		   d := d + dinc1;
		   x := x + xinc1;
		   y := y + yinc1;
		  end
		else
		  begin
		   d := d + dinc2;
		   x := x + xinc2;
		   y := y + yinc2;
		  end;
		CurrentColor := OldCurrentColor;
	     end;
	  end
	 else
	 { Thick width lines }
	  begin
	    { Draw the pixels }
	    for i := 1 to numpixels do
	       begin
		{ all depending on the slope, we can determine         }
		{ in what direction the extra width pixels will be put }
		If Flag then
		  Begin
		    DirectPutPixel(x-1,y);
		    DirectPutPixel(x,y);
		    DirectPutPixel(x+1,y);
		  end
		else
		  Begin
		    DirectPutPixel(x, y-1);
		    DirectPutPixel(x, y);
		    DirectPutPixel(x, y+1);
		  end;
		if d < 0 then
		  begin
		    d := d + dinc1;
		    x := x + xinc1;
		    y := y + yinc1;
		  end
		else
		  begin
		    d := d + dinc2;
		    x := x + xinc2;
		    y := y + yinc2;
		  end;
		CurrentColor := OldCurrentColor;
	       end;
	  end;
	end;
  end
   else
{******************************************}
{  begin patterned lines                   }
{******************************************}
    Begin
      { Convert to global coordinates. }
      x1 := x1 + StartXViewPort;
      x2 := x2 + StartXViewPort;
      y1 := y1 + StartYViewPort;
      y2 := y2 + StartYViewPort;
      { if fully clipped then exit... }
      if ClipPixels then
       begin
       if LineClipped(x1,y1,x2,y2,StartXViewPort, StartYViewPort,
           StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
              exit;
       end;

      OldCurrentColor := CurrentColor;
      PixelCount:=0;
      if y1 = y2 then
	    Begin
	     { Check if we must swap }
         if x1 >= x2 then
	       Begin
	         swtmp := x1;
	         x1 := x2;
	         x2 := swtmp;
	       end;
         if LineInfo.Thickness = NormWidth then
	      Begin
	       for PixelCount:=x1 to x2 do
		     { optimization: PixelCount mod 16 }
  		     if LinePatterns[PixelCount and 15] = TRUE then
		      begin
		        DirectPutPixel(PixelCount,y2);
		      end;
	      end
	     else
	      Begin
	       for i:=-1 to 1 do
		     Begin
		       for PixelCount:=x1 to x2 do
		        { Optimization from Thomas - mod 16 = and 15 }
		         if LinePatterns[PixelCount and 15] = TRUE then
		           begin
   			         DirectPutPixel(PixelCount,y2+i);
		           end;
		     end;
	      end;
        end
      else
      if x1 = x2 then
	   Begin
	    { Check if we must swap }
	    if y1 >= y2 then
	      Begin
	        swtmp := y1;
	        y1 := y2;
	        y2 := swtmp;
	      end;
	    if LineInfo.Thickness = NormWidth then
	      Begin
	        for PixelCount:=y1 to y2 do
		    { compare if we should plot a pixel here , compare }
		    { with predefined line patterns...                 }
		    if LinePatterns[PixelCount and 15] = TRUE then
		      begin
	            DirectPutPixel(x1,PixelCount);
		      end;
	      end
	    else
	      Begin
	        for i:=-1 to 1 do
		     Begin
		       for PixelCount:=y1 to y2 do
		       { compare if we should plot a pixel here , compare }
		       { with predefined line patterns...                 }
		         if LinePatterns[PixelCount and 15] = TRUE then
		           begin
     			     DirectPutPixel(x1+i,PixelCount);
		           end;
		     end;
	      end;
	   end
      else
	   Begin
	     oldCurrentColor := CurrentColor;
	     { Calculate deltax and deltay for initialisation }
	     deltax := abs(x2 - x1);
	     deltay := abs(y2 - y1);

	     { Initialize all vars based on which is the independent variable }
	     if deltax >= deltay then
	       begin

	         Flag := FALSE;
	         { x is independent variable }
	         numpixels := deltax + 1;
	         d := (2 * deltay) - deltax;
	         dinc1 := deltay Shl 1;
	         dinc2 := (deltay - deltax) shl 1;
	         xinc1 := 1;
	         xinc2 := 1;
	         yinc1 := 0;
	         yinc2 := 1;
	      end
	    else
	      begin

	        Flag := TRUE;
	        { y is independent variable }
	        numpixels := deltay + 1;
	        d := (2 * deltax) - deltay;
	        dinc1 := deltax Shl 1;
	        dinc2 := (deltax - deltay) shl 1;
	        xinc1 := 0;
	        xinc2 := 1;
	        yinc1 := 1;
	        yinc2 := 1;
	      end;

	    { Make sure x and y move in the right directions }
	    if x1 > x2 then
	      begin
	        xinc1 := - xinc1;
	        xinc2 := - xinc2;
	      end;
	    if y1 > y2 then
	      begin
	        yinc1 := - yinc1;
	        yinc2 := - yinc2;
	      end;

	    { Start drawing at <x1, y1> }
	    x := x1;
	    y := y1;

	    If LineInfo.Thickness=ThickWidth then

	     Begin
	       TmpNumPixels := NumPixels-1;
	       { Draw the pixels }
	       for i := 0 to TmpNumPixels do
	         begin
		     { all depending on the slope, we can determine         }
		     { in what direction the extra width pixels will be put }
		       If Flag then
		          Begin
		            { compare if we should plot a pixel here , compare }
		            { with predefined line patterns...                 }
		            if LinePatterns[i and 15] = TRUE then
		              begin
		                DirectPutPixel(x-1,y);
		                DirectPutPixel(x,y);
		                DirectPutPixel(x+1,y);
		              end;
		          end
		       else
		          Begin
		            { compare if we should plot a pixel here , compare }
	                { with predefined line patterns...                 }
		            if LinePatterns[i and 15] = TRUE then
		             begin
		               DirectPutPixel(x,y-1);
		               DirectPutPixel(x,y);
		               DirectPutPixel(x,y+1);
		             end;
		          end;
	           if d < 0 then
		         begin
		           d := d + dinc1;
		           x := x + xinc1;
		           y := y + yinc1;
		         end
	           else
		         begin
                   d := d + dinc2;
                   x := x + xinc2;
                   y := y + yinc2;
		         end;
	        end;
	    end
	   else
	    Begin
	     { instead of putting in loop , substract by one now }
	     TmpNumPixels := NumPixels-1;
	    { NormWidth }
	     for i := 0 to TmpNumPixels do
	     begin
		  if LinePatterns[i and 15] = TRUE then
		    begin
			  DirectPutPixel(x,y);
		    end;
	     if d < 0 then
		 begin
		   d := d + dinc1;
		   x := x + xinc1;
		   y := y + yinc1;
		 end
	     else
		 begin
		   d := d + dinc2;
		   x := x + xinc2;
		   y := y + yinc2;
		 end;
	     end;
	    end
	end;
{******************************************}
{  end patterned lines                     }
{******************************************}
       { restore color }
       CurrentColor:=OldCurrentColor;
   end;
 end;  { Line }


  {********************************************************}
  { Procedure InternalEllipse()                            }
  {--------------------------------------------------------}
  { This routine first calculates all points required to   }
  { draw a circle to the screen, and stores the points     }
  { to display in a buffer before plotting them. The       }
  { aspect ratio of the screen is taken into account when  }
  { calculating the values.                                }
  {--------------------------------------------------------}
  { INPUTS: X,Y : Center coordinates of Ellipse.           }
  {  XRadius - X-Axis radius of ellipse.                   }
  {  YRadius - Y-Axis radius of ellipse.                   }
  {  stAngle, EndAngle: Start angle and end angles of the  }
  {  ellipse (used for partial ellipses and circles)       }
  {--------------------------------------------------------}
  { NOTE: - uses the current write mode.                   }
  {       - Angles must both be between 0 and 360          }
  {********************************************************}
  Procedure InternalEllipseDefault(X,Y: Integer;XRadius: word;
    YRadius:word; stAngle,EndAngle: word);
   var
    i:integer;
    xpt: pinttable;
    ypt: pinttable;
    j,Delta:real;
    NumOfPixels: longint;
    NumOfPix: Array[0..2] of longint;
    count: longint;
    ConvFac,TempTerm: real;
    aval,bval: integer;
    OldcurrentColor: word;
    TmpAngle: word;
    DeltaAngle: word;
  Begin
   { check if valid angles }
   if (stAngle > 360) or (EndAngle > 360) then exit;
   { if impossible angles then swap them! }
   if Endangle < StAngle then
     Begin
       TmpAngle:=EndAngle;
       EndAngle:=StAngle;
       Stangle:=TmpAngle;
     end;
   { calculate difference of angle now so we don't always have to calculate it }
   DeltaAngle:= EndAngle-StAngle;
   i:=0;
   if LineInfo.Thickness=NormWidth then
     Begin
       { approximate the number of pixels required by using the circumference }
       { equation of an ellipse.                                              }
       NumOfPixels:=8*Round(2*sqrt((sqr(XRadius)+sqr(YRadius)) div 2));
       GetMem(xpt,NumOfpixels*sizeof(word));
       GetMem(ypt,NumOfPixels*sizeof(word));
       { Calculate the angle precision required }
       Delta := DeltaAngle / (NumOfPixels);
       { Adjust for screen aspect ratio }
       XRadius:=(longint(XRadius)*10000) div XAspect;
       YRadius:=(longint(YRadius)*10000) div YAspect;
       { Initial counter value }
       j:=Delta+StAngle;
       { removed from inner loop to make faster }
       ConvFac:=Pi/180.0;
       Repeat
	 { this used by both sin and cos }
	 TempTerm := j*ConvFac;
	 { Calculate points }
  {$R-}
	 xpt^[i]:=round(XRadius*Cos(TempTerm));
	 { calculate the value of y }
	 ypt^[i]:=round(YRadius*Sin(TempTerm+Pi));
  {$R+}
	 j:=j+Delta;
	 inc(i);
       Until j > DeltaAngle;
     end
   else
   {******************************************}
   {  CIRCLE OR ELLIPSE WITH THICKNESS=3      }
   {******************************************}
    Begin
      NumOfPix[1]:=8*Round(2*sqrt((sqr(XRadius)+sqr(YRadius)) div 2));
      NumOfPix[0]:=8*Round(2*sqrt((sqr(XRadius-1)+sqr(YRadius-1)) div 2));
      NumOfPix[2]:=8*Round(2*sqrt((sqr(XRadius+1)+sqr(YRadius+1)) div 2));
      GetMem(xpt,(NumOfPix[1]+NumOfPix[2]+NumOfPix[0])*sizeof(word));
      GetMem(ypt,(NumOfPix[1]+NumOfPix[2]+NumOfPix[0])*sizeof(word));
      { removed from inner loop to make faster }
      ConvFac:=Pi/180.0;
      for Count:=0 to 2 do
	Begin
	  aval:=XRadius+Count-1;
	  bval:=YRadius+Count-1;
	  Delta := DeltaAngle / (NumOfPix[Count]);
	  aval:= (longint(aval)*10000) div XAspect;
	  bval:= (longint(bval)*10000) div YAspect;
	  j:=Delta+Stangle;
	  Repeat
	    { this used by both sin and cos }
	    TempTerm := j*ConvFac;
    {$R-}
	    xpt^[i]:=round((aval)*Cos(TempTerm));
	    { calculate the value of y }
	    ypt^[i]:=round(bval*Sin(TempTerm+Pi));
    {$R+}
	    j:=j+Delta;
	    inc(i);
	  Until j > DeltaAngle;
	end;
    end;
   {******************************************}
   {  NOW ALL PIXEL POINTS ARE IN BUFFER      }
   {  plot them all to the screen             }
   {******************************************}
   Count:=0;
   OldcurrentColor:=currentColor;
   Repeat
{$R-}
     DirectPutPixel(xpt^[Count]+X,ypt^[Count]+Y);
{$R+}
     inc(count);
   until Count>=i;

   { Get End and Start points into the ArcCall information record }
   ArcCall.X := X;
   ArcCall.Y := Y;
   ArcCall.XStart := xpt^[0] + X;
   ArcCall.YStart := ypt^[0] + Y;
{$R-}
   ArcCall.XEnd := xpt^[Count-1] + X;
   ArcCall.YEnd := ypt^[Count-1] + Y;
{$R+}
   CurrentColor:=OldCurrentColor;
   if LineInfo.Thickness=NormWidth then
     Begin
       Freemem(xpt,NumOfPixels*sizeof(word));
       Freemem(ypt,NumOfPixels*sizeof(word));
     end
   else
     Begin
       FreeMem(xpt,(NumOfPix[1]+NumOfPix[2]+NumOfPix[0])*sizeof(word));
       FreeMem(ypt,(NumOfPix[1]+NumOfPix[2]+NumOfPix[0])*sizeof(word));
     end;
  end;


  procedure PatternLineDefault(x1,x2,y: integer);
  {********************************************************}
  { Draws a horizontal patterned line according to the     }
  { current Fill Settings.                                 }
  {********************************************************}
  { Important notes:                                       }
  {  - CurrentColor must be set correctly before entering  }
  {    this routine.                                       }
  {********************************************************}
   var
    NrIterations: Integer;
    i           : Integer;
    j           : Integer;
    TmpFillPattern : byte;
    OldWriteMode : word;
    OldCurrentColor : word;
   begin
     { convert to global coordinates ... }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y  := y + StartYViewPort;
     { if line was fully clipped then exit...}
     if LineClipped(x1,y,x2,y,StartXViewPort,StartYViewPort,
        StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
         exit;

     OldWriteMode := CurrentWriteMode;
     CurrentWriteMode := NormalPut;

     { number of times to go throuh the 8x8 pattern }
     NrIterations := abs(x2 - x1) div 8;
     Inc(NrIterations);


     { Get the current pattern }
     TmpFillPattern := FillPatternTable
{       [FillSettings.Pattern][(((y+viewport.x1) and $7)+1];}
       [FillSettings.Pattern][(y and $7)+1];

     if FillSettings.Pattern = EmptyFill then
       begin
         OldCurrentColor := CurrentColor;
         CurrentColor := CurrentBkColor;
         HLine(x1,x2,y);
         CurrentColor := OldCurrentColor;
       end
     else
     if  FillSettings.Pattern = SolidFill then
       begin
         HLine(x1,x2,y);
       end
     else
       begin
         For i:= 0 to NrIterations do
           Begin
      	     for j:=0 to 7 do
	          Begin
			  { x1 mod 8 }
	          if RevBitArray[x1 and 7] and TmpFillPattern <> 0 then
   	             DirectPutpixel(x1,y)
	          else
	            begin
 		          { According to the TP graph manual, we overwrite everything }
		          { which is filled up - checked against VGA and CGA drivers  }
		          { of TP.                                                    }
		          OldCurrentColor := CurrentColor;
		          CurrentColor := CurrentBkColor;
		          DirectPutPixel(x1,y);
		          CurrentColor := OldCurrentColor;
	            end;
	          Inc(x1);
	          if x1 > x2 then
	           begin
		         CurrentWriteMode := OldWriteMode;
		         exit;
	           end;
	         end;
           end;
        end;
     CurrentWriteMode := OldWriteMode;
   end;




  procedure LineRel(Dx, Dy: Integer);

   Begin
     Line(CurrentX, CurrentY, CurrentX + Dx, CurrentY + Dy);
     CurrentX := CurrentX + Dx;
     CurrentY := CurrentY + Dy;
   end;


  procedure LineTo(x,y : Integer);

   Begin
     Line(CurrentX, CurrentY, X, Y);
     CurrentX := X;
     CurrentY := Y;
   end;




  procedure Rectangle(x1,y1,x2,y2:integer);

   begin
     { Do not draw the end points }
     Line(x1,y1,x2-1,y1);
     Line(x1,y1+1,x1,y2);
     Line(x2,y1,x2,y2-1);
     Line(x1+1,y2,x2,y2);
   end;


  procedure GetLineSettings(var ActiveLineInfo : LineSettingsType);

   begin
    Activelineinfo:=Lineinfo;
   end;


  procedure SetLineStyle(LineStyle: word; Pattern: word; Thickness: word);

   var
    i: byte;
    j: byte;

   Begin
    if (LineStyle > UserBitLn) or ((Thickness <> Normwidth) and (Thickness <> ThickWidth)) then
      _GraphResult := grError
    else
      begin
       LineInfo.Thickness := Thickness;
       LineInfo.LineStyle := LineStyle;
       case LineStyle of
	    UserBitLn: Lineinfo.Pattern := pattern;
	    SolidLn:   Lineinfo.Pattern  := $ffff;  { ------- }
	    DashedLn : Lineinfo.Pattern := $F8F8;   { -- -- --}
	    DottedLn:  LineInfo.Pattern := $CCCC;   { - - - - }
	    CenterLn: LineInfo.Pattern :=  $FC78;   { -- - -- }
       end; { end case }
       { setup pattern styles }
       j:=15;
       for i:=0 to 15 do
	Begin
	  { bitwise mask for each bit in the word }
	  if (word($01 shl i) AND LineInfo.Pattern) <> 0 then
	      LinePatterns[j]:=TRUE
	  else
	      LinePatterns[j]:=FALSE;
	  Dec(j);
	end;
      end;
   end;




{--------------------------------------------------------------------------}
{                                                                          }
{                    VIEWPORT RELATED ROUTINES                             }
{                                                                          }
{--------------------------------------------------------------------------}


Procedure ClearViewPortDefault;
var
 j: integer;
 OldWriteMode, OldCurColor: word;
 LineSets : LineSettingsType;
Begin
  { CP is always RELATIVE coordinates }
  CurrentX := 0;
  CurrentY := 0;

  { Save all old settings }
  OldCurColor := CurrentColor;
  CurrentColor:=CurrentBkColor;
  OldWriteMode:=CurrentWriteMode;
  GetLineSettings(LineSets);
  { reset to normal line style...}
  SetLineStyle(SolidLn, 0, NormWidth);

  { routines are relative here...}
  for J:=0 to ViewHeight do
       HLine(0, ViewWidth, J);

  { restore old settings...}
  SetLineStyle(LineSets.LineStyle, LineSets.Pattern, LineSets.Thickness);
  CurrentColor := OldCurColor;
  CurrentWriteMode := OldWriteMode;
end;


Procedure SetViewPort(X1, Y1, X2, Y2: Integer; Clip: Boolean);
Begin
  if (X1 > GetMaxX) or (X2 > GetMaxX) or (X1 > X2) or (X1 < 0) then
  Begin
    _GraphResult := grError;
    exit;
  end;
  if (Y1 > GetMaxY) or (Y2 > GetMaxY) or (Y1 > Y2) or (Y1 < 0) then
  Begin
    _GraphResult := grError;
    exit;
  end;
  { CP is always RELATIVE coordinates }
  CurrentX := 0;
  CurrentY := 0;
  StartXViewPort := X1;
  StartYViewPort := Y1;
  ViewWidth :=  X2-X1;
  ViewHeight:=  Y2-Y1;
  ClipPixels := Clip;
end;


procedure GetViewSettings(var viewport : ViewPortType);
begin
  ViewPort.X1 := StartXViewPort;
  ViewPort.Y1 := StartYViewPort;
  ViewPort.X2 := ViewWidth - StartXViewPort;
  ViewPort.Y2 := ViewHeight - StartYViewPort;
  ViewPort.Clip := ClipPixels;
end;

procedure ClearDevice;
var
  ViewPort: ViewPortType;
begin
  { Reset the CP }
  CurrentX := 0;
  CurrentY := 0;
  { save viewport }
  ViewPort.X1 :=  StartXviewPort;
  ViewPort.X2 :=  ViewWidth - StartXViewPort;
  ViewPort.Y1 :=  StartYViewPort;
  ViewPort.Y2 :=  ViewHeight - StartYViewPort;
  ViewPort.Clip := ClipPixels;
  { put viewport to full screen }
  StartXViewPort := 0;
  ViewHeight := MaxY;
  StartYViewPort := 0;
  ViewWidth := MaxX;
  ClipPixels := TRUE;
  ClearViewPort;
  { restore old viewport }
  StartXViewPort := ViewPort.X1;
  ViewWidth := ViewPort.X2-ViewPort.X1;
  StartYViewPort := ViewPort.Y1;
  ViewHeight := ViewPort.Y2-ViewPort.Y1;
  ClipPixels := ViewPort.Clip;
end;



{--------------------------------------------------------------------------}
{                                                                          }
{                      BITMAP PUT/GET ROUTINES                             }
{                                                                          }
{--------------------------------------------------------------------------}


  Procedure GetScanlineDefault (Y : Integer; Var Data);
  {********************************************************}
  { Procedure GetScanLine()                                }
  {--------------------------------------------------------}
  { Returns the full scanline of the video line of the Y   }
  { coordinate. The values are returned in a WORD array    }
  { each WORD representing a pixel of the specified scanline}
  {********************************************************}

  Var
    Offset, x : Integer;
  Begin
     For x:=0 to MaxX Do Begin
	    WordArray(Data)[x]:=GetPixel(x, y);
     End;
  End;



Function DefaultImageSize(X1,Y1,X2,Y2: Integer): longint;
Begin
  { each pixel uses two bytes, to enable modes with colors up to 64K }
  { to work.                                                         }
  DefaultImageSize := 12 + (((X2-X1)*(Y2-Y1))*2);
end;

Procedure DefaultPutImage(X,Y: Integer; var Bitmap; BitBlt: Word);
type
  pt = array[0..32000] of word;
  ptw = array[0..3] of longint;
var
  color: word;
  i,j: Integer;
  Y1,X1: Integer;
  k: integer;
Begin
  X1:= ptw(Bitmap)[0]+X; { get width and adjust end coordinate accordingly }
  Y1:= ptw(Bitmap)[1]+Y; { get height and adjust end coordinate accordingly }
  k:= 12; { Three reserved longs at start of bitmap }
  for j:=Y to Y1 do
   Begin
     for i:=X to X1 do
      begin
	    case BitBlt of
{$R-}
         CopyPut: color:= pt(Bitmap)[k];  { also = normalput }
	     XORPut: color:= pt(Bitmap)[k] XOR GetPixel(i,j);
	     OrPut: color:= pt(Bitmap)[k] OR GetPixel(i,j);
	     AndPut: color:= pt(Bitmap)[k] AND GetPixel(i,j);
	     NotPut: color:= not pt(Bitmap)[k];
{$R+}
   	   end;
	   putpixel(i,j,color);
	   Inc(k);
     end;
   end;
end;


Procedure DefaultGetImage(X1,Y1,X2,Y2: Integer; Var Bitmap);
type
  pt = array[0..32000] of word;
  ptw = array[0..3] of longint;
var
  i,j: integer;
  k: longint;
Begin
  k:= 12; { Three reserved longs at start of bitmap }
  for j:=Y1 to Y2 do
   Begin
     for i:=X1 to X2 do
      begin
{$R-}
	    pt(Bitmap)[k] :=getpixel(i,j);
{$R+}
	    Inc(k);
      end;
   end;
   ptw(Bitmap)[0] := X2-X1;   { First longint  is width  }
   ptw(Bitmap)[1] := Y2-Y1;   { Second longint is height }
   ptw(bitmap)[2] := 0;       { Third longint is reserved}
end;






  Procedure GetArcCoords(var ArcCoords: ArcCoordsType);
   Begin
     ArcCoords.X := ArcCall.X;
     ArcCoords.Y := ArcCall.Y;
     ArcCoords.XStart := ArcCall.XStart;
     ArcCoords.YStart := ArcCall.YStart;
     ArcCoords.XEnd := ArcCall.XEnd;
     ArcCoords.YEnd := ArcCall.YEnd;
   end;


procedure SetVisualPage(page : word);
begin
end;


procedure SetActivePage(page : word);
begin
end;


  Procedure DefaultHooks;
  {********************************************************}
  { Procedure DefaultHooks()                               }
  {--------------------------------------------------------}
  { Resets all hookable routine either to nil for those    }
  { which need overrides, and others to defaults.          }
  { This is called each time SetGraphMode() is called.     }
  {********************************************************}
  Begin
    { All default hooks procedures }

    { required...}
    DirectPutPixel := nil;
    PutPixel := nil;
    GetPixel := nil;

    { optional...}
    ClearViewPort := ClearViewportDefault;
    PutImage := DefaultPutImage;
    GetImage := DefaultGetImage;
    ImageSize := DefaultImageSize;

    GraphFreeMemPtr := nil;
    GraphGetMemPtr := nil;

    GetScanLine := GetScanLineDefault;
    Line := LineDefault;
    InternalEllipse := InternalEllipseDefault;
    PatternLine := PatternLineDefault;
    HLine := HLineDefault;
    VLine := VLineDefault;
  end;

  Procedure InitVars;
  {********************************************************}
  { Procedure InitVars()                                   }
  {--------------------------------------------------------}
  { Resets all internal variables, and resets all          }
  { overridable routines.                                  }
  {********************************************************}
   Begin
    InstalledFonts := 0;
    { Install standard fonts }
    InstallUserFont('TRIP');
    InstallUserFont('LITT');
    InstallUserFont('SANS');
    InstallUserFont('GOTH');
    ArcCall.X := 0;
    ArcCall.Y := 0;
    ArcCall.XStart := 0;
    ArcCall.YStart := 0;
    ArcCall.XEnd := 0;
    ArcCall.YEnd := 0;
    { Reset to default values }
    IntCurrentMode := 0;
    IntCurrentDriver := 0;
    XAspect := 0;
    YAspect := 0;
    MaxX := 0;
    MaxY := 0;
    MaxColor := 0;
    DefaultHooks;
  end;




{$i modes.inc}
{$i graph.inc}


  function InstallUserDriver(Name: string; AutoDetectPtr: Pointer): integer;
   begin
     _graphResult := grError;
   end;

  function RegisterBGIDriver(driver: pointer): integer;

   begin
     _graphResult := grError;
   end;



{ ----------------------------------------------------------------- }



  Procedure Arc(X,Y : Integer; StAngle,EndAngle,Radius: word);

   var
    OldWriteMode: word;

   Begin
     if (Radius = 0) then
	   Exit;

     if (Radius = 1) then
       begin
         { must use clipping ... }
         { don't need to explicitly set NormalPut mode }
         { because PutPixel only supports normal put   }
	     PutPixel(X, Y,CurrentColor);
	     Exit;
       end;

     { Only if we are using thickwidths lines do we accept }
     { XORput write modes.                                 }
     OldWriteMode := CurrentWriteMode;
     if (LineInfo.Thickness = NormWidth) then
       CurrentWriteMode := NormalPut;
	 InternalEllipse(X,Y,Radius,Radius,StAngle,Endangle);
     CurrentWriteMode := OldWriteMode;
   end;


 procedure Ellipse(X,Y : Integer; stAngle, EndAngle: word; XRadius,YRadius: word);
  Begin
    InternalEllipse(X,Y,XRadius,YRadius,stAngle,EndAngle);
  end;


 procedure FillEllipse(X, Y: Integer; XRadius, YRadius: Word);
  {********************************************************}
  { Procedure FillEllipse()                                }
  {--------------------------------------------------------}
  { Draws a filled ellipse using (X,Y) as a center point   }
  { and XRadius and YRadius as the horizontal and vertical }
  { axes. The ellipse is filled with the current fill color}
  { and fill style, and is bordered with the current color.}
  {--------------------------------------------------------}
  { Important notes:                                       }
  {  - CONTRRARY to VGA BGI - SetWriteMode DOES not        }
  {    affect the contour of the ellipses. BGI mode        }
  {    supports XORPut but the FloodFill() is still bounded}
  {    by the ellipse. In OUR case, XOR Mode is simply     }
  {    not supported.                                      }
  {********************************************************}
  var
   OldWriteMode: Word;
  begin
    { only normal put supported }
    OldWriteMode := CurrentWriteMode;
    CurrentWriteMode := NormalPut;
    InternalEllipse(X,Y,XRadius,YRadius,0,360);
    FloodFill(X,Y,CurrentColor);
    { restore old write mode }
    CurrentWriteMode := OldWriteMode;
  end;



 procedure Circle(X, Y: Integer; Radius:Word);
  {********************************************************}
  { Draws a circle centered at X,Y with the given Radius.  }
  {********************************************************}
  { Important notes:                                       }
  {  - Thickwidth circles use the current write mode, while}
  {    normal width circles ALWAYS use CopyPut/NormalPut   }
  {    mode. (Tested against VGA BGI driver -CEC 13/Aug/99 }
  {********************************************************}
  var OriginalArcInfo: ArcCoordsType;
      OldWriteMode: word;

  begin
     if (Radius = 0) then
	  Exit;

     if (Radius = 1) then
     begin
      { only normal put mode is supported by a call to PutPixel }
	  PutPixel(X, Y, CurrentColor);
	  Exit;
     end;

     { save state of arc information }
     { because it is not needed for  }
     { a circle call.                }
     move(ArcCall,OriginalArcInfo, sizeof(ArcCall));
     if LineInfo.Thickness = Normwidth then
       begin
	     OldWriteMode := CurrentWriteMode;
	     CurrentWriteMode := CopyPut;
       end;
     InternalEllipse(X,Y,Radius,Radius,0,360);
     if LineInfo.Thickness = Normwidth then
	 CurrentWriteMode := OldWriteMode;
     { restore arc information }
     move(OriginalArcInfo, ArcCall,sizeof(ArcCall));
 end;



 procedure Sector(x, y: Integer; StAngle,EndAngle, XRadius, YRadius: Word);
  var angle : real;
      writemode : word;
  begin
     Ellipse(x,y,stAngle,endAngle,XRadius,YRadius);
    { As in the TP graph unit - the line settings are used to }
    { define the outline of the sector.                       }
     writemode:=Currentwritemode;
     Currentwritemode:=normalput;
     Line(ArcCall.XStart, ArcCall.YStart, x,y);
     Line(x,y,ArcCall.Xend,ArcCall.YEnd);
     { we must take care of clipping so we call PutPixel instead }
     { of DirectPutPixel...                                      }
     PutPixel(ArcCall.xstart,ArcCall.ystart,CurrentColor);
     PutPixel(x,y,CurrentColor);
     PutPixel(ArcCall.xend,ArcCall.yend,CurrentColor);
     stangle:=Stangle mod 360; EndAngle:=Endangle mod 360;
     if stAngle<=Endangle then
       Angle:=(stAngle+EndAngle)/2
     else
       angle:=(stAngle-360+EndAngle)/2;
     { fill from the point in the middle of the slice }
     XRadius:=(longint(XRadius)*10000) div XAspect;
     YRadius:=(longint(YRadius)*10000) div YAspect;
     { avoid rounding errors }
     if abs(ArcCall.xstart-ArcCall.xend)
	+abs(ArcCall.ystart-ArcCall.yend)>2 then
       FloodFill(x+round(sin((angle+90)*Pi/180)*XRadius/2),
	 y+round(cos((angle+90)*Pi/180)*YRadius/2),CurrentColor);
     CurrentWriteMode := writemode;
  end;



   procedure SetFillStyle(Pattern : word; Color: word);

   begin
     { on invalid input, the current fill setting will be }
     { unchanged.                                         }
     if (Pattern > UserFill) or (Color > GetMaxColor) then
      begin
	   _GraphResult := grError;
	   exit;
      end;
     FillSettings.Color := Color;
     FillSettings.Pattern := Pattern;
   end;


  procedure SetFillPattern(Pattern: FillPatternType; Color: word);
  {********************************************************}
  { Changes the Current FillPattern to a user defined      }
  { pattern and changes also the current fill color.       }
  { The FillPattern is saved in the FillPattern array so   }
  { it can still be used with SetFillStyle(UserFill,Color) }
  {********************************************************}
   var
    i: integer;

   begin
     if Color > GetMaxColor then
       begin
	    _GraphResult := grError;
	    exit;
       end;

     FillSettings.Color := Color;
     FillSettings.Pattern := UserFill;

     { Save the pattern in the buffer }
     For i:=1 to 8 do
       FillPatternTable[UserFill][i] := Pattern[i];

   end;

  procedure Bar(x1,y1,x2,y2:integer);
  {********************************************************}
  { Important notes for compatibility with BP:             }
  {     - WriteMode is always CopyPut                      }
  {     - No contour is drawn for the lines                }
  {********************************************************}
  var y               : Integer;
      origcolor       : longint;
      origlinesettings: Linesettingstype;
      origwritemode   : Integer;
   begin
     origlinesettings:=lineinfo;
     origcolor:=CurrentColor;

     { Always copy mode for Bars }
     origwritemode := CurrentWriteMode;
     CurrentWriteMode := CopyPut;

     { All lines used are of this style }
     Lineinfo.linestyle:=solidln;
     Lineinfo.thickness:=normwidth;

     case Fillsettings.pattern of
     EmptyFill :
           begin
		     Currentcolor:=CurrentBkColor;
		     for y:=y1 to y2 do
		       Hline(x1,x2,y);
		   end;
     SolidFill :
           begin
		     CurrentColor:=FillSettings.color;
		     for y:=y1 to y2 do
		       Hline(x1,x2,y);
		  end;
     else
      Begin
   	    CurrentColor:=FillSettings.color;
        for y:=y1 to y2 do
	          patternline(x1,x2,y);
      end;
    end;
    CurrentColor:= Origcolor;
    LineInfo := OrigLineSettings;
    CurrentWriteMode := OrigWritemode;
   end;




procedure bar3D(x1, y1, x2, y2 : integer;depth : word;top : boolean);
var
 origwritemode : integer;
 OldX, OldY : integer;
begin
  origwritemode := CurrentWriteMode;
  CurrentWriteMode := CopyPut;
  Bar(x1,y1,x2,y2);
  Rectangle(x1,y1,x2,y2);

  { Current CP should not be updated in Bar3D }
  { therefore save it and then restore it on  }
  { exit.                                     }
  OldX := CurrentX;
  OldY := CurrentY;

  if top then begin
    Moveto(x1,y1);
    Lineto(x1+depth,y1-depth);
    Lineto(x2+depth,y1-depth);
    Lineto(x2,y1);
  end;
  if Depth <> 0 then
    Begin
      Moveto(x2+depth,y1-depth);
      Lineto(x2+depth,y2-depth);
      Lineto(x2,y2);
    end;
  { restore CP }
  CurrentX := OldX;
  CurrentY := OldY;
  CurrentWriteMode := origwritemode;
end;



{--------------------------------------------------------------------------}
{                                                                          }
{                       COLOR AND PALETTE ROUTINES                         }
{                                                                          }
{--------------------------------------------------------------------------}

  procedure SetColor(Color: Word);

   Begin
     CurrentColor := Color;
   end;


  function GetColor: Word;

   Begin
     GetColor := CurrentColor;
   end;

  function GetBkColor: Word;

   Begin
     GetBkColor := CurrentBkColor;
   end;


  procedure SetBkColor(ColorNum: Word);
  { Background color means background screen color in this case, and it is  }
  { INDEPENDANT of the viewport settings, so we must clear the whole screen }
  { with the color.                                                         }
   var
     ViewPort: ViewportType;
   Begin
     GetViewSettings(Viewport);
     SetViewPort(0,0,MaxX,MaxY,FALSE);
     CurrentBkColor := ColorNum;
     ClearViewPort;
     SetViewport(ViewPort.X1,Viewport.Y1,Viewport.X2,Viewport.Y2,Viewport.Clip);
   end;


  function GetMaxColor: word;
  { Checked against TP VGA driver - CEC }

   begin
      GetMaxColor:=MaxColor-1; { based on an index of zero so subtract one }
   end;






   Procedure MoveRel(Dx, Dy: Integer);
    Begin
     CurrentX := CurrentX + Dx;
     CurrentY := CurrentY + Dy;
   end;

   Procedure MoveTo(X,Y: Integer);
  {********************************************************}
  { Procedure MoveTo()                                     }
  {--------------------------------------------------------}
  { Moves the current pointer in VIEWPORT relative         }
  { coordinates to the specified X,Y coordinate.           }
  {********************************************************}
    Begin
     CurrentX := X;
     CurrentY := Y;
    end;


function GraphErrorMsg(ErrorCode: Integer): string;
Begin
 GraphErrorMsg:='';
 case ErrorCode of
  grOk,grFileNotFound,grInvalidDriver: exit;
  grNoInitGraph: GraphErrorMsg:='Graphics driver not installed';
  grNotDetected: GraphErrorMsg:='Graphics hardware not detected';
  grNoLoadMem,grNoScanMem,grNoFloodMem: GraphErrorMsg := 'Not enough memory for graphics';
  grNoFontMem: GraphErrorMsg := 'Not enough memory to load font';
  grFontNotFound: GraphErrorMsg:= 'Font file not found';
  grInvalidMode: GraphErrorMsg := 'Invalid graphics mode';
  grError: GraphErrorMsg:='Graphics error';
  grIoError: GraphErrorMsg:='Graphics I/O error';
  grInvalidFont,grInvalidFontNum: GraphErrorMsg := 'Invalid font';
  grInvalidVersion: GraphErrorMsg:='Invalid driver version';
 end;
end;




  Function GetMaxX: Integer;
  { Routine checked against VGA driver - CEC }
   Begin
     GetMaxX := MaxX;
   end;

  Function GetMaxY: Integer;
  { Routine checked against VGA driver - CEC }
   Begin
    GetMaxY := MaxY;
   end;




Function GraphResult: Integer;
Begin
  GraphResult := _GraphResult;
  _GraphResult := grOk;
end;


  Function GetX: Integer;
   Begin
     GetX := CurrentX;
   end;


  Function GetY: Integer;
   Begin
     GetY := CurrentY;
   end;

   Function GetDriverName: string;
    var
     mode: PModeInfo;
    begin
      GetDriverName:=DriverName;
    end;


   procedure graphdefaults;
   { PS: GraphDefaults does not ZERO the ArcCall structure }
   { so a call to GetArcCoords will not change even the    }
   { returned values even if GraphDefaults is called in    }
   { between.                                              }
    var
     i: integer;
      begin
	 lineinfo.linestyle:=solidln;
	 lineinfo.thickness:=normwidth;
	 { reset line style pattern }
	 for i:=0 to 15 do
	   LinePatterns[i] := TRUE;

	 { By default, according to the TP prog's reference }
	 { the default pattern is solid, and the default    }
	 { color is the maximum color in the palette.       }
	 fillsettings.color:=GetMaxColor;
	 fillsettings.pattern:=solidfill;
	 { GraphDefaults resets the User Fill pattern to $ff }
	 { checked with VGA BGI driver - CEC                 }
	 for i:=1 to 8 do
	   FillPatternTable[UserFill][i] := $ff;


	 CurrentColor:=white;
	 SetBkColor(Black);


	 ClipPixels := TRUE;
	 { Reset the viewport }
	 StartXViewPort := 0;
	 StartYViewPort := 0;
	 ViewWidth := MaxX;
	 ViewHeight := MaxY;
	 { Reset CP }
	 CurrentX := 0;
	 CurrentY := 0;

	 { normal write mode }
	 CurrentWriteMode := CopyPut;

	 { Schriftart einstellen }
	 CurrentTextInfo.font := DefaultFont;
	 CurrentTextInfo.direction:=HorizDir;
	 CurrentTextInfo.charsize:=1;
	 CurrentTextInfo.horiz:=LeftText;
	 CurrentTextInfo.vert:=TopText;

	 XAspect:=10000; YAspect:=10000;
      end;


    procedure GetAspectRatio(var Xasp,Yasp : word);
    begin
      XAsp:=XAspect;
      YAsp:=YAspect;
    end;

  procedure SetAspectRatio(Xasp, Yasp : word);
  begin
    Xaspect:= XAsp;
    YAspect:= YAsp;
  end;


  procedure SetWriteMode(WriteMode : integer);
   begin
     if (writemode<>xorput) and (writemode<>CopyPut) then
	exit;
     CurrentWriteMode := WriteMode;
   end;


  procedure GetFillSettings(var Fillinfo:Fillsettingstype);
   begin
     Fillinfo:=Fillsettings;
   end;

  procedure GetFillPattern(var FillPattern:FillPatternType);
   begin
     FillPattern:=FillpatternTable[UserFill];
   end;






    procedure DrawPoly(numpoints : word;var polypoints);

      type
	 ppointtype = ^pointtype;
	 pt = array[0..16000] of pointtype;

      var
	 i : longint;

      begin
	 if numpoints < 2 then
	   begin
	     _GraphResult := grError;
	     exit;
	   end;
	 for i:=0 to numpoints-2 do
	   line(pt(polypoints)[i].x,
		pt(polypoints)[i].y,
		pt(polypoints)[i+1].x,
		pt(polypoints)[i+1].y);
      end;


  procedure PieSlice(X,Y,stangle,endAngle:integer;Radius: Word);
  var angle : real;
      XRadius, YRadius : word;
      writemode : word;
  begin
     Arc(x,y,StAngle,EndAngle,Radius);
     Line(ArcCall.XStart, ArcCall.YStart, x,y);
     Line(x,y, ArcCall.XEnd, ArcCall.YEnd);
     { must use PutPixel() instead of DirectPutPixel because we need }
     { clipping...                                                   }
     PutPixel(ArcCall.xstart,ArcCall.ystart,CurrentColor);
     PutPixel(x,y,CurrentColor);
     PutPixel(ArcCall.xend,ArcCall.yend,CurrentColor);
     Stangle:=stAngle mod 360; EndAngle:=Endangle mod 360;
     if Stangle<=Endangle then
       angle:=(StAngle+EndAngle)/2
     else
       angle:=(Stangle-360+Endangle)/2;
     { fill from the point in the middle of the slice }
     XRadius:=(longint(Radius)*10000) div XAspect;
     YRadius:=(longint(Radius)*10000) div YAspect;
     { avoid rounding errors }
     if abs(ArcCall.xstart-ArcCall.xend)
	+abs(ArcCall.ystart-ArcCall.yend)>2 then
{       FloodFill(x+round(sin((angle+90)*Pi/180)*XRadius/2),
	 y+round(cos((angle+90)*Pi/180)*YRadius/2),truecolor);}
     CurrentWriteMode := writemode;
  end;

{$i fills.inc}
{$i text.inc}


  procedure InitGraph(var GraphDriver:Integer;var GraphMode:Integer;
    const PathToDriver:String);
  var i,index:Integer;
     LoMode, HiMode: Integer;
     CpyMode: Integer;
     CpyDriver: Integer;
  begin
    { path to the fonts (where they will be searched)...}
    bgipath:=PathToDriver;
    if bgipath[length(bgipath)]<>'\' then
    bgipath:=bgipath+'\';

    { make sure our driver list is setup...}
    QueryAdapterInfo;
    if not assigned(SaveVideoState) then
      RunError(216);
    SaveVideoState;
    InitVars;
    DriverName:=InternalDriverName;   { DOS Graphics driver }

    if (Graphdriver=Detect) then
      begin
          HiMode := -1;
          LoMode := -1;
          { We start at VGA-1 }
          GraphDriver := VGA;
          CpyMode := 0;
          { search all possible graphic drivers in ascending order...}
          { usually the new driver numbers indicate newest hardware...}
          { Internal driver numbers start at VGA=9 }
          repeat
             GetModeRange(GraphDriver,LoMode,HiMode);
             { save the highest mode possible...}
             if HiMode = -1 then break;
             CpyMode:=HiMode;
             CpyDriver:=GraphDriver;
             { go to next driver if it exists...}
             Inc(GraphDriver);
          until (CpyMode=-1);
        IntCurrentDriver := CpyDriver;
        { If this is equal to -1 then no graph mode possible...}
        if CpyMode = -1 then
         begin
           _GraphResult := grNotDetected;
           exit;
         end;
        { Actually set the graph mode...}
        SetGraphMode(CpyMode);
      end
    else
      begin
        { Search if that graphics modec actually exists...}
        if SearchMode(GraphDriver,GraphMode) = nil then
          begin
            _GraphResult := grInvalidMode;
            exit;
         end
        else
         begin
           IntCurrentDriver := GraphDriver;
           SetGraphMode(GraphMode);
         end;
      end;
  end;


var
 ExitSave: pointer;

begin
 ModeList := nil;
 SaveVideoState := nil;
 RestoreVideoState := nil;
 { This must be called at startup... because GetGraphMode may }
 { be called even when not in graph mode.                     }
 QueryAdapterInfo;
 { This installs an exit procedure which cleans up the mode list...}
 ExitSave := ExitProc;
 ExitProc := @CleanMode;
end.


GetDefaultPalette
GetPalette
GetPaletteSize
PieSlice
Sector
SetActivePage
SetAllPalette
SetGraphBufSize
SetBkColor
SetPalette
SetRGBPalette
SetVisualPage
DetectGraph

{   DetectGraph()                                       }
{   GetPalette()                                        }
{   SetAllPalette()                                     }
{   SetPalette()                                        }
{   SetVisualPage()                                     }
{   SetActivePage()                                     }
{   SetBkColor()                                        }





