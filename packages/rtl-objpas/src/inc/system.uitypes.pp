{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2019 by Marco van de Voort
        member of the Free Pascal development team.

    Delphi compatibility unit with GUI/imaging related types.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System.UITypes;

{$mode delphi}

// LCL defined all sets with SizeOf()=4
{$PACKSET FIXED}

interface

Type  
    TColor      = -$7FFFFFFF-1..$7FFFFFFF;
    PColor      = ^TColor;
    TColorRef   = Cardinal;
    PColorRef   = ^TColorRef;
    TAlphaColor = Cardinal;
    PAlphaColor = ^TAlphaColor;

    TColorRec = record
                 class operator := (AColor : TColor): TColorRec; inline;
                 class operator := (AColor : TColorRec): TColor; inline;
      const
      // 140 HTML colors.
      AliceBlue          = TColor($FFF8F0);
      AntiqueWhite       = TColor($D7EBFA);
      Aqua               = TColor($FFFF00);
      Aquamarine         = TColor($D4FF7F);
      Azure              = TColor($FFFFF0);
      Beige              = TColor($DCF5F5);
      Bisque             = TColor($C4E4FF);
      Black              = TColor($000000);
      BlanchedAlmond     = TColor($CDEBFF);
      Blue               = TColor($FF0000);
      BlueViolet         = TColor($E22B8A);
      Brown              = TColor($2A2AA5);
      BurlyWood          = TColor($87B8DE);
      CadetBlue          = TColor($A09E5F);
      Chartreuse         = TColor($00FF7F);
      Chocolate          = TColor($1E69D2);
      Coral              = TColor($507FFF);
      CornflowerBlue     = TColor($ED9564);
      Cornsilk           = TColor($DCF8FF);
      Crimson            = TColor($3C14DC);
      Cyan               = TColor($FFFF00);
      DarkBlue           = TColor($8B0000);
      DarkCyan           = TColor($8B8B00);
      DarkGoldenRod      = TColor($0B86B8);
      DarkGray           = TColor($A9A9A9);
      DarkGreen          = TColor($006400);
      DarkGrey           = TColor($A9A9A9);
      DarkKhaki          = TColor($6BB7BD);
      DarkMagenta        = TColor($8B008B);
      DarkOliveGreen     = TColor($2F6B55);
      DarkOrange         = TColor($008CFF);
      DarkOrchid         = TColor($CC3299);
      DarkRed            = TColor($00008B);
      DarkSalmon         = TColor($7A96E9);
      DarkSeaGreen       = TColor($8FBC8F);
      DarkSlateBlue      = TColor($8B3D48);
      DarkSlateGray      = TColor($4F4F2F);
      DarkSlateGrey      = TColor($4F4F2F);
      DarkTurquoise      = TColor($D1CE00);
      DarkViolet         = TColor($D30094);
      DeepPink           = TColor($9314FF);
      DeepSkyBlue        = TColor($FFBF00);
      DimGray            = TColor($696969);
      DimGrey            = TColor($696969);
      DodgerBlue         = TColor($FF901E);
      FireBrick          = TColor($2222B2);
      FloralWhite        = TColor($F0FAFF);
      ForestGreen        = TColor($228B22);
      Fuchsia            = TColor($FF00FF);
      Gainsboro          = TColor($DCDCDC);
      GhostWhite         = TColor($FFF8F8);
      Gold               = TColor($00D7FF);
      GoldenRod          = TColor($20A5DA);
      Gray               = TColor($808080);
      Green              = TColor($008000);
      GreenYellow        = TColor($2FFFAD);
      Grey               = TColor($808080);
      HoneyDew           = TColor($F0FFF0);
      HotPink            = TColor($B469FF);
      IndianRed          = TColor($5C5CCD);
      Indigo             = TColor($82004B);
      Ivory              = TColor($F0FFFF);
      Khaki              = TColor($8CE6F0);
      Lavender           = TColor($FAE6E6);
      LavenderBlush      = TColor($F5F0FF);
      LawnGreen          = TColor($00FC7C);
      LemonChiffon       = TColor($CDFAFF);
      LightBlue          = TColor($E6D8AD);
      LightCoral         = TColor($8080F0);
      LightCyan          = TColor($FFFFE0);
      LightGoldenRodYellow    = TColor($D2FAFA);
      LightGray          = TColor($D3D3D3);
      LightGreen         = TColor($90EE90);
      LightGrey          = TColor($D3D3D3);
      LightPink          = TColor($C1B6FF);
      LightSalmon        = TColor($7AA0FF);
      LightSeaGreen      = TColor($AAB220);
      LightSkyBlue       = TColor($FACE87);
      LightSlateGray     = TColor($998877);
      LightSlateGrey     = TColor($998877);
      LightSteelBlue     = TColor($DEC4B0);
      LightYellow        = TColor($E0FFFF);
      Lime               = TColor($00FF00);
      LimeGreen          = TColor($32CD32);
      Linen              = TColor($E6F0FA);
      Magenta            = TColor($FF00FF);
      Maroon             = TColor($000080);
      MediumAquaMarine   = TColor($AACD66);
      MediumBlue         = TColor($CD0000);
      MediumOrchid       = TColor($D355BA);
      MediumPurple       = TColor($DB7093);
      MediumSeaGreen     = TColor($71B33C);
      MediumSlateBlue    = TColor($EE687B);
      MediumSpringGreen  = TColor($9AFA00);
      MediumTurquoise    = TColor($CCD148);
      MediumVioletRed    = TColor($8515C7);
      MidnightBlue       = TColor($701919);
      MintCream          = TColor($FAFFF5);
      MistyRose          = TColor($E1E4FF);
      Moccasin           = TColor($B5E4FF);
      NavajoWhite        = TColor($ADDEFF);
      Navy               = TColor($800000);
      OldLace            = TColor($E6F5FD);
      Olive              = TColor($008080);
      OliveDrab          = TColor($238E6B);
      Orange             = TColor($00A5FF);
      OrangeRed          = TColor($0045FF);
      Orchid             = TColor($D670DA);
      PaleGoldenRod      = TColor($AAE8EE);
      PaleGreen          = TColor($98FB98);
      PaleTurquoise      = TColor($EEEEAF);
      PaleVioletRed      = TColor($9370DB);
      PapayaWhip         = TColor($D5EFFF);
      PeachPuff          = TColor($B9DAFF);
      Peru               = TColor($3F85CD);
      Pink               = TColor($CBC0FF);
      Plum               = TColor($DDA0DD);
      PowderBlue         = TColor($E6E0B0);
      Purple             = TColor($800080);
      RebeccaPurple      = TColor($993366);
      Red                = TColor($0000FF);
      RosyBrown          = TColor($8F8FBC);
      RoyalBlue          = TColor($E16941);
      SaddleBrown        = TColor($13458B);
      Salmon             = TColor($7280FA);
      SandyBrown         = TColor($60A4F4);
      SeaGreen           = TColor($578B2E);
      SeaShell           = TColor($EEF5FF);
      Sienna             = TColor($2D52A0);
      Silver             = TColor($C0C0C0);
      SkyBlue            = TColor($EBCE87);
      SlateBlue          = TColor($CD5A6A);
      SlateGray          = TColor($908070);
      SlateGrey          = TColor($908070);
      Snow               = TColor($FAFAFF);
      SpringGreen        = TColor($7FFF00);
      SteelBlue          = TColor($B48246);
      Tan                = TColor($8CB4D2);
      Teal               = TColor($808000);
      Thistle            = TColor($D8BFD8);
      Tomato             = TColor($4763FF);
      Turquoise          = TColor($D0E040);
      Violet             = TColor($EE82EE);
      Wheat              = TColor($B3DEF5);
      White              = TColor($FFFFFF);
      WhiteSmoke         = TColor($F5F5F5);
      Yellow             = TColor($00FFFF);
      YellowGreen        = TColor($32CD9A);
      // extended colors (from lazarus Graphics)
      MoneyGreen         = TColor($C0DCC0);
      Cream              = TColor($F0FBFF);
      MedGray            = TColor($A4A0A0);
      // aliases
      LtGray             = TColor($C0C0C0); // clSilver alias
      DkGray             = TColor($808080); // clGray alias
      var
        case Integer of
          0:  {$IFDEF ENDIAN_BIG}
              (A,B,G,R : Byte);
              {$else}
              (R,G,B,A : Byte);
              {$ENDIF}
          1: (Color : TColor);
      end;

      TColors = TColorRec;


  TAlphaColors = record
    const
      Null                 = TAlphaColor(0);
      Alpha                = TAlphaColor($ff000000);
      Black                = Alpha;
      Blue                 = TAlphaColor($ff0000ff);
      Green                = TAlphaColor($ff008000);
      Lime                 = TAlphaColor($ff00ff00);
      Red                  = TAlphaColor($ffff0000);
      White                = TAlphaColor($ffffffff);
      AliceBlue            = TAlphaColor($ffF0F8FF);
      AntiqueWhite         = TAlphaColor($ffFAEBD7);
      Aqua                 = TAlphaColor($ff00FFFF);
      AquaMarine           = TAlphaColor($ff7FFFD4);
      Azure                = TAlphaColor($ffF0FFFF);
      Beige                = TAlphaColor($ffF5F5DC);
      Bisque               = TAlphaColor($ffFFE4C4);
      BlanchedAlmond       = TAlphaColor($ffFFEBCD);
      BlueViolet           = TAlphaColor($ff8A2BE2);
      Brown                = TAlphaColor($ffA52A2A);
      BurlyWood            = TAlphaColor($ffDEB887);
      CadetBlue            = TAlphaColor($ff5F9EA0);
      Chartreuse           = TAlphaColor($ff7FFF00);
      Chocolate            = TAlphaColor($ffD2691E);
      Coral                = TAlphaColor($ffFF7F50);
      CornflowerBlue       = TAlphaColor($ff6495ED);
      CornSilk             = TAlphaColor($ffFFF8DC);
      Crimson              = TAlphaColor($ffDC143C);
      Cyan                 = TAlphaColor($ff00FFFF);
      DarkBlue             = TAlphaColor($ff00008B);
      DarkCyan             = TAlphaColor($ff008B8B);
      DarkGoldenRod        = TAlphaColor($ffB8860B);
      DarkGray             = TAlphaColor($ffA9A9A9);
      DarkGreen            = TAlphaColor($ff006400);
      DarkGrey             = TAlphaColor($ffA9A9A9);
      DarkKhaki            = TAlphaColor($ffBDB76B);
      DarkMagenta          = TAlphaColor($ff8B008B);
      DarkOliveGreen       = TAlphaColor($ff556B2F);
      DarkOrange           = TAlphaColor($ffFF8C00);
      DarkOrchid           = TAlphaColor($ff9932CC);
      DarkRed              = TAlphaColor($ff8B0000);
      DarkSalmon           = TAlphaColor($ffE9967A);
      DarkSeaGreen         = TAlphaColor($ff8FBC8F);
      DarkSlateBlue        = TAlphaColor($ff483D8B);
      DarkSlateGray        = TAlphaColor($ff2F4F4F);
      DarkSlateGrey        = TAlphaColor($ff2F4F4F);
      DarkTurquoise        = TAlphaColor($ff00CED1);
      DarkViolet           = TAlphaColor($ff9400D3);
      DeepPink             = TAlphaColor($ffFF1493);
      DeepSkyBlue          = TAlphaColor($ff00BFFF);
      DimGray              = TAlphaColor($ff696969);
      DimGrey              = TAlphaColor($ff696969);
      DodgerBlue           = TAlphaColor($ff1E90FF);
      Firebrick            = TAlphaColor($ffB22222);
      FloralWhite          = TAlphaColor($ffFFFAF0);
      ForestGreen          = TAlphaColor($ff228B22);
      Fuchsia              = TAlphaColor($ffFF00FF);
      Gainsboro            = TAlphaColor($ffDCDCDC);
      GhostWhite           = TAlphaColor($ffF8F8FF);
      Gold                 = TAlphaColor($ffFFD700);
      GoldenRod            = TAlphaColor($ffDAA520);
      Gray                 = TAlphaColor($ff808080);
      GreenYellow          = TAlphaColor($ffADFF2F);
      Grey                 = TAlphaColor($ff808080);
      HoneyDew             = TAlphaColor($ffF0FFF0);
      HotPink              = TAlphaColor($ffFF69B4);
      IndianRed            = TAlphaColor($ffCD5C5C);
      Indigo               = TAlphaColor($ff4B0082);
      Ivory                = TAlphaColor($ffFFFFF0);
      Khaki                = TAlphaColor($ffF0E68C);
      Lavender             = TAlphaColor($ffE6E6FA);
      LavenderBlush        = TAlphaColor($ffFFF0F5);
      LawnGreen            = TAlphaColor($ff7CFC00);
      LemonChiffon         = TAlphaColor($ffFFFACD);
      LightBlue            = TAlphaColor($ffADD8E6);
      LightCoral           = TAlphaColor($ffF08080);
      LightCyan            = TAlphaColor($ffE0FFFF);
      LightGoldenRodYellow = TAlphaColor($ffFAFAD2);
      LightGray            = TAlphaColor($ffD3D3D3);
      LightGreen           = TAlphaColor($ff90EE90);
      LightGrey            = TAlphaColor($ffD3D3D3);
      LightPink            = TAlphaColor($ffFFB6C1);
      LightSalmon          = TAlphaColor($ffFFA07A);
      LightSeaGreen        = TAlphaColor($ff20B2AA);
      LightSkyBlue         = TAlphaColor($ff87CEFA);
      LightSlateGray       = TAlphaColor($ff778899);
      LightSlateGrey       = TAlphaColor($ff778899);
      LightSteelBlue       = TAlphaColor($ffB0C4DE);
      LightYellow          = TAlphaColor($ffFFFFE0);
      LtGray               = TAlphaColor($ffC0C0C0);
      MedGray              = TAlphaColor($ffA0A0A0);
      DkGray               = TAlphaColor($ff808080);
      MoneyGreen           = TAlphaColor($ffC0DCC0);
      LegacySkyBlue        = TAlphaColor($ffF0CAA6);
      Cream                = TAlphaColor($ffF0FBFF);
      LimeGreen            = TAlphaColor($ff32CD32);
      Linen                = TAlphaColor($ffFAF0E6);
      Magenta              = TAlphaColor($ffFF00FF);
      Maroon               = TAlphaColor($ff800000);
      MediumAquaMarine     = TAlphaColor($ff66CDAA);
      MediumBlue           = TAlphaColor($ff0000CD);
      MediumOrchid         = TAlphaColor($ffBA55D3);
      MediumPurple         = TAlphaColor($ff9370DB);
      MediumSeaGreen       = TAlphaColor($ff3CB371);
      MediumSlateBlue      = TAlphaColor($ff7B68EE);
      MediumSpringGreen    = TAlphaColor($ff00FA9A);
      MediumTurquoise      = TAlphaColor($ff48D1CC);
      MediumVioletRed      = TAlphaColor($ffC71585);
      MidnightBlue         = TAlphaColor($ff191970);
      MintCream            = TAlphaColor($ffF5FFFA);
      MistyRose            = TAlphaColor($ffFFE4E1);
      Moccasin             = TAlphaColor($ffFFE4B5);
      NavajoWhite          = TAlphaColor($ffFFDEAD);
      Navy                 = TAlphaColor($ff000080);
      OldLace              = TAlphaColor($ffFDF5E6);
      Olive                = TAlphaColor($ff808000);
      OliveDrab            = TAlphaColor($ff6B8E23);
      Orange               = TAlphaColor($ffFFA500);
      OrangeRed            = TAlphaColor($ffFF4500);
      Orchid               = TAlphaColor($ffDA70D6);
      PaleGoldenRod        = TAlphaColor($ffEEE8AA);
      PaleGreen            = TAlphaColor($ff98FB98);
      PaleTurquoise        = TAlphaColor($ffAFEEEE);
      PaleVioletRed        = TAlphaColor($ffDB7093);
      PapayaWhip           = TAlphaColor($ffFFEFD5);
      PeachPuff            = TAlphaColor($ffFFDAB9);
      Peru                 = TAlphaColor($ffCD853F);
      Pink                 = TAlphaColor($ffFFC0CB);
      Plum                 = TAlphaColor($ffDDA0DD);
      PowderBlue           = TAlphaColor($ffB0E0E6);
      Purple               = TAlphaColor($ff800080);
      RosyBrown            = TAlphaColor($ffBC8F8F);
      RoyalBlue            = TAlphaColor($ff4169E1);
      SaddleBrown          = TAlphaColor($ff8B4513);
      Salmon               = TAlphaColor($ffFA8072);
      SandyBrown           = TAlphaColor($ffF4A460);
      SeaGreen             = TAlphaColor($ff2E8B57);
      SeaShell             = TAlphaColor($ffFFF5EE);
      Sienna               = TAlphaColor($ffA0522D);
      Silver               = TAlphaColor($ffC0C0C0);
      SkyBlue              = TAlphaColor($ff87CEEB);
      SlateBlue            = TAlphaColor($ff6A5ACD);
      SlateGray            = TAlphaColor($ff708090);
      SlateGrey            = TAlphaColor($ff708090);
      Snow                 = TAlphaColor($ffFFFAFA);
      SpringGreen          = TAlphaColor($ff00FF7F);
      SteelBlue            = TAlphaColor($ff4682B4);
      Tan                  = TAlphaColor($ffD2B48C);
      Teal                 = TAlphaColor($ff008080);
      Thistle              = TAlphaColor($ffD8BFD8);
      Tomato               = TAlphaColor($ffFF6347);
      Turquoise            = TAlphaColor($ff40E0D0);
      Violet               = TAlphaColor($ffEE82EE);
      Wheat                = TAlphaColor($ffF5DEB3);
      WhiteSmoke           = TAlphaColor($ffF5F5F5);
      Yellow               = TAlphaColor($ffFFFF00);
      YellowGreen          = TAlphaColor($ff9ACD32);
  public
    constructor Create(const Color: TAlphaColor);
    class var ColorToRGB: function (Color: TAlphaColor): Longint;
    case Cardinal of
          0:
            (Color: TAlphaColor);
          2:
            (HiWord, LoWord: Word);
          3:
    {$IFDEF BIGENDIAN}
            (A, R, G, B: Byte);
    {$ELSE}
            (B, G, R, A: Byte);
    {$ENDIF}
  end;
  TAlphaColorRec = TAlphaColors;

  TAlphaColorF = record
    R, G, B, A: Single;
  end;


// copied from Lazutils version
///////////////////////////////

type
  // Message dialog related
  TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;

  // ModalResult
  TModalResult = low(Integer)..high(Integer);
  PModalResult = ^TModalResult;

const
  // Used for ModalResult
  mrNone = 0;
  mrOK = mrNone + 1;
  mrCancel = mrNone + 2;
  mrAbort = mrNone + 3;
  mrRetry = mrNone + 4;
  mrIgnore = mrNone + 5;
  mrYes = mrNone + 6;
  mrNo = mrNone + 7;
  mrAll = mrNone + 8;
  mrNoToAll = mrNone + 9;
  mrYesToAll = mrNone + 10;
  mrClose = mrNone + 11;
  mrLast = mrClose;

  // String representation of ModalResult values
  ModalResultStr: array[mrNone..mrLast] of shortstring = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll',
    'mrClose');

// CONTROLS
type
  TCloseAction = (caNone, caHide, caFree, caMinimize);
  TMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2);
  TTabOrder = -1..32767;
  TDragKind = (dkDrag, dkDock);
  TDragMode = (dmManual , dmAutomatic);
  TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop,
                  dmDragCancel,dmFindTarget);

  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;
  TAnchorSideReference = (asrTop, asrBottom, asrCenter);

  TCursor = -32768..32767;

const
  // Cursor constants
  crHigh        = TCursor(0);
  crDefault     = TCursor(0);
  crNone        = TCursor(-1);
  crArrow       = TCursor(-2);
  crCross       = TCursor(-3);
  crIBeam       = TCursor(-4);
  crSize        = TCursor(-22);
  crSizeNESW    = TCursor(-6); // diagonal north east - south west
  crSizeNS      = TCursor(-7);
  crSizeNWSE    = TCursor(-8);
  crSizeWE      = TCursor(-9);
  crSizeNW      = TCursor(-23);
  crSizeN       = TCursor(-24);
  crSizeNE      = TCursor(-25);
  crSizeW       = TCursor(-26);
  crSizeE       = TCursor(-27);
  crSizeSW      = TCursor(-28);
  crSizeS       = TCursor(-29);
  crSizeSE      = TCursor(-30);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
  crSQLWait     = TCursor(-17);
  crNo          = TCursor(-18);
  crAppStart    = TCursor(-19);
  crHelp        = TCursor(-20);
  crHandPoint   = TCursor(-21);
  crSizeAll     = TCursor(-22);
  crLow         = TCursor(-30);


  // font types&styles
  LF_FULLFACESIZE = 64;
  LF_FACESIZE = 32;

type
  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = string;
  TFontDataName = string[LF_FACESIZE -1];
  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;
  TFontStylesBase = set of TFontStyle;
  TFontCharSet = 0..255;
  TFontQuality = (fqDefault, fqDraft, fqProof, fqNonAntialiased, fqAntialiased,
    fqCleartype, fqCleartypeNatural);

// PRINTERS
  TPrinterOrientation = (poPortrait,poLandscape,poReverseLandscape,poReversePortrait);
  TPrinterCapability  = (pcCopies, pcOrientation, pcCollation);
  TPrinterCapabilities= Set of TPrinterCapability;

implementation

class operator TColorRec.:= (AColor : TColor): TColorRec;
begin
  result.Color:=AColor;
end;

class operator TColorRec.:= (AColor : TColorRec): TColor;
begin
  result:=AColor.Color;
end;

constructor TAlphaColors.Create(const Color: TAlphaColor);
begin
  Self := TAlphaColors(Color);
end;


end.
