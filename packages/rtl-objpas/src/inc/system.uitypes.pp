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
    TColorRef   = Type Cardinal;
    PColorRef   = ^TColorRef;
    TAlphaColor = Type Cardinal;
    PAlphaColor = ^TAlphaColor;
    TImageIndex = type Integer;

    TColorRec = record
     class var ColorToRGB: function (Color: TColor): Longint;
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
      // Windows system colors
      SysScrollBar               = TColor($FF000000) platform;
      SysBackground              = TColor($FF000001) platform;
      SysActiveCaption           = TColor($FF000002) platform;
      SysInactiveCaption         = TColor($FF000003) platform;
      SysMenu                    = TColor($FF000004) platform;
      SysWindow                  = TColor($FF000005) platform;
      SysWindowFrame             = TColor($FF000006) platform;
      SysMenuText                = TColor($FF000007) platform;
      SysWindowText              = TColor($FF000008) platform;
      SysCaptionText             = TColor($FF000009) platform;
      SysActiveBorder            = TColor($FF00000A) platform;
      SysInactiveBorder          = TColor($FF00000B) platform;
      SysAppWorkSpace            = TColor($FF00000C) platform;
      SysHighlight               = TColor($FF00000D) platform;
      SysHighlightText           = TColor($FF00000E) platform;
      SysBtnFace                 = TColor($FF00000F) platform;
      SysBtnShadow               = TColor($FF000010) platform;
      SysGrayText                = TColor($FF000011) platform;
      SysBtnText                 = TColor($FF000012) platform;
      SysInactiveCaptionText     = TColor($FF000013) platform;
      SysBtnHighlight            = TColor($FF000014) platform;
      Sys3DDkShadow              = TColor($FF000015) platform;
      Sys3DLight                 = TColor($FF000016) platform;
      SysInfoText                = TColor($FF000017) platform;
      SysInfoBk                  = TColor($FF000018) platform;
      SysHotLight                = TColor($FF00001A) platform;
      SysGradientActiveCaption   = TColor($FF00001B) platform;
      SysGradientInactiveCaption = TColor($FF00001C) platform;
      SysMenuHighlight           = TColor($FF00001D) platform;
      SysMenuBar                 = TColor($FF00001E) platform;
      SysNone                    = TColor($1FFFFFFF) platform;
      Null                       = TColor($00000000);
      SysDefault                 = TColor($20000000) platform;
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
  PAlphaColorRec = ^TAlphaColorRec;


  TAlphaColorF = record
  Public
    R, G, B, A: Single;
  const
    Epsilon = 1.5259E-05; // 1 / 65535, minimal value for TPixelFormat.RGBA16 components

    class function Create(const R, G, B: Single; const A: Single = 1): TAlphaColorF; overload; static; inline;
    class function Create(const aColor: TAlphaColor): TAlphaColorF; overload; static; inline;

    class operator +(const aColor1, aColor2: TAlphaColorF): TAlphaColorF;
    class operator -(const aColor1, aColor2: TAlphaColorF): TAlphaColorF;
    class operator =(const aColor1, aColor2: TAlphaColorF): Boolean;
    class operator <>(const aColor1, aColor2: TAlphaColorF): Boolean;
    class operator -(const aColor: TAlphaColorF): TAlphaColorF;
    class operator *(const aColor1, aColor2: TAlphaColorF): TAlphaColorF;
    class operator *(const aColor: TAlphaColorF; const aFactor: Single): TAlphaColorF;
    class operator *(const aFactor: Single; const aColor: TAlphaColorF): TAlphaColorF; inline;
    class operator /(const aColor: TAlphaColorF; const aFactor: Single): TAlphaColorF; inline;

    function PremultipliedAlpha: TAlphaColorF;
    function UnpremultipliedAlpha: TAlphaColorF;

    function Clamp: TAlphaColorF;
    function ToAlphaColor: TAlphaColor;
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
  mrContinue = mrNone + 12;
  mrTryAgain = mrNone + 13;
  mrLast = mrTryAgain;
  
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
    'mrClose',
    'mrContinue',
    'mrTryAgain');

// CONTROLS
type
  TCloseAction = (caNone, caHide, caFree, caMinimize);
  TCloseActions = set of  TCloseAction;
  
  TMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2);
  TMouseButtons = set of TMouseButton;
  
  TTabOrder = -1..32767;
  
  TDragKind = (dkDrag, dkDock);
  TDragKinds = set of TDragKind;
  
  TDragMode = (dmManual , dmAutomatic);
  TDragModes = set of TDragMode;
  
  TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  TDragStates = set of TDragState;
  
  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop,
                  dmDragCancel,dmFindTarget);
  TDragMessages = set of TDragMessage;

  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;
  TAnchorKinds = TAnchors;

  TAnchorSideReference = (asrTop, asrBottom, asrCenter);
  TAnchorSideReferences = set of TAnchorSideReference;

  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
                scTrack, scTop, scBottom, scEndScroll);
  TScrollCodes = set of TScrollCode;
  
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
  TPrinterOrientations = set of TPrinterOrientation;
  
  TPrinterCapability  = (pcCopies, pcOrientation, pcCollation);
  TPrinterCapabilities= Set of TPrinterCapability;
  
  TPrinterState = (psNoHandle, psHandleIC, psHandleDC);
  TPrinterStates = set of TPrinterState;
  

// Gestures
const
  sgiNoGesture       =  0;
  sgiLeft            =  1;
  sgiRight           =  2;
  sgiUp              =  3;
  sgiDown            =  4;
  sgiUpLeft          =  5;
  sgiUpRight         =  6;
  sgiDownLeft        =  7;
  sgiDownRight       =  8;
  sgiLeftUp          =  9;
  sgiLeftDown        = 10;
  sgiRightUp         = 11;
  sgiRightDown       = 12;
  sgiUpDown          = 13;
  sgiDownUp          = 14;
  sgiLeftRight       = 15;
  sgiRightLeft       = 16;
  sgiUpLeftLong      = 17;
  sgiUpRightLong     = 18;
  sgiDownLeftLong    = 19;
  sgiDownRightLong   = 20;
  sgiScratchout      = 21;
  sgiTriangle        = 22;
  sgiSquare          = 23;
  sgiCheck           = 24;
  sgiCurlicue        = 25;
  sgiDoubleCurlicue  = 26;
  sgiCircle          = 27;
  sgiDoubleCircle    = 28;
  sgiSemiCircleLeft  = 29;
  sgiSemiCircleRight = 30;
  sgiChevronUp       = 31;
  sgiChevronDown     = 32;
  sgiChevronLeft     = 33;
  sgiChevronRight    = 34;
  
  sgiFirst           = sgiLeft;
  sgiLast            = sgiChevronRight;

  // ID range for custom gestures 
 
  cgiFirst = -512;
  cgiLast  = -1;

  // Range for registered custom gestures
  rgiFirst = -1024;
  rgiLast  = -513;

  // Interactive gesture ID range.
  igiFirst = 256;
  igiLast  = 511;

const
  // Interactive gesture IDs
  igiBegin         = igiFirst + 1;
  igiEnd           = igiFirst + 2;
  igiZoom          = igiFirst + 3;
  igiPan           = igiFirst + 4;
  igiRotate        = igiFirst + 5;
  igiTwoFingerTap  = igiFirst + 6;
  igiPressAndTap   = igiFirst + 7;
  igiLongTap       = igiFirst + 8;
  igiDoubleTap     = igiFirst + 9;

// Edit controls

Type
  TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);

  // Forms
  
  TWindowState = (wsNormal, wsMinimized, wsMaximized, wsFullScreen);
  TWindowStates = Set of TWindowState;
  
  TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);
  TBorderIcons = set of TBorderIcon;
    
  // Dialogs
  TOpenOption = (ofReadOnly, ofOverwritePrompt, ofHideReadOnly,
    ofNoChangeDir, ofShowHelp, ofNoValidate, ofAllowMultiSelect,
    ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofCreatePrompt,
    ofShareAware, ofNoReadOnlyReturn, ofNoTestFileCreate, ofNoNetworkButton,
    ofNoLongNames, ofOldStyleDialog, ofNoDereferenceLinks, ofEnableIncludeNotify,
    ofEnableSizing, ofDontAddToRecent, ofForceShowHidden);
  TOpenOptions = set of TOpenOption;

  TOpenOptionEx = (ofExNoPlacesBar);
  TOpenOptionsEx = set of TOpenOptionEx;

  TDialogType = (Standard, Directory);

  TPrintRange = (prAllPages, prSelection, prPageNums);
  TPrintDialogOption = (poPrintToFile, poPageNums, poSelection, poWarning,
    poHelp, poDisablePrintToFile);
  TPrintDialogOptions = set of TPrintDialogOption;

  TPageSetupDialogOption = (psoDefaultMinMargins, psoDisableMargins,
      psoDisableOrientation, psoDisablePagePainting, psoDisablePaper, psoDisablePrinter,
      psoMargins, psoMinMargins, psoShowHelp, psoWarning, psoNoNetworkButton);
    TPageSetupDialogOptions = set of TPageSetupDialogOption;

  TPageMeasureUnits = (pmDefault, pmMillimeters, pmInches);

  TCalDayOfWeek = (dowMonday, dowTuesday, dowWednesday, dowThursday,
    dowFriday, dowSaturday, dowSunday, dowLocaleDefault);

function IsPositiveResult(const AModalResult: TModalResult): Boolean;
function IsNegativeResult(const AModalResult: TModalResult): Boolean;
function IsAbortResult(const AModalResult: TModalResult): Boolean;
function IsAnAllResult(const AModalResult: TModalResult): Boolean;
function StripAllFromResult(const AModalResult: TModalResult): TModalResult;


implementation

function IsPositiveResult(const AModalResult: TModalResult): Boolean;

begin
  Result:=aModalResult in [mrOk,mrYes,mrAll,mrYesToAll,mrContinue]
end;


function IsNegativeResult(const AModalResult: TModalResult): Boolean;

begin
  Result:=aModalResult in [mrNo,mrNoToAll,mrTryAgain]
end;


function IsAbortResult(const AModalResult: TModalResult): Boolean;

begin
   Result:=aModalResult in [mrCancel,mrAbort]
end;


function IsAnAllResult(const AModalResult: TModalResult): Boolean;

begin
  Result:=aModalResult in [mrAll,mrNoToAll,mrYesToAll]
end;


function StripAllFromResult(const AModalResult: TModalResult): TModalResult;

begin
  case aModalResult of
  mrAll:
    Result:=mrOk;
  mrNoToAll:
    Result:=mrNo;
  mrYesToAll: 
    Result:=mrYes;
  else
    Result:=aModalResult;
  end;
end;


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


class function TAlphaColorF.Create(const R, G, B: Single; const A: Single = 1): TAlphaColorF; overload; static; 

begin
  Result.A:=A;
  Result.R:=R;
  Result.G:=G;
  Result.B:=B;
end;


class function TAlphaColorF.Create(const aColor: TAlphaColor): TAlphaColorF; overload; static; 

  function ToSingle(aCom : Byte) : single; inline;
  begin
    Result:=aCom/255;
  end;
  
var
  CR : TAlphaColorRec absolute aColor;  

begin
  Result.A:=ToSingle(CR.A);
  Result.R:=ToSingle(CR.R);
  Result.G:=ToSingle(CR.G);
  Result.B:=ToSingle(CR.B);
end;


class operator TAlphaColorF.+(const aColor1, aColor2: TAlphaColorF): TAlphaColorF;

begin
  With Result do
    begin
    A:=aColor1.A+aColor2.A;
    R:=aColor1.R+aColor2.R;
    G:=aColor1.G+aColor2.G;
    B:=aColor1.B+aColor2.B;
    end;
end;


class operator TAlphaColorF.-(const aColor1, aColor2: TAlphaColorF): TAlphaColorF;

begin
  With Result do
    begin
    A:=aColor1.A-aColor2.A;
    R:=aColor1.R-aColor2.R;
    G:=aColor1.G-aColor2.G;
    B:=aColor1.B-aColor2.B;
    end;
end;


class function Eq(const V1,V2: Single): Boolean;inline;
begin
  Result:=Abs(V1-V2)<=TAlphaColorF.Epsilon;
end;


class operator TAlphaColorF.=(const aColor1, aColor2: TAlphaColorF): Boolean;

begin
  Result:=Eq(aColor1.A,aColor2.A)
          and Eq(aColor1.R,aColor2.R)
          and Eq(aColor1.G,aColor2.G)
          and Eq(aColor1.B,aColor2.B);
end;


class operator TAlphaColorF.<>(const aColor1, aColor2: TAlphaColorF): Boolean;

begin
  Result:=Not (aColor1=aColor2);
end;


class operator TAlphaColorF.-(const aColor: TAlphaColorF): TAlphaColorF;

begin
  With Result do
    begin
    A:=-aColor.A;
    R:=-aColor.R;
    G:=-aColor.G;
    B:=-aColor.B;
    end;
end;


class operator TAlphaColorF.*(const aColor1, aColor2: TAlphaColorF): TAlphaColorF;

begin
  With Result do
    begin
    A:=aColor1.A*aColor2.A;
    R:=aColor1.R*aColor2.R;
    G:=aColor1.G*aColor2.G;
    B:=aColor1.B*aColor2.B;
    end;
end;

class operator TAlphaColorF.*(const aColor: TAlphaColorF; const aFactor: Single): TAlphaColorF;

begin
  With Result do
    begin
    A:=aColor.A*aFactor;
    R:=aColor.R*aFactor;
    G:=aColor.G*aFactor;
    B:=aColor.B*aFactor;
    end;
end;


class operator TAlphaColorF.*(const aFactor: Single; const aColor: TAlphaColorF): TAlphaColorF; 

begin
  With Result do
    begin
    A:=aFactor*aColor.A;
    R:=aFactor*aColor.R;
    G:=aFactor*aColor.G;
    B:=aFactor*aColor.B;
    end;

end;

class operator TAlphaColorF./(const aColor: TAlphaColorF; const aFactor: Single): TAlphaColorF; 

var
  F : Single;

begin
  F:=aFactor;
  if F<Epsilon then 
    F:=1;
  With Result do
    begin
    A:=aColor.A/F;
    R:=aColor.R/F;
    G:=aColor.G/F;
    B:=aColor.B/F;
    end;
end;


function TAlphaColorF.PremultipliedAlpha: TAlphaColorF;

begin
  Result.A:=A;
  Result.R:=A*R;
  Result.G:=A*G;
  Result.B:=A*B;
end;


function TAlphaColorF.UnpremultipliedAlpha: TAlphaColorF;

var
  F : Single;
  
begin
  Result.A:=A;
  if A<Epsilon then
    F:=0
  else if Abs(A-1)<Epsilon then
    F:=1
  else
    F:=1/A;
  Result.R:=F*R;
  Result.G:=F*G;
  Result.B:=F*B;
end;



function TAlphaColorF.Clamp: TAlphaColorF;

  function Limit(C :Single) : Single; inline;
  
  begin
    if C>1 then
      Result:=1
    else if C<0 then
      Result:=0
    else
      Result:=C;
  end;    

begin
  Result.A:=Limit(A);  
  Result.R:=Limit(R);  
  Result.G:=Limit(G);  
  Result.B:=Limit(B);  
end;


function TAlphaColorF.ToAlphaColor: TAlphaColor;

  Function CC(C : Single) : Byte; inline;
  
  begin
    Result:=Round(C*255);
  end;

var
  CR : TAlphaColorRec absolute Result;

begin
  CR.A:=CC(A);
  CR.R:=CC(R);
  CR.G:=CC(G);
  CR.B:=CC(B);
end;

function DefaultColorToRGB(Color: TColor): Longint;
begin
  Result:=Color;
end;

initialization
  TColorRec.ColorToRGB:=@DefaultColorToRGB;
end.
