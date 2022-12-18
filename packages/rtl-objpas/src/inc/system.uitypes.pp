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

end.
