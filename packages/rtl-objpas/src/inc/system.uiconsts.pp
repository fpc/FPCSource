unit System.UIConsts;
{
   This file is part of the Free Pascal run time library.
    Copyright (c) 2023 the Free Pascal development team

   FPC/Lazarus Replacement for UIConsts from Delphi 10.x

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{$MODE OBJFPC}
{$H+}
{$R-}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.UITypes, System.Classes;
{$ELSE}
uses System.UITypes, Classes;
{$ENDIF}

const
  MaxColorChannel = $FF;

const
  claAliceblue = TAlphaColors.AliceBlue;
  claAntiquewhite = TAlphaColors.Antiquewhite;
  claAqua = TAlphaColors.Aqua;
  claAquamarine = TAlphaColors.Aquamarine;
  claAzure = TAlphaColors.Azure;
  claBeige = TAlphaColors.Beige;
  claBisque = TAlphaColors.Bisque;
  claBlack = TAlphaColors.Black;
  claBlanchedalmond = TAlphaColors.Blanchedalmond;
  claBlue = TAlphaColors.Blue;
  claBlueviolet = TAlphaColors.Blueviolet;
  claBrown = TAlphaColors.Brown;
  claBurlywood = TAlphaColors.Burlywood;
  claCadetblue = TAlphaColors.Cadetblue;
  claChartreuse = TAlphaColors.Chartreuse;
  claChocolate = TAlphaColors.Chocolate;
  claCoral = TAlphaColors.Coral;
  claCornflowerblue = TAlphaColors.Cornflowerblue;
  claCornsilk = TAlphaColors.Cornsilk;
  claCrimson = TAlphaColors.Crimson;
  claCyan = TAlphaColors.Cyan;
  claDarkblue = TAlphaColors.Darkblue;
  claDarkcyan = TAlphaColors.Darkcyan;
  claDarkgoldenrod = TAlphaColors.Darkgoldenrod;
  claDarkgray = TAlphaColors.Darkgray;
  claDarkgreen = TAlphaColors.Darkgreen;
  claDarkgrey = TAlphaColors.Darkgrey;
  claDarkkhaki = TAlphaColors.Darkkhaki;
  claDarkmagenta = TAlphaColors.Darkmagenta;
  claDarkolivegreen = TAlphaColors.Darkolivegreen;
  claDarkorange = TAlphaColors.Darkorange;
  claDarkorchid = TAlphaColors.Darkorchid;
  claDarkred = TAlphaColors.Darkred;
  claDarksalmon = TAlphaColors.Darksalmon;
  claDarkseagreen = TAlphaColors.Darkseagreen;
  claDarkslateblue = TAlphaColors.Darkslateblue;
  claDarkslategray = TAlphaColors.Darkslategray;
  claDarkslategrey = TAlphaColors.Darkslategrey;
  claDarkturquoise = TAlphaColors.Darkturquoise;
  claDarkviolet = TAlphaColors.Darkviolet;
  claDeeppink = TAlphaColors.Deeppink;
  claDeepskyblue = TAlphaColors.Deepskyblue;
  claDimgray = TAlphaColors.Dimgray;
  claDimgrey = TAlphaColors.Dimgrey;
  claDodgerblue = TAlphaColors.Dodgerblue;
  claFirebrick = TAlphaColors.Firebrick;
  claFloralwhite = TAlphaColors.Floralwhite;
  claForestgreen = TAlphaColors.Forestgreen;
  claFuchsia = TAlphaColors.Fuchsia;
  claGainsboro = TAlphaColors.Gainsboro;
  claGhostwhite = TAlphaColors.Ghostwhite;
  claGold = TAlphaColors.Gold;
  claGoldenrod = TAlphaColors.Goldenrod;
  claGray = TAlphaColors.Gray;
  claGreen = TAlphaColors.Green;
  claGreenyellow = TAlphaColors.Greenyellow;
  claGrey = TAlphaColors.Grey;
  claHoneydew = TAlphaColors.Honeydew;
  claHotpink = TAlphaColors.Hotpink;
  claIndianred = TAlphaColors.Indianred;
  claIndigo = TAlphaColors.Indigo;
  claIvory = TAlphaColors.Ivory;
  claKhaki = TAlphaColors.Khaki;
  claLavender = TAlphaColors.Lavender;
  claLavenderblush = TAlphaColors.Lavenderblush;
  claLawngreen = TAlphaColors.Lawngreen;
  claLemonchiffon = TAlphaColors.Lemonchiffon;
  claLightblue = TAlphaColors.Lightblue;
  claLightcoral = TAlphaColors.Lightcoral;
  claLightcyan = TAlphaColors.Lightcyan;
  claLightgoldenrodyellow = TAlphaColors.Lightgoldenrodyellow;
  claLightgray = TAlphaColors.Lightgray;
  claLightgreen = TAlphaColors.Lightgreen;
  claLightgrey = TAlphaColors.Lightgrey;
  claLightpink = TAlphaColors.Lightpink;
  claLightsalmon = TAlphaColors.Lightsalmon;
  claLightseagreen = TAlphaColors.Lightseagreen;
  claLightskyblue = TAlphaColors.Lightskyblue;
  claLightslategray = TAlphaColors.Lightslategray;
  claLightslategrey = TAlphaColors.Lightslategrey;
  claLightsteelblue = TAlphaColors.Lightsteelblue;
  claLightyellow = TAlphaColors.Lightyellow;
  claLime = TAlphaColors.Lime;
  claLimegreen = TAlphaColors.Limegreen;
  claLinen = TAlphaColors.Linen;
  claMagenta = TAlphaColors.Magenta;
  claMaroon = TAlphaColors.Maroon;
  claMediumaquamarine = TAlphaColors.Mediumaquamarine;
  claMediumblue = TAlphaColors.Mediumblue;
  claMediumorchid = TAlphaColors.Mediumorchid;
  claMediumpurple = TAlphaColors.Mediumpurple;
  claMediumseagreen = TAlphaColors.Mediumseagreen;
  claMediumslateblue = TAlphaColors.Mediumslateblue;
  claMediumspringgreen = TAlphaColors.Mediumspringgreen;
  claMediumturquoise = TAlphaColors.Mediumturquoise;
  claMediumvioletred = TAlphaColors.Mediumvioletred;
  claMidnightblue = TAlphaColors.Midnightblue;
  claMintcream = TAlphaColors.Mintcream;
  claMistyrose = TAlphaColors.Mistyrose;
  claMoccasin = TAlphaColors.Moccasin;
  claNavajowhite = TAlphaColors.Navajowhite;
  claNavy = TAlphaColors.Navy;
  claOldlace = TAlphaColors.Oldlace;
  claOlive = TAlphaColors.Olive;
  claOlivedrab = TAlphaColors.Olivedrab;
  claOrange = TAlphaColors.Orange;
  claOrangered = TAlphaColors.Orangered;
  claOrchid = TAlphaColors.Orchid;
  claPalegoldenrod = TAlphaColors.Palegoldenrod;
  claPalegreen = TAlphaColors.Palegreen;
  claPaleturquoise = TAlphaColors.Paleturquoise;
  claPalevioletred = TAlphaColors.Palevioletred;
  claPapayawhip = TAlphaColors.Papayawhip;
  claPeachpuff = TAlphaColors.Peachpuff;
  claPeru = TAlphaColors.Peru;
  claPink = TAlphaColors.Pink;
  claPlum = TAlphaColors.Plum;
  claPowderblue = TAlphaColors.Powderblue;
  claPurple = TAlphaColors.Purple;
  claRed = TAlphaColors.Red;
  claRosybrown = TAlphaColors.Rosybrown;
  claRoyalblue = TAlphaColors.Royalblue;
  claSaddlebrown = TAlphaColors.Saddlebrown;
  claSalmon = TAlphaColors.Salmon;
  claSandybrown = TAlphaColors.Sandybrown;
  claSeagreen = TAlphaColors.Seagreen;
  claSeashell = TAlphaColors.Seashell;
  claSienna = TAlphaColors.Sienna;
  claSilver = TAlphaColors.Silver;
  claSkyblue = TAlphaColors.Skyblue;
  claSlateblue = TAlphaColors.Slateblue;
  claSlategray = TAlphaColors.Slategray;
  claSlategrey = TAlphaColors.Slategrey;
  claSnow = TAlphaColors.Snow;
  claSpringgreen = TAlphaColors.Springgreen;
  claSteelblue = TAlphaColors.Steelblue;
  claTan = TAlphaColors.Tan;
  claTeal = TAlphaColors.Teal;
  claThistle = TAlphaColors.Thistle;
  claTomato = TAlphaColors.Tomato;
  claTurquoise = TAlphaColors.Turquoise;
  claViolet = TAlphaColors.Violet;
  claWheat = TAlphaColors.Wheat;
  claWhite = TAlphaColors.White;
  claWhitesmoke = TAlphaColors.Whitesmoke;
  claYellow = TAlphaColors.Yellow;
  claYellowgreen = TAlphaColors.Yellowgreen;
  claNull = TAlphaColors.Null;

{ Cursor string functions }

function CursorToString(Cursor: TCursor): string;
function StringToCursor(const S: string): TCursor;
procedure GetCursorValues(const Proc: TGetStrProc);
function CursorToIdent(Cursor: LongInt; var Ident: string): Boolean; inline;
function IdentToCursor(const Ident: string; var Cursor: LongInt): Boolean; inline;
procedure RegisterCursorIntegerConsts;

{ TColor string functions }

function ColorToString(Color: TColor): string;
function StringToColor(const S: string): TColor;
procedure GetColorValues(Proc: TGetStrProc);
function ColorToIdent(Color: Longint; var Ident: string): Boolean; inline;
function IdentToColor(const Ident: string; var Color: LongInt): Boolean; inline;
procedure RegisterColorIntegerConsts;

{ TAlphaColor string functions }

procedure GetAlphaColorValues(Proc: TGetStrProc);
function AlphaColorToString(Value: TAlphaColor): string;
function StringToAlphaColor(const Value: string): TAlphaColor;
function AlphaColorToIdent(Color: LongInt; var Ident: string): Boolean;
function IdentToAlphaColor(const Ident: string; var Color: Longint): Boolean;
procedure RegisterAlphaColorIntegerConsts;

{ TAlphaColor function's }

/// <summary>Converts TAlphaColor into TColor structure, exchanging red and blue channels while losing alpha channel. </summary>
function AlphaColorToColor(const Color: TAlphaColor): TColor;
function AppendColor(Start, Stop: TAlphaColor): TAlphaColor;
function SubtractColor(Start, Stop: TAlphaColor): TAlphaColor;
function RGBtoBGR(const C: TAlphaColor): TAlphaColor;
function CorrectColor(const C: TAlphaColor): TAlphaColor;
function PremultiplyAlpha(const C: TAlphaColor): TAlphaColor;
function UnpremultiplyAlpha(const C: TAlphaColor): TAlphaColor;
function MakeColor(R, G, B: Byte; A: Byte = MaxColorChannel): TAlphaColor; overload;
function MakeColor(const C: TAlphaColor; const AOpacity: Single): TAlphaColor; overload;
function HSLtoRGB(H, S, L: Single): TAlphaColor;
procedure RGBtoHSL(RGB: TAlphaColor; out H, S, L: Single);
function ChangeHSL(const C: TAlphaColor; dH, dS, dL: Single): TAlphaColor;

const

  // Please keep these sorted.
  CursorNames: array[0..30] of TIdentMapEntry = (
    (Value: crAppStart;   Name: 'crAppStart'),
    (Value: crArrow;      Name: 'crArrow'),
    (Value: crCross;      Name: 'crCross'),
    (Value: crDefault;    Name: 'crDefault'),
    (Value: crDrag;       Name: 'crDrag'),
    (Value: crHandPoint;  Name: 'crHandPoint'),
    (Value: crHelp;       Name: 'crHelp'),
    (Value: crHourGlass;  Name: 'crHourGlass'),
    (Value: crHSplit;     Name: 'crHSplit'),
    (Value: crIBeam;      Name: 'crIBeam'),
    (Value: crMultiDrag;  Name: 'crMultiDrag'),
    (Value: crNoDrop;     Name: 'crNoDrop'),
    (Value: crNo;         Name: 'crNo'),
    (Value: crSizeAll;    Name: 'crSizeAll'),
    (Value: crSizeE;      Name: 'crSizeE'),
    (Value: crSizeNE;     Name: 'crSizeNE'),
    (Value: crSizeNESW;   Name: 'crSizeNESW'),
    (Value: crSizeN;      Name: 'crSizeN'),
    (Value: crSizeNS;     Name: 'crSizeNS'),
    (Value: crSizeNW;     Name: 'crSizeNW'),
    (Value: crSizeNWSE;   Name: 'crSizeNWSE'),
    (Value: crSizeSE;     Name: 'crSizeSE'),
    (Value: crSizeS;      Name: 'crSizeS'),
    (Value: crSizeSW;     Name: 'crSizeSW'),
    (Value: crSizeWE;     Name: 'crSizeWE'),
    (Value: crSizeW;      Name: 'crSizeW'),
    (Value: crSQLWait;    Name: 'crSQLWait'),
    (Value: crUpArrow;    Name: 'crUpArrow'),
    (Value: crVSplit;     Name: 'crVSplit'),
    // These must be last, duplicates!
    (Value: crSize;       Name: 'crSize'),
    (Value: crLow;        Name: 'crLow') 
  );    

  // Please keep these sorted.
  ColorNames: array[0..51] of TIdentMapEntry = (
    (Value: TColors.Aqua;                       Name: 'clAqua'),
    (Value: TColors.Black;                      Name: 'clBlack'),
    (Value: TColors.Blue;                       Name: 'clBlue'),
    (Value: TColors.Cream;                      Name: 'clCream'),
    (Value: TColors.Fuchsia;                    Name: 'clFuchsia'),
    (Value: TColors.Gray;                       Name: 'clGray'),
    (Value: TColors.Green;                      Name: 'clGreen'),
    (Value: TColors.Lime;                       Name: 'clLime'),
    (Value: TColors.Maroon;                     Name: 'clMaroon'),
    (Value: TColors.MedGray;                    Name: 'clMedGray'),
    (Value: TColors.MoneyGreen;                 Name: 'clMoneyGreen'),
    (Value: TColors.Navy;                       Name: 'clNavy'),
    (Value: TColors.Olive;                      Name: 'clOlive'),
    (Value: TColors.Purple;                     Name: 'clPurple'),
    (Value: TColors.Red;                        Name: 'clRed'),
    (Value: TColors.Silver;                     Name: 'clSilver'),
    (Value: TColors.SkyBlue;                    Name: 'clSkyBlue'),
    (Value: TColors.Sys3DDkShadow;              Name: 'cl3DDkShadow'),
    (Value: TColors.Sys3DLight;                 Name: 'cl3DLight'),
    (Value: TColors.SysActiveBorder;            Name: 'clActiveBorder'),
    (Value: TColors.SysActiveCaption;           Name: 'clActiveCaption'),
    (Value: TColors.SysAppWorkSpace;            Name: 'clAppWorkSpace'),
    (Value: TColors.SysBackground;              Name: 'clBackground'),
    (Value: TColors.SysBtnFace;                 Name: 'clBtnFace'),
    (Value: TColors.SysBtnHighlight;            Name: 'clBtnHighlight'),
    (Value: TColors.SysBtnShadow;               Name: 'clBtnShadow'),
    (Value: TColors.SysBtnText;                 Name: 'clBtnText'),
    (Value: TColors.SysCaptionText;             Name: 'clCaptionText'),
    (Value: TColors.SysDefault;                 Name: 'clDefault'),
    (Value: TColors.SysGradientActiveCaption;   Name: 'clGradientActiveCaption'),
    (Value: TColors.SysGradientInactiveCaption; Name: 'clGradientInactiveCaption'),
    (Value: TColors.SysGrayText;                Name: 'clGrayText'),
    (Value: TColors.SysHighlight;               Name: 'clHighlight'),
    (Value: TColors.SysHighlightText;           Name: 'clHighlightText'),
    (Value: TColors.SysHotLight;                Name: 'clHotLight'),
    (Value: TColors.SysInactiveBorder;          Name: 'clInactiveBorder'),
    (Value: TColors.SysInactiveCaption;         Name: 'clInactiveCaption'),
    (Value: TColors.SysInactiveCaptionText;     Name: 'clInactiveCaptionText'),
    (Value: TColors.SysInfoBk;                  Name: 'clInfoBk'),
    (Value: TColors.SysInfoText;                Name: 'clInfoText'),
    (Value: TColors.SysMenuBar;                 Name: 'clMenuBar'),
    (Value: TColors.SysMenuHighlight;           Name: 'clMenuHighlight'),
    (Value: TColors.SysMenu;                    Name: 'clMenu'),
    (Value: TColors.SysMenuText;                Name: 'clMenuText'),
    (Value: TColors.SysNone;                    Name: 'clNone'),
    (Value: TColors.SysScrollBar;               Name: 'clScrollBar'),
    (Value: TColors.SysWindowFrame;             Name: 'clWindowFrame'),
    (Value: TColors.SysWindow;                  Name: 'clWindow'),
    (Value: TColors.SysWindowText;              Name: 'clWindowText'),
    (Value: TColors.Teal;                       Name: 'clTeal'),
    (Value: TColors.White;                      Name: 'clWhite'),
    (Value: TColors.Yellow;                     Name: 'clYellow')
  );

  AlphaColorNames: array [0..154] of TIdentMapEntry = (
      (Value: TAlphaColors.AliceBlue; Name: 'claAliceBlue'),
      (Value: TAlphaColors.Alpha; Name: 'claAlpha'),
      (Value: TAlphaColors.AntiqueWhite; Name: 'claAntiqueWhite'),
      (Value: TAlphaColors.AquaMarine; Name: 'claAquaMarine'),
      (Value: TAlphaColors.Aqua; Name: 'claAqua'),
      (Value: TAlphaColors.Azure; Name: 'claAzure'),
      (Value: TAlphaColors.Beige; Name: 'claBeige'),
      (Value: TAlphaColors.Bisque; Name: 'claBisque'),
      (Value: TAlphaColors.Black; Name: 'claBlack'),
      (Value: TAlphaColors.BlanchedAlmond; Name: 'claBlanchedAlmond'),
      (Value: TAlphaColors.Blue; Name: 'claBlue'),
      (Value: TAlphaColors.BlueViolet; Name: 'claBlueViolet'),
      (Value: TAlphaColors.Brown; Name: 'claBrown'),
      (Value: TAlphaColors.BurlyWood; Name: 'claBurlyWood'),
      (Value: TAlphaColors.CadetBlue; Name: 'claCadetBlue'),
      (Value: TAlphaColors.Chartreuse; Name: 'claChartreuse'),
      (Value: TAlphaColors.Chocolate; Name: 'claChocolate'),
      (Value: TAlphaColors.Coral; Name: 'claCoral'),
      (Value: TAlphaColors.CornflowerBlue; Name: 'claCornflowerBlue'),
      (Value: TAlphaColors.CornSilk; Name: 'claCornSilk'),
      (Value: TAlphaColors.Cream; Name: 'claCream'),
      (Value: TAlphaColors.Crimson; Name: 'claCrimson'),
      (Value: TAlphaColors.Cyan; Name: 'claCyan'),
      (Value: TAlphaColors.DarkBlue; Name: 'claDarkBlue'),
      (Value: TAlphaColors.DarkCyan; Name: 'claDarkCyan'),
      (Value: TAlphaColors.DarkGoldenRod; Name: 'claDarkGoldenRod'),
      (Value: TAlphaColors.DarkGray; Name: 'claDarkGray'),
      (Value: TAlphaColors.DarkGreen; Name: 'claDarkGreen'),
      (Value: TAlphaColors.DarkGrey; Name: 'claDarkGrey'),
      (Value: TAlphaColors.DarkKhaki; Name: 'claDarkKhaki'),
      (Value: TAlphaColors.DarkMagenta; Name: 'claDarkMagenta'),
      (Value: TAlphaColors.DarkOliveGreen; Name: 'claDarkOliveGreen'),
      (Value: TAlphaColors.DarkOrange; Name: 'claDarkOrange'),
      (Value: TAlphaColors.DarkOrchid; Name: 'claDarkOrchid'),
      (Value: TAlphaColors.DarkRed; Name: 'claDarkRed'),
      (Value: TAlphaColors.DarkSalmon; Name: 'claDarkSalmon'),
      (Value: TAlphaColors.DarkSeaGreen; Name: 'claDarkSeaGreen'),
      (Value: TAlphaColors.DarkSlateBlue; Name: 'claDarkSlateBlue'),
      (Value: TAlphaColors.DarkSlateGray; Name: 'claDarkSlateGray'),
      (Value: TAlphaColors.DarkSlateGrey; Name: 'claDarkSlateGrey'),
      (Value: TAlphaColors.DarkTurquoise; Name: 'claDarkTurquoise'),
      (Value: TAlphaColors.DarkViolet; Name: 'claDarkViolet'),
      (Value: TAlphaColors.DeepPink; Name: 'claDeepPink'),
      (Value: TAlphaColors.DeepSkyBlue; Name: 'claDeepSkyBlue'),
      (Value: TAlphaColors.DimGray; Name: 'claDimGray'),
      (Value: TAlphaColors.DimGrey; Name: 'claDimGrey'),
      (Value: TAlphaColors.DkGray; Name: 'claDkGray'),
      (Value: TAlphaColors.DodgerBlue; Name: 'claDodgerBlue'),
      (Value: TAlphaColors.Firebrick; Name: 'claFirebrick'),
      (Value: TAlphaColors.FloralWhite; Name: 'claFloralWhite'),
      (Value: TAlphaColors.ForestGreen; Name: 'claForestGreen'),
      (Value: TAlphaColors.Fuchsia; Name: 'claFuchsia'),
      (Value: TAlphaColors.Gainsboro; Name: 'claGainsboro'),
      (Value: TAlphaColors.GhostWhite; Name: 'claGhostWhite'),
      (Value: TAlphaColors.GoldenRod; Name: 'claGoldenRod'),
      (Value: TAlphaColors.Gold; Name: 'claGold'),
      (Value: TAlphaColors.Gray; Name: 'claGray'),
      (Value: TAlphaColors.Green; Name: 'claGreen'),
      (Value: TAlphaColors.GreenYellow; Name: 'claGreenYellow'),
      (Value: TAlphaColors.Grey; Name: 'claGrey'),
      (Value: TAlphaColors.HoneyDew; Name: 'claHoneyDew'),
      (Value: TAlphaColors.HotPink; Name: 'claHotPink'),
      (Value: TAlphaColors.IndianRed; Name: 'claIndianRed'),
      (Value: TAlphaColors.Indigo; Name: 'claIndigo'),
      (Value: TAlphaColors.Ivory; Name: 'claIvory'),
      (Value: TAlphaColors.Khaki; Name: 'claKhaki'),
      (Value: TAlphaColors.LavenderBlush; Name: 'claLavenderBlush'),
      (Value: TAlphaColors.Lavender; Name: 'claLavender'),
      (Value: TAlphaColors.LawnGreen; Name: 'claLawnGreen'),
      (Value: TAlphaColors.LegacySkyBlue; Name: 'claLegacySkyBlue'),
      (Value: TAlphaColors.LemonChiffon; Name: 'claLemonChiffon'),
      (Value: TAlphaColors.LightBlue; Name: 'claLightBlue'),
      (Value: TAlphaColors.LightCoral; Name: 'claLightCoral'),
      (Value: TAlphaColors.LightCyan; Name: 'claLightCyan'),
      (Value: TAlphaColors.LightGoldenRodYellow; Name: 'claLightGoldenRodYellow'),
      (Value: TAlphaColors.LightGray; Name: 'claLightGray'),
      (Value: TAlphaColors.LightGreen; Name: 'claLightGreen'),
      (Value: TAlphaColors.LightGrey; Name: 'claLightGrey'),
      (Value: TAlphaColors.LightPink; Name: 'claLightPink'),
      (Value: TAlphaColors.LightSalmon; Name: 'claLightSalmon'),
      (Value: TAlphaColors.LightSeaGreen; Name: 'claLightSeaGreen'),
      (Value: TAlphaColors.LightSkyBlue; Name: 'claLightSkyBlue'),
      (Value: TAlphaColors.LightSlateGray; Name: 'claLightSlateGray'),
      (Value: TAlphaColors.LightSlateGrey; Name: 'claLightSlateGrey'),
      (Value: TAlphaColors.LightSteelBlue; Name: 'claLightSteelBlue'),
      (Value: TAlphaColors.LightYellow; Name: 'claLightYellow'),
      (Value: TAlphaColors.LimeGreen; Name: 'claLimeGreen'),
      (Value: TAlphaColors.Lime; Name: 'claLime'),
      (Value: TAlphaColors.Linen; Name: 'claLinen'),
      (Value: TAlphaColors.LtGray; Name: 'claLtGray'),
      (Value: TAlphaColors.Magenta; Name: 'claMagenta'),
      (Value: TAlphaColors.Maroon; Name: 'claMaroon'),
      (Value: TAlphaColors.MedGray; Name: 'claMedGray'),
      (Value: TAlphaColors.MediumAquaMarine; Name: 'claMediumAquaMarine'),
      (Value: TAlphaColors.MediumBlue; Name: 'claMediumBlue'),
      (Value: TAlphaColors.MediumOrchid; Name: 'claMediumOrchid'),
      (Value: TAlphaColors.MediumPurple; Name: 'claMediumPurple'),
      (Value: TAlphaColors.MediumSeaGreen; Name: 'claMediumSeaGreen'),
      (Value: TAlphaColors.MediumSlateBlue; Name: 'claMediumSlateBlue'),
      (Value: TAlphaColors.MediumSpringGreen; Name: 'claMediumSpringGreen'),
      (Value: TAlphaColors.MediumTurquoise; Name: 'claMediumTurquoise'),
      (Value: TAlphaColors.MediumVioletRed; Name: 'claMediumVioletRed'),
      (Value: TAlphaColors.MidnightBlue; Name: 'claMidnightBlue'),
      (Value: TAlphaColors.MintCream; Name: 'claMintCream'),
      (Value: TAlphaColors.MistyRose; Name: 'claMistyRose'),
      (Value: TAlphaColors.Moccasin; Name: 'claMoccasin'),
      (Value: TAlphaColors.MoneyGreen; Name: 'claMoneyGreen'),
      (Value: TAlphaColors.NavajoWhite; Name: 'claNavajoWhite'),
      (Value: TAlphaColors.Navy; Name: 'claNavy'),
      (Value: TAlphaColors.Null; Name: 'claNull'),
      (Value: TAlphaColors.OldLace; Name: 'claOldLace'),
      (Value: TAlphaColors.OliveDrab; Name: 'claOliveDrab'),
      (Value: TAlphaColors.Olive; Name: 'claOlive'),
      (Value: TAlphaColors.Orange; Name: 'claOrange'),
      (Value: TAlphaColors.OrangeRed; Name: 'claOrangeRed'),
      (Value: TAlphaColors.Orchid; Name: 'claOrchid'),
      (Value: TAlphaColors.PaleGoldenRod; Name: 'claPaleGoldenRod'),
      (Value: TAlphaColors.PaleGreen; Name: 'claPaleGreen'),
      (Value: TAlphaColors.PaleTurquoise; Name: 'claPaleTurquoise'),
      (Value: TAlphaColors.PaleVioletRed; Name: 'claPaleVioletRed'),
      (Value: TAlphaColors.PapayaWhip; Name: 'claPapayaWhip'),
      (Value: TAlphaColors.PeachPuff; Name: 'claPeachPuff'),
      (Value: TAlphaColors.Peru; Name: 'claPeru'),
      (Value: TAlphaColors.Pink; Name: 'claPink'),
      (Value: TAlphaColors.Plum; Name: 'claPlum'),
      (Value: TAlphaColors.PowderBlue; Name: 'claPowderBlue'),
      (Value: TAlphaColors.Purple; Name: 'claPurple'),
      (Value: TAlphaColors.Red; Name: 'claRed'),
      (Value: TAlphaColors.RosyBrown; Name: 'claRosyBrown'),
      (Value: TAlphaColors.RoyalBlue; Name: 'claRoyalBlue'),
      (Value: TAlphaColors.SaddleBrown; Name: 'claSaddleBrown'),
      (Value: TAlphaColors.Salmon; Name: 'claSalmon'),
      (Value: TAlphaColors.SandyBrown; Name: 'claSandyBrown'),
      (Value: TAlphaColors.SeaGreen; Name: 'claSeaGreen'),
      (Value: TAlphaColors.SeaShell; Name: 'claSeaShell'),
      (Value: TAlphaColors.Sienna; Name: 'claSienna'),
      (Value: TAlphaColors.Silver; Name: 'claSilver'),
      (Value: TAlphaColors.SkyBlue; Name: 'claSkyBlue'),
      (Value: TAlphaColors.SlateBlue; Name: 'claSlateBlue'),
      (Value: TAlphaColors.SlateGray; Name: 'claSlateGray'),
      (Value: TAlphaColors.SlateGrey; Name: 'claSlateGrey'),
      (Value: TAlphaColors.Snow; Name: 'claSnow'),
      (Value: TAlphaColors.SpringGreen; Name: 'claSpringGreen'),
      (Value: TAlphaColors.SteelBlue; Name: 'claSteelBlue'),
      (Value: TAlphaColors.Tan; Name: 'claTan'),
      (Value: TAlphaColors.Teal; Name: 'claTeal'),
      (Value: TAlphaColors.Thistle; Name: 'claThistle'),
      (Value: TAlphaColors.Tomato; Name: 'claTomato'),
      (Value: TAlphaColors.Turquoise; Name: 'claTurquoise'),
      (Value: TAlphaColors.Violet; Name: 'claViolet'),
      (Value: TAlphaColors.Wheat; Name: 'claWheat'),
      (Value: TAlphaColors.White; Name: 'claWhite'),
      (Value: TAlphaColors.WhiteSmoke; Name: 'claWhiteSmoke'),
      (Value: TAlphaColors.YellowGreen; Name: 'claYellowGreen'),
      (Value: TAlphaColors.Yellow; Name: 'claYellow')
  );

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils;
{$ELSE}
uses SysUtils;
{$ENDIF}


{ ****************************************************************************
  Colors
  ****************************************************************************}

function ColorToIdent(Color: LongInt;var Ident: string): Boolean;
begin
  Result:=IntToIdent(Color,Ident,ColorNames);
end;


function IdentToColor(const Ident: string;var Color: LongInt): Boolean;
begin
  Result:=IdentToInt(Ident,Color,ColorNames);
end;


function ColorToString(Color: TColor): string;
begin
  if ColorToIdent(Color,Result) then
    exit;
  Result:=Format('$%0.8x',[Integer(Color)]);
end;


function StringToColor(const S: string): TColor;

begin
  if IdentToColor(S,LongInt(Result)) then
    exit;
  Result:=TColor(StrToIntDef(S,Integer(TColorRec.Black)));
end;


procedure GetColorValues(Proc: TGetStrProc);

var
  C: Integer;

begin
  for C:=Low(ColorNames) to High(ColorNames) do 
    Proc(ColorNames[C].Name);
end;

procedure RegisterColorIntegerConsts;

begin
  if Assigned(FindIntToIdent(TypeInfo(TColor))) then
    exit;
  RegisterIntegerConsts(TypeInfo(TColor),@IdentToColor,@ColorToIdent);
end;


{ ****************************************************************************
  AlphaColors
  ****************************************************************************}

function AlphaColorToIdent(Color: LongInt; var Ident: string): Boolean;

begin
  Result:=IntToIdent(Color,Ident,AlphaColorNames);
  if not Result then
    begin
    Ident:='x'+IntToHex(Color,8);
    Result:=True;
    end;
end;


function IdentToAlphaColor(const Ident: string; var Color: LongInt): Boolean;

var
  S: string;
  
begin
  S:=Ident;
  Result:=(Length(S)>1) and (S[1]='x');
  if Result then
    Color:=Integer(StringToAlphaColor(S))
  else
    begin
    Result:=IdentToInt(S,Color,AlphaColorNames);
    if not Result and (Length(S)>2) and (S[1]='c') and (S[2]='l') then
      begin
      Insert('a',S,3);
      Result:=IdentToInt(S,Color,AlphaColorNames);
      end;
    end;  
end;


procedure GetAlphaColorValues(Proc: TGetStrProc);

var
  AC: Integer;
  
begin
  for AC:=Low(AlphaColorNames) to High(AlphaColorNames) do
    Proc(Copy(AlphaColorNames[AC].Name,4));
end;

function AlphaColorToString(Value: TAlphaColor): string;
begin
  Result:='';
  if AlphaColorToIdent(Integer(Value),Result) then
    begin
    if Result[1]='x' then
      Result[1]:='#'
    else
      Delete(Result,1,3); // Strip cla...
    end;  
end;

function StringToAlphaColor(const Value: string): TAlphaColor;

var
  S: string;
  
begin
  S:=Value;
  if (S=#0) or (S='') then
    Result:=TAlphaColors.Black
  else if (Length(S)>0) and (S[1] in ['#','x']) then
    begin
    S:='$'+Copy(S,2);
    Result:=TAlphaColor(StrToIntDef(S,TAlphaColors.Black));
    end
  else 
    if not IdentToAlphaColor(S,LongInt(Result)) then
      if not IdentToAlphaColor('cla'+S,LongInt(Result)) then
        Result:=TAlphaColor(StrToIntDef(S,TAlphaColors.Black));
end;


procedure RegisterAlphaColorIntegerConsts;
begin
  if not Assigned(FindIntToIdent(TypeInfo(TAlphaColor))) then
    RegisterIntegerConsts(TypeInfo(TAlphaColor),@IdentToAlphaColor,@AlphaColorToIdent);
end;

function AlphaColorToColor(const Color: TAlphaColor): TColor;

Var
  R : TColorRec;
  
begin
  R.A:=0;
  R.R:=TAlphaColorRec(Color).R;
  R.G:=TAlphaColorRec(Color).G;
  R.B:=TAlphaColorRec(Color).B;
  Result:=TColor(R);
end;

function AppendColor(Start, Stop: TAlphaColor): TAlphaColor;

  function Channel(aStart,aStop : Byte) : byte;
  
  var
    R : Integer;
  
  begin
    Result:=MaxColorChannel;
    R:=aStart+aStop;
    if R<Result then
      Result:=R;
  end;
  
var
  RSA : TAlphaColorRec absolute start;
  RSS : TAlphaColorRec absolute stop;  
  R : TAlphaColorRec;
  
begin
  R.A:=Channel(RSA.A,RSS.A);
  R.R:=Channel(RSA.R,RSS.R);
  R.G:=Channel(RSA.G,RSS.G);
  R.B:=Channel(RSA.B,RSS.B);
  Result:=TAlphaColor(R);
end;

function SubtractColor(Start, Stop: TAlphaColor): TAlphaColor;

  function Channel(aStart,aStop : Byte) : byte;
  
  var
    R : Integer;
  
  begin
    Result:=MaxColorChannel;
    R:=aStart-aStop;
    if R>=0 then
      Result:=R;
  end;
  
var
  RSA : TAlphaColorRec absolute start;
  RSS : TAlphaColorRec absolute stop;  
  R : TAlphaColorRec absolute Result;
  
begin
  R.A:=Channel(RSA.A,RSS.A);
  R.R:=Channel(RSA.R,RSS.R);
  R.G:=Channel(RSA.G,RSS.G);
  R.B:=Channel(RSA.B,RSS.B);
end;

function RGBtoBGR(const C: TAlphaColor): TAlphaColor;

Var
  R : TAlphaColorRec absolute result;
  CR : TAlphaColorRec absolute c;
  
begin
  Result:=C;
  R.R:=CR.B;
  R.B:=CR.R;
end;

function CorrectColor(const C: TAlphaColor): TAlphaColor;

begin
{$IFNDEF WINDOWS}
  Result:=RGBtoBGR(C);
{$ELSE}  
  Result:=C;
{$ENDIF}
end;


function PremultiplyAlpha(const C: TAlphaColor): TAlphaColor;

  Function Mul(C,A : Byte) : Byte; inline;
  
  begin
    Result:=Trunc(C*A/MaxColorChannel);
  end;

var
  CR :  TAlphaColorRec absolute C;
  R : TAlphaColorRec absolute Result;
  
begin
  if CR.A=0 then
    Result:=0
  else if CR.A=MaxColorChannel then
    Result:=C
  else
    begin
    R.A:=CR.A;
    R.R:=Mul(CR.R,CR.A);
    R.G:=Mul(CR.G,CR.A);
    R.B:=Mul(CR.B,CR.A);
    end;
end;

function UnpremultiplyAlpha(const C: TAlphaColor): TAlphaColor;

  Function CDiv(C,A : Byte) : Byte; inline;
  
  begin
    Result:=Trunc(C/A/MaxColorChannel);
  end;
  
var
  CR :  TAlphaColorRec absolute C;
  R : TAlphaColorRec absolute Result;

begin
  if CR.A=0 then
    Result:=0
  else if CR.A=MaxColorChannel then
    Result:=C
  else
    begin
    R.A:=CR.A;
    R.R:=CDiv(CR.R,CR.A);
    R.G:=CDiv(CR.G,CR.A);
    R.B:=CDiv(CR.B,CR.A);
    end;
end;


function MakeColor(const C: TAlphaColor; const AOpacity: Single): TAlphaColor;

var
  CR :  TAlphaColorRec absolute C;
  R : TAlphaColorRec absolute Result;

begin
  Result:=C;
  if AOpacity<1 then
    R.A:=trunc(CR.A*AOpacity);
end;

function MakeColor(R, G, B: Byte; A: Byte = MaxColorChannel): TAlphaColor;

var
  RC : TAlphaColorRec absolute Result;

begin
  RC.A:=A;
  RC.R:=R;
  RC.G:=G;
  RC.B:=B;
end;


function LimitRange01(v : single):single;inline;

begin
  if V<0 then
    V:=0
  else if V>1 then
    V:=1;
  Result:=V;
end;

// Only valid for -1<=V<=2
function ToRange01(v : single):single;inline;

begin
  if V<0 then
    V:=V+1
  else if V>1 then
    V:=V-1;
  Result:=V;
end;

function Max(A,B: Single):Single;inline;
begin
  if (A>B) then Result:=A else Result:=B;
end;

function Min(A,B: Single):Single;inline;
begin
  if (A<B) then Result:=A else Result:=B;
end;


function ChangeHSL(const C: TAlphaColor; dH, dS, dL: Single): TAlphaColor;

var
  H,S,L: Single;
  CR : TAlphaColorRec absolute C;
  R : TAlphaColorRec absolute Result;
  
begin
  RGBtoHSL(C,H,S,L);
  H:=ToRange01(H+dH);
  S:=LimitRange01(S+dS);
  L:=LimitRange01(S+dL);
  Result:=HSLtoRGB(H,S,L);
  R.A:=CR.A;
end;

function Hue2RGBChannel(P,Q,T: Single): Single;
begin
  T:=ToRange01(T);
  if (t<1/6) then
    Exit(P+(Q-P)*6*t);
  if (t<1/2) then
    Exit(Q);
  if (t<2/3) then
    Exit(P+(Q-P)*(2/3-t)*6);
  Result:=LimitRange01(P);
end;

// Adapted from https://www.delphipraxis.net/157099-fast-integer-rgb-hsl.html
function HSLtoRGB(H, S, L: Single): TAlphaColor;

const
  Fact = 1/3;
  
  Function UpScale(S : Single) : Byte; inline;
  
  begin
    Result:=round(S*MaxColorChannel);
  end;

var
  R, G, B: Single;
  Q, P: Single;
  
begin
  if (S = 0) then
    begin
    L:=LimitRange01(L);
    R:=L;
    G:=L;
    B:=L;
    end 
  else
    begin
    if (L < 0.5) then
      Q:=L*(1+S)
    else
      Q:=L+S*(1-L);
    P:=2*L-q;
    G:=Hue2RGBChannel(P,Q,H);
    B:=Hue2RGBChannel(P,Q,H-Fact);
    R:=Hue2RGBChannel(P,Q,H+Fact);
    end;
  Result:=MakeColor(UpScale(R),UpScale(G),UpScale(B));
end;

procedure RGBtoHSL(RGB: TAlphaColor; out H, S, L: Single);

var
  R,G,B,MA,MI,Su,Diff: Single;
  RGBR : TAlphaColorRec absolute RGB;

begin
  R:=RGBR.R/$FF;
  G:=RGBR.G/$FF;
  B:=RGBR.B/$FF;
  MA:=Max(Max(R,G),B);
  MI:=Min(Min(R,G),B);
  Su:=(MI+MA);
  H:=Su/2;
  L:=H;
  if (MI=MA) then
    begin
    S:=0;
    H:=0;
    end
  else
    begin
    S:=H;
    Diff:=MA-MI;
    if L<=0.5 then
      S:=Diff/Su
    else
      S:=Diff/(2-Su);
    if (MA=R) then
      H:=(G-B)/Diff
    else if (MA=G) then
      H:=((B-R)/Diff)+2
    else
      H:=((R-G)/Diff)+4;
    H:=H/6;
    if H<0 then
      H:=H+1;
    end;
end;

function AlphaColorToIntColor(Color: TAlphaColor): Longint;
begin
  Result:=AlphaColorToColor(Color);
end;

{ ****************************************************************************
  Cursors
  ****************************************************************************}

procedure RegisterCursorIntegerConsts;

begin
  if Assigned(FindIntToIdent(TypeInfo(TCursor))) then
    exit;
  RegisterIntegerConsts(TypeInfo(TCursor),@IdentToCursor,@CursorToIdent);
end;


function CursorToIdent(Cursor: LongInt;var Ident: string): Boolean;
begin
  Result:=IntToIdent(Cursor,Ident,CursorNames);
end;


function IdentToCursor(const Ident: string;var Cursor: LongInt): Boolean;
begin
  Result:=IdentToInt(Ident, Cursor, CursorNames);
end;


function CursorToString(Cursor: TCursor): string;

begin
  if CursorToIdent(Cursor,Result) then
    exit;
  Result:=Format('%d',[Cursor]);
end;


function StringToCursor(const S: string): TCursor;

var
  C : Longint;
begin
  if IdentToCursor(S,C) then 
    Exit(TCursor(C));
  Result:=StrToIntDef(S, Integer(crDefault));
end;


procedure GetCursorValues(const Proc: TGetStrProc);

var
  C: Integer;
  
begin
  // Last 2 are duplicates
  for C:=Low(CursorNames) to High(CursorNames)-2 do 
    Proc(CursorNames[C].Name);
end;


initialization
  System.UITypes.TAlphaColorRec.ColorToRGB:=@AlphaColorToIntColor;
end.
