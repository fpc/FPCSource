{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  Copyright (C) 2022, 2024 Nikolay Nikolov <nickysn@users.sourceforge.net>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit FpTerm.Controller;

{$mode objfpc}{$H+}

interface

uses
  FpTerm.Base, FpTerm.Model, FpTerm.Logger;

type
  TTerminalStateMachineState = (
    tsmsInitial,
    tsmsESC,
    tsmsESC_SP,
    tsmsESC_Hash,  { ESC # }
    tsmsESC_Percent,
    tsmsCSI,
    tsmsOSC,
    tsmsOSC_ESC,
    tsmsDCS,
    tsmsDCS_ESC,
    tsmsDesignateG0123CharacterSet94,
    tsmsDesignateG0123CharacterSet94_Percent,
    tsmsDesignateG0123CharacterSet94_Quote,
    tsmsDesignateG0123CharacterSet94_Ampersand,
    tsmsDesignateG0123CharacterSet96,
    tsmsUTF8_1ByteLeft,
    tsmsUTF8_2BytesLeft,
    tsmsUTF8_3BytesLeft,

    tsmsVT52_Initial,
    tsmsVT52_ESC,
    tsmsVT52_ESC_Y,
    tsmsVT52_ESC_Y_Ps
  );
  TTerminalModeFlag = (
    tmfCursorKeysSendApplicationSequences,
    tmfAutoNewLine,
    tmfAutoWrapMode,
    tmfBracketedPasteMode,
    tmfSend8BitC1Controls,
    tmfAllow80To132ModeSwitching,
    tmfInsertMode,
    tmfOriginMode,
    tmfSendReceiveMode,
    tmfKeyboardActionMode,
    tmfReverseVideo,
    tmfUTF8Mode
  );
  TTerminalModeFlags = set of TTerminalModeFlag;
  TTerminalMouseTrackingMode = (
    tmtmNone,
    tmtmX10,
    tmtmNormal,
    tmtmHighlight,
    tmtmButtonEvent,
    tmtmAnyEvent
  );
  TTerminalMouseProtocolEncoding = (
    tmpeX10,
    tmpeUTF8,
    tmpeSGR,
    tmpeURXVT,
    tmpeSGRPixels
  );
  TTerminalType = (
    ttVT52,
    ttVT100,
    ttVT101,
    ttVT102,
    ttVT125,
    ttVT131,
    ttVT132,
    ttVT220,
    ttVT240,
    ttVT241,
    ttVT320,
    ttVT330,
    ttVT340,
    ttVT382,
    ttVT420,
    ttVT510,
    ttVT520,
    ttVT525
  );
  TDecConformanceLevel = (
    dclVT52,
    dclVT100,  { VT100/VT101/VT102/VT105/VT125/VT131/VT180 }
    dclVT200,  { VT220/VT240/VT241 }
    dclVT300,  { VT320/VT340 }
    dclVT400,  { VT420 }
    dclVT500   { VT510/VT520/VT525 }
  );
  TGCharacterSet = (
    gcsG0,
    gcsG1,
    gcsG2,
    gcsG3
  );
  TDecCharacterSet = (
    dcsUSASCII,                  { VT100 }
    dcsBritishNRCS,              { VT100 }
    dcsFinnishNRCS,              { VT200 }
    dcsSwedishNRCS,              { VT200 }
    dcsGermanNRCS,               { VT200 }
    dcsFrenchCanadianNRCS,       { VT200 }
    dcsFrenchNRCS,               { VT200 }
    dcsItalianNRCS,              { VT200 }
    dcsSpanishNRCS,              { VT200 }
    dcsDutchNRCS,                { VT200 }
    dcsGreekNRCS,                { VT500 }
    dcsTurkishNRCS,              { VT500 }
    dcsPortugueseNRCS,           { VT300 }
    dcsHebrewNRCS,               { VT500 }
    dcsSwissNRCS,                { VT200 }
    dcsNorwegianDanishNRCS,      { VT200 }
    dcsDecSpecialGraphics,       { VT100 }
    dcsDecSupplemental,          { VT200 }
    dcsDecTechnical,             { VT300 }
    dcsDecHebrew,                { VT500 }
    dcsDecGreek,                 { VT500 }
    dcsDecTurkish,               { VT500 }
    dcsDecCyrillic,              { VT500 }
    dcsScsNRCS,                  { VT500 }
    dcsRussianNRCS,              { VT500 }

    { 96-character sets }
    dcsISOLatin1Supplemental,    { VT300 }
    dcsISOLatin2Supplemental,    { VT500 }
    dcsISOGreekSupplemental,     { VT500 }
    dcsISOHebrewSupplemental,    { VT500 }
    dcsISOLatinCyrillic,         { VT500 }
    dcsISOLatin5Supplemental     { VT500 }
  );

const
  CharacterSets96 = [dcsISOLatin1Supplemental..dcsISOLatin5Supplemental];

type
  TVT52CharacterSet = (
    v52csASCII,
    v52csGraphics
  );

const
  DefaultModeFlags: TTerminalModeFlags = [tmfAutoWrapMode, tmfSendReceiveMode, tmfUTF8Mode];
  MaxWidth = 65536;

type
  TTransmitDataEvent = procedure(const buf; Bytes: SizeUInt) of object;
  TResizeEvent = procedure(NewWidth, NewHeight: Integer) of object;

  TTerminalSavedCursorFlag = (
    tscfOriginMode,
    tscfNextCharacterWrapsToNextLine
  );
  TTerminalSavedCursorFlags = set of TTerminalSavedCursorFlag;

const
  DefaultSavedCursorFlags: TTerminalSavedCursorFlags = [];

type

  { TTerminalSavedCursor }

  TTerminalSavedCursor = class
  private
    FCursorX: Integer;
    FCursorY: Integer;
    FCursorVisible: Boolean;
    FAttribute: TAttribute;
    FGLCharacterSet: TGCharacterSet;
    FGRCharacterSet: TGCharacterSet;
    FGCharacterSets: array [TGCharacterSet] of TDecCharacterSet;
    FFlags: TTerminalSavedCursorFlags;
    function GetCharacterSets(Index: TGCharacterSet): TDecCharacterSet;
    procedure SetCharacterSets(Index: TGCharacterSet; AValue: TDecCharacterSet);
  public
    constructor Create;
    procedure Reset;

    property CursorX: Integer read FCursorX write FCursorX;
    property CursorY: Integer read FCursorY write FCursorY;
    property CursorVisible: Boolean read FCursorVisible write FCursorVisible;
    property Attribute: TAttribute read FAttribute write FAttribute;
    property GLCharacterSet: TGCharacterSet read FGLCharacterSet write FGLCharacterSet;
    property GRCharacterSet: TGCharacterSet read FGRCharacterSet write FGRCharacterSet;
    property GCharacterSets[Index: TGCharacterSet]: TDecCharacterSet read GetCharacterSets write SetCharacterSets;
    property Flags: TTerminalSavedCursorFlags read FFlags write FFlags;
  end;

  { TTerminalTabStops }

  TTerminalTabStops = class
  private
    FTabStops: bitpacked array [0..MaxWidth - 1] of Boolean;
    function GetTabStop(Index: Integer): Boolean;
    procedure SetTabStop(Index: Integer; AValue: Boolean);
  public
    constructor Create;

    procedure Clear;
    procedure Reset;
    property TabStop[Index: Integer]: Boolean read GetTabStop write SetTabStop; default;
  end;

  { TTerminalController }

  TTerminalController = class
  private
    function GetCursorHomeX: Integer;
    function GetCursorHomeY: Integer;
    function GetCursorLimitX: Integer;
    function GetCursorLimitY: Integer;
    function GetCursorMinX: Integer;
    function GetCursorMinY: Integer;
    function GetCursorOriginX: Integer;
    function GetCursorOriginY: Integer;
  private
    FTerminalType: TTerminalType;

    FState: TTerminalStateMachineState;
    FUTF8_Build: LongWord;

    FLastGraphicCharacter: TExtendedGraphemeCluster;

    FNextCharacterWrapsToNextLine: Boolean;

    FSavedCursor: TTerminalSavedCursor;

    FAttribute: TAttribute;
    FModeFlags: TTerminalModeFlags;
    FDecConformanceLevel: TDecConformanceLevel;

    FDesignatingCharacterSet: TGCharacterSet;
    FGLCharacterSet: TGCharacterSet;
    FGRCharacterSet: TGCharacterSet;
    FGCharacterSets: array [TGCharacterSet] of TDecCharacterSet;
    FVT52CharacterSet: TVT52CharacterSet;

    FScrollingRegionTop: Integer;
    FScrollingRegionBottom: Integer;
    FScrollingRegionLeft: Integer;
    FScrollingRegionRight: Integer;

    {CSI control sequence}
    FControlSequenceParameter: string;
    FControlSequenceIntermediate: string;
    FControlSequenceFinalByte: Char;

    {OSC}
    FOperatingSystemCommand: string;

    {DCS}
    FDeviceControlString: string;

    FMouseTrackingMode: TTerminalMouseTrackingMode;
    FMouseProtocolEncoding: TTerminalMouseProtocolEncoding;

    FOldMouseX, FOldMouseY: Integer;
    FOldMouseButtons: TPointingDeviceButtonState;

    FTabStops: TTerminalTabStops;
    FModel: TTerminalModel;
    FOnTransmitData: TTransmitDataEvent;
    FOnResize: TResizeEvent;
    FLogger: TLogger;

    FVT100AnswerbackString: string;

    function ParseControlSequenceParams_Int(const CommandName: string; var i1: Integer): Boolean;
    function ParseControlSequenceParams_Int_Int(const CommandName: string; var i1, i2: Integer): Boolean;

    procedure SaveCursor;
    procedure RestoreCursor;
    procedure ClearControlSequence;
    procedure ExecuteControlSequence;
    procedure ExecuteOSC;
    procedure ExecuteDCS;
    procedure ExecuteDECRQSS(const ReqStr: string);
    procedure HandleSGRAttribute(SGRAttr: string);
    procedure ScrollUp(LinesCount: Integer = 1);
    procedure ScrollDown(LinesCount: Integer = 1);
    procedure ScrollLeft(CharCount: Integer);
    procedure ScrollRight(CharCount: Integer);
    procedure InsertLines(LinesCount: Integer);
    procedure DeleteLines(LinesCount: Integer);
    procedure DeleteCharacters(CharCount: Integer);
    procedure ErasePage;
    procedure ErasePageToBottom;
    procedure ErasePageToTop;
    procedure EraseLineToRight;
    procedure EraseLineToLeft;
    procedure EraseLine;
    procedure EraseCharacters(CharCount: Integer);
    procedure InsertBlankCharacters(CharCount: Integer);
    procedure WriteUTF32Char(const UTF32Char: LongWord);
    procedure WriteVT100CharFromCharset(Ch: Char; Charset: TDecCharacterSet);
    procedure WriteVT100Char(Ch: Char);
    procedure WriteVT52Char(Ch: Char);
    procedure WriteRepeatedCharacter(const EGC: TExtendedGraphemeCluster; Count: Integer);
    procedure HandleC1(Ch: TC1Char);
    procedure HandleENQ;
    procedure HandleCR;
    procedure HandleLF;
    procedure HandleBS;
    procedure HandleHT;
    procedure CursorForwardTabulation(TabStops: Integer);
    procedure CursorBackwardTabulation(TabStops: Integer);
    procedure EnterVT52Mode;
    procedure LeaveVT52Mode;
    procedure TransmitData(const buf; Bytes: SizeUInt);
    procedure TransmitStr(const S: string);
    procedure HardReset;
    procedure SoftReset;

    property CursorHomeX: Integer read GetCursorHomeX;
    property CursorHomeY: Integer read GetCursorHomeY;
    property CursorOriginX: Integer read GetCursorOriginX;
    property CursorOriginY: Integer read GetCursorOriginY;
    property CursorMinX: Integer read GetCursorMinX;
    property CursorMinY: Integer read GetCursorMinY;
    property CursorLimitX: Integer read GetCursorLimitX;
    property CursorLimitY: Integer read GetCursorLimitY;
  public
    constructor Create(AModel: TTerminalModel; ATerminalType: TTerminalType);
    destructor Destroy; override;

    function Resize(NewWidth, NewHeight: Integer): Boolean;
    procedure ReceiveData(const buf; Bytes: SizeUInt);
    procedure MaybeLocalEcho(const ks: rawbytestring);
    procedure HandleMouseEvent(const pdev: TPointingDeviceEvent);
    function EncodeReturnC1(Ch: TC1Char): string;
    property OnTransmitData: TTransmitDataEvent read FOnTransmitData write FOnTransmitData;
    property OnResize: TResizeEvent read FOnResize write FOnResize;
    property ModeFlags: TTerminalModeFlags read FModeFlags;
    property VT100AnswerbackString: string read FVT100AnswerbackString write FVT100AnswerbackString;
    property TerminalType: TTerminalType read FTerminalType;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Math;
{$ELSE FPC_DOTTEDUNITS}
  SysUtils, Math;
{$ENDIF FPC_DOTTEDUNITS}

const
  MaxConformanceLevelForTerminal: array [TTerminalType] of TDecConformanceLevel = (
    dclVT52,   { ttVT52  }
    dclVT100,  { ttVT100 }
    dclVT100,  { ttVT101 }
    dclVT100,  { ttVT102 }
    dclVT100,  { ttVT125 }
    dclVT100,  { ttVT131 }
    dclVT100,  { ttVT132 }
    dclVT200,  { ttVT220 }
    dclVT200,  { ttVT240 }
    dclVT200,  { ttVT241 }
    dclVT300,  { ttVT320 }
    dclVT300,  { ttVT330 }
    dclVT300,  { ttVT340 }
    dclVT300,  { ttVT382 }
    dclVT400,  { ttVT420 }
    dclVT500,  { ttVT510 }
    dclVT500,  { ttVT520 }
    dclVT500   { ttVT525 }
  );
  DecSpecialGraphicsCharacterSet: array [#$5F..#$7E] of UCS4Char = (
                                                                                                             $0020,
    $25C6, $2592, $2409, $240C, $240D, $240A, $00B0, $00B1, $2424, $240B, $2518, $2510, $250C, $2514, $253C, $23BA,
    $23BB, $2500, $23BC, $23BD, $251C, $2524, $2534, $252C, $2502, $2264, $2265, $03A0, $2260, $00A3, $00B7
  );
  DecTechnicalCharacterSet: array [#$21..#$7E] of UCS4Char = (
          $23B7, $250C, $2500, $2320, $2321, $2502, $23A1, $23A3, $23A4, $23A6, $23A7, $23A9, $23AB, $23AD, $23A8,
   $23AC, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $2264, $2260, $2265, $222B,
   $2234, $221D, $221E, $00F7, $0394, $2207, $03A6, $0393, $223C, $2243, $0398, $00D7, $039B, $21D4, $21D2, $2261,
   $03A0, $03A8, $FFFD, $03A3, $FFFD, $FFFD, $221A, $03A9, $039E, $03A5, $2282, $2283, $2229, $222A, $2227, $2228,
   $00AC, $03B1, $03B2, $03C7, $03B4, $03B5, $03C6, $03B3, $03B7, $03B9, $03B8, $03BA, $03BB, $FFFD, $03BD, $2202,
   $03C0, $03C8, $03C1, $03C3, $03C4, $FFFD, $0192, $03C9, $03BE, $03C5, $03B6, $2190, $2191, $2192, $2193
  );
  DecCyrillicCharacterSet: array [#$40..#$7E] of UCS4Char = (
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427
  );
  VT52GraphicsCharacterSet: array [#$5E..#$7E] of UCS4Char = (
                                                                                                      $0020, $0020,
    $0020, $2588, $215F, $00B3, $2075, $2077, $00B0, $00B1, $2192, $2026, $00F7, $2193, $2594,$1FB76,$1FB77,$1FB78,
   $1FB79,$1FB7A,$1FB7B, $2581, $2080, $2081, $2082, $2083, $2084, $2085, $2086, $2087, $2088, $2089, $00B6
  );

function OnlyDigits(const S: string): Boolean;
var
  Ch: Char;
begin
  for Ch in S do
    if (Ch < '0') or (Ch > '9') then
      exit(False);
  Result := True;
end;

function ExtractStringParameter(var S: string): string;
var
  I: SizeInt;
begin
  I := Pos(';', S);
  if I = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end;
end;

function ExtractSGRParameter(var S: string): string;
var
  P1, P2: LongInt;
  P2S, P3S, P4S, P5S: string;
begin
  Result := ExtractStringParameter(S);
  if (Result <> '') and OnlyDigits(Result) then
  begin
    P1 := StrToInt(Result);
    if (P1 = 38) or (P1 = 48) then
    begin
      P2S := ExtractStringParameter(S);
      Result := Result + ':' + P2S;
      if (P2S <> '') and OnlyDigits(P2S) then
      begin
        P2 := StrToInt(P2S);
        case P2 of
          5:
            begin
              P3S := ExtractStringParameter(S);
              if OnlyDigits(P3S) then
                Result := Result + ':' + P3S;
            end;
          2:
            begin
              P3S := ExtractStringParameter(S);
              if OnlyDigits(P3S) then
              begin
                P4S := ExtractStringParameter(S);
                if OnlyDigits(P4S) then
                begin
                  P5S := ExtractStringParameter(S);
                  if OnlyDigits(P5S) then
                    Result := Result + ':' + P3S + ':' + P4S + ':' + P5S;
                end
                else
                  Result := Result + ':' + P3S + ':' + P4S;
              end
              else
                Result := Result + ':' + P3S;
            end;
        end;
      end;
    end;
  end;
end;

function ExtractIntParameter(var S: string; Default: Integer): Integer;
var
  ParamS: string;
begin
  ParamS := ExtractStringParameter(S);
  if ParamS = '' then
    Result := Default
  else
    Result := StrToInt(ParamS);
end;

{ TTerminalSavedCursor }

function TTerminalSavedCursor.GetCharacterSets(Index: TGCharacterSet): TDecCharacterSet;
begin
  Result := FGCharacterSets[Index];
end;

procedure TTerminalSavedCursor.SetCharacterSets(Index: TGCharacterSet; AValue: TDecCharacterSet);
begin
  FGCharacterSets[Index] := AValue;
end;

constructor TTerminalSavedCursor.Create;
begin
  Reset;
end;

procedure TTerminalSavedCursor.Reset;
begin
  FCursorX := 0;
  FCursorY := 0;
  FCursorVisible := True;
  FAttribute := DefaultAttribute;
  FGLCharacterSet := gcsG0;
  FGRCharacterSet := gcsG1;
  FGCharacterSets[gcsG0] := dcsUSASCII;
  FGCharacterSets[gcsG1] := dcsUSASCII;
  FGCharacterSets[gcsG2] := dcsUSASCII;
  FGCharacterSets[gcsG3] := dcsUSASCII;
  FFlags := DefaultSavedCursorFlags;
end;

{ TTerminalTabStops }

function TTerminalTabStops.GetTabStop(Index: Integer): Boolean;
begin
  if (Index >= Low(FTabStops)) and (Index <= High(FTabStops)) then
    Result := FTabStops[Index]
  else
    Result := False;
end;

procedure TTerminalTabStops.SetTabStop(Index: Integer; AValue: Boolean);
begin
  if (Index >= Low(FTabStops)) and (Index <= High(FTabStops)) then
    FTabStops[Index] := AValue;
end;

constructor TTerminalTabStops.Create;
begin
  Reset;
end;

procedure TTerminalTabStops.Clear;
begin
  FillChar(FTabStops, SizeOf(FTabStops), 0);
end;

procedure TTerminalTabStops.Reset;
var
  I: Integer;
begin
  Clear;
  for I := 0 to High(FTabStops) div 8 do
    TabStop[I * 8] := True;
end;

{ TTerminalController }

function TTerminalController.GetCursorHomeX: Integer;
begin
  Result := 0;
end;

function TTerminalController.GetCursorHomeY: Integer;
begin
  if tmfOriginMode in FModeFlags then
    Result := FScrollingRegionTop
  else
    Result := 0;
end;

function TTerminalController.GetCursorLimitX: Integer;
begin
  Result := FModel.Width - 1;
end;

function TTerminalController.GetCursorLimitY: Integer;
begin
  if tmfOriginMode in FModeFlags then
    Result := FScrollingRegionBottom
  else
    Result := FModel.Height - 1;
end;

function TTerminalController.GetCursorMinX: Integer;
begin
  Result := 0;
end;

function TTerminalController.GetCursorMinY: Integer;
begin
  Result := CursorHomeY;
end;

function TTerminalController.GetCursorOriginX: Integer;
begin
  Result := 0;
end;

function TTerminalController.GetCursorOriginY: Integer;
begin
  Result := CursorHomeY;
end;

function TTerminalController.ParseControlSequenceParams_Int(const CommandName: string; var i1: Integer): Boolean;
var
  S: string;
begin
  Result := False;
  if FControlSequenceIntermediate <> '' then
  begin
    FLogger.LogMessage(vlWarning, 'Unhandled ' + CommandName + ' intermediate bytes: ' + FControlSequenceIntermediate);
    exit;
  end;
  S := FControlSequenceParameter;
  try
    i1 := ExtractIntParameter(S, i1);
  except
    on e: EConvertError do
    begin
      FLogger.LogMessage(vlWarning, 'Invalid ' + CommandName + ' integer parameter: ' + FControlSequenceParameter);
      exit;
    end;
  end;
  if S <> '' then
  begin
    FLogger.LogMessage(vlWarning, 'Too many ' + CommandName + ' parameters: ' + FControlSequenceParameter);
    exit;
  end;
  Result := True;
end;

function TTerminalController.ParseControlSequenceParams_Int_Int(const CommandName: string; var i1, i2: Integer): Boolean;
var
  S: string;
begin
  Result := False;
  if FControlSequenceIntermediate <> '' then
  begin
    FLogger.LogMessage(vlWarning, 'Unhandled ' + CommandName + ' intermediate bytes: ' + FControlSequenceIntermediate);
    exit;
  end;
  S := FControlSequenceParameter;
  try
    i1 := ExtractIntParameter(S, i1);
    i2 := ExtractIntParameter(S, i2);
  except
    on e: EConvertError do
    begin
      FLogger.LogMessage(vlWarning, 'Invalid ' + CommandName + ' integer parameter: ' + FControlSequenceParameter);
      exit;
    end;
  end;
  if S <> '' then
  begin
    FLogger.LogMessage(vlWarning, 'Too many ' + CommandName + ' parameters: ' + FControlSequenceParameter);
    exit;
  end;
  Result := True;
end;

procedure TTerminalController.SaveCursor;
var
  Flags: TTerminalSavedCursorFlags;
begin
  { todo: shape, other attributes? }
  FSavedCursor.CursorX := FModel.CursorX;
  FSavedCursor.CursorY := FModel.CursorY;
  FSavedCursor.CursorVisible := FModel.CursorVisible;
  FSavedCursor.Attribute := FAttribute;
  FSavedCursor.GLCharacterSet := FGLCharacterSet;
  FSavedCursor.GRCharacterSet := FGRCharacterSet;
  FSavedCursor.GCharacterSets[gcsG0] := FGCharacterSets[gcsG0];
  FSavedCursor.GCharacterSets[gcsG1] := FGCharacterSets[gcsG1];
  FSavedCursor.GCharacterSets[gcsG2] := FGCharacterSets[gcsG2];
  FSavedCursor.GCharacterSets[gcsG3] := FGCharacterSets[gcsG3];

  Flags := [];
  if tmfOriginMode in ModeFlags then
    Include(Flags, tscfOriginMode);
  if FNextCharacterWrapsToNextLine then
    Include(Flags, tscfNextCharacterWrapsToNextLine);
  FSavedCursor.Flags := Flags;
end;

procedure TTerminalController.RestoreCursor;
begin
  { todo: shape, other attributes? }
  FModel.SetCursorPos(EnsureRange(FSavedCursor.CursorX, CursorMinX, CursorLimitX), EnsureRange(FSavedCursor.FCursorY, CursorMinY, CursorLimitY));
  FNextCharacterWrapsToNextLine := tscfNextCharacterWrapsToNextLine in FSavedCursor.Flags;
  if FSavedCursor.CursorVisible then
    FModel.ShowCursor
  else
    FModel.HideCursor;
  FAttribute := FSavedCursor.Attribute;
  FGLCharacterSet := FSavedCursor.GLCharacterSet;
  FGRCharacterSet := FSavedCursor.GRCharacterSet;
  FGCharacterSets[gcsG0] := FSavedCursor.GCharacterSets[gcsG0];
  FGCharacterSets[gcsG1] := FSavedCursor.GCharacterSets[gcsG1];
  FGCharacterSets[gcsG2] := FSavedCursor.GCharacterSets[gcsG2];
  FGCharacterSets[gcsG3] := FSavedCursor.GCharacterSets[gcsG3];
  if tscfOriginMode in FSavedCursor.Flags then
    Include(FModeFlags, tmfOriginMode)
  else
    Exclude(FModeFlags, tmfOriginMode);
end;

procedure TTerminalController.ClearControlSequence;
begin
  FControlSequenceParameter := '';
  FControlSequenceIntermediate := '';
  FControlSequenceFinalByte := #0;
end;

procedure TTerminalController.ExecuteControlSequence;

  procedure HandleSGR;
  var
    S, PS: string;
    I: SizeInt;
  begin
    if FControlSequenceIntermediate <> '' then
      FLogger.LogMessage(vlWarning, 'Unhandled SGR intermediate bytes: ' + FControlSequenceIntermediate);
    S := FControlSequenceParameter;
    if S = '' then
      S := '0';
    while S <> '' do
    begin
      PS := ExtractSGRParameter(S);
      HandleSGRAttribute(PS);
    end;
  end;

var
  n, m: Integer;
begin
  case FControlSequenceFinalByte of
    '@':
      begin
        if FControlSequenceIntermediate = ' ' then
        begin
          { SL - SCROLL LEFT }
          n := 1;
          FControlSequenceIntermediate := '';
          if ParseControlSequenceParams_Int('SL', n) then
            ScrollLeft(n);
        end
        else
        begin
          { ICH - INSERT CHARACTER }
          n := 1;
          if ParseControlSequenceParams_Int('ICH', n) then
            InsertBlankCharacters(n);
        end;
      end;
    { CUU - CURSOR UP }
    'A':
      begin
        if FControlSequenceIntermediate = ' ' then
        begin
          { SR - SCROLL RIGHT }
          n := 1;
          FControlSequenceIntermediate := '';
          if ParseControlSequenceParams_Int('SR', n) then
            ScrollRight(n);
        end
        else
        begin
          n := 1;
          if ParseControlSequenceParams_Int('CUU', n) then
          begin
            FModel.SetCursorPos(FModel.CursorX, Max(FModel.CursorY - Max(n, 1), CursorMinY));
            FNextCharacterWrapsToNextLine := False;
          end;
        end;
      end;
    { CUD - CURSOR DOWN }
    'B':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CUD', n) then
        begin
          FModel.SetCursorPos(FModel.CursorX, Min(FModel.CursorY + Max(n, 1), CursorLimitY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CUF - CURSOR RIGHT }
    'C':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CUF', n) then
        begin
          FModel.SetCursorPos(Min(FModel.CursorX + Max(n, 1), CursorLimitX), FModel.CursorY);
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CUB - CURSOR LEFT }
    'D':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CUB', n) then
        begin
          FModel.SetCursorPos(Max(FModel.CursorX - Max(n, 1), CursorMinX), FModel.CursorY);
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CNL - CURSOR NEXT LINE }
    'E':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CNL', n) then
        begin
          FModel.SetCursorPos(0, Min(FModel.CursorY + Max(n, 1), CursorLimitY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CPL - CURSOR PRECEDING LINE }
    'F':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CPL', n) then
        begin
          FModel.SetCursorPos(0, Max(FModel.CursorY - Max(n, 1), CursorMinY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CHA - CURSOR CHARACTER ABSOLUTE }
    'G':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CHA', n) then
        begin
          FModel.SetCursorPos(EnsureRange(n - 1, CursorMinX, CursorLimitX), FModel.CursorY);
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CUP - CURSOR POSITION }
    'H':
      begin
        n := 1;
        m := 1;
        if ParseControlSequenceParams_Int_Int('CUP', n, m) then
        begin
          FModel.SetCursorPos(EnsureRange(CursorOriginX + Max(m, 1) - 1, CursorMinX, CursorLimitX), EnsureRange(CursorOriginY + Max(n, 1) - 1, CursorMinY, CursorLimitY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { CHT - CURSOR FORWARD TABULATION }
    'I':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CHT', n) then
          CursorForwardTabulation(n);
      end;
    { ED - ERASE IN PAGE }
    'J':
      begin
        n := 0;
        if ParseControlSequenceParams_Int('ED', n) then
          case n of
            0:
              ErasePageToBottom;
            1:
              ErasePageToTop;
            2:
              ErasePage;
            else
              FLogger.LogMessage(vlWarning, 'Unsupported ED parameter: ' + FControlSequenceParameter);
          end;
      end;
    { EL - ERASE IN LINE }
    'K':
      begin
        n := 0;
        if ParseControlSequenceParams_Int('EL', n) then
          case n of
            0:
              EraseLineToRight;
            1:
              EraseLineToLeft;
            2:
              EraseLine;
            else
              FLogger.LogMessage(vlWarning, 'Unsupported EL parameter: ' + FControlSequenceParameter);
          end;
      end;
    { IL - INSERT LINE }
    'L':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('IL', n) then
          InsertLines(n);
      end;
    { DL - DELETE LINE }
    'M':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('DL', n) then
          DeleteLines(n);
      end;
    { DCH - DELETE CHARACTER }
    'P':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('DCH', n) then
          DeleteCharacters(n);
      end;
    { SU - SCROLL UP }
    'S':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('SU', n) then
          ScrollUp(n);
      end;
    { SD - SCROLL DOWN }
    'T':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('SD', n) then
          { unlike "CSI 0 S" and "CSI 0 ^", which scroll by one, "CSI 0 T" doesn't do anything in xterm }
          if n <> 0 then
            ScrollDown(n);
      end;
    { ECH - ERASE CHARACTER }
    'X':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('ECH', n) then
          EraseCharacters(n);
      end;
    { CBT - CURSOR BACKWARD TABULATION }
    'Z':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('CBT', n) then
          CursorBackwardTabulation(n);
      end;
    { SD - SCROLL DOWN - ECMA-48, publication error in the original ECMA-48 5th edition (1991), corrected in 2003 }
    '^':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('SD', n) then
          ScrollDown(n);
      end;
    { HPA - CHARACTER POSITION ABSOLUTE }
    '`':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('HPA', n) then
        begin
          FModel.SetCursorPos(EnsureRange(CursorOriginX + Max(n, 1) - 1, CursorMinX, CursorLimitX), FModel.CursorY);
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { HPR - CHARACTER POSITION FORWARD }
    'a':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('HPR', n) then
        begin
          FModel.SetCursorPos(Min(FModel.CursorX + Max(n, 1), CursorLimitY), FModel.CursorY);
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { REP - REPEAT }
    'b':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('REP', n) then
        begin
          if FLastGraphicCharacter <> '' then
            WriteRepeatedCharacter(FLastGraphicCharacter, Max(n, 1));
          FLastGraphicCharacter := '';
        end;
      end;
    { DA - DEVICE ATTRIBUTES }
    'c':
      begin
        if (Length(FControlSequenceParameter) >= 1) and (FControlSequenceParameter[1] = '>') then
        begin
          { Secondary DA }
          Delete(FControlSequenceParameter, 1, 1);
          n := 0;
          if ParseControlSequenceParams_Int('Secondary DA', n) then
            TransmitStr(EncodeReturnC1(C1_CSI) + '>41;371;0c');
        end
        else
        if (Length(FControlSequenceParameter) >= 1) and (FControlSequenceParameter[1] = '=') then
        begin
          { Tertiary DA }
          Delete(FControlSequenceParameter, 1, 1);
          n := 0;
          if ParseControlSequenceParams_Int('Tertiary DA', n) then
            TransmitStr(EncodeReturnC1(C1_DCS) + '!|00000000' + EncodeReturnC1(C1_ST));
        end
        else
        begin
          { Primary DA }
          n := 0;
          if ParseControlSequenceParams_Int('DA', n) then
          begin
            case TerminalType of
              ttVT100:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?1;2c');
              ttVT101:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?1;0c');
              ttVT102:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?6c');
              ttVT125:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?12;2;0;372c');
              ttVT131:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?7c');
              ttVT132:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?4;2c');
              ttVT220:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?62;1;2;6;9;15;16;22;28c');
              ttVT240,
              ttVT241:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?62;1;2;4;6;9;15;16;22;28c');
              ttVT320:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?63;1;2;6;9;15;16;22;28c');
              ttVT330,
              ttVT340,
              ttVT382:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?63;1;2;4;6;9;15;16;22;28c');
              ttVT420:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?64;1;2;6;9;15;16;17;18;21;22;28c');
              ttVT510,
              ttVT520,
              ttVT525:
                TransmitStr(EncodeReturnC1(C1_CSI) + '?65;1;2;6;9;15;16;17;18;21;22;28c');
            end;
          end;
        end;
      end;
    { VPA - LINE POSITION ABSOLUTE }
    'd':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('VPA', n) then
        begin
          FModel.SetCursorPos(FModel.CursorX, EnsureRange(CursorOriginY + Max(n, 1) - 1, CursorMinY, CursorLimitY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { VPR - LINE POSITION FORWARD }
    'e':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('VPR', n) then
        begin
          FModel.SetCursorPos(FModel.CursorX, Min(FModel.CursorY + Max(n, 1), CursorLimitY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { HVP - CHARACTER AND LINE POSITION }
    'f':
      begin
        n := 1;
        m := 1;
        if ParseControlSequenceParams_Int_Int('HVP', n, m) then
        begin
          FModel.SetCursorPos(EnsureRange(CursorHomeX + Max(m, 1) - 1, CursorMinX, CursorLimitX), EnsureRange(CursorHomeY + Max(n, 1) - 1, CursorMinY, CursorLimitY));
          FNextCharacterWrapsToNextLine := False;
        end;
      end;
    { TBC - TABULATION CLEAR }
    'g':
      begin
        n := 0;
        if ParseControlSequenceParams_Int('TBC', n) then
          case n of
            0:
              FTabStops[FModel.CursorX] := False;
            3:
              FTabStops.Clear;
            else
              FLogger.LogMessage(vlWarning, 'Unhandled TBC: ' + IntToStr(n));
          end;
      end;
    { SM - SET MODE }
    'h':
      begin
        if (Length(FControlSequenceParameter) > 1) and (FControlSequenceParameter[1] = '?') then
        begin
          { DEC Private Mode Set (DECSET) }
          Delete(FControlSequenceParameter, 1, 1);
          while FControlSequenceParameter <> '' do
          begin
            n := ExtractIntParameter(FControlSequenceParameter, -1);
            case n of
              1:
                Include(FModeFlags, tmfCursorKeysSendApplicationSequences);
              3:
                if tmfAllow80To132ModeSwitching in FModeFlags then
                begin
                  FScrollingRegionTop := 0;
                  FScrollingRegionBottom := FModel.Height - 1;
                  FScrollingRegionLeft := 0;
                  FScrollingRegionRight := FModel.Width - 1;
                  FModel.SetCursorPos(CursorHomeX, CursorHomeY);
                  ErasePage;
                  Resize(132, FModel.Height);
                end;
              5:
                begin
                  Include(FModeFlags, tmfReverseVideo);
                  FModel.ReverseVideo := True;
                end;
              6:
                begin
                  Include(FModeFlags, tmfOriginMode);
                  FModel.SetCursorPos(CursorHomeX, CursorHomeY);
                end;
              7:
                Include(FModeFlags, tmfAutoWrapMode);
              9:
                FMouseTrackingMode := tmtmX10;
              12:
                FModel.StartBlinkingCursor;
              25:
                FModel.ShowCursor;
              40:
                Include(FModeFlags, tmfAllow80To132ModeSwitching);
              1000:
                FMouseTrackingMode := tmtmNormal;
              1002:
                FMouseTrackingMode := tmtmButtonEvent;
              1003:
                FMouseTrackingMode := tmtmAnyEvent;
              1005:
                FMouseProtocolEncoding := tmpeUTF8;
              1006:
                FMouseProtocolEncoding := tmpeSGR;
              1049:
                begin
                  SaveCursor;
                  FModel.CurrentVisibleScreenBuffer := sbAlternate;
                end;
              2004:
                Include(FModeFlags, tmfBracketedPasteMode);
              else
                FLogger.LogMessage(vlWarning, 'Unhandled DECSET: ' + IntToStr(n));
            end;
          end;
        end
        else
        begin
          while FControlSequenceParameter <> '' do
          begin
            n := ExtractIntParameter(FControlSequenceParameter, -1);
            case n of
              2:
                Include(FModeFlags, tmfKeyboardActionMode);
              4:
                Include(FModeFlags, tmfInsertMode);
              12:
                Include(FModeFlags, tmfSendReceiveMode);
              20:
                Include(FModeFlags, tmfAutoNewLine);
              else
                FLogger.LogMessage(vlWarning, 'Unhandled SET MODE: ' + IntToStr(n));
            end;
          end;
        end;
      end;
    { VPB - LINE POSITION BACKWARD }
    {'k':
      begin
        n := 1;
        if ParseControlSequenceParams_Int('VPB', n) then
          FModel.SetCursorPos(FModel.CursorX, Max(FModel.CursorY - Max(n, 1), 0));
      end;}
    { RM - RESET MODE }
    'l':
      begin
        if (Length(FControlSequenceParameter) > 1) and (FControlSequenceParameter[1] = '?') then
        begin
          { DEC Private Mode Reset (DECRST) }
          Delete(FControlSequenceParameter, 1, 1);
          while FControlSequenceParameter <> '' do
          begin
            n := ExtractIntParameter(FControlSequenceParameter, -1);
            case n of
              1:
                Exclude(FModeFlags, tmfCursorKeysSendApplicationSequences);
              2:
                EnterVT52Mode;
              3:
                if tmfAllow80To132ModeSwitching in FModeFlags then
                begin
                  FScrollingRegionTop := 0;
                  FScrollingRegionBottom := FModel.Height - 1;
                  FScrollingRegionLeft := 0;
                  FScrollingRegionRight := FModel.Width - 1;
                  FModel.SetCursorPos(CursorHomeX, CursorHomeY);
                  ErasePage;
                  Resize(80, FModel.Height);
                end;
              5:
                begin
                  Exclude(FModeFlags, tmfReverseVideo);
                  FModel.ReverseVideo := False;
                end;
              6:
                begin
                  Exclude(FModeFlags, tmfOriginMode);
                  FModel.SetCursorPos(CursorHomeX, CursorHomeY);
                end;
              7:
                Exclude(FModeFlags, tmfAutoWrapMode);
              9:
                FMouseTrackingMode := tmtmNone;
              12:
                FModel.StopBlinkingCursor;
              25:
                FModel.HideCursor;
              40:
                Exclude(FModeFlags, tmfAllow80To132ModeSwitching);
              1000:
                FMouseTrackingMode := tmtmNone;
              1002:
                FMouseTrackingMode := tmtmNone;
              1003:
                FMouseTrackingMode := tmtmNone;
              1005:
                FMouseProtocolEncoding := tmpeX10;
              1006:
                FMouseProtocolEncoding := tmpeX10;
              1049:
                begin
                  FModel.CurrentVisibleScreenBuffer := sbNormal;
                  RestoreCursor;
                end;
              2004:
                Exclude(FModeFlags, tmfBracketedPasteMode);
              else
                FLogger.LogMessage(vlWarning, 'Unhandled DECRST: ' + IntToStr(n));
            end;
          end;
        end
        else
        begin
          while FControlSequenceParameter <> '' do
          begin
            n := ExtractIntParameter(FControlSequenceParameter, -1);
            case n of
              2:
                Exclude(FModeFlags, tmfKeyboardActionMode);
              4:
                Exclude(FModeFlags, tmfInsertMode);
              12:
                Exclude(FModeFlags, tmfSendReceiveMode);
              20:
                Exclude(FModeFlags, tmfAutoNewLine);
              else
                FLogger.LogMessage(vlWarning, 'Unhandled RESET MODE: ' + IntToStr(n));
            end;
          end;
        end;
      end;
    { SGR - SELECT GRAPHIC RENDITION }
    'm':
      HandleSGR;
    { DSR - DEVICE STATUS REPORT }
    'n':
      begin
        if (Length(FControlSequenceParameter) > 1) and (FControlSequenceParameter[1] = '?') then
        begin
          { DSR, DEC format }
          Delete(FControlSequenceParameter, 1, 1);
          n := -1;
          if ParseControlSequenceParams_Int('DSR_DEC', n) then
            case n of
              6:
                if FDecConformanceLevel >= dclVT400 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(FModel.CursorY - CursorOriginY + 1) + ';' + IntToStr(FModel.CursorX - CursorOriginX + 1) + ';1R')
                else
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(FModel.CursorY - CursorOriginY + 1) + ';' + IntToStr(FModel.CursorX - CursorOriginX + 1) + 'R');
              { Report Printer status }
              15:
                if FDecConformanceLevel >= dclVT200 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?13n');  { No printer }
              { Report UDK (User Defined Keys) status }
              25:
                if FDecConformanceLevel >= dclVT200 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?20n');  { UDKs unlocked }
              { Report Keyboard status }
              26:
                if FDecConformanceLevel >= dclVT400 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?27;1;0;0n')
                else if FDecConformanceLevel >= dclVT300 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?27;1;0n')
                else if FDecConformanceLevel >= dclVT200 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?27;1n');
              { Report Locator status }
              53, 55:
                if FDecConformanceLevel >= dclVT300 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?53n');  { No locator }
              { Report Locator type }
              56:
                if FDecConformanceLevel >= dclVT300 then
                  TransmitStr(EncodeReturnC1(C1_CSI) + '?57;0n');  { Cannot identify }
              else
                FLogger.LogMessage(vlWarning, 'Unhandled DEC DEVICE STATUS REPORT: ' + IntToStr(n));
            end;
        end
        else
        begin
          { DSR, ANSI format }
          n := -1;
          if ParseControlSequenceParams_Int('DSR_ANSI', n) then
            case n of
              5:
                TransmitStr(EncodeReturnC1(C1_CSI) + '0n');
              6:
                TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(FModel.CursorY - CursorOriginY + 1) + ';' + IntToStr(FModel.CursorX - CursorOriginX + 1) + 'R');
              else
                FLogger.LogMessage(vlWarning, 'Unhandled ANSI DEVICE STATUS REPORT: ' + IntToStr(n));
            end;
        end;
      end;
    'p':
      begin
        if FControlSequenceIntermediate = '"' then
        begin
          { DECSCL - Select Conformance Level (VT220+) }
          if TerminalType >= ttVT220 then
          begin
            FControlSequenceIntermediate := '';
            n := 0;  { todo:??? }
            m := 0;  { todo:??? }
            if ParseControlSequenceParams_Int_Int('DECSCL', n, m) then
            begin
              case n of
                61:
                  begin
                    FDecConformanceLevel := dclVT100;
                    m := 1;
                  end;
                62:
                  FDecConformanceLevel := dclVT200;
                63:
                  FDecConformanceLevel := dclVT300;
                64:
                  FDecConformanceLevel := dclVT400;
                65:
                  FDecConformanceLevel := dclVT500;
              end;
              if FDecConformanceLevel > MaxConformanceLevelForTerminal[TerminalType] then
                FDecConformanceLevel := MaxConformanceLevelForTerminal[TerminalType];
              case m of
                0, 2:
                  Include(FModeFlags, tmfSend8BitC1Controls);
                1:
                  Exclude(FModeFlags, tmfSend8BitC1Controls);
              end;
              { todo: soft or hard reset? }
            end
            else
              FLogger.LogMessage(vlWarning, 'Unhandled CSI control sequence: ' + FControlSequenceParameter + '"' + FControlSequenceFinalByte);
          end;
        end
        else if FControlSequenceIntermediate = '$' then
        begin
          { DECRQM - Request ANSI/DEC private mode (VT300+) }
          if FDecConformanceLevel >= dclVT300 then
          begin
            FControlSequenceIntermediate := '';
            if (Length(FControlSequenceParameter) > 1) and (FControlSequenceParameter[1] = '?') then
            begin
              { Request DEC private mode }
              Delete(FControlSequenceParameter, 1, 1);
              n := 65535;
              if ParseControlSequenceParams_Int('DECRQM', n) then
                case n of
                  1:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(tmfCursorKeysSendApplicationSequences in FModeFlags, 1, 2)) + '$y');
                  2:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';1$y');
                  3:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(FModel.Width >= 132, 1, 2)) + '$y');
                  5:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(tmfReverseVideo in FModeFlags, 1, 2)) + '$y');
                  6:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(tmfOriginMode in FModeFlags, 1, 2)) + '$y');
                  7:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(tmfAutoWrapMode in FModeFlags, 1, 2)) + '$y');
                  25:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(FModel.CursorVisible, 1, 2)) + '$y');
                  40:
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';' + IntToStr(IfThen(tmfAllow80To132ModeSwitching in FModeFlags, 1, 2)) + '$y');
                  { permanently reset }
                  8:   { DECARM }
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';4$y');
                  { permanently set }
                  14:  { DECTEM }
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';3$y');
                  else
                    TransmitStr(EncodeReturnC1(C1_CSI) + '?' + IntToStr(n) + ';0$y');
                end;
            end
            else
            begin
              { Request ANSI mode }
              n := 65535;
              if ParseControlSequenceParams_Int('DECRQM', n) then
                case n of
                  2:
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';' + IntToStr(IfThen(tmfKeyboardActionMode in FModeFlags, 1, 2)) + '$y');
                  3:
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';2$y');
                  4:
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';' + IntToStr(IfThen(tmfInsertMode in FModeFlags, 1, 2)) + '$y');
                  12:
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';' + IntToStr(IfThen(tmfSendReceiveMode in FModeFlags, 1, 2)) + '$y');
                  20:
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';' + IntToStr(IfThen(tmfAutoNewLine in FModeFlags, 1, 2)) + '$y');
                  { permanently reset }
                  1,   { GATM }
                  5,   { KAM  }
                  7,   { SRTM }
                  10,  { VEM  }
                  11,  { HEM  }
                  13,  { PUM  }
                  14,  { SRM  }
                  15,  { FEAM }
                  16,  { TTM  }
                  17,  { SATM }
                  18,  { TSM  }
                  19:  { EBM  }
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';4$y');
                  else
                    TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n) + ';0$y');
                end;
            end;
          end;
        end
        else if FControlSequenceIntermediate = '!' then
        begin
          { DECSTR - Soft Terminal Reset (VT220+) }
          if FDecConformanceLevel >= dclVT200 then
          begin
            SoftReset;
          end;
        end
        else
          FLogger.LogMessage(vlWarning, 'Unhandled CSI control sequence: ' + FControlSequenceParameter + FControlSequenceIntermediate + FControlSequenceFinalByte);
      end;
    { DECSTBM - Set Top and Bottom Margins }
    'r':
      begin
        n := 1;
        m := FModel.Height;
        if ParseControlSequenceParams_Int_Int('DECSTBM', n, m) then
        begin
          if n = 0 then
            n := 1;
          if m = 0 then
            m := FModel.Height;
          if n > FModel.Height then
            n := FModel.Height;
          if m > FModel.Height then
            m := FModel.Height;
          if n < m then
          begin
            FScrollingRegionTop := n - 1;
            FScrollingRegionBottom := m - 1;
            FModel.SetCursorPos(CursorHomeX, CursorHomeY);
          end;
        end;
      end;
    { DECREQTPARM - Request Terminal Parameters }
    'x':
      begin
        n := 0;
        if ParseControlSequenceParams_Int('DECREQTPARM', n) then
        begin
          case n of
            0, 1:
              begin
                TransmitStr(EncodeReturnC1(C1_CSI) + IntToStr(n + 2) + ';1;1;120;120;1;0x');
              end;
          end;
        end;
      end;
    else
      FLogger.LogMessage(vlWarning, 'Unhandled CSI control sequence: ' + FControlSequenceParameter + FControlSequenceIntermediate + FControlSequenceFinalByte);
  end;
end;

procedure TTerminalController.ExecuteOSC;
begin
  {...}
  FLogger.LogMessage(vlWarning, 'Unhandled OSC control sequence: ' + FOperatingSystemCommand);
end;

procedure TTerminalController.ExecuteDCS;
begin
  if (Length(FDeviceControlString) >= 2) and (FDeviceControlString[1] = '$') and (FDeviceControlString[2] = 'q') then
    ExecuteDECRQSS(Copy(FDeviceControlString, 3, Length(FDeviceControlString) - 2))
  else
    FLogger.LogMessage(vlWarning, 'Unhandled DCS control sequence: ' + FDeviceControlString);
end;

procedure TTerminalController.ExecuteDECRQSS(const ReqStr: string);

  function ConformanceLevel2ID: Integer;
  begin
    case FDecConformanceLevel of
      dclVT200:
        Result := 62;
      dclVT300:
        Result := 63;
      dclVT400:
        Result := 64;
      dclVT500:
        Result := 65;
      else
        Result := -1;
    end;
  end;

  function GetSGRString: string;
  begin
    Result := '0';
    if rfBold in FAttribute.RenditionFlags then
      Result := Result + ';1';
    if rfFaint in FAttribute.RenditionFlags then
      Result := Result + ';2';
    if rfItalicized in FAttribute.RenditionFlags then
      Result := Result + ';3';
    if rfUnderlined in FAttribute.RenditionFlags then
      Result := Result + ';4';
    if rfBlinkSlow in FAttribute.RenditionFlags then
      Result := Result + ';5';
    if rfBlinkFast in FAttribute.RenditionFlags then
      Result := Result + ';6';
    if rfInverse in FAttribute.RenditionFlags then
      Result := Result + ';7';
    if rfInvisible in FAttribute.RenditionFlags then
      Result := Result + ';8';
    if rfCrossedOut in FAttribute.RenditionFlags then
      Result := Result + ';9';
    if rfDoublyUnderlined in FAttribute.RenditionFlags then
      Result := Result + ';21';
    case FAttribute.ForegroundColor of
      cDefaultForeground,
      cDefaultBackground:
        ;
      cBlack:
        Result := Result + ';30';
      cRed:
        Result := Result + ';31';
      cGreen:
        Result := Result + ';32';
      cBrown:
        Result := Result + ';33';
      cBlue:
        Result := Result + ';34';
      cMagenta:
        Result := Result + ';35';
      cCyan:
        Result := Result + ';36';
      cLightGray:
        Result := Result + ';37';
      cDarkGray:
        Result := Result + ';90';
      cLightRed:
        Result := Result + ';91';
      cLightGreen:
        Result := Result + ';92';
      cYellow:
        Result := Result + ';93';
      cLightBlue:
        Result := Result + ';94';
      cLightMagenta:
        Result := Result + ';95';
      cLightCyan:
        Result := Result + ';96';
      cWhite:
        Result := Result + ';97';
      cColor16..cColor255:
        Result := Result + ';38:5:' + IntToStr(Ord(FAttribute.ForegroundColor) - (Ord(cColor16) - 16));
    end;
    case FAttribute.BackgroundColor of
      cDefaultForeground,
      cDefaultBackground:
        ;
      cBlack:
        Result := Result + ';40';
      cRed:
        Result := Result + ';41';
      cGreen:
        Result := Result + ';42';
      cBrown:
        Result := Result + ';43';
      cBlue:
        Result := Result + ';44';
      cMagenta:
        Result := Result + ';45';
      cCyan:
        Result := Result + ';46';
      cLightGray:
        Result := Result + ';47';
      cDarkGray:
        Result := Result + ';100';
      cLightRed:
        Result := Result + ';101';
      cLightGreen:
        Result := Result + ';102';
      cYellow:
        Result := Result + ';103';
      cLightBlue:
        Result := Result + ';104';
      cLightMagenta:
        Result := Result + ';105';
      cLightCyan:
        Result := Result + ';106';
      cWhite:
        Result := Result + ';107';
      cColor16..cColor255:
        Result := Result + ';48:5:' + IntToStr(Ord(FAttribute.BackgroundColor) - (Ord(cColor16) - 16));
    end;
  end;

begin
  case ReqStr of
    'm':
      TransmitStr(EncodeReturnC1(C1_DCS) + '1$r' + GetSGRString + ReqStr + EncodeReturnC1(C1_ST));
    '"p':
      begin
        if FDecConformanceLevel >= dclVT200 then
          TransmitStr(EncodeReturnC1(C1_DCS) + '1$r' + IntToStr(ConformanceLevel2ID) + ';' + IntToStr(IfThen(tmfSend8BitC1Controls in ModeFlags, 0, 1)) + ReqStr + EncodeReturnC1(C1_ST));
      end;
    'r':
      TransmitStr(EncodeReturnC1(C1_DCS) + '1$r' + IntToStr(FScrollingRegionTop + 1) + ';' + IntToStr(FScrollingRegionBottom + 1) + ReqStr + EncodeReturnC1(C1_ST));
    's':
      if FDecConformanceLevel >= dclVT400 then
        TransmitStr(EncodeReturnC1(C1_DCS) + '1$r' + IntToStr(FScrollingRegionLeft + 1) + ';' + IntToStr(FScrollingRegionRight + 1) + ReqStr + EncodeReturnC1(C1_ST))
      else
        TransmitStr(EncodeReturnC1(C1_DCS) + '0$r' + EncodeReturnC1(C1_ST));
    else
    begin
      FLogger.LogMessage(vlWarning, 'Unknown/unsupported DECRQSS: ' + ReqStr);
      TransmitStr(EncodeReturnC1(C1_DCS) + '0$r' + EncodeReturnC1(C1_ST));
    end;
  end;
end;

procedure TTerminalController.HandleSGRAttribute(SGRAttr: string);
var
  ExtPara1, ExtPara2: LongInt;
  ExtParas: TStringArray;
begin
  if SGRAttr = '' then
    SGRAttr := '0';
  if OnlyDigits(SGRAttr) then
  begin
    case StrToInt(SGRAttr) of
      0:  { Normal (default), VT100 }
        FAttribute := DefaultAttribute;
      1:  { Bold, VT100 }
        FAttribute.RenditionFlags := (FAttribute.RenditionFlags + [rfBold]) - [rfFaint];
      2:  { Faint, decreased intensity, ECMA-48 2nd }
        FAttribute.RenditionFlags := (FAttribute.RenditionFlags + [rfFaint]) - [rfBold];
      3:  { Italicized, ECMA-48 2nd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags + [rfItalicized];
      4:  { Underlined, VT100 }
        FAttribute.RenditionFlags := (FAttribute.RenditionFlags + [rfUnderlined]) - [rfDoublyUnderlined];
      5:  { slowly blinking (less than 150 per minute), VT100, ECMA-48 2nd }
        FAttribute.RenditionFlags := (FAttribute.RenditionFlags + [rfBlinkSlow]) - [rfBlinkFast];
      6:  { rapidly blinking (150 per minute or more), ECMA-48 2nd }
        FAttribute.RenditionFlags := (FAttribute.RenditionFlags + [rfBlinkFast]) - [rfBlinkSlow];
      7:  { negative image }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags + [rfInverse];
      8:  { Invisible, i.e., hidden, ECMA-48 2nd, VT300 }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags + [rfInvisible];
      9:  { Crossed-out characters, ECMA-48 3rd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags + [rfCrossedOut];
      21: { Doubly-underlined, ECMA-48 3rd }
        FAttribute.RenditionFlags := (FAttribute.RenditionFlags + [rfDoublyUnderlined]) - [rfUnderlined];
      22: { Normal (neither bold nor faint), ECMA-48 3rd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfBold, rfFaint];
      23: { Not italicized, ECMA-48 3rd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfItalicized];
      24: { Not underlined, ECMA-48 3rd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfUnderlined, rfDoublyUnderlined];
      25: { steady (not blinking), ECMA-48 3rd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfBlinkSlow, rfBlinkFast];
      27: { positive (not inverse) }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfInverse];
      28: { Visible, i.e., not hidden, ECMA-48 3rd, VT300 }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfInvisible];
      29: { Not crossed-out, ECMA-48 3rd }
        FAttribute.RenditionFlags := FAttribute.RenditionFlags - [rfCrossedOut];
      30:  { Set foreground color to Black }
        FAttribute.ForegroundColor := cBlack;
      31:  { Set foreground color to Red }
        FAttribute.ForegroundColor := cRed;
      32:  { Set foreground color to Green }
        FAttribute.ForegroundColor := cGreen;
      33:  { Set foreground color to Yellow }
        FAttribute.ForegroundColor := cBrown;
      34:  { Set foreground color to Blue }
        FAttribute.ForegroundColor := cBlue;
      35:  { Set foreground color to Magenta }
        FAttribute.ForegroundColor := cMagenta;
      36:  { Set foreground color to Cyan }
        FAttribute.ForegroundColor := cCyan;
      37:  { Set foreground color to White }
        FAttribute.ForegroundColor := cLightGray;
      39:  { Set foreground color to default, ECMA-48 3rd }
        FAttribute.ForegroundColor := cDefaultForeground;
      40:  { Set background color to Black }
        FAttribute.BackgroundColor := cBlack;
      41:  { Set background color to Red }
        FAttribute.BackgroundColor := cRed;
      42:  { Set background color to Green }
        FAttribute.BackgroundColor := cGreen;
      43:  { Set background color to Yellow }
        FAttribute.BackgroundColor := cBrown;
      44:  { Set background color to Blue }
        FAttribute.BackgroundColor := cBlue;
      45:  { Set background color to Magenta }
        FAttribute.BackgroundColor := cMagenta;
      46:  { Set background color to Cyan }
        FAttribute.BackgroundColor := cCyan;
      47:  { Set background color to White }
        FAttribute.BackgroundColor := cLightGray;
      49:  { Set background color to default, ECMA-48 3rd }
        FAttribute.BackgroundColor := cDefaultBackground;
      90:  { Set foreground color to Black }
        FAttribute.ForegroundColor := cDarkGray;
      91:  { Set foreground color to Red }
        FAttribute.ForegroundColor := cLightRed;
      92:  { Set foreground color to Green }
        FAttribute.ForegroundColor := cLightGreen;
      93:  { Set foreground color to Yellow }
        FAttribute.ForegroundColor := cYellow;
      94:  { Set foreground color to Blue }
        FAttribute.ForegroundColor := cLightBlue;
      95:  { Set foreground color to Magenta }
        FAttribute.ForegroundColor := cLightMagenta;
      96:  { Set foreground color to Cyan }
        FAttribute.ForegroundColor := cLightCyan;
      97:  { Set foreground color to White }
        FAttribute.ForegroundColor := cWhite;
      100:  { Set background color to Black }
        FAttribute.BackgroundColor := cDarkGray;
      101:  { Set background color to Red }
        FAttribute.BackgroundColor := cLightRed;
      102:  { Set background color to Green }
        FAttribute.BackgroundColor := cLightGreen;
      103:  { Set background color to Yellow }
        FAttribute.BackgroundColor := cYellow;
      104:  { Set background color to Blue }
        FAttribute.BackgroundColor := cLightBlue;
      105:  { Set background color to Magenta }
        FAttribute.BackgroundColor := cLightMagenta;
      106:  { Set background color to Cyan }
        FAttribute.BackgroundColor := cLightCyan;
      107:  { Set background color to White }
        FAttribute.BackgroundColor := cWhite;
      else
        FLogger.LogMessage(vlWarning, 'Unhandled SGR attribute: ' + SGRAttr);
    end;
  end
  else
  begin
    if Pos(':', SGRAttr) <> 0 then
    begin
      ExtParas := SGRAttr.Split(':');
      ExtPara1 := StrToIntDef(ExtParas[0], 0);
      case ExtPara1 of
        38,
        48:
          begin
            ExtPara2 := StrToIntDef(ExtParas[1], 0);
            case ExtPara2 of
              2:
                begin
                  if Length(ExtParas) = 5 then
                  begin
                    if ExtPara1 = 38 then
                      FAttribute.SetForegroundColorRGB(StrToIntDef(ExtParas[2], 0), StrToIntDef(ExtParas[3], 0), StrToIntDef(ExtParas[4], 0))
                    else if ExtPara1 = 48 then
                      FAttribute.SetBackgroundColorRGB(StrToIntDef(ExtParas[2], 0), StrToIntDef(ExtParas[3], 0), StrToIntDef(ExtParas[4], 0));
                  end
                  else if Length(ExtParas) >= 6 then
                  begin
                    if ExtPara1 = 38 then
                      FAttribute.SetForegroundColorRGB(StrToIntDef(ExtParas[3], 0), StrToIntDef(ExtParas[4], 0), StrToIntDef(ExtParas[5], 0))
                    else if ExtPara1 = 48 then
                      FAttribute.SetBackgroundColorRGB(StrToIntDef(ExtParas[3], 0), StrToIntDef(ExtParas[4], 0), StrToIntDef(ExtParas[5], 0));
                  end
                  else
                    FLogger.LogMessage(vlWarning, 'Unhandled SGR attribute: ' + SGRAttr);
                end;
              5:
                begin
                  if ExtPara1 = 38 then
                    FAttribute.ForegroundColor := TColor(StrToIntDef(ExtParas[2], 0))
                  else if ExtPara1 = 48 then
                    FAttribute.BackgroundColor := TColor(StrToIntDef(ExtParas[2], 0));
                end;
              else
                FLogger.LogMessage(vlWarning, 'Unhandled SGR attribute: ' + SGRAttr);
            end;
          end;
        else
          FLogger.LogMessage(vlWarning, 'Unhandled SGR attribute: ' + SGRAttr);
      end;
    end
    else
      FLogger.LogMessage(vlWarning, 'Unhandled SGR attribute: ' + SGRAttr);
  end;
end;

procedure TTerminalController.ScrollUp(LinesCount: Integer);
var
  X, Y: Integer;
begin
  if LinesCount < 0 then
    exit;
  if LinesCount = 0 then
    LinesCount := 1;
  for Y := FScrollingRegionTop to FScrollingRegionBottom - LinesCount do
    for X := FScrollingRegionLeft to FScrollingRegionRight do
      FModel.Cell[Y, X] := FModel.Cell[Y + LinesCount, X];
  for Y := Max(FScrollingRegionBottom - LinesCount + 1, FScrollingRegionTop) to FScrollingRegionBottom do
    for X := FScrollingRegionLeft to FScrollingRegionRight do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
end;

procedure TTerminalController.ScrollDown(LinesCount: Integer);
var
  X, Y: Integer;
begin
  if LinesCount < 0 then
    exit;
  if LinesCount = 0 then
    LinesCount := 1;
  for Y := FScrollingRegionBottom downto FScrollingRegionTop + LinesCount do
    for X := FScrollingRegionLeft to FScrollingRegionRight do
      FModel.Cell[Y, X] := FModel.Cell[Y - LinesCount, X];
  for Y := FScrollingRegionTop to Min(FScrollingRegionTop + LinesCount - 1, FScrollingRegionBottom) do
    for X := FScrollingRegionLeft to FScrollingRegionRight do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
end;

procedure TTerminalController.ScrollLeft(CharCount: Integer);
var
  Y, X: Integer;
begin
  if CharCount < 0 then
    exit;
  if CharCount = 0 then
    CharCount := 1;
  for Y := FScrollingRegionTop to FScrollingRegionBottom do
  begin
    for X := 0 to FModel.Width - CharCount - 1 do
      FModel.Cell[Y, X] := FModel.Cell[Y, X + CharCount];
    for X := Max(0, FModel.Width - CharCount) to FModel.Width - 1 do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
  end;
end;

procedure TTerminalController.ScrollRight(CharCount: Integer);
var
  Y, X: Integer;
begin
  if CharCount < 0 then
    exit;
  if CharCount = 0 then
    CharCount := 1;
  for Y := FScrollingRegionTop to FScrollingRegionBottom do
  begin
    for X := FModel.Width - 1 downto CharCount do
      FModel.Cell[Y, X] := FModel.Cell[Y, X - CharCount];
    for X := 0 to Min(CharCount - 1, FModel.Width - 1) do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
  end;
end;

procedure TTerminalController.InsertLines(LinesCount: Integer);
var
  X, Y: Integer;
begin
  if LinesCount < 0 then
    exit;
  if LinesCount = 0 then
    LinesCount := 1;
  if LinesCount > (FScrollingRegionBottom - FModel.CursorY + 1) then
    LinesCount := FScrollingRegionBottom - FModel.CursorY + 1;

  for Y := FScrollingRegionBottom downto FModel.CursorY + LinesCount do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := FModel.Cell[Y - LinesCount, X];
  for Y := FModel.CursorY to FModel.CursorY + LinesCount - 1 do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);

  FModel.SetCursorPos(CursorHomeX, FModel.CursorY);
end;

procedure TTerminalController.DeleteLines(LinesCount: Integer);
var
  X, Y: Integer;
begin
  if LinesCount < 0 then
    exit;
  if LinesCount = 0 then
    LinesCount := 1;
  if LinesCount > (FScrollingRegionBottom - FModel.CursorY + 1) then
    LinesCount := FScrollingRegionBottom - FModel.CursorY + 1;

  for Y := FModel.CursorY to FScrollingRegionBottom - LinesCount do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := FModel.Cell[Y + LinesCount, X];
  for Y := FScrollingRegionBottom - LinesCount + 1 to FScrollingRegionBottom do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);

  FModel.SetCursorPos(CursorHomeX, FModel.CursorY);
end;

procedure TTerminalController.DeleteCharacters(CharCount: Integer);
var
  X: Integer;
begin
  for X := FModel.CursorX to FModel.Width - 1 do
    if (X + CharCount) < FModel.Width then
      FModel.Cell[FModel.CursorY, X] := FModel.Cell[FModel.CursorY, X + CharCount]
    else
      FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.ErasePage;
var
  X, Y: Integer;
begin
  for Y := 0 to FModel.Height - 1 do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.ErasePageToBottom;
var
  X, Y: Integer;
begin
  for X := FModel.CursorX to FModel.Width - 1 do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  for Y := FModel.CursorY + 1 to FModel.Height - 1 do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.ErasePageToTop;
var
  X, Y: Integer;
begin
  for X := 0 to FModel.CursorX do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  for Y := 0 to FModel.CursorY - 1 do
    for X := 0 to FModel.Width - 1 do
      FModel.Cell[Y, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.EraseLineToRight;
var
  X: Integer;
begin
  for X := FModel.CursorX to FModel.Width - 1 do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.EraseLineToLeft;
var
  X: Integer;
begin
  for X := 0 to FModel.CursorX do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.EraseLine;
var
  X: Integer;
begin
  for X := 0 to FModel.Width - 1 do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.EraseCharacters(CharCount: Integer);
var
  X: Integer;
begin
  if CharCount < 0 then
    exit;
  if CharCount = 0 then
    CharCount := 1;
  for X := FModel.CursorX to Min(FModel.CursorX + CharCount - 1, FModel.Width - 1) do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.InsertBlankCharacters(CharCount: Integer);
var
  X: Integer;
begin
  if CharCount < 0 then
    exit;
  if CharCount = 0 then
    CharCount := 1;
  for X := FModel.Width - 1 downto FModel.CursorX + CharCount do
    FModel.Cell[FModel.CursorY, X] := FModel.Cell[FModel.CursorY, X - CharCount];
  for X := FModel.CursorX to Min(FModel.CursorX + CharCount - 1, FModel.Width - 1) do
    FModel.Cell[FModel.CursorY, X] := ErasedCell(FAttribute);
  FNextCharacterWrapsToNextLine := False;
end;

procedure TTerminalController.WriteUTF32Char(const UTF32Char: LongWord);
var
  SaveLastGraphicCharacter: TExtendedGraphemeCluster;
  X: Integer;
begin
  SaveLastGraphicCharacter := FLastGraphicCharacter;
  FLastGraphicCharacter := '';
  if (UTF32Char >= $D800) and (UTF32Char <= $DFFF) then
    { error }
    exit
  else if UTF32Char <= $FFFF then
    FLastGraphicCharacter := WideChar(UTF32Char)
  else if UTF32Char <= $10FFFF then
    FLastGraphicCharacter := WideChar(((UTF32Char - $10000) shr 10) + $D800) + WideChar(((UTF32Char - $10000) and %1111111111) + $DC00)
  else
    { error }
    exit;
  if (SaveLastGraphicCharacter <> '') and (FModel.StringDisplayWidth(SaveLastGraphicCharacter + FLastGraphicCharacter) <= 1) then
  begin
    { combine with previous character }
    FLastGraphicCharacter := SaveLastGraphicCharacter + FLastGraphicCharacter;
    if FNextCharacterWrapsToNextLine then
      FModel.Cell[FModel.CursorY, FModel.CursorX] := Cell(FLastGraphicCharacter, FAttribute)
    else
      FModel.Cell[FModel.CursorY, FModel.CursorX - 1] := Cell(FLastGraphicCharacter, FAttribute);
  end
  else
  begin
    { type a new character }
    if FNextCharacterWrapsToNextLine then
    begin
      FNextCharacterWrapsToNextLine := False;
      if FModel.CursorY >= FScrollingRegionBottom then
      begin
        ScrollUp;
        FModel.SetCursorPos(CursorHomeX, FModel.CursorY);
      end
      else
        FModel.SetCursorPos(CursorHomeX, FModel.CursorY + 1);
    end;
    if tmfInsertMode in FModeFlags then
      for X := FModel.Width - 1 downto FModel.CursorX + 1 do
        FModel.Cell[FModel.CursorY, X] := FModel.Cell[FModel.CursorY, X - 1];
    FModel.Cell[FModel.CursorY, FModel.CursorX] := Cell(FLastGraphicCharacter, FAttribute);
    if FModel.CursorX >= (FModel.Width - 1) then
    begin
      if tmfAutoWrapMode in FModeFlags then
        FNextCharacterWrapsToNextLine := True;
    end
    else
      FModel.SetCursorPos(FModel.CursorX + 1, FModel.CursorY);
  end;
end;

procedure TTerminalController.WriteVT100CharFromCharset(Ch: Char;
  Charset: TDecCharacterSet);
begin
  Ch := Chr(Ord(Ch) and $7F);
  if (Ch = #127) and not (Charset in CharacterSets96) then
    exit;
  case Charset of
    dcsUSASCII:
      WriteUTF32Char(Ord(Ch));
    dcsBritishNRCS:
      if Ch = '#' then
        WriteUTF32Char($00A3)
      else
        WriteUTF32Char(Ord(Ch));
    dcsFinnishNRCS:
      case Ch of
        #$5B:
          WriteUTF32Char($00C4);
        #$5C:
          WriteUTF32Char($00D6);
        #$5D:
          WriteUTF32Char($00C5);
        #$5E:
          WriteUTF32Char($00DC);
        #$60:
          WriteUTF32Char($00E9);
        #$7B:
          WriteUTF32Char($00E4);
        #$7C:
          WriteUTF32Char($00F6);
        #$7D:
          WriteUTF32Char($00E5);
        #$7E:
          WriteUTF32Char($00FC);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsSwedishNRCS:
      case Ch of
        #$40:
          WriteUTF32Char($00C9);
        #$5B:
          WriteUTF32Char($00C4);
        #$5C:
          WriteUTF32Char($00D6);
        #$5D:
          WriteUTF32Char($00C5);
        #$5E:
          WriteUTF32Char($00DC);
        #$60:
          WriteUTF32Char($00E9);
        #$7B:
          WriteUTF32Char($00E4);
        #$7C:
          WriteUTF32Char($00F6);
        #$7D:
          WriteUTF32Char($00E5);
        #$7E:
          WriteUTF32Char($00FC);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsGermanNRCS:
      case Ch of
        #$40:
          WriteUTF32Char($00A7);
        #$5B:
          WriteUTF32Char($00C4);
        #$5C:
          WriteUTF32Char($00D6);
        #$5D:
          WriteUTF32Char($00DC);
        #$7B:
          WriteUTF32Char($00E4);
        #$7C:
          WriteUTF32Char($00F6);
        #$7D:
          WriteUTF32Char($00FC);
        #$7E:
          WriteUTF32Char($00DF);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsFrenchCanadianNRCS:
      case Ch of
        #$40:
          WriteUTF32Char($00E0);
        #$5B:
          WriteUTF32Char($00E2);
        #$5C:
          WriteUTF32Char($00E7);
        #$5D:
          WriteUTF32Char($00EA);
        #$5E:
          WriteUTF32Char($00EE);
        #$60:
          WriteUTF32Char($00F4);
        #$7B:
          WriteUTF32Char($00E9);
        #$7C:
          WriteUTF32Char($00F9);
        #$7D:
          WriteUTF32Char($00E8);
        #$7E:
          WriteUTF32Char($00FB);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsFrenchNRCS:
      case Ch of
        #$23:
          WriteUTF32Char($00A3);
        #$40:
          WriteUTF32Char($00E0);
        #$5B:
          WriteUTF32Char($00B0);
        #$5C:
          WriteUTF32Char($00E7);
        #$5D:
          WriteUTF32Char($00A7);
        #$7B:
          WriteUTF32Char($00E9);
        #$7C:
          WriteUTF32Char($00F9);
        #$7D:
          WriteUTF32Char($00E8);
        #$7E:
          WriteUTF32Char($00A8);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsItalianNRCS:
      case Ch of
        #$23:
          WriteUTF32Char($00A3);
        #$40:
          WriteUTF32Char($00A7);
        #$5B:
          WriteUTF32Char($00B0);
        #$5C:
          WriteUTF32Char($00E7);
        #$5D:
          WriteUTF32Char($00E9);
        #$60:
          WriteUTF32Char($00F9);
        #$7B:
          WriteUTF32Char($00E0);
        #$7C:
          WriteUTF32Char($00F2);
        #$7D:
          WriteUTF32Char($00E8);
        #$7E:
          WriteUTF32Char($00EC);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsSpanishNRCS:
      case Ch of
        #$23:
          WriteUTF32Char($00A3);
        #$40:
          WriteUTF32Char($00A7);
        #$5B:
          WriteUTF32Char($00A1);
        #$5C:
          WriteUTF32Char($00D1);
        #$5D:
          WriteUTF32Char($00BF);
        #$7B:
          WriteUTF32Char($00B0);
        #$7C:
          WriteUTF32Char($00F1);
        #$7D:
          WriteUTF32Char($00E7);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsDutchNRCS:
      case Ch of
        #$23:
          WriteUTF32Char($00A3);
        #$40:
          WriteUTF32Char($00BE);
        #$5B:
          WriteUTF32Char($0133);
        #$5C:
          WriteUTF32Char($00BD);
        #$5D:
          WriteUTF32Char($007C);
        #$7B:
          WriteUTF32Char($00A8);
        #$7C:
          WriteUTF32Char($0192);
        #$7D:
          WriteUTF32Char($00BC);
        #$7E:
          WriteUTF32Char($00B4);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsSwissNRCS:
      case Ch of
        #$23:
          WriteUTF32Char($00F9);
        #$40:
          WriteUTF32Char($00E0);
        #$5B:
          WriteUTF32Char($00E9);
        #$5C:
          WriteUTF32Char($00E7);
        #$5D:
          WriteUTF32Char($00EA);
        #$5E:
          WriteUTF32Char($00EE);
        #$5F:
          WriteUTF32Char($00E8);
        #$60:
          WriteUTF32Char($00F4);
        #$7B:
          WriteUTF32Char($00E4);
        #$7C:
          WriteUTF32Char($00F6);
        #$7D:
          WriteUTF32Char($00FC);
        #$7E:
          WriteUTF32Char($00FB);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsNorwegianDanishNRCS:
      case Ch of
        #$40:
          WriteUTF32Char($00C4);
        #$5B:
          WriteUTF32Char($00C6);
        #$5C:
          WriteUTF32Char($00D8);
        #$5D:
          WriteUTF32Char($00C5);
        #$5E:
          WriteUTF32Char($00DC);
        #$60:
          WriteUTF32Char($00E4);
        #$7B:
          WriteUTF32Char($00E6);
        #$7C:
          WriteUTF32Char($00F8);
        #$7D:
          WriteUTF32Char($00E5);
        #$7E:
          WriteUTF32Char($00FC);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsPortugueseNRCS:
      case Ch of
        #$5B:
          WriteUTF32Char($00C3);
        #$5C:
          WriteUTF32Char($00C7);
        #$5D:
          WriteUTF32Char($00D5);
        #$7B:
          WriteUTF32Char($00E3);
        #$7C:
          WriteUTF32Char($00E7);
        #$7D:
          WriteUTF32Char($00F5);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsGreekNRCS:
      case Ch of
        'n':
          WriteUTF32Char($03A7);
        'v':
          WriteUTF32Char($039E);
        'a'..'m', 'o'..'q':
          WriteUTF32Char(Ord(Ch) + ($0391 - Ord('a')));
        'r'..'u', 'w'..'x':
          WriteUTF32Char(Ord(Ch) + ($0392 - Ord('a')));
        'y'..'z':
          WriteUTF32Char($0020);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsTurkishNRCS:
      case Ch of
        '&':
          WriteUTF32Char($011F);
        #$40:
          WriteUTF32Char($0130);
        #$5B:
          WriteUTF32Char($015E);
        #$5C:
          WriteUTF32Char($00D6);
        #$5D:
          WriteUTF32Char($00C7);
        #$5E:
          WriteUTF32Char($00DC);
        #$60:
          WriteUTF32Char($011E);
        #$7B:
          WriteUTF32Char($015F);
        #$7C:
          WriteUTF32Char($00F6);
        #$7D:
          WriteUTF32Char($00E7);
        #$7E:
          WriteUTF32Char($00FC);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsHebrewNRCS:
      case Ch of
        #$60..#$7A:
          WriteUTF32Char(Ord(Ch) + ($05D0 - $60));
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsDecHebrew:
      case Ch of
        '!'..'''', ')', '+'..'9', ';'..'?':
          WriteVT100CharFromCharset(Ch, dcsDecSupplemental);
        '(':
          WriteUTF32Char($00A8);
        '*':
          WriteUTF32Char($00D7);
        ':':
          WriteUTF32Char($00F7);
        #$60..#$7A:
          WriteUTF32Char(Ord(Ch) + ($05D0 - $60));
        '@'..'_', '{'..'~':
          WriteUTF32Char($FFFD);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsDecGreek:
      case Ch of
        '!'..'?':
          WriteVT100CharFromCharset(Ch, dcsDecSupplemental);
        '@':
          WriteUTF32Char($03CA);
        'A'..'O':
          WriteUTF32Char(Ord(Ch) + ($0391 - Ord('A')));
        'P':
          WriteUTF32Char($FFFD);
        'Q'..'R':
          WriteUTF32Char(Ord(Ch) + ($03A0 - Ord('Q')));
        'S'..'Y':
          WriteUTF32Char(Ord(Ch) + ($03A3 - Ord('S')));
        'Z'..']':
          WriteUTF32Char(Ord(Ch) + ($03AC - Ord('Z')));
        '^':
          WriteUTF32Char($FFFD);
        '_':
          WriteUTF32Char($03CC);
        '`':
          WriteUTF32Char($03CB);
        'a'..'o':
          WriteUTF32Char(Ord(Ch) + ($03B1 - Ord('a')));
        'p':
          WriteUTF32Char($FFFD);
        'q'..'r':
          WriteUTF32Char(Ord(Ch) + ($03C0 - Ord('q')));
        's'..'y':
          WriteUTF32Char(Ord(Ch) + ($03C3 - Ord('s')));
        'z':
          WriteUTF32Char($03C2);
        '{':
          WriteUTF32Char($03CD);
        '|':
          WriteUTF32Char($03CE);
        '}':
          WriteUTF32Char($0384);
        '~':
          WriteUTF32Char($FFFD);
        else
          WriteUTF32Char(Ord(Ch));
      end;
    dcsDecTurkish:
      case Ch of
        '(':
          WriteUTF32Char($00A8);
        '.':
          WriteUTF32Char($0130);
        '>':
          WriteUTF32Char($0131);
        'P':
          WriteUTF32Char($011E);
        '^':
          WriteUTF32Char($015E);
        'p':
          WriteUTF32Char($011F);
        '~':
          WriteUTF32Char($015F);
        else
          WriteVT100CharFromCharset(Ch, dcsDecSupplemental);
      end;
    dcsDecCyrillic:
      if (Ch >= Low(DecCyrillicCharacterSet)) and (Ch <= High(DecCyrillicCharacterSet)) then
        WriteUTF32Char(DecCyrillicCharacterSet[Ch])
      else
        WriteUTF32Char($FFFD);
    dcsDecSpecialGraphics:
      if (Ch >= Low(DecSpecialGraphicsCharacterSet)) and (Ch <= High(DecSpecialGraphicsCharacterSet)) then
        WriteUTF32Char(DecSpecialGraphicsCharacterSet[Ch])
      else
        WriteUTF32Char(Ord(Ch));
    dcsDecTechnical:
      if (Ch >= Low(DecTechnicalCharacterSet)) and (Ch <= High(DecTechnicalCharacterSet)) then
        WriteUTF32Char(DecTechnicalCharacterSet[Ch])
      else
        WriteUTF32Char(Ord(Ch));
    dcsDecSupplemental:
      case Ch of
        Chr($A8 - $80):
          WriteUTF32Char($00A4);
        Chr($D7 - $80):
          WriteUTF32Char($0152);
        Chr($DD - $80):
          WriteUTF32Char($0178);
        Chr($F7 - $80):
          WriteUTF32Char($0153);
        Chr($FD - $80):
          WriteUTF32Char($00FF);
        Chr($A4 - $80),
        Chr($A6 - $80),
        Chr($AC - $80)..Chr($AF - $80),
        Chr($B4 - $80),
        Chr($B8 - $80),
        Chr($BE - $80),
        Chr($D0 - $80),
        Chr($DE - $80),
        Chr($F0 - $80),
        Chr($FE - $80):
          { REPLACEMENT CHARACTER }
          WriteUTF32Char($FFFD);
        #0..#32,#127:
          WriteUTF32Char(Ord(Ch));
        else
          WriteUTF32Char(Ord(Ch) + $80);
      end;
    dcsISOLatin1Supplemental:
      WriteUTF32Char(Ord(Ch) + $80);
  end;
end;

procedure TTerminalController.WriteVT100Char(Ch: Char);
begin
  if Ord(Ch) <= 127 then
    WriteVT100CharFromCharset(Ch, FGCharacterSets[FGLCharacterSet])
  else
    WriteVT100CharFromCharset(Ch, FGCharacterSets[FGRCharacterSet]);
end;

procedure TTerminalController.WriteVT52Char(Ch: Char);
begin
  Ch := Chr(Ord(Ch) and 127);
  case FVT52CharacterSet of
    v52csASCII:
      WriteUTF32Char(Ord(Ch));
    v52csGraphics:
      if (Ch >= Low(VT52GraphicsCharacterSet)) and (Ch <= High(VT52GraphicsCharacterSet)) then
        WriteUTF32Char(VT52GraphicsCharacterSet[Ch])
      else
        WriteUTF32Char(Ord(Ch));
  end;
end;

procedure TTerminalController.WriteRepeatedCharacter(const EGC: TExtendedGraphemeCluster; Count: Integer);
var
  I, J: Integer;
begin
  for I := 1 to Count do
  begin
    J := 1;
    while J <= Length(EGC) do
    begin
      if (EGC[J] >= #$D800) and (EGC[J] <= #$DFFF) then
      begin
        if (EGC[J] < #$DC00) and (J < Length(EGC)) and (EGC[J + 1] >= #$DC00) and (EGC[J + 1] <= #$DFFF) then
        begin
          WriteUTF32Char($10000 + ((LongWord(EGC[J]) - $D800) shl 10) + (LongWord(EGC[J + 1]) - $DC00));
          Inc(J, 2);
        end
        else
          { invalid UTF-16 code unit, skip it }
          Inc(J);
      end
      else
        WriteUTF32Char(LongWord(EGC[J]));
      Inc(J);
    end;
  end;
end;

procedure TTerminalController.HandleC1(Ch: TC1Char);
begin
  case Ch of
    C1_CSI:
      begin
        FState := tsmsCSI;
        ClearControlSequence;
      end;
    C1_OSC:
      begin
        FState := tsmsOSC;
        FOperatingSystemCommand := '';
      end;
    C1_DCS:
      begin
        FState := tsmsDCS;
        FDeviceControlString := '';
      end;
    C1_RI:
      begin
        FNextCharacterWrapsToNextLine := False;
        FState := tsmsInitial;
        if FModel.CursorY = 0 then
        begin
          ScrollDown;
          FModel.SetCursorPos(FModel.CursorX, FModel.CursorY);
        end
        else
          FModel.SetCursorPos(FModel.CursorX, FModel.CursorY - 1);
      end;
    C1_IND:
      begin
        FNextCharacterWrapsToNextLine := False;
        FState := tsmsInitial;
        if FModel.CursorY >= FScrollingRegionBottom then
        begin
          ScrollUp;
          FModel.SetCursorPos(FModel.CursorX, FModel.CursorY);
        end
        else
          FModel.SetCursorPos(FModel.CursorX, FModel.CursorY + 1);
      end;
    C1_NEL:
      begin
        FNextCharacterWrapsToNextLine := False;
        FState := tsmsInitial;
        if FModel.CursorY >= FScrollingRegionBottom then
        begin
          ScrollUp;
          FModel.SetCursorPos(CursorHomeX, FModel.CursorY);
        end
        else
          FModel.SetCursorPos(CursorHomeX, FModel.CursorY + 1);
      end;
    C1_HTS:
      begin
        FState := tsmsInitial;
        FTabStops[FModel.CursorX] := True;
      end
    else
      { error }
      FLogger.LogMessage(vlWarning, 'Unhandled C1 character: #' + IntToStr(Ord(Ch)));
      FState := tsmsInitial;
  end;
end;

procedure TTerminalController.HandleENQ;
begin
  TransmitStr(VT100AnswerbackString);
end;

procedure TTerminalController.HandleCR;
begin
  FNextCharacterWrapsToNextLine := False;
  FModel.SetCursorPos(CursorHomeX, FModel.CursorY);
end;

procedure TTerminalController.HandleLF;
begin
  FNextCharacterWrapsToNextLine := False;
  if FModel.CursorY >= FScrollingRegionBottom then
  begin
    ScrollUp;
    FModel.SetCursorPos(FModel.CursorX, FModel.CursorY);
  end
  else
    FModel.SetCursorPos(FModel.CursorX, FModel.CursorY + 1);
  if tmfAutoNewLine in ModeFlags then
    FModel.SetCursorPos(CursorHomeX, FModel.CursorY);
end;

procedure TTerminalController.HandleBS;
begin
  FNextCharacterWrapsToNextLine := False;
  if FModel.CursorX > 0 then
    FModel.SetCursorPos(FModel.CursorX - 1, FModel.CursorY);
end;

procedure TTerminalController.HandleHT;
begin
  CursorForwardTabulation(1);
end;

procedure TTerminalController.CursorForwardTabulation(TabStops: Integer);
var
  CX: Integer;
begin
  if TabStops < 0 then
    exit;
  if TabStops = 0 then
    TabStops := 1;
  CX := FModel.CursorX;
  while (TabStops > 0) and (CX < (FModel.Width - 1)) do
  begin
    Dec(TabStops);
    repeat
      if CX < (FModel.Width - 1) then
        Inc(CX);
    until FTabStops[CX] or (CX >= (FModel.Width - 1));
  end;
  FModel.SetCursorPos(CX, FModel.CursorY);
end;

procedure TTerminalController.CursorBackwardTabulation(TabStops: Integer);
var
  CX: Integer;
begin
  if TabStops < 0 then
    exit;
  if TabStops = 0 then
    TabStops := 1;
  CX := FModel.CursorX;
  while (TabStops > 0) and (CX > 0) do
  begin
    Dec(TabStops);
    repeat
      if CX > 0 then
        Dec(CX);
    until FTabStops[CX] or (CX = 0);
  end;
  FModel.SetCursorPos(CX, FModel.CursorY);
end;

procedure TTerminalController.EnterVT52Mode;
begin
  FVT52CharacterSet := v52csASCII;
  FDecConformanceLevel := dclVT52;
  FState := tsmsVT52_Initial;
end;

procedure TTerminalController.LeaveVT52Mode;
begin
  FDecConformanceLevel := dclVT100;
  Exclude(FModeFlags, tmfSend8BitC1Controls);
  FState := tsmsInitial;
end;

procedure TTerminalController.TransmitData(const buf; Bytes: SizeUInt);
begin
  if Assigned(FOnTransmitData) then
    FOnTransmitData(buf, Bytes);
end;

procedure TTerminalController.TransmitStr(const S: string);
begin
  TransmitData(s[1], Length(S));
end;

procedure TTerminalController.HardReset;
begin
  FDecConformanceLevel := MaxConformanceLevelForTerminal[FTerminalType];
  FAttribute := DefaultAttribute;
  FModeFlags := DefaultModeFlags;
  FSavedCursor.Reset;
  FTabStops.Reset;
  { todo: what are the default character sets? }
  FDesignatingCharacterSet := gcsG0;
  FGLCharacterSet := gcsG0;
  FGRCharacterSet := gcsG1;
  FGCharacterSets[gcsG0] := dcsUSASCII;
  FGCharacterSets[gcsG1] := dcsUSASCII;
  FGCharacterSets[gcsG2] := dcsUSASCII;
  FGCharacterSets[gcsG3] := dcsUSASCII;
  FVT52CharacterSet := v52csASCII;
  FOldMouseX := 0;
  FOldMouseY := 0;
  FOldMouseButtons := [];
  FMouseTrackingMode := tmtmNone;
  FMouseProtocolEncoding := tmpeX10;
  FScrollingRegionTop := 0;
  FScrollingRegionBottom := FModel.Height - 1;
  FScrollingRegionLeft := 0;
  FScrollingRegionRight := FModel.Width - 1;
  if FDecConformanceLevel = dclVT52 then
    FState := tsmsVT52_Initial
  else
    FState := tsmsInitial;
  FUTF8_Build := 0;
  ClearControlSequence;
  FOperatingSystemCommand := '';
  FDeviceControlString := '';
  FVT100AnswerbackString := '';
  FLastGraphicCharacter := '';
  FNextCharacterWrapsToNextLine := False;
  FModel.Reset;
end;

procedure TTerminalController.SoftReset;
begin
  FLogger.LogMessage(vlWarning, 'Soft reset not yet implemented');
end;

constructor TTerminalController.Create(AModel: TTerminalModel;
  ATerminalType: TTerminalType);
begin
  FTerminalType := ATerminalType;
  FSavedCursor := TTerminalSavedCursor.Create;
  FLogger := TFileLogger.Create('fpterm.log', DefaultLogLevel);
  FModel := AModel;
  FTabStops := TTerminalTabStops.Create;
  HardReset;
end;

destructor TTerminalController.Destroy;
begin
  FreeAndNil(FSavedCursor);
  FreeAndNil(FTabStops);
  FreeAndNil(FLogger);
  inherited Destroy;
end;

function TTerminalController.Resize(NewWidth, NewHeight: Integer): Boolean;
var
  OldWidth, OldHeight: Integer;
begin
  OldWidth := FModel.Width;
  OldHeight := FModel.Height;
  if FModel.Resize(NewWidth, NewHeight) then
  begin
    Result := True;
    if FScrollingRegionBottom = (OldHeight - 1) then
      FScrollingRegionBottom := NewHeight - 1
    else if FScrollingRegionBottom > (NewHeight - 1) then
      FScrollingRegionBottom := NewHeight - 1;
    if FScrollingRegionTop > FScrollingRegionBottom then
      FScrollingRegionTop := FScrollingRegionBottom;
    if FScrollingRegionRight = (OldWidth - 1) then
      FScrollingRegionRight := NewWidth - 1
    else if FScrollingRegionRight > (NewWidth - 1) then
      FScrollingRegionRight := NewWidth - 1;
    if FScrollingRegionLeft > FScrollingRegionRight then
      FScrollingRegionLeft := FScrollingRegionRight;
    if Assigned(FOnResize) then
      FOnResize(NewWidth, NewHeight);
  end
  else
    Result := False;
end;

procedure TTerminalController.ReceiveData(const buf; Bytes: SizeUInt);
var
  I: SizeUInt;
  Ch: Char;
  Y, X: Integer;
begin
  if Bytes <= 0 then
    exit;
  for I := 0 to Bytes - 1 do
  begin
    Ch := PChar(@buf)[I];
    case FState of
      tsmsInitial:
        begin
          case Ch of
            C0_ENQ:
              HandleENQ;
            C0_CR:
              HandleCR;
            C0_VT,
            C0_FF,
            C0_LF:
              HandleLF;
            C0_BS:
              HandleBS;
            C0_HT:
              HandleHT;
            C0_ESC:
              FState := tsmsESC;
            C0_SI:
              FGLCharacterSet := gcsG0;
            C0_SO:
              FGLCharacterSet := gcsG1;
            #32..#127:
              WriteVT100Char(Ch);
            #$80..#$9F:
              HandleC1(Ch);
            #$A0..#$FF:
              begin
                if tmfUTF8Mode in ModeFlags then
                begin
                  case Ch of
                    #%11000000..#%11011111:
                      begin
                        FState := tsmsUTF8_1ByteLeft;
                        FUTF8_Build := Ord(Ch) and %11111;
                      end;
                    #%11100000..#%11101111:
                      begin
                        FState := tsmsUTF8_2BytesLeft;
                        FUTF8_Build := Ord(Ch) and %1111;
                      end;
                    #%11110000..#%11110111:
                      begin
                        FState := tsmsUTF8_3BytesLeft;
                        FUTF8_Build := Ord(Ch) and %111;
                      end;
                    else
                      FLogger.LogMessage(vlWarning, 'Unhandled character: #' + IntToStr(Ord(Ch)));
                  end;
                end
                else
                  WriteVT100Char(Ch);
              end;
            else
              FLogger.LogMessage(vlWarning, 'Unhandled character: #' + IntToStr(Ord(Ch)));
          end;
        end;
      tsmsESC:
        begin
          case Ch of
            #27:
              FState := tsmsESC;
            #$40..#$5F:
              HandleC1(Chr(Ord(Ch) + $40));
            ' ':
              FState := tsmsESC_SP;
            '#':
              FState := tsmsESC_Hash;
            '%':
              FState := tsmsESC_Percent;
            '(':
              begin
                FState := tsmsDesignateG0123CharacterSet94;
                FDesignatingCharacterSet := gcsG0;
              end;
            ')':
              begin
                FState := tsmsDesignateG0123CharacterSet94;
                FDesignatingCharacterSet := gcsG1;
              end;
            '*':
              begin
                FState := tsmsDesignateG0123CharacterSet94;
                FDesignatingCharacterSet := gcsG2;
              end;
            '+':
              begin
                FState := tsmsDesignateG0123CharacterSet94;
                FDesignatingCharacterSet := gcsG3;
              end;
            '-':
              begin
                FState := tsmsDesignateG0123CharacterSet96;
                FDesignatingCharacterSet := gcsG1;
              end;
            '.':
              begin
                FState := tsmsDesignateG0123CharacterSet96;
                FDesignatingCharacterSet := gcsG2;
              end;
            '/':
              begin
                FState := tsmsDesignateG0123CharacterSet96;
                FDesignatingCharacterSet := gcsG3;
              end;
            '7':
              begin
                SaveCursor;
                FState := tsmsInitial;
              end;
            '8':
              begin
                RestoreCursor;
                FState := tsmsInitial;
              end;
            'c':
              HardReset;
            'n':
              FGLCharacterSet := gcsG2;
            'o':
              FGLCharacterSet := gcsG3;
            '|':
              FGRCharacterSet := gcsG3;
            '}':
              FGRCharacterSet := gcsG2;
            '~':
              FGRCharacterSet := gcsG1;
            else
            begin
              { error }
              FLogger.LogMessage(vlWarning, 'Unhandled ESC sequence character: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsDesignateG0123CharacterSet94:
        begin
          case Ch of
            'A':
              begin
                FGCharacterSets[FDesignatingCharacterSet] := dcsBritishNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'B':
              begin
                FGCharacterSets[FDesignatingCharacterSet] := dcsUSASCII;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'C', '5':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsFinnishNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'H', '7':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsSwedishNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'K':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsGermanNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'Q', '9':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsFrenchCanadianNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'R', 'f':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsFrenchNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'Y':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsItalianNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'Z':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsSpanishNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '4':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDutchNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '=':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsSwissNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '`', 'E', '6':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsNorwegianDanishNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '0':
              begin
                FGCharacterSets[FDesignatingCharacterSet] := dcsDecSpecialGraphics;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '<':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecSupplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '>':
              begin
                if FDecConformanceLevel >= dclVT300 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecTechnical;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '%':
              FState := tsmsDesignateG0123CharacterSet94_Percent;
            '"':
              FState := tsmsDesignateG0123CharacterSet94_Quote;
            '&':
              FState := tsmsDesignateG0123CharacterSet94_Ampersand;
            #27:
              begin
                FState := tsmsESC;
                FDesignatingCharacterSet := gcsG0;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Designate G' + IntToStr(Ord(FDesignatingCharacterSet)) + ' Character Set not implemented: ' + Ch);
              FState := tsmsInitial;
              FDesignatingCharacterSet := gcsG0;
            end;
          end;
        end;
      tsmsDesignateG0123CharacterSet94_Percent:
        begin
          case Ch of
            '0':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecTurkish;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '2':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsTurkishNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '5':
              begin
                if FDecConformanceLevel >= dclVT300 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecSupplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '6':
              begin
                if FDecConformanceLevel >= dclVT300 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsPortugueseNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '=':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsHebrewNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            #27:
              begin
                FState := tsmsESC;
                FDesignatingCharacterSet := gcsG0;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Designate G' + IntToStr(Ord(FDesignatingCharacterSet)) + ' Character Set not implemented: %' + Ch);
              FState := tsmsInitial;
              FDesignatingCharacterSet := gcsG0;
            end;
          end;
        end;
      tsmsDesignateG0123CharacterSet94_Quote:
        begin
          case Ch of
            '>':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsGreekNRCS;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '4':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecHebrew;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            '?':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecGreek;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            #27:
              begin
                FState := tsmsESC;
                FDesignatingCharacterSet := gcsG0;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Designate G' + IntToStr(Ord(FDesignatingCharacterSet)) + ' Character Set not implemented: "' + Ch);
              FState := tsmsInitial;
              FDesignatingCharacterSet := gcsG0;
            end;
          end;
        end;
      tsmsDesignateG0123CharacterSet94_Ampersand:
        begin
          case Ch of
            '4':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsDecCyrillic;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            #27:
              begin
                FState := tsmsESC;
                FDesignatingCharacterSet := gcsG0;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Designate G' + IntToStr(Ord(FDesignatingCharacterSet)) + ' Character Set not implemented: &' + Ch);
              FState := tsmsInitial;
              FDesignatingCharacterSet := gcsG0;
            end;
          end;
        end;
      tsmsDesignateG0123CharacterSet96:
        begin
          case Ch of
            'A':
              begin
                if FDecConformanceLevel >= dclVT300 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsISOLatin1Supplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'B':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsISOLatin2Supplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'F':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsISOGreekSupplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'H':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsISOHebrewSupplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'L':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsISOLatinCyrillic;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            'M':
              begin
                if FDecConformanceLevel >= dclVT500 then
                  FGCharacterSets[FDesignatingCharacterSet] := dcsISOLatin5Supplemental;
                FState := tsmsInitial;
                FDesignatingCharacterSet := gcsG0;
              end;
            #27:
              begin
                FState := tsmsESC;
                FDesignatingCharacterSet := gcsG0;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Designate G' + IntToStr(Ord(FDesignatingCharacterSet)) + ' 96-Character Set not implemented: ' + Ch);
              FState := tsmsInitial;
              FDesignatingCharacterSet := gcsG0;
            end;
          end;
        end;
      tsmsESC_SP:
        begin
          case Ch of
            'F':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  Exclude(FModeFlags, tmfSend8BitC1Controls);
                FState := tsmsInitial;
              end;
            'G':
              begin
                if FDecConformanceLevel >= dclVT200 then
                  Include(FModeFlags, tmfSend8BitC1Controls);
                FState := tsmsInitial;
              end;
            #27:
              FState := tsmsESC;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Unhandled ESC SP sequence character: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsESC_Hash:
        begin
          case Ch of
            { DEC Screen Alignment Test (DECALN), VT100 }
            '8':
              begin
                for Y := 0 to FModel.Height - 1 do
                  for X := 0 to FModel.Width - 1 do
                    FModel.Cell[Y, X] := Cell('E', DefaultAttribute);
                FScrollingRegionTop := 0;
                FScrollingRegionBottom := FModel.Height - 1;
                FModel.SetCursorPos(CursorHomeX, CursorHomeY);
                FState := tsmsInitial;
              end;
            #27:
              FState := tsmsESC;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Unhandled ESC # sequence character: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsESC_Percent:
        begin
          case Ch of
            '@':
              begin
                Exclude(FModeFlags, tmfUTF8Mode);
                FState := tsmsInitial;
              end;
            'G':
              begin
                Include(FModeFlags, tmfUTF8Mode);
                FState := tsmsInitial;
              end;
            #27:
              FState := tsmsESC;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Unhandled ESC % sequence character: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsCSI:
        begin
          case Ch of
            C0_CR:
              HandleCR;
            C0_VT,
            C0_FF,
            C0_LF:
              HandleLF;
            C0_BS:
              HandleBS;
            { Parameter Byte }
            { 0123456789:;<=>? }
            #$30..#$3F:
              begin
                if FControlSequenceIntermediate <> '' then
                begin
                  { error }
                  FLogger.LogMessage(vlWarning, 'Invalid CSI control sequence parameter byte following after intermediate bytes: ' + FControlSequenceParameter + FControlSequenceIntermediate + Ch);
                  FState := tsmsInitial;
                  ClearControlSequence;
                end
                else
                  FControlSequenceParameter := FControlSequenceParameter + Ch;
              end;
            { Intermediate Byte }
            {  !"#$%&'()*+,-./ }
            #$20..#$2F:
              begin
                FControlSequenceIntermediate := FControlSequenceIntermediate + Ch;
              end;
            { Final Byte }
            //@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
            #$40..#$7E:
              begin
                FControlSequenceFinalByte := Ch;
                ExecuteControlSequence;
                if FDecConformanceLevel = dclVT52 then
                  FState := tsmsVT52_Initial
                else
                  FState := tsmsInitial;
                ClearControlSequence;
              end;
            else
            begin
              { error }
              FLogger.LogMessage(vlWarning, 'Unhandled CSI control sequence character: ' + FControlSequenceParameter + FControlSequenceIntermediate + Ch);
              FState := tsmsInitial;
              ClearControlSequence;
            end;
          end;
        end;
      tsmsOSC:
        begin
          case Ch of
            #8..#13,#$20..#$7E:
              FOperatingSystemCommand := FOperatingSystemCommand + Ch;
            C1_ST:
              begin
                ExecuteOSC;
                FOperatingSystemCommand := '';
                FState := tsmsInitial;
              end;
            C0_ESC:
              FState := tsmsOSC_ESC;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Invalid OSC control sequence character: ' + FOperatingSystemCommand + Ch);
              FOperatingSystemCommand := '';
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsOSC_ESC:
        begin
          case Ch of
            Chr(Ord(C1_ST) - $40):
              begin
                ExecuteOSC;
                FOperatingSystemCommand := '';
                FState := tsmsInitial;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Invalid OSC control sequence character: ' + FOperatingSystemCommand + #27 + Ch);
              FOperatingSystemCommand := '';
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsDCS:
        begin
          case Ch of
            #8..#13,#$20..#$7E:
              FDeviceControlString := FDeviceControlString + Ch;
            C1_ST:
              begin
                ExecuteDCS;
                FDeviceControlString := '';
                FState := tsmsInitial;
              end;
            C0_ESC:
              FState := tsmsDCS_ESC;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Invalid DCS control sequence character: ' + FDeviceControlString + Ch);
              FDeviceControlString := '';
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsDCS_ESC:
        begin
          case Ch of
            Chr(Ord(C1_ST) - $40):
              begin
                ExecuteDCS;
                FDeviceControlString := '';
                FState := tsmsInitial;
              end;
            else
            begin
              FLogger.LogMessage(vlWarning, 'Invalid DCS control sequence character: ' + FDeviceControlString + #27 + Ch);
              FDeviceControlString := '';
              FState := tsmsInitial;
            end;
          end;
        end;
      tsmsUTF8_1ByteLeft:
        begin
          case Ch of
            #%10000000..#%10111111:
              begin
                FUTF8_Build := (FUTF8_Build shl 6) or (Ord(Ch) and %111111);
                WriteUTF32Char(FUTF8_Build);
                FState := tsmsInitial;
              end;
            else
            begin
              { error }
              FLogger.LogMessage(vlWarning, 'Invalid UTF-8 continuation byte: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
              FUTF8_Build := 0;
            end;
          end;
        end;
      tsmsUTF8_2BytesLeft:
        begin
          case Ch of
            #%10000000..#%10111111:
              begin
                FUTF8_Build := (FUTF8_Build shl 6) or (Ord(Ch) and %111111);
                FState := tsmsUTF8_1ByteLeft;
              end;
            else
            begin
              { error }
              FLogger.LogMessage(vlWarning, 'Invalid UTF-8 continuation byte: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
              FUTF8_Build := 0;
            end;
          end;
        end;
      tsmsUTF8_3BytesLeft:
        begin
          case Ch of
            #%10000000..#%10111111:
              begin
                FUTF8_Build := (FUTF8_Build shl 6) or (Ord(Ch) and %111111);
                FState := tsmsUTF8_2BytesLeft;
              end;
            else
            begin
              { error }
              FLogger.LogMessage(vlWarning, 'Invalid UTF-8 continuation byte: #' + IntToStr(Ord(Ch)));
              FState := tsmsInitial;
              FUTF8_Build := 0;
            end;
          end;
        end;

      tsmsVT52_Initial:
        begin
          case Chr(Ord(Ch) and 127) of
            C0_CR:
              HandleCR;
            C0_LF:
              HandleLF;
            C0_BS:
              HandleBS;
            C0_HT:
              HandleHT;
            C0_ESC:
              FState := tsmsVT52_ESC;
            #32..#126:
              WriteVT52Char(Ch);
            else
              FLogger.LogMessage(vlWarning, 'Unhandled character (VT52 mode): #' + IntToStr(Ord(Ch)));
          end;
        end;
      tsmsVT52_ESC:
        begin
          case Chr(Ord(Ch) and 127) of
            { Exit VT52 mode (Enter VT100 mode). }
            '<':
              if TerminalType >= ttVT100 then
                LeaveVT52Mode;
            { Cursor up. }
            'A':
              begin
                FModel.SetCursorPos(FModel.CursorX, Max(FModel.CursorY - 1, 0));
                FNextCharacterWrapsToNextLine := False;
                FState := tsmsVT52_Initial;
              end;
            { Cursor down. }
            'B':
              begin
                FModel.SetCursorPos(FModel.CursorX, Min(FModel.CursorY + 1, FModel.Height - 1));
                FNextCharacterWrapsToNextLine := False;
                FState := tsmsVT52_Initial;
              end;
            { Cursor right. }
            'C':
              begin
                FModel.SetCursorPos(Min(FModel.CursorX + 1, FModel.Width - 1), FModel.CursorY);
                FNextCharacterWrapsToNextLine := False;
                FState := tsmsVT52_Initial;
              end;
            { Cursor left. }
            'D':
              begin
                FModel.SetCursorPos(Max(FModel.CursorX - 1, 0), FModel.CursorY);
                FNextCharacterWrapsToNextLine := False;
                FState := tsmsVT52_Initial;
              end;
            { Enter graphics mode. }
            'F':
              begin
                FVT52CharacterSet := v52csGraphics;
                FState := tsmsVT52_Initial;
              end;
            { Exit graphics mode. }
            'G':
              begin
                FVT52CharacterSet := v52csAscii;
                FState := tsmsVT52_Initial;
              end;
            { Move the cursor to the home position. }
            'H':
              begin
                FModel.SetCursorPos(0, 0);
                FNextCharacterWrapsToNextLine := False;
                FState := tsmsVT52_Initial;
              end;
            { Reverse line feed. }
            'I':
              begin
                HandleC1(C1_RI);
                FState := tsmsVT52_Initial;
              end;
            { Erase from the cursor to the end of the screen. }
            'J':
              begin
                ErasePageToBottom;
                FState := tsmsVT52_Initial;
              end;
            { Erase from the cursor to the end of the line. }
            'K':
              begin
                EraseLineToRight;
                FState := tsmsVT52_Initial;
              end;
            'Y':
              FState := tsmsVT52_ESC_Y;
            { Identify. }
            'Z':
              begin
                { "I am a VT52." }
                TransmitStr(#27'/Z');
              end;
            else
            begin
              { error }
              FLogger.LogMessage(vlWarning, 'Unhandled ESC sequence character (VT52 mode): #' + IntToStr(Ord(Ch)));
              FState := tsmsVT52_Initial;
            end;
          end;
        end;
      tsmsVT52_ESC_Y:
        begin
          case Chr(Ord(Ch) and 127) of
            #32..#127:
              begin
                FModel.SetCursorPos(FModel.CursorX, EnsureRange((Ord(Ch) and 127) - 32, 0, FModel.Height - 1));
                FState := tsmsVT52_ESC_Y_Ps;
              end;
            else
              FLogger.LogMessage(vlWarning, 'Unhandled ESC Y sequence character (VT52 mode): #' + IntToStr(Ord(Ch)));
          end;
        end;
      tsmsVT52_ESC_Y_Ps:
        begin
          case Chr(Ord(Ch) and 127) of
            #32..#127:
              begin
                FModel.SetCursorPos(EnsureRange((Ord(Ch) and 127) - 32, 0, FModel.Width - 1), FModel.CursorY);
                FState := tsmsVT52_Initial;
              end;
            else
              FLogger.LogMessage(vlWarning, 'Unhandled ESC Y Ps sequence character (VT52 mode): #' + IntToStr(Ord(Ch)));
          end;
        end;
    end;
  end;
  FModel.UpdateScreen;
end;

procedure TTerminalController.MaybeLocalEcho(const ks: rawbytestring);
begin
  if not (tmfSendReceiveMode in FModeFlags) then
  begin
    { todo: handle non-ASCII characters }
    if (Length(ks) = 1) and ((Ord(ks[1]) >= 32) and (Ord(ks[1]) <= 126)) then
      ReceiveData(ks[1], 1);
  end;
end;

procedure TTerminalController.HandleMouseEvent(const pdev: TPointingDeviceEvent);

  procedure SendX10MouseEvent(X, Y, Button: Integer; Press: Boolean);
  var
    PressChar: Char;
  begin
    if (X < 0) or (Y < 0) or (Button < 0) then
      exit;
    if Press then
      PressChar := 'M'
    else
      PressChar := 'm';
    case FMouseProtocolEncoding of
      tmpeX10:
        begin
          if ((32 + X) > 255) or ((32 + Y) > 255) or ((32 + Button) > 255) then
            exit;
          TransmitStr(EncodeReturnC1(C1_CSI) + PressChar + Chr(32 + Button) + Chr(32 + X) + Chr(32 + Y));
        end;
      tmpeUTF8:
        begin
          if (X > 2015) or (Y > 2015) or (Button > 2015) then
            exit;
          TransmitStr(EncodeReturnC1(C1_CSI) + PressChar + UTF8Encode(WideChar(32 + Button)) + UTF8Encode(WideChar(32 + X)) + UTF8Encode(WideChar(32 + Y)));
        end;
      tmpeSGR:
        TransmitStr(EncodeReturnC1(C1_CSI) + '<' + IntToStr(Button) + ';' + IntToStr(X) + ';' + IntToStr(Y) + PressChar);
    end;
  end;

var
  MouseX, MouseY: Integer;
  MouseMoved: Boolean;
  ButtonsPressed, ButtonsReleased: TPointingDeviceButtonState;

  procedure SendNormalEvent;
  const
    ButtonTranslation: array [pdButton1..pdButton11] of Integer = (0, 2, 1, 64, 65, 66, 67, 128, 129, 130, 131);
  var
    Mask: Byte = 0;
    B: TPointingDeviceButton;
  begin
    for B := Low(ButtonTranslation) to High(ButtonTranslation) do
      if B in ButtonsPressed then
        SendX10MouseEvent(MouseX + 1, MouseY + 1, ButtonTranslation[B] or Mask, True);
    for B := Low(ButtonTranslation) to High(ButtonTranslation) do
      if B in ButtonsReleased then
        SendX10MouseEvent(MouseX + 1, MouseY + 1, ButtonTranslation[B] or Mask, False);
    if (ButtonsPressed = []) and (ButtonsReleased = []) then
    begin
      if not MouseMoved then
        exit;
      if pdev.ButtonState = [] then
        SendX10MouseEvent(MouseX + 1, MouseY + 1, (32 + 3) or Mask, True)
      else
        for B := Low(ButtonTranslation) to High(ButtonTranslation) do
          if B in pdev.ButtonState then
          begin
            SendX10MouseEvent(MouseX + 1, MouseY + 1, ButtonTranslation[B] or 32 or Mask, True);
            break;
          end;
    end;
  end;

begin
  MouseX := pdev.X;
  MouseY := pdev.Y;
  MouseMoved := (MouseX <> FOldMouseX) or (MouseY <> FOldMouseY);
  ButtonsPressed := pdev.ButtonState - FOldMouseButtons;
  ButtonsReleased := FOldMouseButtons - pdev.ButtonState;

  case FMouseTrackingMode of
    tmtmX10:
      if ButtonsPressed <> [] then
      begin
        if pdButton1 in ButtonsPressed then
          SendX10MouseEvent(MouseX + 1, MouseY + 1, 0, True);
        if pdButton2 in ButtonsPressed then
          SendX10MouseEvent(MouseX + 1, MouseY + 1, 2, True);
        if pdButton3 in ButtonsPressed then
          SendX10MouseEvent(MouseX + 1, MouseY + 1, 1, True);
      end;
    tmtmNormal:
      if (ButtonsPressed <> []) or (ButtonsReleased <> []) then
        SendNormalEvent;
    tmtmButtonEvent:
      if (ButtonsPressed <> []) or (ButtonsReleased <> []) or (pdev.ButtonState <> []) then
        SendNormalEvent;
    tmtmAnyEvent:
      SendNormalEvent;
  end;

  FOldMouseX := MouseX;
  FOldMouseY := MouseY;
  FOldMouseButtons := pdev.ButtonState;
end;

function TTerminalController.EncodeReturnC1(Ch: TC1Char): string;
begin
  if tmfSend8BitC1Controls in FModeFlags then
    Result := Ch
  else
    Result := #27 + Chr(Ord(Ch) - $40);
end;

end.

