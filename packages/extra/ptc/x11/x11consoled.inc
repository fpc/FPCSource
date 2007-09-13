Type
  TX11Console = Class(TPTCBaseConsole)
  Private
    FX11Display : TX11Display;
    FTitle : String;
    FFlags : TX11Flags;
    FModes : Array Of TPTCMode;

    Procedure UpdateCursor;

    Function CreateDisplay : TX11Display; { Factory method }

    Function GetWidth : Integer; Override;
    Function GetHeight : Integer; Override;
    Function GetPitch : Integer; Override;
    Function GetArea : TPTCArea; Override;
    Function GetFormat : TPTCFormat; Override;
    Function GetPages : Integer; Override;
    Function GetName : String; Override;
    Function GetTitle : String; Override;
    Function GetInformation : String; Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Open(Const ATitle : String; APages : Integer = 0); Overload; Override;
    Procedure Open(Const ATitle : String; Const AFormat : TPTCFormat;
                   APages : Integer = 0); Overload; Override;
    Procedure Open(Const ATitle : String; AWidth, AHeight : Integer;
                   Const AFormat : TPTCFormat; APages : Integer = 0); Overload; Override;
    Procedure Open(Const ATitle : String; Const AMode : TPTCMode;
                   APages : Integer = 0); Overload; Override;
    Procedure Close; Override;

    Procedure Copy(Var ASurface : TPTCBaseSurface); Override;
    Procedure Copy(Var ASurface : TPTCBaseSurface;
                   Const ASource, ADestination : TPTCArea); Override;

    Procedure Load(Const APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette); Override;
    Procedure Load(Const APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette;
                   Const ASource, ADestination : TPTCArea); Override;
    Procedure Save(APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette); Override;
    Procedure Save(APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette;
                   Const ASource, ADestination : TPTCArea); Override;

    Function Lock : Pointer; Override;
    Procedure Unlock; Override;

    Procedure Clear; Override;
    Procedure Clear(Const AColor : TPTCColor); Override;
    Procedure Clear(Const AColor : TPTCColor;
                    Const AArea : TPTCArea); Override;

    Procedure Configure(Const AFileName : String); Override;
    Function Option(Const AOption : String) : Boolean; Override;

    Procedure Palette(Const APalette : TPTCPalette); Override;
    Procedure Clip(Const AArea : TPTCArea); Override;
    Function Clip : TPTCArea; Override;
    Function Palette : TPTCPalette; Override;
    Function Modes : PPTCMode; Override;

    Procedure Flush; Override;
    Procedure Finish; Override;
    Procedure Update; Override;
    Procedure Update(Const AArea : TPTCArea); Override;

    Function NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean; Override;
    Function PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent; Override;
  End;
