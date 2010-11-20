{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

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

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$MODE objfpc}{$H+}
{$ASMMODE intel}

unit VESA;

interface

type
  TVesaModeInfoBlock = packed record
    {Mandatory information for all VBE revisions}
    ModeAttributes: Word;        {mode attributes}
    WinAAttributes: Byte;        {window A attributes}
    WinBAttributes: Byte;        {window B attributes}
    WinGranularity: Word;        {window granularity}
    WinSize: Word;               {window size}
    WinASegment: Word;           {window A start segment}
    WinBSegment: Word;           {window B start segment}
    WinFuncPtr: DWord;           {real mode pointer to window function}
    BytesPerScanLine: Word;      {bytes per scan line}

    {Mandatory information for VBE 1.2 and above}
    XResolution: Word;           {horizontal resolution in pixels or characters}
    YResolution: Word;           {vertical resolution in pixels or characters}
    XCharSize: Byte;             {character cell width in pixels}
    YCharSize: Byte;             {character cell height in pixels}
    NumberOfPlanes: Byte;        {number of memory planes}
    BitsPerPixel: Byte;          {bits per pixel}
    NumberOfBanks: Byte;         {number of banks}
    MemoryModel: Byte;           {memory model type}
    BankSize: Byte;              {bank size in KB}
    NumberOfImagePages: Byte;    {number of images}
    Reserved: Byte;{=1}          {reserved for page function}

    {Direct color fields (required for direct/6 and YUV/7 memory models)}
    RedMaskSize: Byte;           {size of direct color red mask in bits}
    RedFieldPosition: Byte;      {bit position of lsb of red mask}
    GreenMaskSize: Byte;         {size of direct color green mask in bits}
    GreenFieldPosition: Byte;    {bit position of lsb of green mask}
    BlueMaskSize: Byte;          {size of direct color blue mask in bits}
    BlueFieldPosition: Byte;     {bit position of lsb of blue mask}
    RsvdMaskSize: Byte;          {size of direct color reserved mask in bits}
    RsvdFieldPosition: Byte;     {bit position of lsb of reserved mask}
    DirectColorModeInfo: Byte;   {direct color mode attributes}

    {Mandatory information for VBE 2.0 and above}
    PhysBasePtr: DWord;          {physical address for flat memory frame buffer}
    Reserved2: DWord;{=0}        {Reserved - always set to 0}
    Reserved3: Word;{=0}         {Reserved - always set to 0}

    {Mandatory information for VBE 3.0 and above}
    LinBytesPerScanLine: Word;   {bytes per scan line for linear modes}
    BnkNumberOfImagePages: Byte; {number of images for banked modes}
    LinNumberOfImagePages: Byte; {number of images for linear modes}
    LinRedMaskSize: Byte;        {size of direct color red mask (linear modes)}
    LinRedFieldPosition: Byte;   {bit position of lsb of red mask (linear modes)}
    LinGreenMaskSize: Byte;      {size of direct color green mask (linear modes)}
    LinGreenFieldPosition: Byte; {bit position of lsb of green mask (linear modes)}
    LinBlueMaskSize: Byte;       {size of direct color blue mask (linear modes)}
    LinBlueFieldPosition: Byte;  {bit position of lsb of blue mask (linear modes)}
    LinRsvdMaskSize: Byte;       {size of direct color reserved mask (linear modes)}
    LinRsvdFieldPosition: Byte;  {bit position of lsb of reserved mask (linear modes)}
    MaxPixelClock: DWord;        {maximum pixel clock (in Hz) for graphics mode}

    Reserved4: array [1..189] of Byte; {remainder of ModeInfoBlock}
  end;

  TLogProcedure = procedure(const S: string);

  TVBEModeMemoryModel =
   (vmmmTextMode,
    vmmmCGAGraphics,
    vmmmHerculesGraphics,
    vmmmPlanar,
    vmmmPackedPixel,
    vmmmNonChain4_256Color,
    vmmmDirectColor,
    vmmmYUV,
    vmmmUnknownVESADefined,
    vmmmUnknownOEMDefined);

  TVBEFBWindow = class
  private
    FWindowID: Integer;
    FRelocatable: Boolean;
    FReadable: Boolean;
    FWritable: Boolean;
    FGranularity: Integer;
    FSize: Integer;
    FSegment: Word;

    function GetSupported: Boolean;
  public
    constructor Create(AWindowID: Integer; AAttributes: Byte; AGranularity, ASize, ASegment: Word);

    property WindowID: Integer read FWindowID;
    property Relocatable: Boolean read FRelocatable;
    property Readable: Boolean read FReadable;
    property Writable: Boolean read FWritable;
    property Granularity: Integer read FGranularity;
    property Size: Integer read FSize;
    property Segment: Word read FSegment;

    property Supported: Boolean read GetSupported;
  end;

  TVBEMode = class
  private
    FVBEModeID: DWord;
    
    FSupported: Boolean;
    FSupportsTTY: Boolean;
    FIsColor: Boolean;
    FIsGraphics: Boolean;
    FIsVGA: Boolean;
    FSupportsWindowed: Boolean;
    FSupportsLFB: Boolean;
    FSupportsDoubleScan: Boolean;
    FSupportsInterlaced: Boolean;
    FSupportsTripleBuffering: Boolean;
    FSupportsStereoscopicDisplay: Boolean;
    FSupportsDualDisplayStartAddresses: Boolean;
    
    FXResolution: Integer;
    FYResolution: Integer;
    FXCharSize: Integer;
    FYCharSize: Integer;
    FMemoryModel: TVBEModeMemoryModel;

    FBitsPerPixel: Integer;

    FNumberOfPlanes: Integer;

    FNumberOfBanks: Integer;
    FBankSize: Integer;

    FWindowA: TVBEFBWindow;
    FWindowB: TVBEFBWindow;

    FReadWindow: TVBEFBWindow;
    FWriteWindow: TVBEFBWindow;

    FPhysBasePtr: DWord;

    FWindowedBytesPerScanLine: Integer;
    FWindowedNumberOfImagePages: Integer;
    FWindowedRedMaskSize: Integer;
    FWindowedRedFieldPosition: Integer;
    FWindowedGreenMaskSize: Integer;
    FWindowedGreenFieldPosition: Integer;
    FWindowedBlueMaskSize: Integer;
    FWindowedBlueFieldPosition: Integer;
    FWindowedReservedMaskSize: Integer;
    FWindowedReservedFieldPosition: Integer;

    FLFBBytesPerScanLine: Integer;
    FLFBNumberOfImagePages: Integer;
    FLFBRedMaskSize: Integer;
    FLFBRedFieldPosition: Integer;
    FLFBGreenMaskSize: Integer;
    FLFBGreenFieldPosition: Integer;
    FLFBBlueMaskSize: Integer;
    FLFBBlueFieldPosition: Integer;
    FLFBReservedMaskSize: Integer;
    FLFBReservedFieldPosition: Integer;
  public
    constructor Create(AModeID: DWord; const AModeInfoBlock: TVesaModeInfoBlock);
    destructor Destroy; override;

    property VBEModeID: DWord read FVBEModeID;

    property Supported: Boolean read FSupported;
    property SupportsTTY: Boolean read FSupportsTTY;
    property IsColor: Boolean read FIsColor;
    property IsGraphics: Boolean read FIsGraphics;
    property IsVGA: Boolean read FIsVGA;
    property SupportsWindowed: Boolean read FSupportsWindowed;
    property SupportsLFB: Boolean read FSupportsLFB;
    property SupportsDoubleScan: Boolean read FSupportsDoubleScan;
    property SupportsInterlaced: Boolean read FSupportsInterlaced;
    property SupportsTripleBuffering: Boolean read FSupportsTripleBuffering;
    property SupportsStereoscopicDisplay: Boolean read FSupportsStereoscopicDisplay;
    property SupportsDualDisplayStartAddresses: Boolean read FSupportsDualDisplayStartAddresses;

    property XResolution: Integer read FXResolution;
    property YResolution: Integer read FYResolution;
    property XCharSize: Integer read FXCharSize;
    property YCharSize: Integer read FYCharSize;
    property MemoryModel: TVBEModeMemoryModel read FMemoryModel;

    property BitsPerPixel: Integer read FBitsPerPixel;
    
    property NumberOfPlanes: Integer read FNumberOfPlanes;
    
    property NumberOfBanks: Integer read FNumberOfBanks;
    property BankSize: Integer read FBankSize;

    property WindowA: TVBEFBWindow read FWindowA;
    property WindowB: TVBEFBWindow read FWindowB;

    property ReadWindow: TVBEFBWindow read FReadWindow;
    property WriteWindow: TVBEFBWindow read FWriteWindow;

    property PhysBasePtr: DWord read FPhysBasePtr;

    property WindowedBytesPerScanLine: Integer read FWindowedBytesPerScanLine;
    property WindowedNumberOfImagePages: Integer read FWindowedNumberOfImagePages;
    property WindowedRedMaskSize: Integer read FWindowedRedMaskSize;
    property WindowedRedFieldPosition: Integer read FWindowedRedFieldPosition;
    property WindowedGreenMaskSize: Integer read FWindowedGreenMaskSize;
    property WindowedGreenFieldPosition: Integer read FWindowedGreenFieldPosition;
    property WindowedBlueMaskSize: Integer read FWindowedBlueMaskSize;
    property WindowedBlueFieldPosition: Integer read FWindowedBlueFieldPosition;
    property WindowedReservedMaskSize: Integer read FWindowedReservedMaskSize;
    property WindowedReservedFieldPosition: Integer read FWindowedReservedFieldPosition;

    property LFBBytesPerScanLine: Integer read FLFBBytesPerScanLine;
    property LFBNumberOfImagePages: Integer read FLFBNumberOfImagePages;
    property LFBRedMaskSize: Integer read FLFBRedMaskSize;
    property LFBRedFieldPosition: Integer read FLFBRedFieldPosition;
    property LFBGreenMaskSize: Integer read FLFBGreenMaskSize;
    property LFBGreenFieldPosition: Integer read FLFBGreenFieldPosition;
    property LFBBlueMaskSize: Integer read FLFBBlueMaskSize;
    property LFBBlueFieldPosition: Integer read FLFBBlueFieldPosition;
    property LFBReservedMaskSize: Integer read FLFBReservedMaskSize;
    property LFBReservedFieldPosition: Integer read FLFBReservedFieldPosition;
  end;

{  PModeInfo = ^TModeInfo;
  TModeInfo = record
    ModeNumber: DWord;
    VesaModeInfo: TVesaModeInfoBlock;
  end;}

const
//  TryLFBDefault = true;
  TryDPMI508hDefault = true;
  TryNearPtrDefault = false;
  ScanModesManuallyDefault = false;

var
{  ModeInfo: PModeInfo;}
  VBEModes: array of TVBEMode;
{  NrOfModes: Integer;}
  VBEPresent: Boolean;
  LFBUsed: Boolean;
  LogProcedure: TLogProcedure = nil;

//  TryLFB: Boolean = TryLFBDefault;
  TryDPMI508h: Boolean = TryDPMI508hDefault;
  TryNearPtr: Boolean = TryNearPtrDefault;
  ScanModesManually: Boolean = ScanModesManuallyDefault;
  
  EightBitDACEnabled: Boolean = true;

procedure InitVESA;
function SetVESAMode(M: Integer; AUseLFB: Boolean): Boolean;
procedure RestoreTextMode;
procedure WriteToVideoMemory(Src: Pointer; Dest: DWord; Size: DWord);
procedure SetPalette(Palette: Pointer; First, Num: Integer);
procedure GetPalette(Palette: Pointer; First, Num: Integer);
procedure SetDisplayStart(X, Y: Word; WaitRetrace: Boolean);
procedure WaitRetraceSinglePage;
function MakeMask(MaskSize, FieldPosition: Integer): DWord;
function LFBNearPtrAccessAvailable: Boolean;
function LFBNearPtrAccessPtr: Pointer;

implementation

uses
  go32fix;

type
  TVBEInfoBlock = packed record
    {VBE 1.0+}
    VBESignature: array [1..4] of Char; {'VESA'}
    VBEVersion: Word;
    OemStringPtr: DWord; {VbeFarPtr to OEM string}
    Capabilities: DWord; {Capabilities of graphics controller}
    VideoModePtr: DWord; {VbeFarPtr to VideoModeList}
    {added for VBE 1.1+}
    TotalMemory: Word; {Number of 64kb memory blocks}
    {added for VBE 2.0+}
    OemSoftwareRev: Word; {VBE implementation Software revision}
    OemVendorNamePtr: DWord; {VbeFarPtr to Vendor Name string}
    OemProductNamePtr: DWord; {VbeFarPtr to Product Name string}
    OemProductRevPtr: DWord; {VbeFarPtr to Product Revision string}
    Reserved: array [1..222] of Byte; {Reserved for VBE implementation scratch area}
    OemData: array [1..256] of Char; {Data Area for OEM Strings}
  end;
  TVideoModeList = array of Word;

var
  VBEInfoBlock: TVBEInfoBlock;
  VideoModeList: TVideoModeList;
  VideoMemory: DWord;
  EightBitDACSupported: Boolean;
  nonVGA: Boolean;
  SnowyRAMDAC: Boolean;
  StereoSignalingSupport: Boolean;
  StereoSignalingVesaEVC: Boolean;
  OEMString: string;
  OEMVendorName: string;
  OEMProductName: string;
  OEMProductRev: string;
  OEMSoftwareRev: Integer;
  CurrentMode: TVBEMode = nil;

  RealModePaletteSel: Word;
  RealModePaletteSeg: Word;
  SetPaletteHW: Boolean;
  PaletteDACbits: Integer;

  ReadWindow, WriteWindow: Integer;
  ReadWindowStart, WriteWindowStart: Integer;
  ReadWindowAddress, WriteWindowAddress: Integer;
  WindowGranularity: DWord;
  WindowSize, WindowSizeG: DWord;

  DPMIPageSize: DWord;

  LFBPhysicalAddress: DWord;
  LFBBufferSize: DWord;

  LFB0508AllocatedMemoryBlock: Pointer;
  LFB0508MemoryBlockPadding: DWord;
  LFB0508MappedVideoBufferStart: Pointer;
  LFB0508NumberOfPagesMapped: Integer;
  LFB0508Mapped: Boolean = false;

  LFB0800LinearAddress: DWord;
  LFB0800LinearAddressMapped: Boolean = false;

  LFBSegmentSelector: Word = 0;

  VESAInit: Boolean;

procedure Debugln(const S: string);
begin
  if Assigned(LogProcedure) then
    LogProcedure(S);
end;

procedure Debugln;
begin
  Debugln('');
end;

function IntToStr(Value: Integer): string;
begin
  System.Str(Value, Result);
end;

function IntToStr(Value: Int64): string;
begin
  System.Str(Value, Result);
end;

function IntToStr(Value: QWord): string;
begin
  System.Str(Value, Result);
end;

function BoolToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function CheckVBEStatus(AX: Word): Boolean;
begin
  if AX = $004F then
  begin
    Result := true;
  end
  else
  begin
    Result := false;
    Debugln('VBE returned error status (AX=$' + HexStr(AX, 4) + ')');
  end;
end;

constructor TVBEFBWindow.Create(AWindowID: Integer; AAttributes: Byte; AGranularity, ASize, ASegment: Word);
begin
  FWindowID := AWindowID;

  FRelocatable := (AAttributes and 1) <> 0;
  FReadable := (AAttributes and 2) <> 0;
  FWritable := (AAttributes and 4) <> 0;

  FGranularity := AGranularity;
  FSize := ASize;
  FSegment := ASegment;
end;

function TVBEFBWindow.GetSupported: Boolean;
begin
  Result := (FReadable or FWritable) and ((FSegment <> 0) or FRelocatable);
end;

constructor TVBEMode.Create(AModeID: DWord; const AModeInfoBlock: TVesaModeInfoBlock);
begin
  FVBEModeID := AModeID;

  FSupported                         := (AModeInfoBlock.ModeAttributes and 1) <> 0;
  FSupportsTTY                       := (AModeInfoBlock.ModeAttributes and 4) <> 0;
  FIsColor                           := (AModeInfoBlock.ModeAttributes and 8) <> 0;
  FIsGraphics                        := (AModeInfoBlock.ModeAttributes and 16) <> 0;
  FIsVGA                             := (AModeInfoBlock.ModeAttributes and 32) = 0;
  FSupportsWindowed                  := (AModeInfoBlock.ModeAttributes and 64) = 0;
  FSupportsLFB                       := (AModeInfoBlock.ModeAttributes and 128) <> 0;
  FSupportsDoubleScan                := (AModeInfoBlock.ModeAttributes and 256) <> 0;
  FSupportsInterlaced                := (AModeInfoBlock.ModeAttributes and 512) <> 0;
  FSupportsTripleBuffering           := (AModeInfoBlock.ModeAttributes and 1024) <> 0;
  FSupportsStereoscopicDisplay       := (AModeInfoBlock.ModeAttributes and 2048) <> 0;
  FSupportsDualDisplayStartAddresses := (AModeInfoBlock.ModeAttributes and 4096) <> 0;

  if (AModeInfoBlock.ModeAttributes and 2) = 0 then
    FSupported := false;

  FXResolution := AModeInfoBlock.XResolution;
  FYResolution := AModeInfoBlock.YResolution;
  FXCharSize := AModeInfoBlock.XCharSize;
  FYCharSize := AModeInfoBlock.YCharSize;

  case AModeInfoBlock.MemoryModel of
    0: FMemoryModel := vmmmTextMode;
    1: FMemoryModel := vmmmCGAGraphics;
    2: FMemoryModel := vmmmHerculesGraphics;
    3: FMemoryModel := vmmmPlanar;
    4: FMemoryModel := vmmmPackedPixel;
    5: FMemoryModel := vmmmNonChain4_256Color;
    6: FMemoryModel := vmmmDirectColor;
    7: FMemoryModel := vmmmYUV;
    8..15: FMemoryModel := vmmmUnknownVESADefined;
    else
      FMemoryModel := vmmmUnknownOEMDefined;
  end;

  FBitsPerPixel := AModeInfoBlock.BitsPerPixel;

  FNumberOfPlanes := AModeInfoBlock.NumberOfPlanes;

  FNumberOfBanks := AModeInfoBlock.NumberOfBanks;
  FBankSize := AModeInfoBlock.BankSize;

  if FSupportsWindowed then
  begin
    FWindowA := TVBEFBWindow.Create(0, AModeInfoBlock.WinAAttributes, AModeInfoBlock.WinGranularity, AModeInfoBlock.WinSize, AModeInfoBlock.WinASegment);
    FWindowB := TVBEFBWindow.Create(1, AModeInfoBlock.WinBAttributes, AModeInfoBlock.WinGranularity, AModeInfoBlock.WinSize, AModeInfoBlock.WinBSegment);

    FReadWindow := nil;
    FWriteWindow := nil;
    if FWindowA.Supported then
    begin
      if FWindowA.Readable then
        FReadWindow := FWindowA;
      if FWindowA.Writable then
        FWriteWindow := FWindowA;
    end;
    if FWindowB.Supported then
    begin
      if FWindowB.Readable then
        FReadWindow := FWindowB;
      if FWindowB.Writable then
        FWriteWindow := FWindowB;
    end;
    if (FReadWindow = nil) or (FWriteWindow = nil) then
      FSupportsWindowed := false;
  end;

  if (not FSupportsWindowed) and (not FSupportsLFB) then
    FSupported := false;

  FPhysBasePtr := AModeInfoBlock.PhysBasePtr;

  FWindowedBytesPerScanLine      := AModeInfoBlock.BytesPerScanLine;
  FWindowedNumberOfImagePages    := AModeInfoBlock.BnkNumberOfImagePages;
  FWindowedRedMaskSize           := AModeInfoBlock.RedMaskSize;
  FWindowedRedFieldPosition      := AModeInfoBlock.RedFieldPosition;
  FWindowedGreenMaskSize         := AModeInfoBlock.GreenMaskSize;
  FWindowedGreenFieldPosition    := AModeInfoBlock.GreenFieldPosition;
  FWindowedBlueMaskSize          := AModeInfoBlock.BlueMaskSize;
  FWindowedBlueFieldPosition     := AModeInfoBlock.BlueFieldPosition;
  FWindowedReservedMaskSize      := AModeInfoBlock.RsvdMaskSize;
  FWindowedReservedFieldPosition := AModeInfoBlock.RsvdFieldPosition;

  FLFBBytesPerScanLine           := AModeInfoBlock.LinBytesPerScanLine;
  FLFBNumberOfImagePages         := AModeInfoBlock.LinNumberOfImagePages;
  FLFBRedMaskSize                := AModeInfoBlock.LinRedMaskSize;
  FLFBRedFieldPosition           := AModeInfoBlock.LinRedFieldPosition;
  FLFBGreenMaskSize              := AModeInfoBlock.LinGreenMaskSize;
  FLFBGreenFieldPosition         := AModeInfoBlock.LinGreenFieldPosition;
  FLFBBlueMaskSize               := AModeInfoBlock.LinBlueMaskSize;
  FLFBBlueFieldPosition          := AModeInfoBlock.LinBlueFieldPosition;
  FLFBReservedMaskSize           := AModeInfoBlock.LinRsvdMaskSize;
  FLFBReservedFieldPosition      := AModeInfoBlock.LinRsvdFieldPosition;
end;

destructor TVBEMode.Destroy;
begin
  FWindowA.Free;
  FWindowB.Free;
  inherited;
end;

procedure StandardMode(ModeNumber: DWord; var ModeInfo: TVesaModeInfoBlock);
const
  StandardModes: array [$100..$10C, 1..7] of Integer = (
  {(XResolution, YResolution, XCharSize, YCharSize, NumberOfPlanes, BitsPerPixel, MemoryModel),}
  ( 640,  400, 8, 16, 1, 8, 4),   { 100 640x400x256 }
  ( 640,  480, 8, 16, 1, 8, 4),   { 101 640x480x256 }
  ( 800,  600, 8, 16, 4, 4, 3),   { 102 800x600x16 }
  ( 800,  600, 8, 16, 1, 8, 4),   { 103 800x600x256 }
  (1024,  768, 8, 16, 4, 4, 3),   { 104 1024x768x16 }
  (1024,  768, 8, 16, 1, 8, 4),   { 105 1024x768x256 }
  (1280, 1024, 8, 16, 4, 4, 3),   { 106 1280x1024x16 }
  (1280, 1024, 8, 16, 1, 8, 4),   { 107 1280x1024x256 }
  (  80,   60, 8, 16, 4, 4, 0),   { 108 80x60t }
  ( 132,   25, 8, 16, 4, 4, 0),   { 109 132x25t }
  ( 132,   43, 8, 16, 4, 4, 0),   { 10A 132x43t }
  ( 132,   50, 8, 16, 4, 4, 0),   { 10B 132x50t }
  ( 132,   60, 8, 16, 4, 4, 0));  { 10C 132x60t }
{
10D 320x200x32k
10E 320x200x64k
10F 320x200x16.8m
110 640x480x32k
111 640x480x64k
112 640x480x16.8m
113 800x600x32k
114 800x600x64k
115 800x600x16.8m
116 1024x768x32k
117 1024x768x64k
118 1024x768x16.8m
119 1280x1024x32k
11A 1280x1024x64k
11B 1280x1024x16.8m
}
begin
  with ModeInfo do
  begin
    ModeAttributes := ModeAttributes or 2;
    if ModeNumber = $6A then
      ModeNumber := $102;
    case ModeNumber of
      $100..$10C: begin
        XResolution := StandardModes[ModeNumber, 1];
        YResolution := StandardModes[ModeNumber, 2];
        XCharSize := StandardModes[ModeNumber, 3];
        YCharSize := StandardModes[ModeNumber, 4];
        NumberOfPlanes := StandardModes[ModeNumber, 5];
        BitsPerPixel := StandardModes[ModeNumber, 6];
        MemoryModel := StandardModes[ModeNumber, 7];
      end;
      {todo:10D..11B}
      else
        ModeAttributes := ModeAttributes and $FFFD;
    end;
//        NumberOfImagePages := 0;{...}
  end;
end;

function bcd(q: Integer): Integer;
begin
  q := q and $FF;
  if ((q and $F) < 10) and ((q shr 4) < 10) then
    bcd := (q and $F) + (q shr 4) * 10
  else
    bcd := q;
end;

procedure DisposeRealModePalette;
begin
  if RealModePaletteSel = 0 then
    exit;
  global_dos_free(RealModePaletteSel);
  RealModePaletteSel := 0;
  RealModePaletteSeg := 0;
end;

procedure AllocateRealModePalette;
var
  Addr: DWord;
begin
  DisposeRealModePalette;
  Addr := global_dos_alloc(256*4);
  RealModePaletteSeg := Addr shr 16;
  RealModePaletteSel := Addr and $FFFF;
end;

procedure SetPalette2(Palette: Pointer; Num: Integer); assembler; register;
asm
  push es

  cld
  mov esi, Palette
  mov ecx, Num
  mov ax, fs
  mov es, ax
  movzx edi, word [RealModePaletteSeg]
  shl edi, 4
  mov edx, 0003F3F3Fh

@@1:
  lodsd

  shr eax, 2 {convert 8->6bit}
  and eax, edx

  stosd
  dec ecx
  jnz @@1

  pop es
end;

procedure SetPalette3(Palette: Pointer; Num: Integer); assembler; register;
asm
  push es

  cld
  mov esi, Palette
  mov ecx, Num
  mov ax, fs
  mov es, ax
  movzx edi, word [RealModePaletteSeg]
  shl edi, 4
  mov edx, 0007F7F7Fh

@@1:
  lodsd

  shr eax, 1 {convert 8->7bit}
  and eax, edx

  stosd
  dec ecx
  jnz @@1

  pop es
end;

procedure SetPaletteHW6(Palette: Pointer; First, Num: Integer);
var
  p: PDWord;
  c: DWord;
begin
  p := PDWord(Palette);
  outportb($3C8, First);
  while Num > 0 do
  begin
    c := (p^ shr 2) and $3F3F3F;
    outportb($3C9, Byte(c shr 16));
    outportb($3C9, Byte(c shr 8));
    outportb($3C9, Byte(c));

    Inc(p);
    Dec(Num);
  end;
end;

procedure SetPaletteHW7(Palette: Pointer; First, Num: Integer);
var
  p: PDWord;
  c: DWord;
begin
  p := PDWord(Palette);
  outportb($3C8, First);
  while Num > 0 do
  begin
    c := (p^ shr 1) and $7F7F7F;
    outportb($3C9, Byte(c shr 16));
    outportb($3C9, Byte(c shr 8));
    outportb($3C9, Byte(c));

    Inc(p);
    Dec(Num);
  end;
end;

procedure SetPaletteHW8(Palette: Pointer; First, Num: Integer);
var
  p: PDWord;
begin
  p := PDWord(Palette);
  outportb($3C8, First);
  while Num > 0 do
  begin
    outportb($3C9, Byte(p^ shr 16));
    outportb($3C9, Byte(p^ shr 8));
    outportb($3C9, Byte(p^));

    Inc(p);
    Dec(Num);
  end;
end;

procedure SetPalette(Palette: Pointer; First, Num: Integer);
var
  RealRegs: TRealRegs;
begin
  if SetPaletteHW then
  begin
    case PaletteDACbits of
      8: SetPaletteHW8(Palette, First, Num);
      7: SetPaletteHW7(Palette, First, Num);
      6: SetPaletteHW6(Palette, First, Num);
    end;
  end
  else
  begin
    if PaletteDACbits = 8 then
      dosmemput(RealModePaletteSeg, 0, Palette^, Num * 4) {8bits}
    else
      if PaletteDACbits = 7 then
        SetPalette3(Palette, Num) {7bits}
      else
        SetPalette2(Palette, Num); {6bits}
    RealRegs.ax := $4F09;
    RealRegs.bl := 0;
    RealRegs.cx := Num;
    RealRegs.dx := First;
    RealRegs.es := RealModePaletteSeg;
    RealRegs.di := 0;
    realintr($10, RealRegs);
  end;
end;

procedure GetPalette(Palette: Pointer; First, Num: Integer);
var
  RealRegs: TRealRegs;
begin
  RealRegs.ax := $4F09;
  RealRegs.bl := 1;
  RealRegs.cx := Num;
  RealRegs.dx := First;
  RealRegs.es := RealModePaletteSeg;
  RealRegs.di := 0;
  realintr($10, RealRegs);
  {...}
end;

procedure SwitchTo8bitDAC;
var
  RealRegs: TRealRegs;
begin
  Debugln('Trying to switch to 8-bit DAC');
  RealRegs.ax := $4F08;
  RealRegs.bl := 0;
  RealRegs.bh := 8;
  realintr($10, RealRegs);
  if not CheckVBEStatus(RealRegs.ax) then
  begin
    Debugln('Switching to 8-bit DAC failed');
    exit;
  end;
  PaletteDACbits := RealRegs.bh;
  Debugln('DAC switched to ' + IntToStr(PaletteDACbits) + ' bits');
  if PaletteDACbits < 6 then
  begin
    Debugln('DAC switched to less than 6 bits?! All VBE video cards should support at least 6 bits DAC width!!!');
    Debugln('We''re assuming that the VBE BIOS is buggy and that we got a bogus value and the DAC is actually in 6-bits mode!!!');
    Debugln('If it looks wrong, try the ''no8bitdac'' option in the ptcpas.cfg file.');
    PaletteDACbits := 6;
  end;
end;

function MakeMask(MaskSize, FieldPosition: Integer): DWord;
var
  Mask: DWord;
  I: Integer;
begin
  Mask := 1 shl FieldPosition;
  for I := 2 to MaskSize do
    Mask := Mask or (Mask shl 1);
  MakeMask := Mask;
end;

function GetRMString(SegOfs: DWord): string;
var
  S: string;
  C: Char;
  Seg, Ofs: Word;
begin
  if SegOfs = 0 then
  begin
    GetRMString := '';
    exit;
  end;
  S := '';
  Ofs := SegOfs and $FFFF;
  Seg := SegOfs shr 16;
  repeat
    dosmemget(Seg, Ofs, C, 1);
    if C <> #0 then
    begin
      S := S + C;
      if Ofs = $FFFF then
      begin
        Ofs := 0;
        Inc(Seg, $1000);
      end
      else
        Inc(Ofs);
    end;
  until C = #0;
  Result := S;
end;

procedure SetWriteWindowStart(WinPos: DWord);
var
  RealRegs: TRealRegs;
begin
  RealRegs.ax := $4F05;
  RealRegs.bx := WriteWindow;
  RealRegs.dx := WinPos;
  realintr($10, RealRegs);
end;

procedure VGAWaitRetrace;
begin
  while (inportb($3DA) and 8) <> 0 do;
  while (inportb($3DA) and 8) = 0 do;
end;

{ (X <> 0) or (Y <> 0) requires VBE 1.1+ }
procedure SetDisplayStart(X, Y: Word; WaitRetrace: Boolean);
var
  RealRegs: TRealRegs;
begin
  RealRegs.ax := $4F07;
  RealRegs.bx := $0000;
  if WaitRetrace then
    if VBEInfoBlock.VBEVersion >= $0200 then
      RealRegs.bx := $0080
    else
    begin
      VGAWaitRetrace;
      if VBEInfoBlock.VBEVersion < $0101 then
        exit;  { VBE 1.0 does not support function 07h - set display start }
    end;
  RealRegs.cx := X;
  RealRegs.dx := Y;
  realintr($10, RealRegs);
end;

procedure WaitRetraceSinglePage;
var
  RealRegs: TRealRegs;
begin
  if (VBEInfoBlock.VBEVersion >= $0200) and (not CurrentMode.IsVGA) then
  begin
    RealRegs.ax := $4F07;
    RealRegs.bx := $0080;
    RealRegs.cx := 0;
    RealRegs.dx := 0;
    realintr($10, RealRegs);
  end
  else
    VGAWaitRetrace;
end;

procedure WriteToVideoMemoryLFB(Src: Pointer; Dest: DWord; Size: DWord);
begin
  asm
    push es
    mov esi, Src
    mov edi, Dest
    mov ax, LFBSegmentSelector
    mov es, ax
    mov ecx, Size
    shr ecx, 2
    cld
    rep movsd
    mov ecx, Size
    and ecx, 3
    jz @@1
    rep movsb
@@1:
    pop es
  end ['EAX', 'ECX', 'ESI', 'EDI'];
end;

procedure WriteToVideoMemory(Src: Pointer; Dest: DWord; Size: DWord);
var
  WW: Integer;
  ToDo: Integer;
begin
  if LFBUsed then
  begin
    WriteToVideoMemoryLFB(Src, Dest, Size);
    exit;
  end;
  
  WW := Dest div WindowGranularity;
  Dest := Dest mod WindowGranularity;
{  Writeln(WindowSize);}
  while Size > 0 do
  begin
{    Write(WW, ' ');}
    SetWriteWindowStart(WW);
    ToDo := WindowSize - Dest;
    if Size < ToDo then
      ToDo := Size;
    asm
      push es
      mov esi, Src
      mov edi, Dest
      add edi, WriteWindowAddress
      mov ax, fs
      mov es, ax
      mov ecx, ToDo
      shr ecx, 2
      cld
      rep movsd
      mov ecx, ToDo
      and ecx, 3
      jz @@1
      rep movsb
@@1:
      pop es
    end ['EAX', 'ECX', 'ESI', 'EDI'];
    Dest := 0;
    Inc(WW, WindowSizeG);
{    Inc(WW);}
    Inc(Src, ToDo);
    Dec(Size, ToDo);
  end;
end;

function WinAttrib(q: Integer): string;
begin
  if (q and 1) <> 0 then
    Result := 'supported'
  else
    Result := 'not_supported';
  if (q and 2) <> 0 then
    Result := Result + ' readable';
  if (q and 4) <> 0 then
    Result := Result + ' writeable';
end;

function ModeAttrib(AModeAttributes: Integer): string;
begin
  if (AModeAttributes and 1) <> 0 then
    Result := 'supported'
  else
    Result := 'not_supported';
  if (AModeAttributes and 2) <> 0 then
  else
    Result := Result + ' reserved_is_zero(noresolutioninfo_for_vbe1.1-)';
  if (AModeAttributes and 4) <> 0 then
    Result := Result + ' TTY'
  else
    Result := Result + ' noTTY';
  if (AModeAttributes and 8) <> 0 then
    Result := Result + ' color'
  else
    Result := Result + ' monochrome';
  if (AModeAttributes and 16) <> 0 then
    Result := Result + ' graph'
  else
    Result := Result + ' text';
  if (AModeAttributes and 32) <> 0 then
    Result := Result + ' nonVGA'
  else
    Result := Result + ' VGA';
  if (AModeAttributes and 64) <> 0 then
    Result := Result + ' noWINDOWED'
  else
    Result := Result + ' WINDOWED';
  if (AModeAttributes and 128) <> 0 then
    Result := Result + ' LFB'
  else
    Result := Result + ' noLFB';
  if (AModeAttributes and 256) <> 0 then
    Result := Result + ' DoubleScanMode_is_available';
  if (AModeAttributes and 512) <> 0 then
    Result := Result + ' InterlacedMode_is_available';
  if (AModeAttributes and 1024) <> 0 then
    Result := Result + ' TripleBuffering';
  if (AModeAttributes and 2048) <> 0 then
    Result := Result + ' StereoscopicDisplaySupport';
  if (AModeAttributes and 4096) <> 0 then
    Result := Result + ' DualDisplayStartAddressSupport';
end;

function MemoryModelStr(AMemoryModel: Integer): string;
begin
  case AMemoryModel of
    0: Result := 'Text mode';
    1: Result := 'CGA graphics';
    2: Result := 'Hercules graphics';
    3: Result := 'Planar';
    4: Result := 'Packed pixel';
    5: Result := 'Non-chain 4, 256 color';
    6: Result := 'Direct Color';
    7: Result := 'YUV';
    8..15: Write('Reserved, to be defined by VESA');
    else
      Result := 'to be defined by OEM';
  end;
  Result := Result + '/' + IntToStr(AMemoryModel);
end;

function DirectColorModeInfoStr(ADirectColorModeInfo: Integer): string;
begin
  if (ADirectColorModeInfo and 1) <> 0 then
    Result := 'Color_ramp_is_programmable'
  else
    Result := 'Color_ramp_is_fixed';
  if (ADirectColorModeInfo and 2) <> 0 then
    Result := Result + ' Rsvd_bits_usable_by_app'
  else
    Result := Result + ' Rsvd_bits_reserved';
end;

procedure FreeModes;
var
  I: Integer;
begin
  CurrentMode := nil;
  for I := Low(VBEModes) to High(VBEModes) do
{    FreeAndNil(VBEModes[I])}
    VBEModes[I].Free;
  SetLength(VBEModes, 0);
end;

procedure GetModes;
var
  Addr: DWord;
  AddrSeg, AddrSel: Word;

  procedure LogModeInfo(ModeNumber: Integer; const VesaModeInfo: TVesaModeInfoBlock);
  begin
    Debugln('           ModeNumber: $' + HexStr(ModeNumber, 4));
    Debugln('       ModeAttributes: ' + ModeAttrib(VesaModeInfo.ModeAttributes));
    Debugln('       WinAAttributes: ' + WinAttrib(VesaModeInfo.WinAAttributes));
    Debugln('       WinBAttributes: ' + WinAttrib(VesaModeInfo.WinBAttributes));
    Debugln('       WinGranularity: ' + IntToStr(VesaModeInfo.WinGranularity) + ' KB');
    Debugln('              WinSize: ' + IntToStr(VesaModeInfo.WinSize) + ' KB');
    Debugln('          WinASegment: $' + HexStr(VesaModeInfo.WinASegment, 4));
    Debugln('          WinBSegment: $' + HexStr(VesaModeInfo.WinBSegment, 4));
    Debugln('           WinFuncPtr: ' + HexStr(VesaModeInfo.WinFuncPtr shr 16, 4) + ':' + HexStr(VesaModeInfo.WinFuncPtr and $FFFF, 4));
    Debugln('     BytesPerScanLine: ' + IntToStr(VesaModeInfo.BytesPerScanLine));
    Debugln('vbe1.2+');
    Debugln('          XResolution: ' + IntToStr(VesaModeInfo.XResolution));
    Debugln('          YResolution: ' + IntToStr(VesaModeInfo.YResolution));
    Debugln('            XCharSize: ' + IntToStr(VesaModeInfo.XCharSize));
    Debugln('            YCharSize: ' + IntToStr(VesaModeInfo.YCharSize));
    Debugln('       NumberOfPlanes: ' + IntToStr(VesaModeInfo.NumberOfPlanes));
    Debugln('         BitsPerPixel: ' + IntToStr(VesaModeInfo.BitsPerPixel));
    Debugln('        NumberOfBanks: ' + IntToStr(VesaModeInfo.NumberOfBanks));
    Debugln('          MemoryModel: ' + MemoryModelStr(VesaModeInfo.MemoryModel));
    Debugln('             BankSize: ' + IntToStr(VesaModeInfo.BankSize) + ' KB');
    Debugln('   NumberOfImagePages: ' + IntToStr(VesaModeInfo.NumberOfImagePages));
    Debugln('         Reserved(=1): ' + IntToStr(VesaModeInfo.Reserved));
    Debugln('          RedMaskSize: ' + IntToStr(VesaModeInfo.RedMaskSize));
    Debugln('     RedFieldPosition: ' + IntToStr(VesaModeInfo.RedFieldPosition));
    Debugln('        GreenMaskSize: ' + IntToStr(VesaModeInfo.GreenMaskSize));
    Debugln('   GreenFieldPosition: ' + IntToStr(VesaModeInfo.GreenFieldPosition));
    Debugln('         BlueMaskSize: ' + IntToStr(VesaModeInfo.BlueMaskSize));
    Debugln('    BlueFieldPosition: ' + IntToStr(VesaModeInfo.BlueFieldPosition));
    Debugln('         RsvdMaskSize: ' + IntToStr(VesaModeInfo.RsvdMaskSize));
    Debugln('    RsvdFieldPosition: ' + IntToStr(VesaModeInfo.RsvdFieldPosition));
    Debugln('  DirectColorModeInfo: ' + DirectColorModeInfoStr(VesaModeInfo.DirectColorModeInfo));
    Debugln('vbe2.0+');
    Debugln('          PhysBasePtr: $' + HexStr(VesaModeInfo.PhysBasePtr, 8));
    Debugln('        Reserved2(=0): ' + IntToStr(VesaModeInfo.Reserved2));
    Debugln('        Reserved3(=0): ' + IntToStr(VesaModeInfo.Reserved3));
    Debugln('vbe3.0+');
    Debugln('  LinBytesPerScanLine: ' + IntToStr(VesaModeInfo.LinBytesPerScanLine));
    Debugln('BnkNumberOfImagePages: ' + IntToStr(VesaModeInfo.BnkNumberOfImagePages));
    Debugln('LinNumberOfImagePages: ' + IntToStr(VesaModeInfo.LinNumberOfImagePages));
    Debugln('       LinRedMaskSize: ' + IntToStr(VesaModeInfo.LinRedMaskSize));
    Debugln('  LinRedFieldPosition: ' + IntToStr(VesaModeInfo.LinRedFieldPosition));
    Debugln('     LinGreenMaskSize: ' + IntToStr(VesaModeInfo.LinGreenMaskSize));
    Debugln('LinGreenFieldPosition: ' + IntToStr(VesaModeInfo.LinGreenFieldPosition));
    Debugln('      LinBlueMaskSize: ' + IntToStr(VesaModeInfo.LinBlueMaskSize));
    Debugln(' LinBlueFieldPosition: ' + IntToStr(VesaModeInfo.LinBlueFieldPosition));
    Debugln('      LinRsvdMaskSize: ' + IntToStr(VesaModeInfo.LinRsvdMaskSize));
    Debugln(' LinRsvdFieldPosition: ' + IntToStr(VesaModeInfo.LinRsvdFieldPosition));
    Debugln('        MaxPixelClock: ' + IntToStr(VesaModeInfo.MaxPixelClock));

    Debugln;
{    Write(VesaModeInfo.XResolution, 'x', VesaModeInfo.YResolution, 'x',
           VesaModeInfo.BitsPerPixel, '-', VesaModeInfo.MemoryModel,
           'R', VesaModeInfo.RedMaskSize, ':', VesaModeInfo.RedFieldPosition,
           'G', VesaModeInfo.GreenMaskSize, ':', VesaModeInfo.GreenFieldPosition,
           'B', VesaModeInfo.BlueMaskSize, ':', VesaModeInfo.BlueFieldPosition,
           'A', VesaModeInfo.RsvdMaskSize, ':', VesaModeInfo.RsvdFieldPosition, ' ');}
  end;

  procedure FillModeMissingData(ModeNumber: Integer; var VesaModeInfo: TVesaModeInfoBlock);
  begin
    if (VesaModeInfo.ModeAttributes and 1) <> 0 then
    begin
      if (VesaModeInfo.ModeAttributes and 2) = 0 then
      begin
        if VBEInfoBlock.VBEVersion < $0102 then
          StandardMode(ModeNumber, VesaModeInfo);
      end;
      if VBEInfoBlock.VBEVersion < $0300 then
      begin
        VesaModeInfo.LinBytesPerScanLine := VesaModeInfo.BytesPerScanLine;
	VesaModeInfo.BnkNumberOfImagePages := VesaModeInfo.NumberOfImagePages;
	VesaModeInfo.LinNumberOfImagePages := VesaModeInfo.NumberOfImagePages;
	VesaModeInfo.LinRedMaskSize := VesaModeInfo.RedMaskSize;
	VesaModeInfo.LinRedFieldPosition := VesaModeInfo.RedFieldPosition;
	VesaModeInfo.LinGreenMaskSize := VesaModeInfo.GreenMaskSize;
	VesaModeInfo.LinGreenFieldPosition := VesaModeInfo.GreenFieldPosition;
	VesaModeInfo.LinBlueMaskSize := VesaModeInfo.BlueMaskSize;
	VesaModeInfo.LinBlueFieldPosition := VesaModeInfo.BlueFieldPosition;
	VesaModeInfo.LinRsvdMaskSize := VesaModeInfo.RsvdMaskSize;
	VesaModeInfo.LinRsvdFieldPosition := VesaModeInfo.RsvdFieldPosition;
      end;
    end;
  end;

  procedure TryAddMode(ModeNumber: Word);
  var
    RealRegs: TRealRegs;
    VesaModeInfo: TVesaModeInfoBlock;
    VBEMode: TVBEMode;
  begin
    FillChar(VesaModeInfo, SizeOf(VesaModeInfo), 0);
    dosmemput(AddrSeg, 0, VesaModeInfo, SizeOf(VesaModeInfo));
    RealRegs.ax := $4F01; {return VBE mode information}
    RealRegs.cx := ModeNumber;
    RealRegs.es := AddrSeg;
    RealRegs.di := 0;
    realintr($10, RealRegs);
    dosmemget(AddrSeg, 0, VesaModeInfo, SizeOf(VesaModeInfo));

    if ((VesaModeInfo.ModeAttributes and 1) <> 0) or
       (VesaModeInfo.BytesPerScanLine <> 0) then
    begin
      LogModeInfo(ModeNumber, VesaModeInfo);
    end;

    FillModeMissingData(ModeNumber, VesaModeInfo);

    VBEMode := TVBEMode.Create(ModeNumber, VesaModeInfo);
    try
      if VBEMode.Supported then
      begin
        SetLength(VBEModes, Length(VBEModes) + 1);
        VBEModes[High(VBEModes)] := VBEMode;
        VBEMode := nil;
      end;
    finally
      VBEMode.Free;
    end;
  end;

var
  I: Integer;
  ModeNumber: Integer;
  ScanStart, Scanend: Integer;
begin
  FreeModes;
  Addr := global_dos_alloc(512);
  AddrSeg := Addr shr 16;
  AddrSel := Addr and $FFFF;
  try
    if ScanModesManually then
    begin
      ScanStart := 0;
{      Scanend := $7FFF;} {VBE 1.0+ ??}
{      Scanend := $3FFF;} {VBE 1.2+ ??}
//      Scanend := $7FF; {VBE 3.0+}
      Scanend := $1FF; {VBE 3.0+}
      Debugln('scanning modes $' + HexStr(ScanStart, 4) + '..$' + HexStr(Scanend, 4));
      for ModeNumber := ScanStart to Scanend do
      begin
        TryAddMode(ModeNumber);
      end;
    end
    else
    begin
      Debugln('Using the mode list, returned in the VBEInfoBlock');
      
      for I := Low(VideoModeList) to High(VideoModeList) do
      begin
        ModeNumber := VideoModeList[I];

	TryAddMode(ModeNumber);
      end;
    end;
  finally
    global_dos_free(AddrSel);
  end;
end;

procedure GetVBEInfo;

  function GetModeList: TVideoModeList;
  var
    ModeListAddr: DWord;
    NumberOfModes: Integer;
    I: Integer;
  begin
    NumberOfModes := 0;
    ModeListAddr := (VBEInfoBlock.VideoModePtr shr 16) * 16 + (VBEInfoBlock.VideoModePtr and $FFFF);
    while MemW[ModeListAddr] <> $FFFF do
    begin
      Inc(NumberOfModes);
      Inc(ModeListAddr, 2);
    end;
    SetLength(Result, NumberOfModes);

    I := 0;
    ModeListAddr := (VBEInfoBlock.VideoModePtr shr 16) * 16 + (VBEInfoBlock.VideoModePtr and $FFFF);
    while MemW[ModeListAddr] <> $FFFF do
    begin
      Result[I] := MemW[ModeListAddr];
      Inc(I);
      Inc(ModeListAddr, 2);
    end;
  end;

var
  Addr: DWord;
  AddrSeg: Word;
  AddrSel: Word;
  RealRegs: TRealRegs;
begin
  Addr := global_dos_alloc(512);
  try
    AddrSeg := Addr shr 16;
    AddrSel := Addr and $FFFF;
    VBEInfoBlock.VBESignature := 'VBE2';
    dosmemput(AddrSeg, 0, VBEInfoBlock, 4);
    RealRegs.ax := $4F00;
    RealRegs.es := AddrSeg;
    RealRegs.di := 0;
    realintr($10, RealRegs);
    VBEPresent := CheckVBEStatus(RealRegs.ax);
    if VBEPresent then
    begin
      dosmemget(AddrSeg, 0, VBEInfoBlock, SizeOf(VBEInfoBlock));
      VBEPresent := VBEInfoBlock.VBESignature = 'VESA';
      if not VBEPresent then
        Debugln('VBEInfoBlock returned no ''VESA'' VBESignature. Assuming VBE is not supported.');
    end;
    
    if VBEPresent then
    begin
      VideoMemory := VBEInfoBlock.TotalMemory * 64;
      EightBitDACSupported := (VBEInfoBlock.Capabilities and 1) <> 0;
      nonVGA := (VBEInfoBlock.Capabilities and 2) <> 0;
      SnowyRAMDAC := (VBEInfoBlock.Capabilities and 4) <> 0;
      StereoSignalingSupport := (VBEInfoBlock.Capabilities and 8) <> 0;
      StereoSignalingVesaEVC := (VBEInfoBlock.Capabilities and 16) <> 0;
      OEMString := GetRMString(VBEInfoBlock.OemStringPtr);
      if VBEInfoBlock.VBEVersion >= $0200 then
      begin
        OEMVendorName := GetRMString(VBEInfoBlock.OemVendorNamePtr);
        OEMProductName := GetRMString(VBEInfoBlock.OemProductNamePtr);
        OEMProductRev := GetRMString(VBEInfoBlock.OemProductRevPtr);
        OEMSoftwareRev := VBEInfoBlock.OemSoftwareRev;
      end
      else
      begin
        OEMVendorName := '';
        OEMProductName := '';
        OEMProductRev := '';
        OEMSoftwareRev := -1;
      end;

      Debugln('VBEVersion: ' + IntToStr(bcd(VBEInfoBlock.VBEVersion shr 8)) + '.' + IntToStr(bcd(VBEInfoBlock.VBEVersion and $FF)));
      Debugln('VideoMemory: ' + IntToStr(VideoMemory) + ' KB');
      Debugln('VideoModePtr: ' + HexStr(VBEInfoBlock.VideoModePtr shr 16, 4) + ':' + HexStr(VBEInfoBlock.VideoModePtr and $FFFF, 4));
      Debugln('EightBitDACSupported: ' + BoolToStr(EightBitDACSupported));
      Debugln('nonVGA: ' + BoolToStr(nonVGA));
      Debugln('SnowyRAMDAC: ' + BoolToStr(SnowyRAMDAC));
      Debugln('StereoSignalingSupport: ' + BoolToStr(StereoSignalingSupport));
      if StereoSignalingSupport then
        if StereoSignalingVesaEVC then
          Debugln('Stereo signaling supported via VESA EVC connector')
        else
          Debugln('Stereo signaling supported via external VESA stereo connector');
      if OEMString <> '' then
        Debugln('OEMString: ' + OEMString);
      if OEMVendorName <> '' then
        Debugln('OEMVendorName: ' + OEMVendorName);
      if OEMProductName <> '' then
        Debugln('OEMProductName: ' + OEMProductName);
      if OEMProductRev <> '' then
        Debugln('OEMProductRev: ' + OEMProductRev);
      if OEMSoftwareRev <> -1 then
        Debugln('OEMSoftwareRev: ' + IntToStr(bcd(OEMSoftwareRev shr 8)) + '.' + IntToStr(bcd(OEMSoftwareRev and $FF)));

      VideoModeList := GetModeList;

      {Write('VideoModeList:');
      tmp := (VBEInfoBlock.VideoModePtr shr 16) * 16 + (VBEInfoBlock.VideoModePtr and $FFFF);
      while MemW[tmp] <> $FFFF do
      begin
        Write(' $', HexStr(MemW[tmp], 4));
        Inc(tmp, 2);
      end;
      Writeln;}
      Debugln;

    end;
  finally
    global_dos_free(AddrSel);
  end;
end;

var
  __crt0_startup_flags: Byte; external name '__crt0_startup_flags';
  ___djgpp_base_address: DWord; external name '___djgpp_base_address';
  ___djgpp_selector_limit: DWord; external name '___djgpp_selector_limit';
  ___v2prt0_ds_alias: Word; external name '___v2prt0_ds_alias';
  ___djgpp_memory_handle_list: DWord; external name '___djgpp_memory_handle_list';

{  LFB0508AllocatedMemoryBlock: Pointer;
  LFB0508MappedVideoBufferStart: Pointer;
  LFB0508NumberOfPagesMapped: Integer;
  LFB0508Mapped: Boolean;}

function MapLFBToLinearSpace0508(PhysicalAddress, Size: DWord): Boolean;
var
  LinearAddress: DWord;
  PhysicalAddressLeftPadding, PhysicalAddressRightPadding: DWord;
  PaddedSize: DWord;
  MapSuccess: Boolean;
  MappedPageAttribute: Word;
begin
  Debugln('Trying to map LFB, using DPMI function 0508h. Physical addr=$' + HexStr(PhysicalAddress, 8) + '; Size=$' + HexStr(Size, 8));

  {align physical buffer to page boundaries...}
  PhysicalAddressLeftPadding := PhysicalAddress mod DPMIPageSize;
  PhysicalAddressRightPadding := (PhysicalAddress + Size) mod DPMIPageSize;
  if PhysicalAddressRightPadding <> 0 then
    PhysicalAddressRightPadding := DPMIPageSize - PhysicalAddressRightPadding;
  Debugln('PhysicalAddressLeftPadding = ' + IntToStr(PhysicalAddressLeftPadding));
  Debugln('PhysicalAddressRightPadding = ' + IntToStr(PhysicalAddressRightPadding));

  PaddedSize := Size + PhysicalAddressLeftPadding + PhysicalAddressRightPadding;
  Debugln('PaddedSize = ' + IntToStr(PaddedSize));
  
  LFB0508AllocatedMemoryBlock := GetMem(PaddedSize + DPMIPageSize - 1);
  LFB0508MemoryBlockPadding := (PtrUInt(LFB0508AllocatedMemoryBlock) + ___djgpp_base_address) mod DPMIPageSize;
  if LFB0508MemoryBlockPadding <> 0 then
    LFB0508MemoryBlockPadding := DPMIPageSize - LFB0508MemoryBlockPadding;

  LFB0508NumberOfPagesMapped := PaddedSize div DPMIPageSize;
//  Write('Before map...'); Readln;
  MapSuccess := map_device_in_memory_block(___djgpp_memory_handle_list, PtrUInt(LFB0508AllocatedMemoryBlock) + LFB0508MemoryBlockPadding, LFB0508NumberOfPagesMapped, PhysicalAddress - PhysicalAddressLeftPadding);
  if (not MapSuccess) or (int31error <> 0) then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    FreeMem(LFB0508AllocatedMemoryBlock);
    LFB0508AllocatedMemoryBlock := nil;
    exit;
  end;
  Debugln('DPMI function 0508h returned success!');
  
  Debugln('Checking page attributes, to see if it really succeeded. (shitty NTVDM reports success, even though it does not support DPMI 0508h, so we need this extra check)');
  MappedPageAttribute := $FFFF;
  MapSuccess := get_page_attributes(___djgpp_memory_handle_list, PtrUInt(LFB0508AllocatedMemoryBlock) + LFB0508MemoryBlockPadding, 1, @MappedPageAttribute);
  if (not MapSuccess) or (int31error <> 0) then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    FreeMem(LFB0508AllocatedMemoryBlock);
    LFB0508AllocatedMemoryBlock := nil;
    exit;
  end;
  Debugln('Page attribute = %' + BinStr(MappedPageAttribute, 16));
  if (MappedPageAttribute and %111) <> 2 then
  begin
    Debugln('Page is not mapped!!! Probably a buggy NTVDM host.');
    Result := false;
    FreeMem(LFB0508AllocatedMemoryBlock);
    LFB0508AllocatedMemoryBlock := nil;
    exit;
  end;
  
  LFB0508MappedVideoBufferStart := LFB0508AllocatedMemoryBlock + LFB0508MemoryBlockPadding + PhysicalAddressLeftPadding;
  LFBPhysicalAddress := PhysicalAddress;
  LFBBufferSize := Size;
  LFB0508Mapped := true;
  Result := true;
end;

function FreeLFBMapping0508: Boolean;
var
  SetPageAttributes: PWord;
  UnMapSuccess: Boolean;
begin
  if not LFB0508Mapped then
    exit;

  Debugln('Freeing the 0508h LFB mapping...');
  SetPageAttributes := GetMem(LFB0508NumberOfPagesMapped * SizeOf(Word));
  FillWord(SetPageAttributes^, LFB0508NumberOfPagesMapped, %01001);
  UnMapSuccess := set_page_attributes(___djgpp_memory_handle_list, PtrUInt(LFB0508AllocatedMemoryBlock) + LFB0508MemoryBlockPadding, LFB0508NumberOfPagesMapped, SetPageAttributes);
  FreeMem(SetPageAttributes);
  
  if (not UnMapSuccess) or (int31error <> 0) then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    exit;
  end;
  
  Debugln('Mapped memory changed back to committed. Now freeing the allocated memory block from the pascal heap.');
  FreeMem(LFB0508AllocatedMemoryBlock);
  LFB0508Mapped := false;
  Result := true;
  Debugln('LFB 0508h mapping freed.');
end;

function MapLFBToLinearSpace0800(PhysicalAddress, Size: DWord): Boolean;
var
  LinearAddress: DWord;
begin
  Debugln('Trying to map LFB to linear address space. Physical addr=$' + HexStr(PhysicalAddress, 8) + '; Size=$' + HexStr(Size, 8));
  LinearAddress := get_linear_addr(PhysicalAddress, VideoMemory * 1024);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    exit;
  end;
  Debugln('Mapped successfully at linear address $' + HexStr(LinearAddress, 8));
  LFBPhysicalAddress := PhysicalAddress;
  LFBBufferSize := Size;
  LFB0800LinearAddress := LinearAddress;
  LFB0800LinearAddressMapped := true;
  Result := true;
end;

function FreeLFBMapping0800: Boolean;
begin
  if not LFB0800LinearAddressMapped then
    exit;

  Debugln('Freeing the LFB mapping in linear address space.');
  free_linear_addr_mapping(LFB0800LinearAddress);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Debugln('Ignoring the error, as this function exists only in the DPMI 1.0 specification, and most DPMI hosts are 0.9');
    Debugln('(well, at least we tried to be nice and called it)');
  end;
  LFB0800LinearAddressMapped := false;
  LFB0800LinearAddress := 0;
  Debugln('LFB mapping freed.');
  Result := true;
end;

function NearPtrEnabled: Boolean;
const
  _CRT0_FLAG_NEARPTR = $80;
begin
  Result := (__crt0_startup_flags and _CRT0_FLAG_NEARPTR) <> 0;
end;

function EnableNearPtr: Boolean;
const
  _CRT0_FLAG_NEARPTR = $80;
var
  CurrentDSLimit: DWord;
begin
  Debugln('Trying to enable nearptr (aka "Fat DS") mode...');
  if NearPtrEnabled then
  begin
    Debugln('Already enabled... nothing to do...');
    Result := true;
    exit;
  end;
  CurrentDSLimit := get_segment_limit(get_ds);
  Debugln('___djgpp_base_address=$' + HexStr(___djgpp_base_address, 8));
  Debugln('___djgpp_selector_limit=$' + HexStr(___djgpp_selector_limit, 8));
  Debugln('Current CS base=$' + HexStr(get_segment_base_address(get_cs), 8));
  Debugln('Current DS base=$' + HexStr(get_segment_base_address(get_ds), 8));
  Debugln('Current CS limit=$' + HexStr(get_segment_limit(get_cs), 8));
  Debugln('Current DS limit=$' + HexStr(CurrentDSLimit, 8));
  Debugln('__crt0_startup_flags=$' + HexStr(__crt0_startup_flags, 2));

  Debugln('Trying to set DS limit to $FFFFFFFF...');
  set_segment_limit(get_ds, $FFFFFFFF);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    exit;
  end;

  CurrentDSLimit := get_segment_limit(get_ds);
  Debugln('___djgpp_base_address=$' + HexStr(___djgpp_base_address, 8));
  Debugln('___djgpp_selector_limit=$' + HexStr(___djgpp_selector_limit, 8));
  Debugln('Current CS base=$' + HexStr(get_segment_base_address(get_cs), 8));
  Debugln('Current DS base=$' + HexStr(get_segment_base_address(get_ds), 8));
  Debugln('Current CS limit=$' + HexStr(get_segment_limit(get_cs), 8));
  Debugln('Current DS limit=$' + HexStr(CurrentDSLimit, 8));
  Debugln('__crt0_startup_flags=$' + HexStr(__crt0_startup_flags, 2));
  
  if CurrentDSLimit <> $FFFFFFFF then
  begin
    Debugln('Not $FFFFFFFF...');
    Debugln('Probably running under NT or DOSEMU.');
    Result := false;

    if CurrentDSLimit <> ___djgpp_selector_limit then
    begin
      { fix limit back to what it was }
      set_segment_limit(get_ds, ___djgpp_selector_limit);
    end;

    exit;
  end;
  
  set_segment_limit(___v2prt0_ds_alias, $FFFFFFFF);

  __crt0_startup_flags := __crt0_startup_flags or _CRT0_FLAG_NEARPTR;

  Debugln('Nearptr mode enabled successfully.');
  Result := true;
end;

procedure DisableNearPtr;
const
  _CRT0_FLAG_NEARPTR = $80;
begin
  if not NearPtrEnabled then
    exit;

  Debugln('Trying to disable nearptr (aka "Fat DS") mode...');

  __crt0_startup_flags := __crt0_startup_flags and (not _CRT0_FLAG_NEARPTR);

  Debugln('Setting DS limit...');
  set_segment_limit(get_ds, ___djgpp_selector_limit);
  if int31error <> 0 then
    Debugln('DPMI error $' + HexStr(int31error, 4));

  Debugln('Setting DS alias limit...');
  set_segment_limit(___v2prt0_ds_alias, ___djgpp_selector_limit);
  if int31error <> 0 then
    Debugln('DPMI error $' + HexStr(int31error, 4));

  Debugln('Nearptr mode disabled.');
end;

function LFBNearPtrAccessAvailable: Boolean;
begin
  Result := LFB0508Mapped or (NearPtrEnabled and LFB0800LinearAddressMapped);
end;

function LFBNearPtrAccessPtr: Pointer;
begin
  if LFB0508Mapped then
    Result := LFB0508MappedVideoBufferStart
  else
    if NearPtrEnabled and LFB0800LinearAddressMapped then
      Result := Pointer(LFB0800LinearAddress - ___djgpp_base_address)
    else
      Result := nil;
end;

{procedure TestLFB;
var
  pixels: PDWord;
  I: Integer;
begin
  pixels := LFB0508MappedVideoBufferStart;
  for I := 0 to 1000 do
    pixels[I] := I;
  Readln;
end;}

function CreateLFBSegmentSelector: Boolean;
var
  Selector: Word;
  
  procedure InternalFreeLFBSegmentSelector;
  begin
    if Selector = 0 then
      exit;

    Debugln('Freeing the LFB descriptor for far ptr LFB access...');
    if not free_ldt_descriptor(Selector) then
      Debugln('DPMI error $' + HexStr(int31error, 4));

    Selector := 0;
  end;

begin
  Debugln('Allocating a LDT descriptor for far ptr LFB access...');
  Selector := allocate_ldt_descriptors(1);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    exit;
  end;
  Debugln('Got selector ' + IntToStr(Selector));
  
  Debugln('Setting selector base address to ' + HexStr(LFB0800LinearAddress, 8));
  set_segment_base_address(Selector, LFB0800LinearAddress);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    InternalFreeLFBSegmentSelector;
    exit;
  end;
  
  Debugln('Setting segment limit to ' + HexStr((LFBBufferSize - 1) or $FFF, 8));
  set_segment_limit(Selector, (LFBBufferSize - 1) or $FFF);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Result := false;
    InternalFreeLFBSegmentSelector;
    exit;
  end;

  LFBSegmentSelector := Selector;
end;

procedure FreeLFBSegmentSelector;
begin
  if LFBSegmentSelector = 0 then
    exit;

  Debugln('Freeing the LFB descriptor for far ptr LFB access...');
  if not free_ldt_descriptor(LFBSegmentSelector) then
    Debugln('DPMI error $' + HexStr(int31error, 4));

  LFBSegmentSelector := 0;
end;

procedure Cleanup;
begin
  { cleanup LFB stuff }
  FreeLFBMapping0508;
  DisableNearPtr;
  FreeLFBSegmentSelector;
  FreeLFBMapping0800;

  DisposeRealModePalette;
end;

function SetVESAMode(M: Integer; AUseLFB: Boolean): Boolean;
var
  ModeAttr: DWord;
  lLFBUsed: Boolean;
  lReadWindow, lWriteWindow: Integer;
  lReadWindowStart, lWriteWindowStart: Integer;
  lReadWindowAddress, lWriteWindowAddress: Integer;
  lWindowGranularity: DWord;
  lWindowSize, lWindowSizeG: DWord;
  RealRegs: TRealRegs;
  DPMI508Success: Boolean;
begin
  Debugln('Setting VBE mode $' + HexStr(VBEModes[M].VBEModeID, 4));
  Result := false;
  Cleanup;

  lLFBUsed := AUseLFB;

  if not AUseLFB then
  begin
    if not VBEModes[M].SupportsWindowed then
      exit;

    lReadWindow := VBEModes[M].ReadWindow.WindowID;
    lReadWindowAddress := VBEModes[M].ReadWindow.Segment shl 4;
    lWriteWindow := VBEModes[M].WriteWindow.WindowID;
    lWriteWindowAddress := VBEModes[M].WriteWindow.Segment shl 4;
    
    lWindowGranularity := VBEModes[M].WriteWindow.Granularity * 1024;
    lWindowSize := VBEModes[M].WriteWindow.Size * 1024;
    lWindowSizeG := lWindowSize div lWindowGranularity;
    lWindowSize := lWindowSizeG * lWindowGranularity;
  end
  else
  begin
    if not VBEModes[M].SupportsLFB then
      exit;

    DPMI508Success := false;
    if TryDPMI508h then
    begin
      DPMI508Success := MapLFBToLinearSpace0508(VBEModes[M].PhysBasePtr, VideoMemory * 1024);
      if not DPMI508Success then
        Debugln('DPMI 508h mapping failed, will try other methods to map the lfb...');
    end;
    
    if not DPMI508Success then
    begin
      if not MapLFBToLinearSpace0800(VBEModes[M].PhysBasePtr, VideoMemory * 1024) then
      begin
        Result := false;
        exit;
      end;
      
      if TryNearPtr then
      begin
        if not EnableNearPtr then
	  Debugln('Enabling nearptr (aka "Fat DS") mode failed, will try other methods...');
      end;
      
      if not NearPtrEnabled then
      begin
        Debugln('Falling back to far ptr lfb access...');
	CreateLFBSegmentSelector;
      end;
    end;
  end;
  RealRegs.ax := $4F02;
  if lLFBUsed then
    RealRegs.bx := VBEModes[M].VBEModeID or $4000
  else
    RealRegs.bx := VBEModes[M].VBEModeID;
  realintr($10, RealRegs);
  if not CheckVBEStatus(RealRegs.AX) then
  begin
    Cleanup;
    Result := false;
    exit;
  end;
  PaletteDACbits := 6;
  with VBEModes[M] do
  begin
    if (BitsPerPixel = 8) and (MemoryModel = vmmmPackedPixel) then
    begin
      SetPaletteHW := True;
      if (VBEInfoBlock.VBEVersion >= $200) and
         (not IsVGA) then {if nonVGA, use func9 to set palette}
        SetPaletteHW := False;

      if EightBitDACSupported and EightBitDACEnabled then
        SwitchTo8bitDAC;

      if not SetPaletteHW then
        AllocateRealModePalette;
    end;
  end;

  CurrentMode := VBEModes[M];
  LFBUsed := lLFBUsed;
  ReadWindow := lReadWindow;
  WriteWindow := lWriteWindow;
  ReadWindowStart := lReadWindowStart;
  WriteWindowStart := lWriteWindowStart;
  ReadWindowAddress := lReadWindowAddress;
  WriteWindowAddress := lWriteWindowAddress;
  WindowGranularity := lWindowGranularity;
  WindowSize := lWindowSize;
  WindowSizeG := lWindowSizeG;

  Result := true;
//  TestLFB;
end;

procedure GetDPMIInfo;
var
  DPMIVersionInfo: TDPMIVersionInfo;
begin
  Debugln('GO32 run_mode: ' + IntToStr(get_run_mode));

  Debugln('Getting DPMI version...');
  get_dpmi_version(DPMIVersionInfo);
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
  end
  else
  begin
    Debugln('DPMI version: ' + IntToStr(DPMIVersionInfo.major) + '.' + IntToStr(DPMIVersionInfo.minor));
    Debugln('DPMI flags: %' + BinStr(DPMIVersionInfo.flags, 16));
    Debugln('DPMI cpu type: ' + IntToStr(DPMIVersionInfo.cpu));
    Debugln('DPMI virtual master PIC base: $' + HexStr(DPMIVersionInfo.master_pic, 2));
    Debugln('DPMI virtual slave PIC base: $' + HexStr(DPMIVersionInfo.slave_pic, 2));
  end;

  Debugln('Getting DPMI page size...');
  DPMIPageSize := get_page_size;
  if int31error <> 0 then
  begin
    Debugln('DPMI error $' + HexStr(int31error, 4));
    Debugln('Assuming 4k page size...');
    DPMIPageSize := 4096;
  end;
  if DPMIPageSize = 0 then
  begin
    Debugln('DPMI reported 0 bytes page size, which is an invalid value, assuming 4k page size!!!');
    DPMIPageSize := 4096;
  end;
  Debugln('Page size is ' + IntToStr(DPMIPageSize) + ' bytes');
end;

procedure RestoreTextMode;
var
  RealRegs: TRealRegs;
begin
  Cleanup;

  RealRegs.ax := $0003;
  realintr($10, RealRegs);
  CurrentMode := nil;
end;

procedure InitVESA;
begin
  if not VESAInit then
    VESAInit := True
  else
    exit;
  GetDPMIInfo;
  GetVBEInfo;
  if VBEPresent then
    GetModes;
end;

initialization
  VESAInit := False;
  RealModePaletteSel := 0;
  RealModePaletteSeg := 0;

finalization
  Cleanup;
  FreeModes;

end.
