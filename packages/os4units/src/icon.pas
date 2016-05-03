{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    icon.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit icon;

interface

uses
  exec, workbench, utility, amigados, agraphics, intuition;

const
  ICONNAME: PChar = 'icon.library';

  ICONA_Dummy                   = TAG_USER + $9000; // Start of icon.library tags
  ICONA_ErrorCode               = ICONA_Dummy + 1;  // (PLongInt) Errorcode
  ICONA_ErrorTagItem            = ICONA_Dummy + 75; // (^PTagItem) Points to the tag item that caused the error .

// Global options for IconControlA()

  ICONCTRLA_SetGlobalScreen     = ICONA_Dummy + 2;  // (PScreen) Screen to use for remapping Workbench icons to
  ICONCTRLA_GetGlobalScreen     = ICONA_Dummy + 3;
  ICONCTRLA_SetGlobalPrecision  = ICONA_Dummy + 4;  // (LongInt) Icon color remapping precision; defaults to PRECISION_ICON
  ICONCTRLA_GetGlobalPrecision  = ICONA_Dummy + 5;
  ICONCTRLA_SetGlobalEmbossRect = ICONA_Dummy + 6;  // (PRectangle) Icon frame size dimensions
  ICONCTRLA_GetGlobalEmbossRect = ICONA_Dummy + 7;
  ICONCTRLA_SetGlobalFrameless  = ICONA_Dummy + 8;  // (LongBool) Render image without frame
  ICONCTRLA_GetGlobalFrameless  = ICONA_Dummy + 9;
  ICONCTRLA_SetGlobalNewIconsSupport  = ICONA_Dummy + 10; // (LongBool) Enable NewIcons support
  ICONCTRLA_GetGlobalNewIconsSupport  = ICONA_Dummy + 11;
  ICONCTRLA_SetGlobalColorIconSupport = ICONA_Dummy + 77; // (LongBool) Enable color icon support
  ICONCTRLA_GetGlobalColorIconSupport = ICONA_Dummy + 78;
  ICONCTRLA_SetGlobalIdentifyHook     = ICONA_Dummy + 12; // (PHook) Set/Get the hook to be called when identifying a file
  ICONCTRLA_GetGlobalIdentifyHook     = ICONA_Dummy + 13;
  ICONCTRLA_SetGlobalMaxNameLength    = ICONA_Dummy + 67; // (LongInt) Set/get the maximum length of a file/drawer name supported by icon.library
  ICONCTRLA_GetGlobalMaxNameLength    = ICONA_Dummy + 68;
  ICONCTRLA_SetGlobalLeftOutMarking   = ICONA_Dummy + 96; // (LongBool) Enable marking of left-out icons (V51)
  ICONCTRLA_GetGlobalLeftOutMarking   = ICONA_Dummy + 97;
  ICONCTRLA_SetGlobalSelectEffect     = ICONA_Dummy +113; // (LongWord) Set/get the required visual effect for selected icons (V51)
  ICONCTRLA_GetGlobalSelectEffect     = ICONA_Dummy +114;
// Values for ICONCTRLA_Set/GetGlobalSelectEffect
  SEL_DARKEN    = $00000000; // Darken the icon, by the amount specified in the lowest byte (000000XX)
  SEL_BRIGHTEN  = $01000000; // Brighten the icon, by the amount specified in the lowest byte (000000XX)
  SEL_IMPOSE    = $02000000; // Impose a color over the icon image, specified in the lower bytes (00RRGGBB)
  SEL_NEGATIVE  = $03000000; // Invert the colors of the icon, as specified in the lowest byte (000000XX)
  SEL_ROTATERGB = $04000000; // Rotate RGB, as specified in the lowest byte (000000XX): 0 = left, 1 = right

// Tags for use with GetIconTagList()
  ICONGETA_GetDefaultType       = ICONA_Dummy + 45; // Default icon type to retrieve (LongInt)
  ICONGETA_GetDefaultName       = ICONA_Dummy + 46; // Retrieve default icon for the given name (PChar)
  ICONGETA_FailIfUnavailable    = ICONA_Dummy + 47; // Return a default icon if the requested icon file cannot be found (BOOL).
  ICONGETA_GetPaletteMappedIcon = ICONA_Dummy + 48; // If possible, retrieve a palette mapped icon (BOOL).
  ICONGETA_IsDefaultIcon        = ICONA_Dummy + 49; // Set if the icon returned is a default icon (PLongBool).
  ICONGETA_RemapIcon            = ICONA_Dummy + 50; // Remap the icon to the default screen, if possible (BOOL).
  ICONGETA_GenerateImageMasks   = ICONA_Dummy + 51; // Generate icon image masks (BOOL).
  ICONGETA_Label                = ICONA_Dummy + 52; // Label text to be assigned to the icon (PChar).
  ICONGETA_Screen               = ICONA_Dummy + 69; // Screen to remap the icon to (PScreen).
  ICONGETA_UseFriendBitMap      = ICONA_Dummy + 90; // Allocate a bitmap for the icon images instead of the traditional planar icon images (LongBool). (V50)
  ICONGETA_FileFormat           = ICONA_Dummy + 112; // Returns the format of the file the icon is loaded from. If the file is in native Amiga format, the returned name will be "Amiga" (^STRPTR). (V51)
  ICONGETA_Width                = ICONA_Dummy + 117; // Request a specific size for the icon to be loaded.
  ICONGETA_Height               = ICONA_Dummy + 118; //
  ICONGETA_ForceScaling         = ICONA_Dummy + 120; // Tell icon.library to perform a manual bitmap rescaling of the loaded icon if it doesn't already have the size which was requested with ICONGETA_Width and ICONGETA_Height.
  ICONGETA_IdentifyBuffer       = ICONA_Dummy + 122; // Request the type of the actual file whose icon is being loaded by GetIconTagList()
  ICONGETA_IdentifyOnly         = ICONA_Dummy + 123; //
  ICONGETA_SizeBounds           = ICONA_Dummy + 124; // Specify the size limits for the icon to be loaded.
  ICONGETA_AllowUpscaling       = ICONA_Dummy + 126; // This means the icon image may be made larger even if no upscaling was explicitly requested.

// Per icon local options for IconControlA()  }
  ICONCTRLA_GetImageMask1        = ICONA_Dummy + 14; // (TPlanePtr) Get the icon rendering masks
  ICONCTRLA_GetImageMask2        = ICONA_Dummy + 15;
  ICONCTRLA_SetTransparentColor1 = ICONA_Dummy + 16; // Transparent image color; set to -1 if opaque
  ICONCTRLA_GetTransparentColor1 = ICONA_Dummy + 17;
  ICONCTRLA_SetTransparentColor2 = ICONA_Dummy + 18;
  ICONCTRLA_GetTransparentColor2 = ICONA_Dummy + 19;
  ICONCTRLA_SetPalette1 = ICONA_Dummy + 20; // (PColorRegister) Image color palette
  ICONCTRLA_GetPalette1 = ICONA_Dummy + 21;
  ICONCTRLA_SetPalette2 = ICONA_Dummy + 22;
  ICONCTRLA_GetPalette2 = ICONA_Dummy + 23;
  ICONCTRLA_SetPaletteSize1 = ICONA_Dummy + 24; // (LongInt) Size of image color palette
  ICONCTRLA_GetPaletteSize1 = ICONA_Dummy + 25;
  ICONCTRLA_SetPaletteSize2 = ICONA_Dummy + 26;
  ICONCTRLA_GetPaletteSize2 = ICONA_Dummy + 27;
  ICONCTRLA_SetImageData1   = ICONA_Dummy + 28; // (PByte) Image data; one by per pixel
  ICONCTRLA_GetImageData1   = ICONA_Dummy + 29;
  ICONCTRLA_SetImageData2   = ICONA_Dummy + 30;
  ICONCTRLA_GetImageData2   = ICONA_Dummy + 31;
  ICONCTRLA_SetFrameless       = ICONA_Dummy + 32; // (LongBool) Render image without frame
  ICONCTRLA_GetFrameless       = ICONA_Dummy + 33;
  ICONCTRLA_SetNewIconsSupport = ICONA_Dummy + 34; // (LongBool) Enable NewIcons support
  ICONCTRLA_GetNewIconsSupport = ICONA_Dummy + 35;
  ICONCTRLA_SetAspectRatio     = ICONA_Dummy + 36; // (PByte) Icon aspect ratio
  ICONCTRLA_GetAspectRatio     = ICONA_Dummy + 37;
  ICONCTRLA_SetWidth  = ICONA_Dummy + 38; // (LongInt) Icon dimensions; valid only for palette mapped icon images
  ICONCTRLA_GetWidth  = ICONA_Dummy + 39;
  ICONCTRLA_SetHeight = ICONA_Dummy + 40;
  ICONCTRLA_GetHeight = ICONA_Dummy + 41;
  ICONCTRLA_IsPaletteMapped = ICONA_Dummy + 42; // (PLongInt) Check whether the icon is palette mapped
  ICONCTRLA_GetScreen       = ICONA_Dummy + 43; // (^PScreen) Get the screen the icon is attached to
  ICONCTRLA_HasRealImage2   = ICONA_Dummy + 44; // (PLongInt) Check whether the icon has a real select image
  ICONCTRLA_IsNewIcon       = ICONA_Dummy + 79; // (PLongInt) Check whether the icon is of the NewIcon type
  ICONCTRLA_IsNativeIcon    = ICONA_Dummy + 80; // (PLongInt) Check whether this icon was allocated by icon.library or if consists solely of a statically allocated TDiskObject.
  ICONCTRLA_UseFriendBitMap = ICONGETA_UseFriendBitMap; // (LongBool) Alias for ICONGETA_UseFriendBitMap. (V50)
  ICONCTRLA_GetBitMap1 = ICONA_Dummy + 91; // (PBitMap) Get the bitmaps of the icon. Can be nil, only valid after an icon has been layouted. (V50)
  ICONCTRLA_GetBitMap2 = ICONA_Dummy + 92;
  ICONCTRLA_SetImageDataFormat = ICONA_Dummy + 103; // (LongWord) Format of icon image data (supersedes ICONCTRLA_IsPaletteMapped). The possible values are listed below. (V51)
  ICONCTRLA_GetImageDataFormat = ICONA_Dummy + 104;
  ICONCTRLA_SetIconModuleData  = ICONA_Dummy + 105; // (APTR) The module-specific data for an icon. Only for private use by icon loader/saver modules. (V51)
  ICONCTRLA_GetIconModuleData  = ICONA_Dummy + 106;
  ICONCTRLA_GetIconFileFormat  = ICONA_Dummy + 111; // (^STRPTR) Get the format of the file an icon was loaded from. (V51)
  ICONCTRLA_AddScaledSize      = ICONA_Dummy + 115; // (LongWord) Generate a scaled version of the icon's imagery. (V51)
  ICONCTRLA_RemScaledSize      = ICONA_Dummy + 116; // (LongWord) Remove a particular scaled version of the icon's imagery, and free all memory associated with it.


// Icon Aspect Handling
  ICON_ASPECT_RATIO_UNKNOWN = 0; // Icon aspect ratio is not known.

// Tags for use with PutIconTagList()

  ICONPUTA_NotifyWorkbench       = ICONA_Dummy + 53; // Notify Workbench of the icon being written (BOOL)
  ICONPUTA_PutDefaultType        = ICONA_Dummy + 54; // Store icon as the default for this type (LongInt)
  ICONPUTA_PutDefaultName        = ICONA_Dummy + 55; // Store icon as a default for the given name (PChar)
  ICONPUTA_DropPlanarIconImage   = ICONA_Dummy + 56; // When storing a palette mapped icon, don't save the the original planar icon image with the file. Replace it with a tiny replacement image.
  ICONPUTA_DropChunkyIconImage   = ICONA_Dummy + 57; // Don't write the chunky icon image data to disk.
  ICONPUTA_DropNewIconToolTypes  = ICONA_Dummy + 58; // Don't write the NewIcons tool types to disk.
  ICONPUTA_OptimizeImageSpace    = ICONA_Dummy + 59; // If this tag is enabled, the writer will examine the icon image data to find out whether it can compress it more efficiently. This may take extra time and is not generally recommended.
  ICONPUTA_OnlyUpdatePosition    = ICONA_Dummy + 72; // Don't write the entire icon file back to disk, only change the do^.do_CurrentX/do^.do_CurrentY members.
  ICONPUTA_PreserveOldIconImages = ICONA_Dummy + 84; // Before writing a palette mapped icon back to disk, icon.library will make sure that the original
                                                     // planar image data is stored in the file. If you don't want that to happen, set this option to
                                                     // FALSE. This will allow you to change the planar icon image data written back to disk.
  ICONPUTA_FileFormat            = ICONA_Dummy + 107; // Tell icon.library what file format the icon should be saved in, by passing the name of a file format which is supported by some external icon saver module.
  ICONPUTA_NoFallback            = ICONA_Dummy + 108; // Tell icon.library to fail if the icon cannot be saved in the requested file format, rather than falling back to saving it in native Amiga format.

// For use with the file identification hook.
type
  PIconIdentifyMsg = ^TIconIdentifyMsg;
  TIconIdentifyMsg = record
    // Libraries that are already opened for your use.
    iim_SysBase : PLibrary;
    iim_DOSBase : PLibrary;
    iim_UtilityBase : PLibrary;
    iim_IconBase : PLibrary;
    // File context information.
    iim_FileLock : BPTR;     // Lock on the object to return an icon for.
    iim_ParentLock : BPTR;   // Lock on the object's parent directory, if available.
    iim_FIB : PFileInfoBlock;// Already initialized for you.
    iim_FileHandle : BPTR;   // If non-nil, pointer to the file to examine, positioned right at the first byte, ready for you to use.
    iim_Tags : PTagItem;     // Tags passed to GetIconTagList().
    iim_IExec: PInterface;
    iim_IDOS: PInterface;
    iim_IUtility: PInterface;
    iim_IIcon: PInterface;
  end;

// Tags for use with DupDiskObjectA()
const
  ICONDUPA_DuplicateDrawerData  = ICONA_Dummy + 60; // Duplicate do_DrawerData
  ICONDUPA_DuplicateImages      = ICONA_Dummy + 61; // Duplicate the Image structures.
  ICONDUPA_DuplicateImageData   = ICONA_Dummy + 62; // Duplicate the image data (Image->ImageData) itself.
  ICONDUPA_DuplicateDefaultTool = ICONA_Dummy + 63; // Duplicate the default tool.
  ICONDUPA_DuplicateToolTypes   = ICONA_Dummy + 64; // Duplicate the tool types list.
  ICONDUPA_DuplicateToolWindow  = ICONA_Dummy + 65; // Duplicate the tool window.
  ICONDUPA_ActivateImageData    = ICONA_Dummy + 82; // If the icon to be duplicated is in fact a palette mapped icon which has never been
                                                    // set up to be displayed on the   screen, turn the duplicate into that palette mapped icon.
  ICONDUPA_UseFriendBitMap      = ICONGETA_UseFriendBitMap; // Alias for ICONGETA_UseFriendBitMap (BOOL). (V50)
  ICONDUPA_DuplicateScaledSizes = ICONA_Dummy + 119; // Duplicate any scaled versions of the icon's imagery (BOOL). (V51)
  ICONDUPA_Width                = ICONGETA_Width;    // Request a specific size for the icon to be duplicated.
  ICONDUPA_Height               = ICONGETA_Height;
  ICONDUPA_ForceScaling         = ICONGETA_ForceScaling; // Tell icon.library to perform a manual bitmap rescaling of the duplicate icon
  ICONDUPA_SizeBounds           = ICONGETA_SizeBounds; // Specify the size limits for the icon to be duplicated.
  ICONDUPA_AllowUpscaling       = ICONGETA_AllowUpscaling; // Tell icon.library to enforce the minimum size specified with ICONDUPA_SizeBounds

// Tags for use with DrawIconStateA() and GetIconRectangleA().
  ICONDRAWA_DrawInfo        = ICONA_Dummy + 66; // Drawing information to use (PDrawInfo).
  ICONDRAWA_Frameless       = ICONA_Dummy + 70; // Draw the icon without the surrounding frame (BOOL).
  ICONDRAWA_EraseBackground = ICONA_Dummy + 71; // Erase the background before drawing a frameless icon (BOOL).
  ICONDRAWA_Borderless      = ICONA_Dummy + 83; // Draw the icon without the surrounding border and frame (BOOL).
  ICONDRAWA_IsLink               = ICONA_Dummy + 89;  // The icon to be drawn refers to a linked object (BOOL). */
  ICONDRAWA_LabelShadow          = ICONA_Dummy + 93;  // Draw the icon label with shadow (BOOL). (V50) */
  ICONDRAWA_LabelOutline         = ICONA_Dummy + 94;  // Draw the icon label with outline (BOOL). (V50) */
  ICONDRAWA_Properties           = ICONA_Dummy + 95;  // Special properties of the icon to be drawn (ULONG). (V51) */
  ICONDRAWA_DrawIcon             = ICONA_Dummy + 98;  // Draw the actual icon image. Defaults to TRUE (BOOL). (V51) */
  ICONDRAWA_EraseLabelBackground = ICONA_Dummy + 101; // Erase the background before drawing an icon label (BOOL). (V51) */
  ICONDRAWA_Transparency         = ICONA_Dummy + 102; // Override the transparency value which is used for drawing the icon image in range from 0 to 255 (opaque) (LONG). (V51)
  ICONDRAWA_Width                = ICONGETA_Width;  // Specify a size for the icon to be drawn.
  ICONDRAWA_Height               = ICONGETA_Height;
  ICONDRAWA_SizeBounds           = ICONGETA_SizeBounds; // Specify the size limits for the icon to be drawn.
  ICONDRAWA_AllowUpscaling       = ICONGETA_AllowUpscaling; // Tell icon.library to enforce the minimum size specified with ICONDRAWA_SizeBounds
  ICONDRAWA_Borders              = ICONA_Dummy + 125; // Specify the size of the (possibly invisible) borders around the icon to be drawn
// Property flags for ICONDRAWA_Properties
  ICON_DRAGGED   = $00000001; // Icon is being dragged
  ICON_DROPPABLE = $00000002; // Icon is over a drop area
  ICON_LEFTOUT   = $00000004; // Icon has been "left out"
  ICON_POINTED   = $00000008; // Icon is under mouse pointer
// Reserved tags; don't use!
  ICONA_Reserved1   = ICONA_Dummy + 73;
  ICONA_Reserved2   = ICONA_Dummy + 74;
  ICONA_Reserved3   = ICONA_Dummy + 76;
  ICONA_Reserved4   = ICONA_Dummy + 81;
  ICONA_Reserved5   = ICONA_Dummy + 85;
  ICONA_Reserved6   = ICONA_Dummy + 86;
  ICONA_Reserved7   = ICONA_Dummy + 87;
  ICONA_Reserved8   = ICONA_Dummy + 88;
  ICONA_Reserved9   = ICONA_Dummy + 99;
  ICONA_Reserved10  = ICONA_Dummy + 100;
  ICONA_Reserved11  = ICONA_Dummy + 109;
  ICONA_Reserved12  = ICONA_Dummy + 110;
  ICONA_Reserved13  = ICONA_Dummy + 121;

  ICONA_LAST_TAG    = ICONA_Dummy + 126;

var
  IconBase: PLibrary;
  IIcon: PInterface;

function IcontObtain(): LongWord; syscall IIcon 60;
function IconRelease(): LongWord; syscall IIcon 64;
procedure IconExpunge(); syscall IIcon 68;
function IconClone(): PInterface; syscall IIcon 72;
// 76-88 private
procedure FreeFreeList(FreeList: PFreeList); syscall IIcon 92;
// 92-96 private
function AddFreeList(FreeList: PFreeList; const Mem: APTR; Size: LongWord): LongBool; syscall IIcon 104;
function GetDiskObject(const Name: STRPTR): PDiskObject; syscall IIcon 108;
function PutDiskObject(const Name: STRPTR; const Icon: PDiskObject): LongBool; syscall IIcon 112;
procedure FreeDiskObject(DiskObj: PDiskObject); syscall IIcon 116;
function FindToolType(const ToolTypeArray: PPChar; const TypeName: STRPTR): STRPTR; syscall IIcon 120;
function MatchToolValue(const TypeString: STRPTR; const Value: STRPTR): BOOL; syscall IIcon 124;
function BumpRevision(NewName: PChar; const OldName: PChar): PChar; syscall IIcon 128;
function FreeAlloc(FreeList: PFreeList; Len, Type_: LongWord): APTR; syscall IIcon 132;
function GetDefDiskObject(Typ: LongInt): PDiskObject; syscall IIcon 136;
function PutDefDiskObject(const Icon: PDiskObject): LongBool; syscall IIcon 140;
function GetDiskObjectNew(const Name: STRPTR): PDiskObject; syscall IIcon 144;
function DeleteDiskObject(const Name: STRPTR): LongBool; syscall IIcon 148;
function FreeFree(FreeList: PFreeList; Address: APTR): LongBool; syscall IIcon 152;
function DupDiskObjectA(const DiskObject: PDiskObject; const Tags: PTagItem): PDiskObject; syscall IIcon 156;
// 160 DupDiskObject
function IconControlA(Icon: PDiskObject; const Tags: PTagItem): LongWord; syscall IIcon 164;
// 168 IconControl
procedure DrawIconStateA(Rp: PRastPort; const Icon: PDiskObject; const Label_: STRPTR; LeftOffset, TopOffset: LongInt; State: LongWord; const Tags: PTagItem); syscall IIcon 172;
// 176 DrawIconState
function GetIconRectangleA(Rp: PRastPort; const Icon: PDiskObject; const Label_: STRPTR; Rect: PRectangle; const Tags: PTagItem): LongBool; syscall IIcon 180;
// 184 GetIconRectangle
function NewDiskObject(Type_: LongWord): PDiskObject; syscall IIcon 188;
function GetIconTagList(const Name: STRPTR; const Tags: PTagItem): PDiskObject; syscall IIcon 192;
// 196 GetIconTags
function PutIconTagList(const Name: STRPTR; const Icon: PDiskObject; const Tags: PTagItem): LongBool; syscall IIcon 200;
// 204 PutIconTags
function LayoutIconA(Icon: PDiskObject; Screen: PScreen; Tags: PTagItem): LongBool; syscall IIcon 208;
// 212 LayoutIcon
procedure ChangeToSelectedIconColor(Cr: Pointer); syscall IIcon 216; //TODO: PColorRegister
function BumpRevisionLength(NewName: STRPTR; const OldName: STRPTR; MaxLength: LongWord): STRPTR; syscall IIcon 220;

{macros}
function PACK_ICON_ASPECT_RATIO(Num, Den: LongInt): LongInt;
procedure UNPACK_ICON_ASPECT_RATIO(Aspect: LongInt; var Num, Den: LongInt);

type
  TToolTypeArray= array of AnsiString;

function GetToolTypes(Filename: AnsiString): TToolTypeArray;


implementation

function GetToolTypes(Filename: AnsiString): TToolTypeArray;
var
  DObj: PDiskObject;
  Tooltype: PPChar;
  Idx: Integer;
begin
  SetLength(GetToolTypes, 0);
  DObj := GetDiskObject(PChar(FileName));
  if not Assigned(Dobj) then
    Exit;
  Tooltype := DObj^.do_Tooltypes;
  while Assigned(ToolType^) do
  begin
    Idx := Length(GetToolTypes);
    SetLength(GetToolTypes, Idx + 1);
    GetToolTypes[Idx] := ToolType^;
    Inc(ToolType);
  end;
  FreeDiskObject(DObj);
end;

function PACK_ICON_ASPECT_RATIO(Num, Den: LongInt): LongInt; inline;
begin
  PACK_ICON_ASPECT_RATIO := (Num shl 4) or Den;
end;

procedure UNPACK_ICON_ASPECT_RATIO(Aspect: LongInt; var Num, Den: LongInt); inline;
begin
  Num := (Aspect shr 4) and $F;
  Den := Aspect and $15;
end;

initialization
  IconBase := OpenLibrary(ICONNAME, 40);
  if Assigned(IconBase) then
    IIcon := GetInterface(IconBase, 'main', 1, nil);
finalization
  if Assigned(IIcon) then
    DropInterface(IIcon);
  if Assigned(IconBase) then
  CloseLibrary(IconBase);
end.




