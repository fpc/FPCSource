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

{$mode delphi}

interface

uses
  exec, workbench,utility,amigados, agraphics, intuition;
//,datatypes;

const
  ICONNAME    : PChar = 'icon.library';  
  
  ICONA_Dummy = TAG_USER + $9000;  // Start of icon.library tags
  ICONA_ErrorCode = ICONA_Dummy + 1; // Errorcode (PLongInt)
  ICONA_ErrorTagItem = ICONA_Dummy + 75; //Points to the tag item that caused the error (^PTagItem).
{Global options for IconControlA()}
  { Screen to use for remapping Workbench icons to (PScreen)  }
  ICONCTRLA_SetGlobalScreen = ICONA_Dummy + 2;
  ICONCTRLA_GetGlobalScreen = ICONA_Dummy + 3;
  { Icon color remapping precision; defaults to PRECISION_ICON (LongInt)  }
  ICONCTRLA_SetGlobalPrecision = ICONA_Dummy + 4;
  ICONCTRLA_GetGlobalPrecision = ICONA_Dummy + 5;
  { Icon frame size dimensions (PRectange)  }
  ICONCTRLA_SetGlobalEmbossRect = ICONA_Dummy + 6;
  ICONCTRLA_GetGlobalEmbossRect = ICONA_Dummy + 7;
  { Render image without frame (BOOL)  }
  ICONCTRLA_SetGlobalFrameless = ICONA_Dummy + 8;
  ICONCTRLA_GetGlobalFrameless = ICONA_Dummy + 9;
  { Enable NewIcons support (BOOL)  }
  ICONCTRLA_SetGlobalNewIconsSupport = ICONA_Dummy + 10;
  ICONCTRLA_GetGlobalNewIconsSupport = ICONA_Dummy + 11;
  { Enable color icon support (BOOL)  }
  ICONCTRLA_SetGlobalColorIconSupport = ICONA_Dummy + 77;
  ICONCTRLA_GetGlobalColorIconSupport = ICONA_Dummy + 78;
  { Set/Get the hook to be called when identifying a file (PHook)  }
  ICONCTRLA_SetGlobalIdentifyHook = ICONA_Dummy + 12;
  ICONCTRLA_GetGlobalIdentifyHook = ICONA_Dummy + 13;
  { Set/get the maximum length of a file/drawer name supported
     by icon.library (LONG). }
  ICONCTRLA_SetGlobalMaxNameLength = ICONA_Dummy + 67;
  ICONCTRLA_GetGlobalMaxNameLength = ICONA_Dummy + 68;

{ Per icon local options for IconControlA()  }
  { Get the icon rendering masks (PLANEPTR)  }
  ICONCTRLA_GetImageMask1 = ICONA_Dummy + 14;
  ICONCTRLA_GetImageMask2 = ICONA_Dummy + 15;
  { Transparent image color; set to -1 if opaque  }
  ICONCTRLA_SetTransparentColor1 = ICONA_Dummy + 16;
  ICONCTRLA_GetTransparentColor1 = ICONA_Dummy + 17;
  ICONCTRLA_SetTransparentColor2 = ICONA_Dummy + 18;
  ICONCTRLA_GetTransparentColor2 = ICONA_Dummy + 19;
  { Image color palette (PColorRegister)  }
  ICONCTRLA_SetPalette1 = ICONA_Dummy + 20;
  ICONCTRLA_GetPalette1 = ICONA_Dummy + 21;
  ICONCTRLA_SetPalette2 = ICONA_Dummy + 22;
  ICONCTRLA_GetPalette2 = ICONA_Dummy + 23;
  { Size of image color palette (LongInt)  }
  ICONCTRLA_SetPaletteSize1 = ICONA_Dummy + 24;
  ICONCTRLA_GetPaletteSize1 = ICONA_Dummy + 25;
  ICONCTRLA_SetPaletteSize2 = ICONA_Dummy + 26;
  ICONCTRLA_GetPaletteSize2 = ICONA_Dummy + 27;
  { Image data; one by per pixel (PBYTE)  }
  ICONCTRLA_SetImageData1 = ICONA_Dummy + 28;
  ICONCTRLA_GetImageData1 = ICONA_Dummy + 29;
  ICONCTRLA_SetImageData2 = ICONA_Dummy + 30;
  ICONCTRLA_GetImageData2 = ICONA_Dummy + 31;
  { Render image without frame (BOOL)  }
  ICONCTRLA_SetFrameless = ICONA_Dummy + 32;
  ICONCTRLA_GetFrameless = ICONA_Dummy + 33;
  { Enable NewIcons support (BOOL)  }
  ICONCTRLA_SetNewIconsSupport = ICONA_Dummy + 34;
  ICONCTRLA_GetNewIconsSupport = ICONA_Dummy + 35;
  { Icon aspect ratio (PBYTE)  }
  ICONCTRLA_SetAspectRatio = ICONA_Dummy + 36;
  ICONCTRLA_GetAspectRatio = ICONA_Dummy + 37;
  { Icon dimensions; valid only for palette mapped icon images (LongInt) }
  ICONCTRLA_SetWidth = ICONA_Dummy + 38;
  ICONCTRLA_GetWidth = ICONA_Dummy + 39;
  ICONCTRLA_SetHeight = ICONA_Dummy + 40;
  ICONCTRLA_GetHeight = ICONA_Dummy + 41;
  { Check whether the icon is palette mapped (PLongInt).  }
  ICONCTRLA_IsPaletteMapped = ICONA_Dummy + 42;
  { Get the screen the icon is attached to (^PScreen).  }
  ICONCTRLA_GetScreen = ICONA_Dummy + 43;
  { Check whether the icon has a real select image (PLongInt).  }
  ICONCTRLA_HasRealImage2 = ICONA_Dummy + 44;
  { Check whether the icon is of the NewIcon type (PLongInt).  }
  ICONCTRLA_IsNewIcon = ICONA_Dummy + 79;
  { Check whether this icon was allocated by icon.library
     or if consists solely of a statically allocated
     struct DiskObject. (PLongInt).}
  ICONCTRLA_IsNativeIcon = ICONA_Dummy + 80;

{ Icon Aspect Handling}

  { Icon aspect ratio is not known.  }
  ICON_ASPECT_RATIO_UNKNOWN = 0;

{ Tags for use with GetIconTagList() }

  { Default icon type to retrieve (LongInt) }
  ICONGETA_GetDefaultType    = ICONA_Dummy+45;
  { Retrieve default icon for the given name (PChar) }
  ICONGETA_GetDefaultName = ICONA_Dummy + 46;
  { Return a default icon if the requested icon
    file cannot be found (BOOL).}
  ICONGETA_FailIfUnavailable = ICONA_Dummy + 47;
  { If possible, retrieve a palette mapped icon (BOOL).  }
  ICONGETA_GetPaletteMappedIcon = ICONA_Dummy + 48;
  { Set if the icon returned is a default icon (PLongBool).  }
  ICONGETA_IsDefaultIcon = ICONA_Dummy + 49;
  { Remap the icon to the default screen, if possible (BOOL).  }
  ICONGETA_RemapIcon = ICONA_Dummy + 50;
  { Generate icon image masks (BOOL).  }
  ICONGETA_GenerateImageMasks = ICONA_Dummy + 51;
  { Label text to be assigned to the icon (PChar).  }
  ICONGETA_Label = ICONA_Dummy + 52;
  { Screen to remap the icon to (PScreen).  }
  ICONGETA_Screen = ICONA_Dummy + 69;

{ Tags for use with PutIconTagList()  }

  { Notify Workbench of the icon being written (BOOL)  }
  ICONPUTA_NotifyWorkbench = ICONA_Dummy + 53;
  { Store icon as the default for this type (LongInt)  }
  ICONPUTA_PutDefaultType = ICONA_Dummy + 54;
  { Store icon as a default for the given name (PChar)  }
  ICONPUTA_PutDefaultName = ICONA_Dummy + 55;
  { When storing a palette mapped icon, don't save the
       the original planar icon image with the file. Replace
       it with a tiny replacement image.}
  ICONPUTA_DropPlanarIconImage = ICONA_Dummy + 56;
  { Don't write the chunky icon image data to disk.  }
  ICONPUTA_DropChunkyIconImage = ICONA_Dummy + 57;
  { Don't write the NewIcons tool types to disk.  }
  ICONPUTA_DropNewIconToolTypes = ICONA_Dummy + 58;
  { If this tag is enabled, the writer will examine the
    icon image data to find out whether it can compress
    it more efficiently. This may take extra time and
    is not generally recommended.}
  ICONPUTA_OptimizeImageSpace = ICONA_Dummy + 59;
  { Don't write the entire icon file back to disk,
    only change the do^.do_CurrentX/do^.do_CurrentY
    members.}
  ICONPUTA_OnlyUpdatePosition = ICONA_Dummy + 72;
  { Before writing a palette mapped icon back to disk,
    icon.library will make sure that the original
    planar image data is stored in the file. If you
    don't want that to happen, set this option to
    FALSE. This will allow you to change the planar icon
    image data written back to disk.}
  ICONPUTA_PreserveOldIconImages = ICONA_Dummy + 84;

{ For use with the file identification hook. }
type
  PIconIdentifyMsg = ^TIconIdentifyMsg;
  TIconIdentifyMsg = record
    { Libraries that are already opened for your use. }
    iim_SysBase : PLibrary;
    iim_DOSBase : PLibrary;
    iim_UtilityBase : PLibrary;
    iim_IconBase : PLibrary;
    { File context information. }
    iim_FileLock : BPTR;     // Lock on the object to return an icon for.
    iim_ParentLock : BPTR;   // Lock on the object's parent directory, if available.
    iim_FIB : PFileInfoBlock;// Already initialized for you.
    iim_FileHandle : BPTR;   // If non-NULL, pointer to the file to examine,
                             //   positioned right at the first byte, ready
                             //   for you to use.
    iim_Tags : PTagItem;     // Tags passed to GetIconTagList().
  end;

{ Tags for use with DupDiskObjectA()  }
const
  ICONDUPA_DuplicateDrawerData = ICONA_Dummy + 60; // Duplicate do_DrawerData
  ICONDUPA_DuplicateImages = ICONA_Dummy + 61; // Duplicate the Image structures.
  ICONDUPA_DuplicateImageData = ICONA_Dummy + 62; // Duplicate the image data (Image->ImageData) itself.
  ICONDUPA_DuplicateDefaultTool = ICONA_Dummy + 63; // Duplicate the default tool.
  ICONDUPA_DuplicateToolTypes = ICONA_Dummy + 64; // Duplicate the tool types list.
  ICONDUPA_DuplicateToolWindow = ICONA_Dummy + 65; // Duplicate the tool window.
  ICONDUPA_ActivateImageData = ICONA_Dummy + 82;{ If the icon to be duplicated is in fact a palette mapped
                                                  icon which has never been set up to be displayed on the
                                                  screen, turn the duplicate into that palette mapped icon.}

{ Tags for use with DrawIconStateA() and GetIconRectangleA().  }
  ICONDRAWA_DrawInfo = ICONA_Dummy + 66; // Drawing information to use (PDrawInfo). 
  ICONDRAWA_Frameless = ICONA_Dummy + 70; // Draw the icon without the surrounding frame (BOOL).
  ICONDRAWA_EraseBackground = ICONA_Dummy + 71; // Erase the background before drawing a frameless icon (BOOL).
  ICONDRAWA_Borderless = ICONA_Dummy + 83; // Draw the icon without the surrounding border and frame (BOOL).

{ Reserved tags; don't use!  }
  ICONA_Reserved1 = ICONA_Dummy + 73;
  ICONA_Reserved2 = ICONA_Dummy + 74;
  ICONA_Reserved3 = ICONA_Dummy + 76;
  ICONA_Reserved4 = ICONA_Dummy + 81;
  ICONA_Reserved5 = ICONA_Dummy + 85;
  ICONA_Reserved6 = ICONA_Dummy + 86;
  ICONA_Reserved7 = ICONA_Dummy + 87;
  ICONA_Reserved8 = ICONA_Dummy + 88;
{ The last Tag}  
  ICONA_LAST_TAG = ICONA_Dummy + 88;


var
  IconBase: PLibrary;

function AddFreeList(FreeList: PFreeList; const Mem: APTR; Size: ULONG): BOOL; syscall IconBase 12;
function BumpRevision(NewName: PChar; const OldName: PChar): PChar; syscall IconBase 18;
function DeleteDiskObject(const Name: PChar): BOOL; syscall IconBase 23;
function FindToolType(const ToolTypeArray: PPChar; const TypeName: STRPTR): STRPTR; syscall IconBase 16;
procedure FreeDiskObject(DiskObj: PDiskObject); syscall IconBase 15;
procedure FreeFreeList(FreeList: PFreeList); syscall IconBase 9;
function GetDefDiskObject(Typ: LongInt): PDiskObject; syscall IconBase 20;
function GetDiskObject(const Name: STRPTR): PDiskObject; syscall IconBase 13;
function GetDiskObjectNew(const Name : PChar): PDiskObject; syscall IconBase 22;
function MatchToolValue(const TypeString: PChar; const Value: PChar): BOOL; syscall IconBase 17;
function PutDefDiskObject(const Icon: PDiskObject): BOOL; syscall IconBase 21;
function PutDiskObject(const Name: STRPTR; const Icon: PDiskObject): BOOL; syscall IconBase 14;

{ version 44 }
function DupDiskObjectA(const Icon: PDiskObject; const Tags: PTagItem): PDiskObject; syscall IconBase 25;
function IconControlA(Icon: PDiskObject; const Tags: PTagItem): ULONG;  syscall IconBase 26;
procedure DrawIconStateA(Rp: PRastPort; const Icon: PDiskObject; const Label_: STRPTR;
  LeftEdge: LongInt; TopEdge: LongInt; State: ULONG; const Tags: PTagItem);  syscall IconBase 27;
function GetIconRectangleA(Rp: PRastPort; const Icon: PDiskObject; const Label_: PChar;
  Rect: PRectangle; const Tags: PTagItem): BOOL;  syscall IconBase 28;
function NewDiskObject(Type_: ULONG): PDiskObject; syscall IconBase 29;
function GetIconTagList(const Name: STRPTR; const Tags: PTagItem): PDiskObject; syscall IconBase 30;
function PutIconTagList(const Name: STRPTR; const Icon: PDiskObject; const Tags: PTagItem): BOOL; syscall IconBase 31;
function LayoutIconA(Icon: PDiskObject; Screen: PScreen; Tags: PTagItem): BOOL; syscall IconBase 32;
procedure ChangeToSelectedIconColor(Cr: Pointer); syscall IconBase 33; //TODO: pColorRegister  // 33

{macros}
function PACK_ICON_ASPECT_RATIO(Num, Den: LongInt): LongInt;
procedure UNPACK_ICON_ASPECT_RATIO(Aspect: LongInt; out Num, Den: LongInt);

type
  TToolTypeArray= array of string;
  
function GetToolTypes(Filename: string): TToolTypeArray;


implementation

function GetToolTypes(Filename: string): TToolTypeArray;
var
  DObj: PDiskObject;
  Tooltype: PPChar;
  Idx: Integer;
begin
  SetLength(Result, 0);
  DObj := GetDiskObject(PChar(FileName));
  if not Assigned(Dobj) then
    Exit;
  Tooltype := DObj^.do_Tooltypes;
  while Assigned(ToolType^) do
  begin
    Idx := Length(Result);
    SetLength(Result, Idx + 1);
    Result[Idx] := ToolType^;
    Inc(ToolType);
  end;
  FreeDiskObject(DObj);
end;

function PACK_ICON_ASPECT_RATIO(Num, Den: LongInt): LongInt;
begin
  PACK_ICON_ASPECT_RATIO := (Num shl 4) or Den;
end;

procedure UNPACK_ICON_ASPECT_RATIO(Aspect: LongInt; out Num, Den: LongInt);
begin
  Num := (Aspect shr 4) and $F;
  Den := Aspect and $15;
end;

initialization
  IconBase := OpenLibrary(ICONNAME, 40);
finalization
  CloseLibrary(IconBase);
end.




