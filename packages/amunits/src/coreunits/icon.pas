{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:
    Added overlay functions for Pchar->Strings, functions
    and procedures.
    14 Jul 2000.

    Removed amigaoverlays, use smartlink instead.
    05 Nov 2002.

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening of
    the library.
    14 Jan 2003.

    Update for AmigaOS 3.9.
    A lof of new const and a record.
    Functions added.
         FUNCTION DupDiskObjectA
         FUNCTION IconControlA
         PROCEDURE DrawIconStateA
         FUNCTION GetIconRectangleA
         FUNCTION NewDiskObject
         FUNCTION GetIconTagList
         FUNCTION PutIconTagList
         FUNCTION LayoutIconA
         PROCEDURE ChangeToSelectedIconColor
    plus overlay for
         FUNCTION GetIconTagList;
         FUNCTION PutIconTagList
    Changed start code for unit.
    02 Feb 2003.

    Changed cardinal > longword.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se
}

unit icon;

INTERFACE


uses exec, workbench,utility,amigados,agraphics,intuition,datatypes;

    const
    ICONA_Dummy = TAG_USER + $9000;
  {                                                                           }
  { Error reporting (LONG  )  }
     ICONA_ErrorCode = ICONA_Dummy + 1;

  { Points to the tag item that caused the error (struct TagItem   ).  }
     ICONA_ErrorTagItem = ICONA_Dummy + 75;
  {                                                                           }
  { Global options for IconControlA()  }

  { Screen to use for remapping Workbench icons to (struct Screen  )  }
     ICONCTRLA_SetGlobalScreen = ICONA_Dummy + 2;
     ICONCTRLA_GetGlobalScreen = ICONA_Dummy + 3;

  { Icon color remapping precision; defaults to PRECISION_ICON (LONG)  }
     ICONCTRLA_SetGlobalPrecision = ICONA_Dummy + 4;
     ICONCTRLA_GetGlobalPrecision = ICONA_Dummy + 5;

  { Icon frame size dimensions (struct Rectangle  )  }
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

  { Set/Get the hook to be called when identifying a file (struct Hook  )  }
     ICONCTRLA_SetGlobalIdentifyHook = ICONA_Dummy + 12;
     ICONCTRLA_GetGlobalIdentifyHook = ICONA_Dummy + 13;

  { Set/get the maximum length of a file/drawer name supported
     by icon.library (LONG).
    }
     ICONCTRLA_SetGlobalMaxNameLength = ICONA_Dummy + 67;
     ICONCTRLA_GetGlobalMaxNameLength = ICONA_Dummy + 68;

  {**************************************************************************}
  { Per icon local options for IconControlA()  }

  { Get the icon rendering masks (PLANEPTR)  }
     ICONCTRLA_GetImageMask1 = ICONA_Dummy + 14;
     ICONCTRLA_GetImageMask2 = ICONA_Dummy + 15;

  { Transparent image color; set to -1 if opaque  }
     ICONCTRLA_SetTransparentColor1 = ICONA_Dummy + 16;
     ICONCTRLA_GetTransparentColor1 = ICONA_Dummy + 17;
     ICONCTRLA_SetTransparentColor2 = ICONA_Dummy + 18;
     ICONCTRLA_GetTransparentColor2 = ICONA_Dummy + 19;

  { Image color palette (struct ColorRegister  )  }
     ICONCTRLA_SetPalette1 = ICONA_Dummy + 20;
     ICONCTRLA_GetPalette1 = ICONA_Dummy + 21;
     ICONCTRLA_SetPalette2 = ICONA_Dummy + 22;
     ICONCTRLA_GetPalette2 = ICONA_Dummy + 23;

  { Size of image color palette (LONG)  }
     ICONCTRLA_SetPaletteSize1 = ICONA_Dummy + 24;
     ICONCTRLA_GetPaletteSize1 = ICONA_Dummy + 25;
     ICONCTRLA_SetPaletteSize2 = ICONA_Dummy + 26;
     ICONCTRLA_GetPaletteSize2 = ICONA_Dummy + 27;

  { Image data; one by per pixel (UBYTE  )  }
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

  { Icon aspect ratio (UBYTE  )  }
     ICONCTRLA_SetAspectRatio = ICONA_Dummy + 36;
     ICONCTRLA_GetAspectRatio = ICONA_Dummy + 37;

  { Icon dimensions; valid only for palette mapped icon images (LONG)  }
     ICONCTRLA_SetWidth = ICONA_Dummy + 38;
     ICONCTRLA_GetWidth = ICONA_Dummy + 39;
     ICONCTRLA_SetHeight = ICONA_Dummy + 40;
     ICONCTRLA_GetHeight = ICONA_Dummy + 41;

  { Check whether the icon is palette mapped (LONG  ).  }
     ICONCTRLA_IsPaletteMapped = ICONA_Dummy + 42;

  { Get the screen the icon is attached to (struct Screen   ).  }
     ICONCTRLA_GetScreen = ICONA_Dummy + 43;

  { Check whether the icon has a real select image (LONG  ).  }
     ICONCTRLA_HasRealImage2 = ICONA_Dummy + 44;

  { Check whether the icon is of the NewIcon type (LONG  ).  }
     ICONCTRLA_IsNewIcon = ICONA_Dummy + 79;

  { Check whether this icon was allocated by icon.library
     or if consists solely of a statically allocated
     struct DiskObject. (LONG  ).
    }
     ICONCTRLA_IsNativeIcon = ICONA_Dummy + 80;

  {**************************************************************************}

  { Icon aspect ratio is not known.  }
     ICON_ASPECT_RATIO_UNKNOWN = 0;

{ Tags for use with GetIconTagList() }

{ Default icon type to retrieve (LONG) }
     ICONGETA_GetDefaultType    = ICONA_Dummy+45;

{ Retrieve default icon for the given name (STRPTR) }
       ICONGETA_GetDefaultName = ICONA_Dummy + 46;

    { Return a default icon if the requested icon
       file cannot be found (BOOL).
      }
       ICONGETA_FailIfUnavailable = ICONA_Dummy + 47;

    { If possible, retrieve a palette mapped icon (BOOL).  }
       ICONGETA_GetPaletteMappedIcon = ICONA_Dummy + 48;

    { Set if the icon returned is a default icon (BOOL  ).  }
       ICONGETA_IsDefaultIcon = ICONA_Dummy + 49;

    { Remap the icon to the default screen, if possible (BOOL).  }
       ICONGETA_RemapIcon = ICONA_Dummy + 50;

    { Generate icon image masks (BOOL).  }
       ICONGETA_GenerateImageMasks = ICONA_Dummy + 51;

    { Label text to be assigned to the icon (STRPTR).  }
       ICONGETA_Label = ICONA_Dummy + 52;

    { Screen to remap the icon to (struct Screen  ).  }
       ICONGETA_Screen = ICONA_Dummy + 69;

    {**************************************************************************}

    { Tags for use with PutIconTagList()  }

    { Notify Workbench of the icon being written (BOOL)  }
       ICONPUTA_NotifyWorkbench = ICONA_Dummy + 53;

    { Store icon as the default for this type (LONG)  }
       ICONPUTA_PutDefaultType = ICONA_Dummy + 54;

    { Store icon as a default for the given name (STRPTR)  }
       ICONPUTA_PutDefaultName = ICONA_Dummy + 55;

    { When storing a palette mapped icon, don't save the
       the original planar icon image with the file. Replace
       it with a tiny replacement image.
      }
       ICONPUTA_DropPlanarIconImage = ICONA_Dummy + 56;

    { Don't write the chunky icon image data to disk.  }
       ICONPUTA_DropChunkyIconImage = ICONA_Dummy + 57;

    { Don't write the NewIcons tool types to disk.  }
       ICONPUTA_DropNewIconToolTypes = ICONA_Dummy + 58;

    { If this tag is enabled, the writer will examine the
       icon image data to find out whether it can compress
       it more efficiently. This may take extra time and
       is not generally recommended.
      }
       ICONPUTA_OptimizeImageSpace = ICONA_Dummy + 59;

    { Don't write the entire icon file back to disk,
       only change the do->do_CurrentX/do->do_CurrentY
       members.
      }
       ICONPUTA_OnlyUpdatePosition = ICONA_Dummy + 72;

    { Before writing a palette mapped icon back to disk,
       icon.library will make sure that the original
       planar image data is stored in the file. If you
       don't want that to happen, set this option to
       FALSE. This will allow you to change the planar icon
       image data written back to disk.
      }
      ICONPUTA_PreserveOldIconImages = ICONA_Dummy + 84;

{**************************************************************************}

{ For use with the file identification hook. }

          type
       PIconIdentifyMsg = ^tIconIdentifyMsg;
       tIconIdentifyMsg = record
            { Libraries that are already opened for your use. }
            iim_SysBase : PLibrary;
            iim_DOSBase : PLibrary;
            iim_UtilityBase : PLibrary;
            iim_IconBase : PLibrary;

            { File context information. }
            iim_FileLock : BPTR;           { Lock on the object to return an icon for. }
            iim_ParentLock : BPTR;         { Lock on the object's parent directory, if available. }
            iim_FIB : PFileInfoBlock;      { Already initialized for you. }
            iim_FileHandle : BPTR;         { If non-NULL, pointer to the file to examine,
                                                 * positioned right at the first byte, ready
                                                 * for you to use.
                                                 }
            iim_Tags : PTagItem;           { Tags passed to GetIconTagList(). }
         end;

{**************************************************************************}

    { Tags for use with DupDiskObjectA()  }

    const
    { Duplicate do_DrawerData  }
       ICONDUPA_DuplicateDrawerData = ICONA_Dummy + 60;

    { Duplicate the Image structures.  }
       ICONDUPA_DuplicateImages = ICONA_Dummy + 61;

    { Duplicate the image data (Image->ImageData) itself.  }
       ICONDUPA_DuplicateImageData = ICONA_Dummy + 62;

    { Duplicate the default tool.  }
       ICONDUPA_DuplicateDefaultTool = ICONA_Dummy + 63;

    { Duplicate the tool types list.  }
       ICONDUPA_DuplicateToolTypes = ICONA_Dummy + 64;

    { Duplicate the tool window.  }
       ICONDUPA_DuplicateToolWindow = ICONA_Dummy + 65;

    { If the icon to be duplicated is in fact a palette mapped
       icon which has never been set up to be displayed on the
       screen, turn the duplicate into that palette mapped icon.
      }
       ICONDUPA_ActivateImageData = ICONA_Dummy + 82;

{**************************************************************************}

    { Tags for use with DrawIconStateA() and GetIconRectangleA().  }

    { Drawing information to use (struct DrawInfo  ).  }
       ICONDRAWA_DrawInfo = ICONA_Dummy + 66;

    { Draw the icon without the surrounding frame (BOOL).  }
       ICONDRAWA_Frameless = ICONA_Dummy + 70;

    { Erase the background before drawing a frameless icon (BOOL).  }
       ICONDRAWA_EraseBackground = ICONA_Dummy + 71;

    { Draw the icon without the surrounding border and frame (BOOL).  }
       ICONDRAWA_Borderless = ICONA_Dummy + 83;

    { The icon to be drawn refers to a linked object (BOOL).  }
       ICONDRAWA_IsLink = ICONA_Dummy + 89;

{**************************************************************************}

    { Reserved tags; don't use!  }
       ICONA_Reserved1 = ICONA_Dummy + 73;
       ICONA_Reserved2 = ICONA_Dummy + 74;
       ICONA_Reserved3 = ICONA_Dummy + 76;
       ICONA_Reserved4 = ICONA_Dummy + 81;
       ICONA_Reserved5 = ICONA_Dummy + 85;
       ICONA_Reserved6 = ICONA_Dummy + 86;
       ICONA_Reserved7 = ICONA_Dummy + 87;
       ICONA_Reserved8 = ICONA_Dummy + 88;
    {                                                                           }
       ICONA_LAST_TAG = ICONA_Dummy + 89;

{**************************************************************************}


Const

    ICONNAME    : PChar = 'icon.library';

VAR IconBase : pLibrary;

FUNCTION AddFreeList(freelist : pFreeList location 'a0'; const mem : POINTER location 'a1'; size : ULONG location 'a2') : LongBool; syscall IconBase 072;
FUNCTION BumpRevision(newname : pCHAR location 'a0'; const oldname : pCHAR location 'a1') : pCHAR; syscall IconBase 108;
FUNCTION DeleteDiskObject(const name : pCHAR location 'a0') : LongBool; syscall IconBase 138;
FUNCTION FindToolType(const toolTypeArray : POINTER location 'a0'; const typeName : pCHAR location 'a1') : pCHAR; syscall IconBase 096;
PROCEDURE FreeDiskObject(diskobj : pDiskObject location 'a0'); syscall IconBase 090;
PROCEDURE FreeFreeList(freelist : pFreeList location 'a0'); syscall IconBase 054;
FUNCTION GetDefDiskObject(typ : LONGINT location 'd0') : pDiskObject; syscall IconBase 120;
FUNCTION GetDiskObject(const name : pCHAR location 'a0') : pDiskObject; syscall IconBase 078;
FUNCTION GetDiskObjectNew(const name : pCHAR location 'a0') : pDiskObject; syscall IconBase 132;
FUNCTION MatchToolValue(const typeString : pCHAR location 'a0'; const value : pCHAR location 'a1') : LongBool; syscall IconBase 102;
FUNCTION PutDefDiskObject(const diskObject : pDiskObject location 'a0') : LongBool; syscall IconBase 126;
FUNCTION PutDiskObject(const name : pCHAR location 'a0'; const diskobj : pDiskObject location 'a1') : LongBool; syscall IconBase 084;

{ version 44 }
FUNCTION DupDiskObjectA(CONST diskObject : pDiskObject location 'a0'; CONST tags : pTagItem location 'a1') : pDiskObject; syscall IconBase 150;
FUNCTION IconControlA(icon : pDiskObject location 'a0'; CONST tags : pTagItem location 'a1') : longword; syscall IconBase 156;
PROCEDURE DrawIconStateA(rp : pRastPort location 'a0'; CONST icon : pDiskObject location 'a1'; CONST label_ : pCHAR location 'a2'; leftOffset : LONGINT location 'd0'; topOffset : LONGINT location 'd1'; state : longword location 'd2'; CONST tags : pTagItem location 'a3'); syscall IconBase 162;
FUNCTION GetIconRectangleA(rp : pRastPort location 'a0'; CONST icon : pDiskObject location 'a1'; CONST label_ : pCHAR location 'a2'; rect : pRectangle location 'a3'; CONST tags : pTagItem location 'a4') : LongBool; syscall IconBase 168;
FUNCTION NewDiskObject(type_ : LONGINT location 'd0') : pDiskObject; syscall IconBase 174;
FUNCTION GetIconTagList(CONST name : pCHAR location 'a0'; CONST tags : pTagItem location 'a1') : pDiskObject; syscall IconBase 180;
FUNCTION PutIconTagList(CONST name : pCHAR location 'a0'; CONST icon : pDiskObject location 'a1'; CONST tags : pTagItem location 'a2') : LongBool; syscall IconBase 186;
FUNCTION LayoutIconA(icon : pDiskObject location 'a0'; screen : pScreen location 'a1'; tags : pTagItem location 'a2') : LongBool; syscall IconBase 192;
PROCEDURE ChangeToSelectedIconColor(cr : pColorRegister location 'a0'); syscall IconBase 198;

{ overlay }
FUNCTION BumpRevision(newname : string;const oldname : pCHAR) : pCHAR;
FUNCTION BumpRevision(newname : pCHar;const oldname : string) : pCHAR;
FUNCTION BumpRevision(newname : string;const oldname : string) : pCHAR;
FUNCTION DeleteDiskObject(const name : string) : BOOLEAN;
FUNCTION FindToolType(const toolTypeArray : POINTER;const typeName : string) : pCHAR;
FUNCTION GetDiskObject(const name : string) : pDiskObject;
FUNCTION GetDiskObjectNew(const name : string) : pDiskObject;
FUNCTION MatchToolValue(const typeString :string;const value : pCHAR) : BOOLEAN;
FUNCTION MatchToolValue(const typeString : pCHAR;const value : string) : BOOLEAN;
FUNCTION MatchToolValue(const typeString : string;const value : string) : BOOLEAN;
FUNCTION PutDiskObject(const name : string;const diskobj : pDiskObject) : BOOLEAN;

{ version 44 overlay}
FUNCTION GetIconTagList(CONST name : string; CONST tags : pTagItem) : pDiskObject;
FUNCTION PutIconTagList(CONST name : string; CONST icon : pDiskObject; CONST tags : pTagItem) : BOOLEAN;

{macros}
function PACK_ICON_ASPECT_RATIO(num,den : longint) : longint;


{Here we read how to compile this unit}
{You can remove this include and use a define instead}
{$I useautoopenlib.inc}
{$ifdef use_init_openlib}
procedure InitICONLibrary;
{$endif use_init_openlib}

{This is a variable that knows how the unit is compiled}
var
    ICONIsCompiledHow : longint;

IMPLEMENTATION

uses
{$ifndef dont_use_openlib}
amsgbox,
{$endif dont_use_openlib}
pastoc;

function PACK_ICON_ASPECT_RATIO(num,den : longint) : longint;
begin
    PACK_ICON_ASPECT_RATIO:=(num shl 4) or den;
end;


FUNCTION BumpRevision(newname : string;const oldname : pCHAR) : pCHAR;
begin
      BumpRevision := BumpRevision(pas2c(newname),oldname);
end;

FUNCTION BumpRevision(newname : pCHar;const oldname : string) : pCHAR;
begin
      BumpRevision := BumpRevision(newname,pas2c(oldname));
end;

FUNCTION BumpRevision(newname : string;const oldname : string) : pCHAR;
begin
      BumpRevision := BumpRevision(pas2c(newname),pas2c(oldname));
end;

FUNCTION DeleteDiskObject(const name : string) : BOOLEAN;
begin
      DeleteDiskObject := DeleteDiskObject(pas2c(name));
end;

FUNCTION FindToolType(const toolTypeArray : POINTER;const typeName : string) : pCHAR;
begin
      FindToolType := FindToolType(toolTypeArray,pas2c(typeName));
end;

FUNCTION GetDiskObject(const name : string) : pDiskObject;
begin
      GetDiskObject := GetDiskObject(pas2c(name));
end;

FUNCTION GetDiskObjectNew(const name : string) : pDiskObject;
begin
      GetDiskObjectNew := GetDiskObjectNew(pas2c(name));
end;

FUNCTION MatchToolValue(const typeString : string;const value : pCHAR) : BOOLEAN;
begin
       MatchToolValue := MatchToolValue(pas2c(typeString),value);
end;

FUNCTION MatchToolValue(const typeString : pCHAR;const value : string) : BOOLEAN;
begin
       MatchToolValue := MatchToolValue(typeString,pas2c(value));
end;

FUNCTION MatchToolValue(const typeString : string;const value : string) : BOOLEAN;
begin
       MatchToolValue := MatchToolValue(pas2c(typeString),pas2c(value));
end;

FUNCTION PutDiskObject(const name : string;const diskobj : pDiskObject) : BOOLEAN;
begin
       PutDiskObject := PutDiskObject(pas2c(name),diskobj);
end;

FUNCTION GetIconTagList(CONST name : string; CONST tags : pTagItem) : pDiskObject;
begin
       GetIconTagList := GetIconTagList(pas2c(name),tags);
end;

FUNCTION PutIconTagList(CONST name : string; CONST icon : pDiskObject; CONST tags : pTagItem) : BOOLEAN;
begin
       PutIconTagList := PutIconTagList(pas2c(name),icon,tags);
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

{$ifdef use_init_openlib}
  {$Info Compiling initopening of icon.library}
  {$Info don't forget to use InitICONLibrary in the beginning of your program}

var
    icon_exit : Pointer;

procedure CloseiconLibrary;
begin
    ExitProc := icon_exit;
    if IconBase <> nil then begin
        CloseLibrary(IconBase);
        IconBase := nil;
    end;
end;

procedure InitICONLibrary;
begin
    IconBase := nil;
    IconBase := OpenLibrary(ICONNAME,LIBVERSION);
    if IconBase <> nil then begin
        icon_exit := ExitProc;
        ExitProc := @CloseiconLibrary;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open icon.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;
end;

begin
    ICONIsCompiledHow := 2;
{$endif use_init_openlib}

{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of icon.library}

var
    icon_exit : Pointer;

procedure CloseiconLibrary;
begin
    ExitProc := icon_exit;
    if IconBase <> nil then begin
        CloseLibrary(IconBase);
        IconBase := nil;
    end;
end;

begin
    IconBase := nil;
    IconBase := OpenLibrary(ICONNAME,LIBVERSION);
    if IconBase <> nil then begin
        icon_exit := ExitProc;
        ExitProc := @CloseiconLibrary;
        ICONIsCompiledHow := 1;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open icon.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$endif use_auto_openlib}

{$ifdef dont_use_openlib}
begin
    ICONIsCompiledHow := 3;
   {$Warning No autoopening of icon.library compiled}
   {$Warning Make sure you open icon.library yourself}
{$endif dont_use_openlib}


END. (* UNIT ICON *)




