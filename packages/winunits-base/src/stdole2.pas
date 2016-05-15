Unit stdole2;

//  Imported on 24/12/2011 13:43:11 from C:\WINDOWS\system32\stdole2.tlb
//  Modified by Ludo Brands to remove redeclarations
//  Warning: renamed method 'Reset' in IEnumVARIANT to 'Reset_'
//  Warning: renamed property 'Type' in IPicture to 'Type_'
//  Warning: 'pointer' not automatable in Picturedisp.Render
//  Warning: renamed property 'Type' in Picture to 'Type_'

{$mode delphi}{$H+}

interface
uses Windows, ActiveX, Classes, OleServer, Variants;
Const
  stdoleMajorVersion = 2;
  stdoleMinorVersion = 0;

  LIBID_stdole : TGUID = '{00020430-0000-0000-C000-000000000046}';

  IID_IUnknown : TGUID = '{00000000-0000-0000-C000-000000000046}';
  IID_IDispatch : TGUID = '{00020400-0000-0000-C000-000000000046}';
  IID_IEnumVARIANT : TGUID = '{00020404-0000-0000-C000-000000000046}';
  IID_IFont : TGUID = '{BEF6E002-A874-101A-8BBA-00AA00300CAB}';
  IID_Font : TGUID = '{BEF6E003-A874-101A-8BBA-00AA00300CAB}';
  CLASS_StdFont : TGUID = '{0BE35203-8F91-11CE-9DE3-00AA004BB851}';
  IID_IPicture : TGUID = '{7BF80980-BF32-101A-8BBB-00AA00300CAB}';
  IID_Picture : TGUID = '{7BF80981-BF32-101A-8BBB-00AA00300CAB}';
  CLASS_StdPicture : TGUID = '{0BE35204-8F91-11CE-9DE3-00AA004BB851}';
  IID_FontEvents : TGUID = '{4EF6100A-AF88-11D0-9846-00C04FC29993}';

//Enums
Type
  OLE_TRISTATE =TOleEnum;
Const
  Unchecked = $0000000000000000;
  Checked = $0000000000000001;
  Gray = $0000000000000002;
Type
  LoadPictureConstants =TOleEnum;
Const
  Default = $0000000000000000;
  Monochrome = $0000000000000001;
  VgaColor = $0000000000000002;
  Color = $0000000000000004;

//Forward declarations
Type
 // for activex aliases see mantis 25907
 IEnumVARIANT = ActiveX.IEnumVariant;
 IFont = ActiveX.IFont;
 Font = dispinterface;
 IPicture =  ActiveX.IPicture;
 Picture = dispinterface;
 FontEvents = dispinterface;

//records, unions, aliases
 EXCEPINFO = packed record
     wCode : Word;
     wReserved : Word;
     bstrSource : WideString;
     bstrDescription : WideString;
     bstrHelpFile : WideString;
     dwHelpContext : LongWord;
     pvReserved : Ppointer;
     pfnDeferredFillIn : Ppointer;
     scode : SCODE;
 end;
     OLE_COLOR = LongWord;
     OLE_XPOS_PIXELS = Integer;
     OLE_YPOS_PIXELS = Integer;
     OLE_XSIZE_PIXELS = Integer;
     OLE_YSIZE_PIXELS = Integer;
     OLE_XPOS_HIMETRIC = Integer;
     OLE_YPOS_HIMETRIC = Integer;
     OLE_XSIZE_HIMETRIC = Integer;
     OLE_YSIZE_HIMETRIC = Integer;
     OLE_XPOS_CONTAINER = Single;
     OLE_YPOS_CONTAINER = Single;
     OLE_XSIZE_CONTAINER = Single;
     OLE_YSIZE_CONTAINER = Single;
     OLE_HANDLE = SYSINT;
     OLE_OPTEXCLUSIVE = WordBool;
     OLE_CANCELBOOL = WordBool;
     OLE_ENABLEDEFAULTBOOL = WordBool;
     FONTNAME = WideString;
     FONTSIZE = Currency;
     FONTBOLD = WordBool;
     FONTITALIC = WordBool;
     FONTUNDERSCORE = WordBool;
     FONTSTRIKETHROUGH = WordBool;
     IFontDisp = Font;
     IPictureDisp = Picture;
     IFontEventsDisp = FontEvents;

//interface declarations

// Font : 

 Font = dispinterface
   ['{BEF6E003-A874-101A-8BBA-00AA00300CAB}']
    // Name :  
   property Name:WideString  dispid 0;
    // Size :  
   property Size:Currency  dispid 2;
    // Bold :  
   property Bold:WordBool  dispid 3;
    // Italic :  
   property Italic:WordBool  dispid 4;
    // Underline :  
   property Underline:WordBool  dispid 5;
    // Strikethrough :  
   property Strikethrough:WordBool  dispid 6;
    // Weight :  
   property Weight:Smallint  dispid 7;
    // Charset :  
   property Charset:Smallint  dispid 8;
  end;

// Picture : 

 Picture = dispinterface
   ['{7BF80981-BF32-101A-8BBB-00AA00300CAB}']
    // Render :  
   procedure Render(hdc:SYSINT;x:Integer;y:Integer;cx:Integer;cy:Integer;xSrc:OLE_XPOS_HIMETRIC;ySrc:OLE_YPOS_HIMETRIC;cxSrc:OLE_XSIZE_HIMETRIC;cySrc:OLE_YSIZE_HIMETRIC;prcWBounds:{!! pointer !!} OleVariant);dispid 6;
    // Handle :  
   property Handle:OLE_HANDLE  dispid 0;
    // hPal :  
   property hPal:OLE_HANDLE  dispid 2;
    // Type :  
   property Type_:Smallint  dispid 3;
    // Width :  
   property Width:OLE_XSIZE_HIMETRIC  dispid 4;
    // Height :  
   property Height:OLE_YSIZE_HIMETRIC  dispid 5;
  end;

// FontEvents : Event interface for the Font object

 FontEvents = dispinterface
   ['{4EF6100A-AF88-11D0-9846-00C04FC29993}']
    // FontChanged :  
   procedure FontChanged(PropertyName:WideString);dispid 9;
  end;

//CoClasses
  CoStdFont =class
    class function Create: Font;
    class function CreateRemote(const MachineName: string): Font;
  end;
  CoStdPicture =class
    class function Create: Picture;
    class function CreateRemote(const MachineName: string): Picture;
  end;

implementation

uses comobj;

class function CoStdFont.Create: Font;
begin
  Result := CreateComObject(CLASS_StdFont) as Font;
end;

class function CoStdFont.CreateRemote(const MachineName: string): Font;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_StdFont) as Font;
end;

class function CoStdPicture.Create: Picture;
begin
  Result := CreateComObject(CLASS_StdPicture) as Picture;
end;

class function CoStdPicture.CreateRemote(const MachineName: string): Picture;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_StdPicture) as Picture;
end;


end.
