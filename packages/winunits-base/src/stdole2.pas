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
 IEnumVARIANT = interface;
 IFont = interface;
 Font = dispinterface;
 IPicture = interface;
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

// IEnumVARIANT :

 IEnumVARIANT = interface(IUnknown)
   ['{00020404-0000-0000-C000-000000000046}']
    // Next :  
   procedure Next(celt:LongWord;var rgvar:OleVariant;out pceltFetched:LongWord);stdcall;
    // Skip :  
   procedure Skip(celt:LongWord);stdcall;
    // Reset_ :  
   procedure Reset_;stdcall;
    // Clone :  
   procedure Clone(out ppenum:IEnumVARIANT);stdcall;
  end;

// IFont : Font Object

 IFont = interface(IUnknown)
   ['{BEF6E002-A874-101A-8BBA-00AA00300CAB}']
   function Get_Name : WideString; stdcall;
   procedure Set_Name(const pname:WideString); stdcall;
   function Get_Size : Currency; stdcall;
   procedure Set_Size(const psize:Currency); stdcall;
   function Get_Bold : WordBool; stdcall;
   procedure Set_Bold(const pbold:WordBool); stdcall;
   function Get_Italic : WordBool; stdcall;
   procedure Set_Italic(const pitalic:WordBool); stdcall;
   function Get_Underline : WordBool; stdcall;
   procedure Set_Underline(const punderline:WordBool); stdcall;
   function Get_Strikethrough : WordBool; stdcall;
   procedure Set_Strikethrough(const pstrikethrough:WordBool); stdcall;
   function Get_Weight : Smallint; stdcall;
   procedure Set_Weight(const pweight:Smallint); stdcall;
   function Get_Charset : Smallint; stdcall;
   procedure Set_Charset(const pcharset:Smallint); stdcall;
   function Get_hFont : OLE_HANDLE; stdcall;
    // Clone :  
   procedure Clone(out ppfont:IFont);stdcall;
    // IsEqual :  
   procedure IsEqual(pfontOther:IFont);stdcall;
    // SetRatio :  
   procedure SetRatio(cyLogical:Integer;cyHimetric:Integer);stdcall;
    // AddRefHfont :  
   procedure AddRefHfont(hFont:OLE_HANDLE);stdcall;
    // ReleaseHfont :  
   procedure ReleaseHfont(hFont:OLE_HANDLE);stdcall;
    // Name :  
   property Name:WideString read Get_Name write Set_Name;
    // Size :  
   property Size:Currency read Get_Size write Set_Size;
    // Bold :  
   property Bold:WordBool read Get_Bold write Set_Bold;
    // Italic :  
   property Italic:WordBool read Get_Italic write Set_Italic;
    // Underline :  
   property Underline:WordBool read Get_Underline write Set_Underline;
    // Strikethrough :  
   property Strikethrough:WordBool read Get_Strikethrough write Set_Strikethrough;
    // Weight :  
   property Weight:Smallint read Get_Weight write Set_Weight;
    // Charset :  
   property Charset:Smallint read Get_Charset write Set_Charset;
    // hFont :  
   property hFont:OLE_HANDLE read Get_hFont ;
  end;

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

// IPicture : Picture Object

 IPicture = interface(IUnknown)
   ['{7BF80980-BF32-101A-8BBB-00AA00300CAB}']
   function Get_Handle : OLE_HANDLE; stdcall;
   function Get_hPal : OLE_HANDLE; stdcall;
   function Get_Type_ : Smallint; stdcall;
   function Get_Width : OLE_XSIZE_HIMETRIC; stdcall;
   function Get_Height : OLE_YSIZE_HIMETRIC; stdcall;
    // Render :  
   procedure Render(hdc:SYSINT;x:Integer;y:Integer;cx:Integer;cy:Integer;xSrc:OLE_XPOS_HIMETRIC;ySrc:OLE_YPOS_HIMETRIC;cxSrc:OLE_XSIZE_HIMETRIC;cySrc:OLE_YSIZE_HIMETRIC;var prcWBounds:pointer);stdcall;
   procedure Set_hPal(const phpal:OLE_HANDLE); stdcall;
   function Get_CurDC : SYSINT; stdcall;
    // SelectPicture :  
   procedure SelectPicture(hdcIn:SYSINT;out phdcOut:SYSINT;out phbmpOut:OLE_HANDLE);stdcall;
   function Get_KeepOriginalFormat : WordBool; stdcall;
   procedure Set_KeepOriginalFormat(const pfkeep:WordBool); stdcall;
    // PictureChanged :  
   procedure PictureChanged;stdcall;
    // SaveAsFile :  
   procedure SaveAsFile(var pstm:pointer;fSaveMemCopy:WordBool;out pcbSize:Integer);stdcall;
   function Get_Attributes : Integer; stdcall;
    // SetHdc :  
   procedure SetHdc(hdc:OLE_HANDLE);stdcall;
    // Handle :  
   property Handle:OLE_HANDLE read Get_Handle ;
    // hPal :  
   property hPal:OLE_HANDLE read Get_hPal write Set_hPal;
    // Type :  
   property Type_:Smallint read Get_Type_ ;
    // Width :  
   property Width:OLE_XSIZE_HIMETRIC read Get_Width ;
    // Height :  
   property Height:OLE_YSIZE_HIMETRIC read Get_Height ;
    // CurDC :  
   property CurDC:SYSINT read Get_CurDC ;
    // KeepOriginalFormat :  
   property KeepOriginalFormat:WordBool read Get_KeepOriginalFormat write Set_KeepOriginalFormat;
    // Attributes :  
   property Attributes:Integer read Get_Attributes ;
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
