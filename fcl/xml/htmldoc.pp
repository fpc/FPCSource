{
    $Id$
    This file is part of the Free Component Library
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Implementation of a HTMLdocument class,
    following the W3 recommendation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit htmldoc;

interface

Uses Sysutils,   // Uppercase
     Classes,    // TList and the like
     DOM;        // Naturally...

{ ---------------------------------------------------------------------
    Forward Class definitions
  ---------------------------------------------------------------------}
Type

THTMLCollection = Class;
THTMLDocument = Class;
THTMLElement = Class;
THTMLHtmlElement = Class;
THTMLHeadElement = Class;
THTMLLinkElement = Class;
THTMLTitleElement = Class;
THTMLMetaElement = Class;
THTMLBaseElement = Class;
THTMLIsIndexElement = Class;
THTMLStyleElement = Class;
THTMLBodyElement = Class;
THTMLFormElement = Class;
THTMLTableSectionElement = Class;

{ ---------------------------------------------------------------------
    Miscellaneous objects
  ---------------------------------------------------------------------}

// HTMLCollection
THTMLCollection = Class
  Private
    Flist : TList;
    Function GetLength : longword;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Item(Index : longword) : TDOMNode;
    Function NamedItem(Name : DomString) : TDOMNode;
    Property Length : LongWord Read GetLength;
  end;

{ ---------------------------------------------------------------------
    THTMLDocument class
  ---------------------------------------------------------------------}

THTMLDocument = Class(TDOMDocument)
  Private
    FTitle,
    FReferrer,
    FDomain,
    FCookie,
    FURL : DOMString;
    FBody : THTMLElement;
    FImages,
    FApplets,
    FLinks,
    FForms,
    Fanchors : THTMLCollection;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Open;
    Procedure Close;
    Procedure Write (TheText : DOMString);
    Procedure Writeln (TheText : DOMString);
    Function GetElementById (Id :longword) : TDOMElement;
    Function GetElementByName (Name : DOMString) : TDOMNodeList;

    Property Title : DOMString Read FTitle Write FTitle;
    Property Referrer : DOMString Read FReferrer;
    Property Domain : DOMString Read FDomain;
    Property URL : DOMString Read FURL;
    Property Body : THTMLElement Read FBody;
    Property Images : THTMLCollection Read Fimages;
    Property Applets : THTMLCollection Read FApplets;
    Property Links : THTMLCollection Read FLinks;
    Property Forms : THTMLCollection Read FForms;
    Property Anchors : THTMLCollection Read Fanchors;
    Property Cookie : DOMString Read FCookie;
  end;

{ ---------------------------------------------------------------------
    THTMLElement class
  ---------------------------------------------------------------------}

THTMLElement = Class(TDOMElement)
  Private
    FID,
    FTitle,
    FLang,
    FDir,
    FHTMLClassname : DOMString;
  Public
    Property ID : DOMString Read FID Write FID;
    Property Title : DOMString Read FTitle Write FTitle;
    Property Lang : DOMString Read FLang Write FLang;
    Property Dir : DOMString Read FDir Write FDir;
    Property HTMLClassName : DOMString Read FHTMLClassName Write FHTMLClassName;
  end;

{ ---------------------------------------------------------------------
    THTMLElement descendent classes
  ---------------------------------------------------------------------}

THTMLHtmlElement = Class(THTMLElement)
 Private
   FVersion : DOMString;
 Public
   Property Version : DOMString Read FVersion Write FVersion;
 end;

THTMLHeadElement = Class(THTMLElement)
  Private
    FProfile : DOMString;
  Public
    Property Profile : DOMString Read FProfile Write FProfile;
  end;

THTMLLinkElement = Class(THTMLElement)
  Private
    FDisabled : Boolean;
    FCharset,
    FHREF,
    FHREFLang,
    FMedia,
    FRel,
    FREV,
    FTarget,
    FHTMLType : DOMString;
  Public
    Property Disabled : Boolean Read FDisabled Write FDisabled;
    Property Charset : DOMString Read FCharset Write FCharset;
    Property HREF : DOMString Read FHREF Write FHREF;
    Property HREFLang : DOMString Read FHREFLang Write FHREFLang;
    Property Media : DOMString Read FMEdia Write FMedia;
    Property Rel : DOMString READ FRel Write FRel;
    Property Target : DOMString Read FTarget Write FTarget;
    Property HTMLType : DOMString Read FHTMLType Write FHTMLtype;
  end;

THTMLTitleElement = Class(THTMLElement)
  Private
    FHTMLtext : DOMString;
  Public
    Property HTMLText : DOMString Read FHTMLText Write FHTMLtext;
  end;

THTMLMetaElement = Class(THTMLElement)
  Private
    FContent,
    FhttpEquiv,
    FName,
    FScheme : DOMString;
  Public
    Property Content : DOMString Read FContent Write FContent;
    Property HttpEquiv  : DOMString Read FHTTPEquiv Write FHTTPEquiv;
    Property Name : DOMString Read FName Write FName;
    Property Scheme : DOMString Read FScheme Write FScheme;
  end;

THTMLBaseElement = Class(TDOMElement)
  Private
    FHref,
    FTarget : DOMString;
  Public
    Property HRef : DOMString Read FHref Write FHRef;
    Property Target : DOMstring Read FTarget Write FTarget;
  end;

THTMLIsIndexElement = Class(THTMLElement)
  Private
    FForm : THTMLFormElement;
    FPrompt : DomString;
  Public
    Property Form : THTMLFormElement Read FForm;
    Property Prompt : DOMString Read FPrompt Write FPrompt;
  end;


THTMLStyleElement = Class(THTMLElement)
  Private
    FDisabled : Boolean;
    FMEdia,
    FHTMLtype : DOMString;
  Public
    Property Disabled : Boolean Read FDisabled Write FDisabled;
    Property HTMLtype : DOMString Read FHTMLType Write FHTMLtype;
  end;

THTMLBodyElement = Class(THTMLElement)
  Private
    Falink,
    Fbackground,
    Fbgcolor,
    flink,
    fhtmltext,
    fvlink : DOMString;
  Public
    Property alink : DOMString Read falink write falink;
    Property background  : DOMString Read Fbackground write FBackground;
    Property bgcolor : DOMString  Read fbgcolor write fbgcolor;
    Property link : DOMString  Read Flink Write flink;
    Property htmltext : DOMString read fhtmltext Write fhtmltext;
    Property vlink : DOMString Read fvLink Write fvLink ;
  end;


  THTMLAnchorElement = Class(THTMLElement)
    Private
      FaccessKey : DOMString;
      Fcharset : DOMString;
      Fcoords : DOMString;
      Fhref : DOMString;
      Fhreflang : DOMString;
      Fname : DOMString;
      Frel : DOMString;
      Frev : DOMString;
      Fshape : DOMString;
      FtabIndex : longint;
      Ftarget : DOMString;
      Ftype : DOMString;
    Public
      Procedure blur;
      Procedure focus;
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property charset : DOMString Read Fcharset Write Fcharset;
      Property coords : DOMString Read Fcoords Write Fcoords;
      Property href : DOMString Read Fhref Write Fhref;
      Property hreflang : DOMString Read Fhreflang Write Fhreflang;
      Property name : DOMString Read Fname Write Fname;
      Property rel : DOMString Read Frel Write Frel;
      Property rev : DOMString Read Frev Write Frev;
      Property shape : DOMString Read Fshape Write Fshape;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
      Property target : DOMString Read Ftarget Write Ftarget;
      Property htmltype : DOMString Read Ftype Write Ftype;
  End;

  THTMLAppletElement = Class(THTMLElement)
    Private
      Falign : DOMString;
      Falt : DOMString;
      Farchive : DOMString;
      Fcode : DOMString;
      FcodeBase : DOMString;
      Fheight : DOMString;
      Fhspace : DOMString;
      Fname : DOMString;
      Fobject : DOMString;
      Fvspace : DOMString;
      Fwidth : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
      Property alt : DOMString Read Falt Write Falt;
      Property archive : DOMString Read Farchive Write Farchive;
      Property code : DOMString Read Fcode Write Fcode;
      Property codeBase : DOMString Read FcodeBase Write FcodeBase;
      Property height : DOMString Read Fheight Write Fheight;
      Property hspace : DOMString Read Fhspace Write Fhspace;
      Property name : DOMString Read Fname Write Fname;
      Property htmlobject : DOMString Read Fobject Write Fobject;
      Property vspace : DOMString Read Fvspace Write Fvspace;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLAreaElement = Class(THTMLElement)
    Private
      FaccessKey : DOMString;
      Falt : DOMString;
      Fcoords : DOMString;
      Fhref : DOMString;
      FnoHref : boolean;
      Fshape : DOMString;
      FtabIndex : longint;
      Ftarget : DOMString;
    Public
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property alt : DOMString Read Falt Write Falt;
      Property coords : DOMString Read Fcoords Write Fcoords;
      Property href : DOMString Read Fhref Write Fhref;
      Property noHref : boolean Read FnoHref Write FnoHref;
      Property shape : DOMString Read Fshape Write Fshape;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
      Property target : DOMString Read Ftarget Write Ftarget;
  End;

  THTMLBaseFontElement = Class(THTMLElement)
    Private
      Fcolor : DOMString;
      Fface : DOMString;
      Fsize : DOMString;
    Public
      Property color : DOMString Read Fcolor Write Fcolor;
      Property face : DOMString Read Fface Write Fface;
      Property size : DOMString Read Fsize Write Fsize;
  End;

  THTMLBlockquoteElement = Class(THTMLElement)
    Private
      Fcite : DOMString;
    Public
      Property cite : DOMString Read Fcite Write Fcite;
  End;

  THTMLBRElement = Class(THTMLElement)
    Private
      Fclear : DOMString;
    Public
      Property clear : DOMString Read Fclear Write Fclear;
  End;

  THTMLButtonElement = Class(THTMLElement)
    Private
      Fform : THTMLFormElement;
      FaccessKey : DOMString;
      Fdisabled : boolean;
      Fname : DOMString;
      FtabIndex : longint;
      Ftype : DOMString;
      Fvalue : DOMString;
    Public
      Property form : THTMLFormElement Read Fform;
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property disabled : boolean Read Fdisabled Write Fdisabled;
      Property name : DOMString Read Fname Write Fname;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
      Property htmltype : DOMString Read Ftype;
      Property value : DOMString Read Fvalue Write Fvalue;
  End;

  THTMLDirectoryElement = Class(THTMLElement)
    Private
      Fcompact : boolean;
    Public
      Property compact : boolean Read Fcompact Write Fcompact;
  End;

  THTMLDivElement = Class(THTMLElement)
    Private
      Falign : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
  End;

  THTMLDListElement = Class(THTMLElement)
    Private
      Fcompact : boolean;
    Public
      Property compact : boolean Read Fcompact Write Fcompact;
  End;

  THTMLFieldSetElement = Class(THTMLElement)
    Private
      Fform : THTMLFormElement;
    Public
      Property form : THTMLFormElement Read Fform;
  End;

  THTMLFontElement = Class(THTMLElement)
    Private
      Fcolor : DOMString;
      Fface : DOMString;
      Fsize : DOMString;
    Public
      Property color : DOMString Read Fcolor Write Fcolor;
      Property face : DOMString Read Fface Write Fface;
      Property size : DOMString Read Fsize Write Fsize;
  End;

  THTMLFormElement = Class(THTMLElement)
    Private
      Felements : THTMLCollection;
      Flength : longint;
      Fname : DOMString;
      FacceptCharset : DOMString;
      Faction : DOMString;
      Fenctype : DOMString;
      Fmethod : DOMString;
      Ftarget : DOMString;
    Public
      Constructor Create(AOwner : TDOMDocument);override;
      Destructor Destroy;override;
      Procedure submit;
      Procedure reset;
      Property elements : THTMLCollection Read Felements;
      Property length : longint Read Flength;
      Property name : DOMString Read Fname Write Fname;
      Property acceptCharset : DOMString Read FacceptCharset Write FacceptCharset;
      Property action : DOMString Read Faction Write Faction;
      Property enctype : DOMString Read Fenctype Write Fenctype;
      Property method : DOMString Read Fmethod Write Fmethod;
      Property target : DOMString Read Ftarget Write Ftarget;
  End;

  THTMLFrameElement = Class(THTMLElement)
    Private
      FframeBorder : DOMString;
      FlongDesc : DOMString;
      FmarginHeight : DOMString;
      FmarginWidth : DOMString;
      Fname : DOMString;
      FnoResize : boolean;
      Fscrolling : DOMString;
      Fsrc : DOMString;
    Public
      Property frameBorder : DOMString Read FframeBorder Write FframeBorder;
      Property longDesc : DOMString Read FlongDesc Write FlongDesc;
      Property marginHeight : DOMString Read FmarginHeight Write FmarginHeight;
      Property marginWidth : DOMString Read FmarginWidth Write FmarginWidth;
      Property name : DOMString Read Fname Write Fname;
      Property noResize : boolean Read FnoResize Write FnoResize;
      Property scrolling : DOMString Read Fscrolling Write Fscrolling;
      Property src : DOMString Read Fsrc Write Fsrc;
  End;

  THTMLFrameSetElement = Class(THTMLElement)
    Private
      Fcols : DOMString;
      Frows : DOMString;
    Public
      Property cols : DOMString Read Fcols Write Fcols;
      Property rows : DOMString Read Frows Write Frows;
  End;


  THTMLHeadingElement = Class(THTMLElement)
    Private
      Falign : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
  End;

  THTMLHRElement = Class(THTMLElement)
    Private
      Falign : DOMString;
      FnoShade : boolean;
      Fsize : DOMString;
      Fwidth : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
      Property noShade : boolean Read FnoShade Write FnoShade;
      Property size : DOMString Read Fsize Write Fsize;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;


  THTMLIFrameElement = Class(THTMLElement)
    Private
      Falign : DOMString;
      FframeBorder : DOMString;
      Fheight : DOMString;
      FlongDesc : DOMString;
      FmarginHeight : DOMString;
      FmarginWidth : DOMString;
      Fname : DOMString;
      Fscrolling : DOMString;
      Fsrc : DOMString;
      Fwidth : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
      Property frameBorder : DOMString Read FframeBorder Write FframeBorder;
      Property height : DOMString Read Fheight Write Fheight;
      Property longDesc : DOMString Read FlongDesc Write FlongDesc;
      Property marginHeight : DOMString Read FmarginHeight Write FmarginHeight;
      Property marginWidth : DOMString Read FmarginWidth Write FmarginWidth;
      Property name : DOMString Read Fname Write Fname;
      Property scrolling : DOMString Read Fscrolling Write Fscrolling;
      Property src : DOMString Read Fsrc Write Fsrc;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLImageElement = Class(THTMLElement)
    Private
      FlowSrc : DOMString;
      Fname : DOMString;
      Falign : DOMString;
      Falt : DOMString;
      Fborder : DOMString;
      Fheight : DOMString;
      Fhspace : DOMString;
      FisMap : boolean;
      FlongDesc : DOMString;
      Fsrc : DOMString;
      FuseMap : DOMString;
      Fvspace : DOMString;
      Fwidth : DOMString;
    Public
      Property lowSrc : DOMString Read FlowSrc Write FlowSrc;
      Property name : DOMString Read Fname Write Fname;
      Property align : DOMString Read Falign Write Falign;
      Property alt : DOMString Read Falt Write Falt;
      Property border : DOMString Read Fborder Write Fborder;
      Property height : DOMString Read Fheight Write Fheight;
      Property hspace : DOMString Read Fhspace Write Fhspace;
      Property isMap : boolean Read FisMap Write FisMap;
      Property longDesc : DOMString Read FlongDesc Write FlongDesc;
      Property src : DOMString Read Fsrc Write Fsrc;
      Property useMap : DOMString Read FuseMap Write FuseMap;
      Property vspace : DOMString Read Fvspace Write Fvspace;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLInputElement = Class(THTMLElement)
    Private
      FdefaultValue : DOMString;
      FdefaultChecked : boolean;
      Fform : THTMLFormElement;
      Faccept : DOMString;
      FaccessKey : DOMString;
      Falign : DOMString;
      Falt : DOMString;
      Fchecked : boolean;
      Fdisabled : boolean;
      FmaxLength : longint;
      Fname : DOMString;
      FreadOnly : boolean;
      Fsize : DOMString;
      Fsrc : DOMString;
      FtabIndex : longint;
      Ftype : DOMString;
      FuseMap : DOMString;
      Fvalue : DOMString;
    Public
      Procedure blur;
      Procedure focus;
      Procedure select;
      Procedure click;
      Property defaultValue : DOMString Read FdefaultValue Write FdefaultValue;
      Property defaultChecked : boolean Read FdefaultChecked Write FdefaultChecked;
      Property form : THTMLFormElement Read Fform;
      Property accept : DOMString Read Faccept Write Faccept;
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property align : DOMString Read Falign Write Falign;
      Property alt : DOMString Read Falt Write Falt;
      Property checked : boolean Read Fchecked Write Fchecked;
      Property disabled : boolean Read Fdisabled Write Fdisabled;
      Property maxLength : longint Read FmaxLength Write FmaxLength;
      Property name : DOMString Read Fname Write Fname;
      Property readOnly : boolean Read FreadOnly Write FreadOnly;
      Property size : DOMString Read Fsize Write Fsize;
      Property src : DOMString Read Fsrc Write Fsrc;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
      Property htmltype : DOMString Read Ftype;
      Property useMap : DOMString Read FuseMap Write FuseMap;
      Property value : DOMString Read Fvalue Write Fvalue;
  End;

  THTMLLabelElement = Class(THTMLElement)
    Private
      Fform : THTMLFormElement;
      FaccessKey : DOMString;
      FhtmlFor : DOMString;
    Public
      Property form : THTMLFormElement Read Fform;
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property htmlFor : DOMString Read FhtmlFor Write FhtmlFor;
  End;

  THTMLLegendElement = Class(THTMLElement)
    Private
      Fform : THTMLFormElement;
      FaccessKey : DOMString;
      Falign : DOMString;
    Public
      Property form : THTMLFormElement Read Fform;
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property align : DOMString Read Falign Write Falign;
  End;

  THTMLLIElement = Class(THTMLElement)
    Private
      Ftype : DOMString;
      Fvalue : longint;
    Public
      Property htmltype : DOMString Read Ftype Write Ftype;
      Property value : longint Read Fvalue Write Fvalue;
  End;

  THTMLMapElement = Class(THTMLElement)
    Private
      Fareas : THTMLCollection;
      Fname : DOMString;
    Public
      Property areas : THTMLCollection Read Fareas;
      Property name : DOMString Read Fname Write Fname;
  End;

  THTMLMenuElement = Class(THTMLElement)
    Private
      Fcompact : boolean;
    Public
      Property compact : boolean Read Fcompact Write Fcompact;
  End;


  THTMLModElement = Class(THTMLElement)
    Private
      Fcite : DOMString;
      FdateTime : DOMString;
    Public
      Property cite : DOMString Read Fcite Write Fcite;
      Property dateTime : DOMString Read FdateTime Write FdateTime;
  End;

  THTMLObjectElement = Class(THTMLElement)
    Private
      Fform : THTMLFormElement;
      Fcode : DOMString;
      Falign : DOMString;
      Farchive : DOMString;
      Fborder : DOMString;
      FcodeBase : DOMString;
      FcodeType : DOMString;
      Fdata : DOMString;
      Fdeclare : boolean;
      Fheight : DOMString;
      Fhspace : DOMString;
      Fname : DOMString;
      Fstandby : DOMString;
      FtabIndex : longint;
      Ftype : DOMString;
      FuseMap : DOMString;
      Fvspace : DOMString;
      Fwidth : DOMString;
    Public
      Property form : THTMLFormElement Read Fform;
      Property code : DOMString Read Fcode Write Fcode;
      Property align : DOMString Read Falign Write Falign;
      Property archive : DOMString Read Farchive Write Farchive;
      Property border : DOMString Read Fborder Write Fborder;
      Property codeBase : DOMString Read FcodeBase Write FcodeBase;
      Property codeType : DOMString Read FcodeType Write FcodeType;
      Property data : DOMString Read Fdata Write Fdata;
      Property declare : boolean Read Fdeclare Write Fdeclare;
      Property height : DOMString Read Fheight Write Fheight;
      Property hspace : DOMString Read Fhspace Write Fhspace;
      Property name : DOMString Read Fname Write Fname;
      Property standby : DOMString Read Fstandby Write Fstandby;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
      Property htmltype : DOMString Read Ftype Write Ftype;
      Property useMap : DOMString Read FuseMap Write FuseMap;
      Property vspace : DOMString Read Fvspace Write Fvspace;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLOListElement = Class(THTMLElement)
    Private
      Fcompact : boolean;
      Fstart : longint;
      Ftype : DOMString;
    Public
      Property compact : boolean Read Fcompact Write Fcompact;
      Property start : longint Read Fstart Write Fstart;
      Property htmltype : DOMString Read Ftype Write Ftype;
  End;

  THTMLOptGroupElement = Class(THTMLElement)
    Private
      Fdisabled : boolean;
      Flabel : DOMString;
    Public
      Property disabled : boolean Read Fdisabled Write Fdisabled;
      Property htmllabel : DOMString Read Flabel Write Flabel;
  End;

  THTMLOptionElement = Class(THTMLElement)
    Private
      Fform : THTMLFormElement;
      FdefaultSelected : boolean;
      Ftext : DOMString;
      Findex : longint;
      Fdisabled : boolean;
      Flabel : DOMString;
      Fselected : boolean;
      Fvalue : DOMString;
    Public
      Property form : THTMLFormElement Read Fform;
      Property defaultSelected : boolean Read FdefaultSelected Write FdefaultSelected;
      Property htmltext : DOMString Read Ftext;
      Property index : longint Read Findex Write Findex;
      Property disabled : boolean Read Fdisabled Write Fdisabled;
      Property htmllabel : DOMString Read Flabel Write Flabel;
      Property selected : boolean Read Fselected;
      Property value : DOMString Read Fvalue Write Fvalue;
  End;

  THTMLParagraphElement = Class(THTMLElement)
    Private
      Falign : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
  End;

  THTMLParamElement = Class(THTMLElement)
    Private
      Fname : DOMString;
      Ftype : DOMString;
      Fvalue : DOMString;
      FvalueType : DOMString;
    Public
      Property name : DOMString Read Fname Write Fname;
      Property htmltype : DOMString Read Ftype Write Ftype;
      Property value : DOMString Read Fvalue Write Fvalue;
      Property valueType : DOMString Read FvalueType Write FvalueType;
  End;

  THTMLPreElement = Class(THTMLElement)
    Private
      Fwidth : longint;
    Public
      Property width : longint Read Fwidth Write Fwidth;
  End;

  THTMLQuoteElement = Class(THTMLElement)
    Private
      Fcite : DOMString;
    Public
      Property cite : DOMString Read Fcite Write Fcite;
  End;

  THTMLScriptElement = Class(THTMLElement)
    Private
      Ftext : DOMString;
      FhtmlFor : DOMString;
      Fevent : DOMString;
      Fcharset : DOMString;
      Fdefer : boolean;
      Fsrc : DOMString;
      Ftype : DOMString;
    Public
      Property htmltext : DOMString Read Ftext Write Ftext;
      Property htmlFor : DOMString Read FhtmlFor Write FhtmlFor;
      Property event : DOMString Read Fevent Write Fevent;
      Property charset : DOMString Read Fcharset Write Fcharset;
      Property defer : boolean Read Fdefer Write Fdefer;
      Property src : DOMString Read Fsrc Write Fsrc;
      Property htmltype : DOMString Read Ftype Write Ftype;
  End;

  THTMLSelectElement = Class(THTMLElement)
    Private
      Ftype : DOMString;
      FselectedIndex : longint;
      Fvalue : DOMString;
      Flength : longint;
      Fform : THTMLFormElement;
      Foptions : THTMLCollection;
      Fdisabled : boolean;
      Fmultiple : boolean;
      Fname : DOMString;
      Fsize : longint;
      FtabIndex : longint;
    Public
      Procedure add;
      Procedure remove;
      Procedure blur;
      Procedure focus;
      Property htmltype : DOMString Read Ftype;
      Property selectedIndex : longint Read FselectedIndex Write FselectedIndex;
      Property value : DOMString Read Fvalue Write Fvalue;
      Property length : longint Read Flength;
      Property form : THTMLFormElement Read Fform;
      Property options : THTMLCollection Read Foptions;
      Property disabled : boolean Read Fdisabled Write Fdisabled;
      Property multiple : boolean Read Fmultiple Write Fmultiple;
      Property name : DOMString Read Fname Write Fname;
      Property size : longint Read Fsize Write Fsize;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
  End;

  THTMLTableCaptionElement = Class(THTMLElement)
    Private
      Falign : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
  End;

  THTMLTableCellElement = Class(THTMLElement)
    Private
      FcellIndex : longint;
      Fabbr : DOMString;
      Falign : DOMString;
      Faxis : DOMString;
      FbgColor : DOMString;
      Fch : DOMString;
      FchOff : DOMString;
      FcolSpan : longint;
      Fheaders : DOMString;
      Fheight : DOMString;
      FnoWrap : boolean;
      FrowSpan : longint;
      Fscope : DOMString;
      FvAlign : DOMString;
      Fwidth : DOMString;
    Public
      Property cellIndex : longint Read FcellIndex Write FcellIndex;
      Property abbr : DOMString Read Fabbr Write Fabbr;
      Property align : DOMString Read Falign Write Falign;
      Property axis : DOMString Read Faxis Write Faxis;
      Property bgColor : DOMString Read FbgColor Write FbgColor;
      Property ch : DOMString Read Fch Write Fch;
      Property chOff : DOMString Read FchOff Write FchOff;
      Property colSpan : longint Read FcolSpan Write FcolSpan;
      Property headers : DOMString Read Fheaders Write Fheaders;
      Property height : DOMString Read Fheight Write Fheight;
      Property noWrap : boolean Read FnoWrap Write FnoWrap;
      Property rowSpan : longint Read FrowSpan Write FrowSpan;
      Property scope : DOMString Read Fscope Write Fscope;
      Property vAlign : DOMString Read FvAlign Write FvAlign;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLTableColElement = Class(THTMLElement)
    Private
      Falign : DOMString;
      Fch : DOMString;
      FchOff : DOMString;
      Fspan : longint;
      FvAlign : DOMString;
      Fwidth : DOMString;
    Public
      Property align : DOMString Read Falign Write Falign;
      Property ch : DOMString Read Fch Write Fch;
      Property chOff : DOMString Read FchOff Write FchOff;
      Property span : longint Read Fspan Write Fspan;
      Property vAlign : DOMString Read FvAlign Write FvAlign;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLTableElement = Class(THTMLElement)
    Private
      Fcaption : THTMLTableCaptionElement;
      FtHead : THTMLTableSectionElement;
      FtFoot : THTMLTableSectionElement;
      Frows : THTMLCollection;
      FtBodies : THTMLCollection;
      Falign : DOMString;
      FbgColor : DOMString;
      Fborder : DOMString;
      FcellPadding : DOMString;
      FcellSpacing : DOMString;
      Fframe : DOMString;
      Frules : DOMString;
      Fsummary : DOMString;
      Fwidth : DOMString;
    Public
      Function createTHead : THTMLElement;
      Procedure deleteTHead;
      Function createTFoot : THTMLElement;
      Procedure deleteTFoot;
      Function createCaption : THTMLElement;
      Procedure deleteCaption;
      Function insertRow : THTMLElement;
      Procedure deleteRow;
      Property caption : THTMLTableCaptionElement Read Fcaption Write Fcaption;
      Property tHead : THTMLTableSectionElement Read FtHead Write FtHead;
      Property tFoot : THTMLTableSectionElement Read FtFoot Write FtFoot;
      Property rows : THTMLCollection Read Frows;
      Property tBodies : THTMLCollection Read FtBodies;
      Property align : DOMString Read Falign Write Falign;
      Property bgColor : DOMString Read FbgColor Write FbgColor;
      Property border : DOMString Read Fborder Write Fborder;
      Property cellPadding : DOMString Read FcellPadding Write FcellPadding;
      Property cellSpacing : DOMString Read FcellSpacing Write FcellSpacing;
      Property frame : DOMString Read Fframe Write Fframe;
      Property rules : DOMString Read Frules Write Frules;
      Property summary : DOMString Read Fsummary Write Fsummary;
      Property width : DOMString Read Fwidth Write Fwidth;
  End;

  THTMLTableRowElement = Class(THTMLElement)
    Private
      FrowIndex : longint;
      FsectionRowIndex : longint;
      Fcells : THTMLCollection;
      Falign : DOMString;
      FbgColor : DOMString;
      Fch : DOMString;
      FchOff : DOMString;
      FvAlign : DOMString;
    Public
      Function insertCell : THTMLElement;
      Procedure deleteCell;
      Property rowIndex : longint Read FrowIndex Write FrowIndex;
      Property sectionRowIndex : longint Read FsectionRowIndex Write FsectionRowIndex;
      Property cells : THTMLCollection Read Fcells Write Fcells;
      Property align : DOMString Read Falign Write Falign;
      Property bgColor : DOMString Read FbgColor Write FbgColor;
      Property ch : DOMString Read Fch Write Fch;
      Property chOff : DOMString Read FchOff Write FchOff;
      Property vAlign : DOMString Read FvAlign Write FvAlign;
  End;

  THTMLTableSectionElement = Class(THTMLElement)
    Private
      Falign : DOMString;
      Fch : DOMString;
      FchOff : DOMString;
      FvAlign : DOMString;
      Frows : THTMLCollection;
    Public
      Function insertRow : THTMLElement;
      Procedure deleteRow;
      Property align : DOMString Read Falign Write Falign;
      Property ch : DOMString Read Fch Write Fch;
      Property chOff : DOMString Read FchOff Write FchOff;
      Property vAlign : DOMString Read FvAlign Write FvAlign;
      Property rows : THTMLCollection Read Frows;
  End;

  THTMLTextAreaElement = Class(THTMLElement)
    Private
      FdefaultValue : DOMString;
      Fform : THTMLFormElement;
      FaccessKey : DOMString;
      Fcols : longint;
      Fdisabled : boolean;
      Fname : DOMString;
      FreadOnly : boolean;
      Frows : longint;
      FtabIndex : longint;
      Ftype : DOMString;
      Fvalue : DOMString;
    Public
      Procedure blur;
      Procedure focus;
      Procedure select;
      Property defaultValue : DOMString Read FdefaultValue Write FdefaultValue;
      Property form : THTMLFormElement Read Fform;
      Property accessKey : DOMString Read FaccessKey Write FaccessKey;
      Property cols : longint Read Fcols Write Fcols;
      Property disabled : boolean Read Fdisabled Write Fdisabled;
      Property name : DOMString Read Fname Write Fname;
      Property readOnly : boolean Read FreadOnly Write FreadOnly;
      Property rows : longint Read Frows Write Frows;
      Property tabIndex : longint Read FtabIndex Write FtabIndex;
      Property htmltype : DOMString Read Ftype;
      Property value : DOMString Read Fvalue Write Fvalue;
  End;

  THTMLUListElement = Class(THTMLElement)
    Private
      Fcompact : boolean;
      Ftype : DOMString;
    Public
      Property compact : boolean Read Fcompact Write Fcompact;
      Property htmltype : DOMString Read Ftype Write Ftype;
  End;

implementation

{ ---------------------------------------------------------------------
    THTMLCollection
  ---------------------------------------------------------------------}

Constructor THTMLCollection.Create;

begin
  FList := TList.Create;
end;

Destructor THTMLCollection.Destroy;

begin
  FList.Free;
  Inherited Destroy;
end;

Function THTMLCollection.GetLength : LongWord;

begin
  Result:=FList.Count;
end;

Function THTMLCollection.Item(Index : longword) : TDOMNode;

begin
  If (Index<0) or (Index>Flist.Count-1) then
    Result:=Nil
  else
    Result:=TDOMNode(Flist[Index]);
end;

Function THTMLCollection.NamedItem(Name : DomString) : TDOMNode;

Var I : longword;

begin
  Name:=UpperCase(Name);
  // linear search, since the list is not ordered.
  // W3 says nothing about ordering; maybe we can implement it ?
  For i:=0 to FList.Count-1 do
    If UpperCase(TDomNode(FList[i]).NodeName)=Name then
      begin
      Result:=TDomNode(Flist[I]);
      Exit;
      end;
  Result:=Nil;
end;

{ ---------------------------------------------------------------------
    THTMLDocument class
  ---------------------------------------------------------------------}


Constructor THTMLDocument.Create;

begin
  Inherited Create;
end;


Destructor THTMLDocument.Destroy;

begin
  Inherited Destroy;
end;


Procedure THTMLDocument.Open;

begin
end;


Procedure THTMLDocument.Close;

begin
end;


Procedure THTMLDocument.Write (TheText : DOMString);

begin
end;


Procedure THTMLDocument.Writeln (TheText : DOMString);

begin
end;


Function THTMLDocument.GetElementById (Id :longword) : TDOMElement;

begin
end;


Function THTMLDocument.GetElementByName (Name : DOMString) : TDOMNodeList;

begin
end;


Constructor THTMLFormElement.Create(AOwner : TDOMDocument);

begin
  Inherited Create(AOWner);
  FElements:=THTMLCollection.Create;
end;


Destructor THTMLFormElement.Destroy;

begin
  FElements.Free;
  Inherited Destroy;
end;

Procedure THTMLFormElement.Submit;

begin
end;

Procedure THTMLFormElement.Reset;

begin
end;

// Created From file htmlanchorelement.xml
Procedure THTMLAnchorElement.blur;

Begin
End;


Procedure THTMLAnchorElement.focus;

Begin
End;



Procedure THTMLInputElement.blur;

Begin
End;


Procedure THTMLInputElement.focus;

Begin
End;


Procedure THTMLInputElement.select;

Begin
End;


Procedure THTMLInputElement.click;

Begin
End;


Procedure THTMLSelectElement.add;

Begin
End;


Procedure THTMLSelectElement.remove;

Begin
End;


Procedure THTMLSelectElement.blur;

Begin
End;


Procedure THTMLSelectElement.focus;

Begin
End;


Function THTMLTableElement.createTHead : THTMLElement;

Begin
End;


Procedure THTMLTableElement.deleteTHead;

Begin
End;


Function THTMLTableElement.createTFoot : THTMLElement;

Begin
End;


Procedure THTMLTableElement.deleteTFoot;

Begin
End;


Function THTMLTableElement.createCaption : THTMLElement;

Begin
End;


Procedure THTMLTableElement.deleteCaption;

Begin
End;


Function THTMLTableElement.insertRow : THTMLElement;

Begin
End;


Procedure THTMLTableElement.deleteRow;

Begin
End;


// Created From file htmltablerowelement.xml
Function THTMLTableRowElement.insertCell : THTMLElement;

Begin
End;


Procedure THTMLTableRowElement.deleteCell;

Begin
End;


// Created From file htmltablesectionelement.xml
Function THTMLTableSectionElement.insertRow : THTMLElement;

Begin
End;


Procedure THTMLTableSectionElement.deleteRow;

Begin
End;


// Created From file htmltextareaelement.xml
Procedure THTMLTextAreaElement.blur;

Begin
End;


Procedure THTMLTextAreaElement.focus;

Begin
End;


Procedure THTMLTextAreaElement.select;

Begin
End;


end.  $Log$
end.  Revision 1.3  2002-09-07 15:15:29  peter
end.    * old logs removed and tabs fixed
end.
}
