{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Michael Van Canneyt, member of 
    the Free Pascal development team

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

THTMLFormElement = Class(THTMLElement)
  Private
    FElements : THTMLCollection;
  Public
    Constructor Create(AOwner : TDOMDocument);override;
    Destructor Destroy;
    Procedure Submit;
    Procedure Reset;
  end;
  
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


end.