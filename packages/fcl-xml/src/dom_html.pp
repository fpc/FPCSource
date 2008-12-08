{
    This file is part of the Free Component Library

    Implementation of DOM HTML interfaces
    Copyright (c) 2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Please note that this is a very early version, most properties and methods
  are not implemented yet. }

{$mode objfpc}
{$H+}

unit DOM_HTML;

interface

uses DOM, xmlutils;

type

  THTMLDocument = class;
  THTMLFormElement = class;
  THTMLTableCaptionElement = class;
  THTMLTableSectionElement = class;

  THTMLCollection = class
  public
    property Length: Cardinal;  // !!!: ro
    function Item(Index: Cardinal): TDOMNode;
    function NamedItem(const Index: DOMString): TDOMNode;
  end;

  THTMLOptionsCollection = class
  public
    property Length: Cardinal;  // !!!: ro
    function Item(Index: Cardinal): TDOMNode;
    function NamedItem(const Index: DOMString): TDOMNode;
  end;

  THTMLElement = class(TDOMElement)
  private
    function GetID: DOMString;
    procedure SetID(const Value: DOMString);
    function GetTitle: DOMString;
    procedure SetTitle(const Value: DOMString);
    function GetLang: DOMString;
    procedure SetLang(const Value: DOMString);
    function GetDir: DOMString;
    procedure SetDir(const Value: DOMString);
    function GetClassName: DOMString;
    procedure SetClassName(const Value: DOMString);
  public
    property ID: DOMString read GetID write SetID;
    property Title: DOMString read GetTitle write SetTitle;
    property Lang: DOMString read GetLang write SetLang;
    property Dir: DOMString read GetDir write SetDir;
    property ClassName: DOMString read GetClassName write SetClassName;
  end;

  THTMLHtmlElement = class(THTMLElement)
  private
    function GetVersion: DOMString;
    procedure SetVersion(const Value: DOMString);
  public
    property Version: DOMString read GetVersion write SetVersion;
  end;

  THTMLHeadElement = class(THTMLElement)
  private
    function GetProfile: DOMString;
    procedure SetProfile(const Value: DOMString);
  public
    property Profile: DOMString read GetProfile write SetProfile;
  end;

  THTMLLinkElement = class(THTMLElement)
  public
    property Disabled: Boolean; // !!!: rw
    property Charset: DOMString;        // !!!: rw
    property HRef: DOMString;   // !!!: rw
    property HRefLang: DOMString;       // !!!: rw
    property Media: DOMString;  // !!!: rw
    property Rel: DOMString;    // !!!: rw
    property Rev: DOMString;    // !!!: rw
    property Target: DOMString; // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
  end;

  THTMLTitleElement = class(THTMLElement)
  public
    property Text: DOMString;   // !!!: rw
  end;

  THTMLMetaElement = class(THTMLElement)
  public
    property Content: DOMString;        // !!!: rw
    property HTTPEqiv: DOMString;       // !!!: rw
    property Name: DOMString;   // !!!: rw
    property Scheme: DOMString; // !!!: rw
  end;

  THTMLBaseElement = class(THTMLElement)
  public
    property HRef: DOMString;   // !!!: rw
    property Target: DOMString; // !!!: rw
  end;

  THTMLIsIndexElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
    property Prompt: DOMString; // !!!: rw
  end;

  THTMLStyleElement = class(THTMLElement)
  public
    property Disabled: Boolean; // !!!: rw
    property Media: DOMString;  // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
  end;

  THTMLBodyElement = class(THTMLElement)
  public
    property ALink: DOMString;  // !!!: rw
    property Background: DOMString;     // !!!: rw
    property BgColor: DOMString;        // !!!: rw
    property Link: DOMString;   // !!!: rw
    property Text: DOMString;   // !!!: rw
    property VLink: DOMString;  // !!!: rw
  end;

  THTMLFormElement = class(THTMLElement)
  public
    property Elements: THTMLCollection; // !!!: ro
    property Length: Integer;   // !!!: ro
    property Name: DOMString;   // !!!: rw
    property AcceptCharset: DOMString;  // !!!: rw
    property Action: DOMString; // !!!: rw
    property EncType: DOMString;        // !!!: rw
    property Method: DOMString; // !!!: rw
    property Target: DOMString; // !!!: rw
    procedure Submit; virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  THTMLSelectElement = class(THTMLElement)
  public
    property HTMLType: DOMString;       // !!!: ro
    property SelectedIndex: Integer;    // !!!: rw
    property Value: DOMString;  // !!!: rw
    property Length: Cardinal;  // !!!: rw
    property Form: THTMLFormElement;    // !!!: ro
    property Options: THTMLOptionsCollection;   // !!!: ro
    property Disabled: Boolean; // !!!: rw
    property Multiple: Boolean; // !!!: rw
    property Name: DOMString;   // !!!: rw
    property Size: Integer;     // !!!: rw
    property TabIndex: Integer; // !!!: rw
    procedure Add(Element, Before: THTMLElement);
    procedure Remove(Index: Integer);
    procedure Blur; virtual; abstract;
    procedure Focus; virtual; abstract;
  end;

  THTMLOptGroupElement = class(THTMLElement)
  public
    property Disabled: Boolean; // !!!: rw
    property GroupLabel: DOMString;     // !!!: rw
  end;

  THTMLOptionElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
    property DefaultSelected: Boolean;  // !!!: rw
    property Text: DOMString;   // !!!: ro
    property Index: Integer;    // !!!: ro
    property Disabled: Boolean; // !!!: rw
    property OptionLabel: DOMString;    // !!!: rw
    property Selected: Boolean; // !!!: rw
    property Value: DOMString;  // !!!: rw
  end;

  THTMLInputElement = class(THTMLElement)
  public
    property DefaultValue: DOMString;   // !!!: rw
    property DefaultChecked: Boolean;   // !!!: rw
    property Form: THTMLFormElement;    // !!!: ro
    property Accept: DOMString; // !!!: rw
    property AccessKey: DOMString;      // !!!: rw
    property Align: DOMString;  // !!!: rw
    property Alt: DOMString;    // !!!: rw
    property Checked: Boolean;  // !!!: rw
    property Disabled: Boolean; // !!!: rw
    property MaxLength: Integer;        // !!!: rw
    property Name: DOMString;   // !!!: rw
    property ReadOnly: Boolean; // !!!: rw
    property Size: Cardinal;    // !!!: rw
    property Src: DOMString;    // !!!: rw
    property TabIndex: Integer; // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
    property UseMap: DOMString; // !!!: rw
    property Value: DOMString;  // !!!: rw
    procedure Blur; virtual; abstract;
    procedure Focus; virtual; abstract;
    procedure Select; virtual; abstract;
    procedure Click; virtual; abstract;
  end;

  THTMLTextAreaElement = class(THTMLElement)
  public
    property DefaultValue: DOMString;   // !!!: rw
    property Form: THTMLFormElement;    // !!!: ro
    property AccessKey: DOMString;      // !!!: rw
    property Cols: Integer;     // !!!: rw
    property Disabled: Boolean; // !!!: rw
    property Name: DOMString;   // !!!: rw
    property ReadOnly: Boolean; // !!!: rw
    property Rows: Integer;     // !!!: rw
    property TabIndex: Integer; // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
    property Value: DOMString;  // !!!: rw
    procedure Blur; virtual; abstract;
    procedure Focus; virtual; abstract;
    procedure Select; virtual; abstract;
  end;

  THTMLButtonElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
    property AccessKey: DOMString;      // !!!: rw
    property Disabled: Boolean; // !!!: rw
    property Name: DOMString;   // !!!: rw
    property TabIndex: Integer; // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
    property Value: DOMString;  // !!!: rw
  end;

  THTMLLabelElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
    property AccessKey: DOMString;      // !!!: rw
    property HtmlFor: DOMString;        // !!!: rw
  end;

  THTMLFieldSetElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
  end;

  THTMLLegendElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
    property AccessKey: DOMString;      // !!!: rw
    property Align: DOMString;  // !!!: rw
  end;

  THTMLUListElement = class(THTMLElement)
  public
    property Compact: Boolean;  // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
  end;

  THTMLOListElement = class(THTMLElement)
  public
    property Compact: Boolean;  // !!!: rw
    property Start: Integer;    // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
  end;

  THTMLDListElement = class(THTMLElement)
  public
    property Compact: Boolean;  // !!!: rw
  end;

  THTMLDirectoryElement = class(THTMLElement)
  public
    property Compact: Boolean;  // !!!: rw
  end;

  THTMLMenuElement = class(THTMLElement)
  public
    property Compact: Boolean;  // !!!: rw
  end;

  THTMLLIElement = class(THTMLElement)
  public
    property HTMLType: DOMString;       // !!!: rw
    property Value: Integer;    // !!!: rw
  end;

  THTMLDivElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
  end;

  THTMLParagraphElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
  end;

  THTMLHeadingElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
  end;

  THTMLQuoteElement = class(THTMLElement)
  public
    property Cite: DOMString;   // !!!: rw
  end;

  THTMLPreElement = class(THTMLElement)
  public
    property Width: Integer;    // !!!: rw
  end;

  THTMLBREElement = class(THTMLElement)
  public
    property Clear: DOMString;  // !!!: rw
  end;

  THTMLBaseFontElement = class(THTMLElement)
  public
    property Color: DOMString;  // !!!: rw
    property Face: DOMString;   // !!!: rw
    property Size: Integer;     // !!!: rw
  end;

  THTMLFontElement = class(THTMLElement)
  public
    property Color: DOMString;  // !!!: rw
    property Face: DOMString;   // !!!: rw
    property Size: Integer;     // !!!: rw
  end;

  THTMLHRElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
    property NoShade: Boolean;  // !!!: rw
    property Size: DOMString;   // !!!: rw
    property Width: DOMString;  // !!!: rw
  end;

  THTMLModElement = class(THTMLElement)
  public
    property Cite: DOMString;   // !!!: rw
    property DateTime: DOMString;       // !!!: rw
  end;

  THTMLAnchorElement = class(THTMLElement)
  public
    property AccessKey: DOMString;      // !!!: rw
    property Charset: DOMString;        // !!!: rw
    property Coords: DOMString; // !!!: rw
    property HRef: DOMString;   // !!!: rw
    property HRefLang: DOMString;       // !!!: rw
    property Name: DOMString;   // !!!: rw
    property Rel: DOMString;    // !!!: rw
    property Rev: DOMString;    // !!!: rw
    property Shape: DOMString;  // !!!: rw
    property TabIndex: Integer; // !!!: rw
    property Target: DOMString; // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
    procedure Blur; virtual; abstract;
    procedure Focus; virtual; abstract;
  end;

  THTMLImageElement = class(THTMLElement)
  public
    property Name: DOMString;   // !!!: rw
    property Align: DOMString;  // !!!: rw
    property Alt: DOMString;    // !!!: rw
    property Border: DOMString; // !!!: rw
    property Height: Integer;   // !!!: rw
    property HSpace: Integer;   // !!!: rw
    property IsMap: Boolean;    // !!!: rw
    property LongDesc: DOMString;       // !!!: rw
    property Src: Integer;      // !!!: rw
    property UseMap: DOMString; // !!!: rw
    property VSpace: Integer;   // !!!: rw
    property Width: Integer;    // !!!: rw
  end;

  THTMLObjectElement = class(THTMLElement)
  public
    property Form: THTMLFormElement;    // !!!: ro
    property Code: DOMString;   // !!!: rw
    property Align: DOMString;  // !!!: rw
    property Archive: DOMString;        // !!!: rw
    property Border: DOMString; // !!!: rw
    property CodeBase: DOMString;       // !!!: rw
    property CodeType: DOMString;       // !!!: rw
    property Data: DOMString;   // !!!: rw
    property Declare: Boolean;  // !!!: rw
    property Height: DOMString; // !!!: rw
    property HSpace: Integer;   // !!!: rw
    property Name: DOMString;   // !!!: rw
    property StandBy: DOMString;        // !!!: rw
    property TabIndex: Integer; // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
    property UseMap: DOMString; // !!!: rw
    property VSpace: Integer;   // !!!: rw
    property Width: Integer;    // !!!: rw
    property ContentDocument: TDOMDocument;     // !!!: ro
  end;

  THTMLParamElement = class(THTMLElement)
  public
    property Name: DOMString;   // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
    property Value: DOMString;  // !!!: rw
    property ValueType: DOMString;      // !!!: rw
  end;

  THTMLAppletElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
    property Alt: DOMString;    // !!!: rw
    property Archive: DOMString;        // !!!: rw
    property Code: DOMString;   // !!!: rw
    property CodeBase: DOMString;       // !!!: rw
    property Height: DOMString; // !!!: rw
    property HSpace: Integer;   // !!!: rw
    property Name: DOMString;   // !!!: rw
    property AppletObject: DOMString;   // !!!: rw
    property VSpace: Integer;   // !!!: rw
    property Width: Integer;    // !!!: rw
  end;

  THTMLMapElement = class(THTMLElement)
  public
    property Areas: THTMLCollection;    // !!!: ro
    property Name: DOMString;   // !!!: rw
  end;

  THTMLAreaElement = class(THTMLElement)
  public
    property AccessKey: DOMString;      // !!!: rw
    property Alt: DOMString;    // !!!: rw
    property Coords: DOMString; // !!!: rw
    property HRef: DOMString;   // !!!: rw
    property NoHRef: Boolean;   // !!!: rw
    property Shape: DOMString;  // !!!: rw
    property TabIndex: Integer; // !!!: rw
    property Target: DOMString; // !!!: rw
  end;

  THTMLScriptElement = class(THTMLElement)
  public
    property Text: DOMString;   // !!!: rw
    property HtmlFor: DOMString;        // !!!: rw
    property Event: DOMString;  // !!!: rw
    property Charset: DOMString;        // !!!: rw
    property Defer: Boolean;    // !!!: rw
    property Src: DOMString;    // !!!: rw
    property HTMLType: DOMString;       // !!!: rw
  end;

  THTMLTableElement = class(THTMLElement)
  public
    property Caption: THTMLTableCaptionElement; // !!!: rw
    property THead: THTMLTableSectionElement;   // !!!: rw
    property TFoot: THTMLTableSectionElement;   // !!!: rw
    property Rows: THTMLCollection;     // !!!: ro
    property TBodies: THTMLCollection;  // !!!: ro
    property Align: DOMString;  // !!!: rw
    property BgColor: DOMString;        // !!!: rw
    property Border: DOMString; // !!!: rw
    property CellPadding: DOMString;    // !!!: rw
    property CellSpacing: DOMString;    // !!!: rw
    property Frame: DOMString;  // !!!: rw
    property Rules: DOMString;  // !!!: rw
    property Summary: DOMString;        // !!!: rw
    property Width: DOMString;  // !!!: rw
    function CreateTHead: THTMLElement;
    procedure DeleteTHead;
    function CreateTFoot: THTMLElement;
    procedure DeleteTFoot;
    function CreateCaption: THTMLElement;
    procedure DeleteCaption;
    function InsertRow(Index: Integer): THTMLElement;
    procedure DeleteRow(Index: Integer);
  end;

  THTMLTableCaptionElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
  end;

  THTMLTableColElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
    property Ch: DOMString;     // !!!: rw
    property ChOff: DOMString;  // !!!: rw
    property Span: Integer;     // !!!: rw
    property VAlign: DOMString; // !!!: rw
    property Width: DOMString;  // !!!: rw
  end;

  THTMLTableSectionElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
    property Ch: DOMString;     // !!!: rw
    property ChOff: DOMString;  // !!!: rw
    property VAlign: DOMString; // !!!: rw
    property Rows: THTMLCollection;     // !!!: ro
    function InsertRow(Index: Integer): THTMLElement;
    procedure DeleteRow(Index: Integer);
  end;

  THTMLTableRowElement = class(THTMLElement)
  public
    property RowIndex: Integer; // !!!: ro
    property SectionRowIndex: Integer;  // !!!: ro
    property Cells: THTMLCollection;    // !!!: ro
    property Align: DOMString;  // !!!: rw
    property BgColor: DOMString;        // !!!: rw
    property Ch: DOMString;     // !!!: rw
    property ChOff: DOMString;  // !!!: rw
    property VAlign: DOMString; // !!!: rw
    function InsertCell(Index: Integer): THTMLElement;
    procedure DeleteCell(Index: Integer);
  end;

  THTMLTableCellElement = class(THTMLElement)
  public
    property CellIndex: Integer;        // !!!: ro
    property Abbr: DOMString;   // !!!: rw
    property Align: DOMString;  // !!!: rw
    property Axis: DOMString;   // !!!: rw
    property BgColor: DOMString;        // !!!: rw
    property Ch: DOMString;     // !!!: rw
    property ChOff: DOMString;  // !!!: rw
    property ColSpan: Integer;  // !!!: rw
    property Headers: DOMString;        // !!!: rw
    property Height: DOMString; // !!!: rw
    property NoWrap: Boolean;   // !!!: rw
    property RowSpan: Integer;  // !!!: rw
    property Scope: DOMString;  // !!!: rw
    property VAlign: DOMString; // !!!: rw
    property Width: DOMString;  // !!!: rw
  end;

  THTMLFrameSetElement = class(THTMLElement)
  public
    property Cols: DOMString;   // !!!: rw
    property Rows: DOMString;   // !!!: rw
  end;

  THTMLFrameElement = class(THTMLElement)
  public
    property FrameBorder: DOMString;    // !!!: rw
    property LongDesc: DOMString;       // !!!: rw
    property MarginHeight: DOMString;   // !!!: rw
    property MarginWidth: DOMString;    // !!!: rw
    property Name: DOMString;   // !!!: rw
    property NoResize: Boolean; // !!!: rw
    property Scrolling: DOMString;      // !!!: rw
    property Src: DOMString;    // !!!: rw
    property ContentDocument: TDOMDocument;     // !!!: ro
  end;

  THTMLIFrameElement = class(THTMLElement)
  public
    property Align: DOMString;  // !!!: rw
    property FrameBorder: DOMString;    // !!!: rw
    property Height: DOMString; // !!!: rw
    property LongDesc: DOMString;       // !!!: rw
    property MarginHeight: DOMString;   // !!!: rw
    property MarginWidth: DOMString;    // !!!: rw
    property Name: DOMString;   // !!!: rw
    property Scrolling: DOMString;      // !!!: rw
    property Src: DOMString;    // !!!: rw
    property Width: DOMString;  // !!!: rw
    property ContentDocument: TDOMDocument;     // !!!: ro
  end;

  THTMLDocument = class(TXMLDocument)
  private
    function GetTitle: DOMString;
    procedure SetTitle(const Value: DOMString);
  public
    property Title: DOMString read GetTitle write SetTitle;
    property Referrer: DOMString;       // !!!: ro
    property Domain: DOMString; // !!!: ro
    property URL: DOMString;    // !!!: ro
    property Body: THTMLElement;        // !!!: rw
    property Images: THTMLCollection;   // !!!: ro
    property Applets: THTMLCollection;  // !!!: ro
    property Links: THTMLCollection;    // !!!: ro
    property Forms: THTMLCollection;    // !!!: ro
    property Anchors: THTMLCollection;  // !!!: ro
    property Cookie: DOMString;         // !!!: rw

    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Write(const AText: DOMString);
    procedure WriteLn(const AText: DOMString);
    function GetElementsByName(const ElementName: DOMString): TDOMNodeList;
    function HashForName(const aName: DOMString): PHashItem;

    // Helper functions (not in DOM standard):
    function CreateElement(const tagName: DOMString): THTMLElement;
    function CreateSubElement: THTMLElement;
    function CreateSupElement: THTMLElement;
    function CreateSpanElement: THTMLElement;
    function CreateBDOElement: THTMLElement;
    function CreateTTElement: THTMLElement;
    function CreateIElement: THTMLElement;
    function CreateBElement: THTMLElement;
    function CreateUElement: THTMLElement;
    function CreateSElement: THTMLElement;
    function CreateStrikeElement: THTMLElement;
    function CreateBigElement: THTMLElement;
    function CreateSmallElement: THTMLElement;
    function CreateEmElement: THTMLElement;
    function CreateStrongElement: THTMLElement;
    function CreateDfnElement: THTMLElement;
    function CreateCodeElement: THTMLElement;
    function CreateSampElement: THTMLElement;
    function CreateKbdElement: THTMLElement;
    function CreateVarElement: THTMLElement;
    function CreateCiteElement: THTMLElement;
    function CreateAcronymElement: THTMLElement;
    function CreateAbbrElement: THTMLElement;
    function CreateDDElement: THTMLElement;
    function CreateDTElement: THTMLElement;
    function CreateNoFramesElement: THTMLElement;
    function CreateNoScriptElement: THTMLElement;
    function CreateAddressElement: THTMLElement;
    function CreateCenterElement: THTMLElement;
    function CreateHtmlElement: THTMLHtmlElement;
    function CreateHeadElement: THTMLHeadElement;
    function CreateLinkElement: THTMLLinkElement;
{    function CreateTitleElement: THTMLTitleElement;
    function CreateMetaElement: THTMLMetaElement;
    function CreateBaseElement: THTMLBaseElement;
    function CreateIsIndexElement: THTMLIsIndexElement;
    function CreateStyleElement: THTMLStyleElement;}
    function CreateBodyElement: THTMLBodyElement;
{    function CreateFormElement: THTMLFormElement;
    function CreateSelectElement: THTMLSelectElement;
    function CreateOptGroupElement: THTMLOptGroupElement;
    function CreateOptionElement: THTMLOptionElement;
    function CreateInputElement: THTMLInputElement;
    function CreateTextAreaElement: THTMLTextAreaElement;
    function CreateButtonElement: THTMLButtonElement;
    function CreateLabelElement: THTMLLabelElement;
    function CreateFieldSetElement: THTMLFieldSetElement;
    function CreateLegendElement: THTMLLegendElement;}
    function CreateUListElement: THTMLUListElement;
    function CreateOListElement: THTMLOListElement;
    function CreateDListElement: THTMLDListElement;
{    function CreateDirectoryElement: THTMLDirectoryElement;
    function CreateMenuElement: THTMLMenuElement;}
    function CreateLIElement: THTMLLIElement;
{    function CreateDivElement: THTMLDivElement;}
    function CreateParagraphElement: THTMLParagraphElement;
{    function CreateHeadingElement: THTMLHeadingElement;
    function CreateQuoteElement: THTMLQuoteElement;
    function CreatePreElement: THTMLPreElement;
    function CreateBRElement: THTMLBreElement;
    function CreateBaseFontElement: THTMLBaseFontElement;
    function CreateFontElement: THTMFontLElement;
    function CreateHRElement: THTMLHREElement;
    function CreateModElement: THTMLModElement;
    function CreateAnchorElement: THTMLAnchorElement;
    function CreateImageElement: THTMLImageElement;
    function CreateObjectElement: THTMLObjectElement;
    function CreateParamElement: THTMLParamElement;
    function CreateAppletElement: THTMLAppletElement;
    function CreateMapElement: THTMLMapElement;
    function CreateAreaElement: THTMLAreaElement;
    function CreateScriptElement: THTMLScriptElement;
    function CreateTableElement: THTMLTableElement;
    function CreateTableCaptionElement: THTMLTableCaptionElement;
    function CreateTableColElement: THTMLTableColElement;
    function CreateTableSectionElement: THTMLTableSectionElement;
    function CreateTableRowElement: THTMLTableRowElement;
    function CreateTableCellElement: THTMLTableCellElement;
    function CreateFrameSetElement: THTMLFrameSetElement;
    function CreateFrameElement: THTMLFrameElement;
    function CreateIFrameElement: THTMLIFrameElement;}
  end;


implementation


function THTMLCollection.Item(Index: Cardinal): TDOMNode;
begin
  Result := nil;
end;

function THTMLCollection.NamedItem(const Index: DOMString): TDOMNode;
begin
  Result := nil;
end;


function THTMLOptionsCollection.Item(Index: Cardinal): TDOMNode;
begin
  Result := nil;
end;

function THTMLOptionsCollection.NamedItem(const Index: DOMString): TDOMNode;
begin
  Result := nil;
end;

function THTMLElement.GetID: DOMString; begin Result := GetAttribute('id') end;
procedure THTMLElement.SetID(const Value: DOMString); begin SetAttribute('id', Value) end;
function THTMLElement.GetTitle: DOMString; begin Result := GetAttribute('title') end;
procedure THTMLElement.SetTitle(const Value: DOMString); begin SetAttribute('title', Value) end;
function THTMLElement.GetLang: DOMString; begin Result := GetAttribute('lang') end;
procedure THTMLElement.SetLang(const Value: DOMString); begin SetAttribute('lang', Value) end;
function THTMLElement.GetDir: DOMString; begin Result := GetAttribute('dir') end;
procedure THTMLElement.SetDir(const Value: DOMString); begin SetAttribute('dir', Value) end;
function THTMLElement.GetClassName: DOMString; begin  Result := GetAttribute('class') end;
procedure THTMLElement.SetClassName(const Value: DOMString); begin SetAttribute('class', Value) end;


function THTMLHtmlElement.GetVersion: DOMString; begin  Result := GetAttribute('version') end;
procedure THTMLHtmlElement.SetVersion(const Value: DOMString); begin SetAttribute('version', Value) end;


function THTMLHeadElement.GetProfile: DOMString; begin  Result := GetAttribute('profile') end;
procedure THTMLHeadElement.SetProfile(const Value: DOMString); begin SetAttribute('profile', Value) end;


procedure THTMLSelectElement.Add(Element, Before: THTMLElement);
begin
end;

procedure THTMLSelectElement.Remove(Index: Integer);
begin
end;


function THTMLTableElement.CreateTHead: THTMLElement;
begin
  Result := nil;
end;

procedure THTMLTableElement.DeleteTHead;
begin
end;

function THTMLTableElement.CreateTFoot: THTMLElement;
begin
  Result := nil;
end;

procedure THTMLTableElement.DeleteTFoot;
begin
end;

function THTMLTableElement.CreateCaption: THTMLElement;
begin
  Result := nil;
end;

procedure THTMLTableElement.DeleteCaption;
begin
end;

function THTMLTableElement.InsertRow(Index: Integer): THTMLElement;
begin
  Result := nil;
end;

procedure THTMLTableElement.DeleteRow(Index: Integer);
begin
end;


function THTMLTableSectionElement.InsertRow(Index: Integer): THTMLElement;
begin
  Result := nil;
end;

procedure THTMLTableSectionElement.DeleteRow(Index: Integer);
begin
end;


function THTMLTableRowElement.InsertCell(Index: Integer): THTMLElement;
begin
  Result := nil;
end;

procedure THTMLTableRowElement.DeleteCell(Index: Integer);
begin
end;


function THTMLDocument.GetTitle: DOMString;
var
  Node: TDOMNode;
begin
  Result := '';
  if not Assigned(DocumentElement) then
    exit;
  Node := DocumentElement.FirstChild;
  while Assigned(Node) and (Node.NodeName <> 'head') do
    Node := Node.NextSibling;
  if not Assigned(Node) then
    exit;
  Node := Node.FirstChild;
  while Assigned(Node) and (Node.NodeName <> 'title') do
    Node := Node.NextSibling;
  if not Assigned(Node) then
    exit;
  Node := Node.FirstChild;
  if Assigned(Node) and (Node.NodeType = TEXT_NODE) then
    Result := Node.NodeValue;
end;

procedure THTMLDocument.SetTitle(const Value: DOMString);
var
  Node: TDOMNode;
  TitleEl: TDOMElement;
begin
  if not Assigned(DocumentElement) then
    AppendChild(CreateHtmlElement);
  Node := DocumentElement.FirstChild;
  while Assigned(Node) and (Node.NodeName <> 'head') do
    Node := Node.NextSibling;
  if not Assigned(Node) then
  begin
    Node := CreateHeadElement;
    DocumentElement.InsertBefore(Node, DocumentElement.FirstChild);
  end;
  TitleEl := TDOMElement(Node.FirstChild);
  while Assigned(TitleEl) and (TitleEl.NodeName <> 'title') do
    TitleEl := TDOMElement(TitleEl.NextSibling);
  if not Assigned(TitleEl) then
  begin
    TitleEl := CreateElement('title');
    Node.AppendChild(TitleEl);
  end;
  while Assigned(TitleEl.FirstChild) do
    TitleEl.RemoveChild(TitleEl.FirstChild);
  TitleEl.AppendChild(CreateTextNode(Value));
end;

procedure THTMLDocument.Write(const AText: DOMString);
begin
end;

procedure THTMLDocument.WriteLn(const AText: DOMString);
begin
end;

function THTMLDocument.GetElementsByName(const ElementName: DOMString): TDOMNodeList;
begin
  Result := nil;
end;

function THTMLDocument.CreateElement(const tagName: DOMString): THTMLElement;
begin
  Result := THTMLElement.Create(Self);
  Result.FNSI.QName := FNames.FindOrAdd(DOMPChar(tagName), Length(tagName));
end;

function THTMLDocument.HashForName(const aName: DOMString): PHashItem;
begin
  Result := FNames.FindOrAdd(DOMPChar(aName), Length(aName));
end;

function THTMLDocument.CreateSubElement: THTMLElement; begin Result := CreateElement('sub') end;
function THTMLDocument.CreateSupElement: THTMLElement; begin Result := CreateElement('sup') end;
function THTMLDocument.CreateSpanElement: THTMLElement; begin Result := CreateElement('span') end;
function THTMLDocument.CreateBDOElement: THTMLElement; begin Result := CreateElement('bdo') end;
function THTMLDocument.CreateTTElement: THTMLElement; begin Result := CreateElement('tt') end;
function THTMLDocument.CreateIElement: THTMLElement; begin Result := CreateElement('i') end;
function THTMLDocument.CreateBElement: THTMLElement; begin Result := CreateElement('b') end;
function THTMLDocument.CreateUElement: THTMLElement; begin Result := CreateElement('u') end;
function THTMLDocument.CreateSElement: THTMLElement; begin Result := CreateElement('s') end;
function THTMLDocument.CreateStrikeElement: THTMLElement; begin Result := CreateElement('strike') end;
function THTMLDocument.CreateBigElement: THTMLElement; begin Result := CreateElement('big') end;
function THTMLDocument.CreateSmallElement: THTMLElement; begin Result := CreateElement('small') end;
function THTMLDocument.CreateEmElement: THTMLElement; begin Result := CreateElement('em') end;
function THTMLDocument.CreateStrongElement: THTMLElement; begin Result := CreateElement('strong') end;
function THTMLDocument.CreateDfnElement: THTMLElement; begin Result := CreateElement('dfn') end;
function THTMLDocument.CreateCodeElement: THTMLElement; begin Result := CreateElement('code') end;
function THTMLDocument.CreateSampElement: THTMLElement; begin Result := CreateElement('samp') end;
function THTMLDocument.CreateKbdElement: THTMLElement; begin Result := CreateElement('kbd') end;
function THTMLDocument.CreateVarElement: THTMLElement; begin Result := CreateElement('var') end;
function THTMLDocument.CreateCiteElement: THTMLElement; begin Result := CreateElement('cite') end;
function THTMLDocument.CreateAcronymElement: THTMLElement; begin Result := CreateElement('acronym') end;
function THTMLDocument.CreateAbbrElement: THTMLElement; begin Result := CreateElement('abbr') end;
function THTMLDocument.CreateDDElement: THTMLElement; begin Result := CreateElement('dd') end;
function THTMLDocument.CreateDTElement: THTMLElement; begin Result := CreateElement('dt') end;
function THTMLDocument.CreateNoFramesElement: THTMLElement; begin Result := CreateElement('noframes') end;
function THTMLDocument.CreateNoScriptElement: THTMLElement; begin Result := CreateElement('noscript') end;
function THTMLDocument.CreateAddressElement: THTMLElement; begin Result := CreateElement('address') end;
function THTMLDocument.CreateCenterElement: THTMLElement; begin Result := CreateElement('center') end;

function THTMLDocument.CreateHtmlElement: THTMLHtmlElement;
begin
  Result := THTMLHtmlElement.Create(Self);
  Result.FNSI.QName := HashForName('html');
end;

function THTMLDocument.CreateHeadElement: THTMLHeadElement;
begin
  Result := THTMLHeadElement.Create(Self);
  Result.FNSI.QName := HashForName('head');
end;

function THTMLDocument.CreateLinkElement: THTMLLinkElement;
begin
  Result := THTMLLinkElement.Create(Self);
  Result.FNSI.QName := HashForName('a');
end;

function THTMLDocument.CreateBodyElement: THTMLBodyElement;
begin
  Result := THTMLBodyElement.Create(Self);
  Result.FNSI.QName := HashForName('body');
end;

function THTMLDocument.CreateUListElement: THTMLUListElement;
begin
  Result := THTMLUListElement.Create(Self);
  Result.FNSI.QName := HashForName('ul');
end;

function THTMLDocument.CreateOListElement: THTMLOListElement;
begin
  Result := THTMLOListElement.Create(Self);
  Result.FNSI.QName := HashForName('ol');
end;

function THTMLDocument.CreateDListElement: THTMLDListElement;
begin
  Result := THTMLDListElement.Create(Self);
  Result.FNSI.QName := HashForName('dl');
end;

function THTMLDocument.CreateLIElement: THTMLLIElement;
begin
  Result := THTMLLIElement.Create(Self);
  Result.FNSI.QName := HashForName('li');
end;
//...
function THTMLDocument.CreateParagraphElement: THTMLParagraphElement;
begin
  Result := THTMLParagraphElement.Create(Self);
  Result.FNSI.QName := HashForName('p');
end;

end.
