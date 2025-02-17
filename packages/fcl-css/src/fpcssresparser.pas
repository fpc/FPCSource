{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2023 by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS utility class

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSResParser;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$Interfaces CORBA}
{$ModeSwitch AdvancedRecords}

{$IF FPC_FULLVERSION>30300}
  {$WARN 6060 off} // Case statement does not handle all possible cases
{$ENDIF}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Math, System.Contnrs, System.StrUtils,
  Fcl.AVLTree, FpCss.Tree, FpCss.Scanner, FpCss.Parser;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Math, Contnrs, AVL_Tree, fpCSSTree, fpCSSScanner,
  fpCSSParser;
{$ENDIF FPC_DOTTEDUNITS}

const
  CSSIDNone = 0;
  // built-in attribute IDs
  CSSAttributeID_ID = 1; // id of attribute key 'id'
  CSSAttributeID_Class = 2; // id of attribute key 'class'
  CSSAttributeID_All = 3; // id of attribute key 'all'
  CSSAttributeID_LastResolver = CSSAttributeID_All;

  // built-in type IDs
  CSSTypeID_Universal = 1; // id of type '*'
  CSSTypeID_LastResolver = CSSTypeID_Universal;

  // built-in pseudo class IDs
  CSSPseudoID_Root = 1; // :root
  CSSPseudoID_Empty = CSSPseudoID_Root+1; // :empty
  CSSPseudoID_FirstChild = CSSPseudoID_Empty+1; // :first-child
  CSSPseudoID_LastChild = CSSPseudoID_FirstChild+1; // :last-child
  CSSPseudoID_OnlyChild = CSSPseudoID_LastChild+1; // :only-child
  CSSPseudoID_FirstOfType = CSSPseudoID_OnlyChild+1; // :first-of-type
  CSSPseudoID_LastOfType = CSSPseudoID_FirstOfType+1; // :last-of-type
  CSSPseudoID_OnlyOfType = CSSPseudoID_LastOfType+1; // :only-of-type
  CSSPseudoID_LastResolver = CSSPseudoID_OnlyOfType;

  CSSPseudoClassNames: array[0..CSSPseudoID_LastResolver] of TCSSString = (
    '?',
    ':root',
    ':empty',
    ':first-child',
    ':last-child',
    ':only-child',
    ':first-of-type',
    ':last-of-type',
    ':only-of-type'
    );

  // built-in pseudo function IDs
  CSSCallID_Not = 1; // :not()
  CSSCallID_Is = CSSCallID_Not+1; // :is()
  CSSCallID_Where = CSSCallID_Is+1; // :where()
  CSSCallID_Has = CSSCallID_Where+1; // :has()
  CSSCallID_NthChild = CSSCallID_Has+1; // :nth-child(n)
  CSSCallID_NthLastChild = CSSCallID_NthChild+1; // :nth-last-child(n)
  CSSCallID_NthOfType = CSSCallID_NthLastChild+1; // :nth-of-type(n)
  CSSCallID_NthLastOfType = CSSCallID_NthOfType+1; // :nth-last-of-type(n)
  CSSCallID_LastResolver = CSSCallID_NthLastOfType;

  CSSSelectorCallNames: array[0..CSSCallID_LastResolver] of TCSSString = (
    '?',
    ':not()',
    ':is()',
    ':where()',
    ':has()',
    ':nth-child(n)',
    ':nth-last-child(n)',
    ':nth-of-type(n)',
    ':nth-last-of-type(n)'
    );

  // keywords
  CSSKeywordNone = 1;
  CSSKeywordInitial = CSSKeywordNone+1;
  CSSKeywordInherit = CSSKeywordInitial+1;
  CSSKeywordUnset = CSSKeywordInherit+1;
  CSSKeywordRevert = CSSKeywordUnset+1;
  CSSKeywordRevertLayer = CSSKeywordRevert+1;
  CSSKeywordAuto = CSSKeywordRevertLayer+1;
  CSSKeyword_LastResolver = CSSKeywordAuto;

  // attribute functions
  CSSAttrFuncVar = 1;

  CSSMinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 54 bits (52 plus signed bit plus implicit highest bit)
  CSSMaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991

type
  TCSSMsgID = int64; // used for message numbers, e.g. error codes
  TCSSNumericalID = integer; // used for IDs of each type and attribute
  TCSSNumericalIDArray = array of TCSSNumericalID;

  TCSSNumericalIDKind = (
    nikAttribute,
    nikPseudoClass, // e.g. "hover" of ":hover"
    nikPseudoFunction, // e.g. "is" of ":is()"
    nikType,
    nikKeyword,
    nikAttrFunction // e.g. "calc" of "calc()"
    );
  TCSSNumericalIDs = set of TCSSNumericalIDKind;

const
  nikAllDescriptors = [nikAttribute,nikPseudoClass,nikType]; // all items having a descriptor

  CSSNumericalIDKindNames: array[TCSSNumericalIDKind] of TCSSString = (
    'Type',
    'PseudoClass',
    'PseudoFunction',
    'Attribute',
    'Keyword',
    'AttributeFunction'
    );

type
  TCSSAlphaColor = DWord;

  TCSSNamedColor = record
    Name: TCSSString;
    Color: TCSSAlphaColor;
  end;
const
  CSSNamedColors: array[0..148] of TCSSNamedColor = (
    (Name: 'aliceblue'; Color: TCSSAlphaColor($fff0f8ff)),
    (Name: 'antiquewhite'; Color: TCSSAlphaColor($fffaebd7)),
    (Name: 'aqua'; Color: TCSSAlphaColor($ff00ffff)),
    (Name: 'aquamarine'; Color: TCSSAlphaColor($ff7fffd4)),
    (Name: 'azure'; Color: TCSSAlphaColor($fff0ffff)),
    (Name: 'beige'; Color: TCSSAlphaColor($fff5f5dc)),
    (Name: 'bisque'; Color: TCSSAlphaColor($ffffe4c4)),
    (Name: 'black'; Color: TCSSAlphaColor($ff000000)),
    (Name: 'blanchedalmond'; Color: TCSSAlphaColor($ffffebcd)),
    (Name: 'blue'; Color: TCSSAlphaColor($ff0000ff)),
    (Name: 'blueviolet'; Color: TCSSAlphaColor($ff8a2be2)),
    (Name: 'brown'; Color: TCSSAlphaColor($ffa52a2a)),
    (Name: 'burlywood'; Color: TCSSAlphaColor($ffdeb887)),
    (Name: 'cadetblue'; Color: TCSSAlphaColor($ff5f9ea0)),
    (Name: 'chartreuse'; Color: TCSSAlphaColor($ff7fff00)),
    (Name: 'chocolate'; Color: TCSSAlphaColor($ffd2691e)),
    (Name: 'coral'; Color: TCSSAlphaColor($ffff7f50)),
    (Name: 'cornflowerblue'; Color: TCSSAlphaColor($ff6495ed)),
    (Name: 'cornsilk'; Color: TCSSAlphaColor($fffff8dc)),
    (Name: 'crimson'; Color: TCSSAlphaColor($ffdc143c)),
    (Name: 'cyan'; Color: TCSSAlphaColor($ff00ffff)),
    (Name: 'darkblue'; Color: TCSSAlphaColor($ff00008b)),
    (Name: 'darkcyan'; Color: TCSSAlphaColor($ff008b8b)),
    (Name: 'darkgoldenrod'; Color: TCSSAlphaColor($ffb8860b)),
    (Name: 'darkgray'; Color: TCSSAlphaColor($ffa9a9a9)),
    (Name: 'darkgreen'; Color: TCSSAlphaColor($ff006400)),
    (Name: 'darkgrey'; Color: TCSSAlphaColor($ffa9a9a9)),
    (Name: 'darkkhaki'; Color: TCSSAlphaColor($ffbdb76b)),
    (Name: 'darkmagenta'; Color: TCSSAlphaColor($ff8b008b)),
    (Name: 'darkolivegreen'; Color: TCSSAlphaColor($ff556b2f)),
    (Name: 'darkorange'; Color: TCSSAlphaColor($ffff8c00)),
    (Name: 'darkorchid'; Color: TCSSAlphaColor($ff9932cc)),
    (Name: 'darkred'; Color: TCSSAlphaColor($ff8b0000)),
    (Name: 'darksalmon'; Color: TCSSAlphaColor($ffe9967a)),
    (Name: 'darkseagreen'; Color: TCSSAlphaColor($ff8fbc8f)),
    (Name: 'darkslateblue'; Color: TCSSAlphaColor($ff483d8b)),
    (Name: 'darkslategray'; Color: TCSSAlphaColor($ff2f4f4f)),
    (Name: 'darkslategrey'; Color: TCSSAlphaColor($ff2f4f4f)),
    (Name: 'darkturquoise'; Color: TCSSAlphaColor($ff00ced1)),
    (Name: 'darkviolet'; Color: TCSSAlphaColor($ff9400d3)),
    (Name: 'deeppink'; Color: TCSSAlphaColor($ffff1493)),
    (Name: 'deepskyblue'; Color: TCSSAlphaColor($ff00bfff)),
    (Name: 'dimgray'; Color: TCSSAlphaColor($ff696969)),
    (Name: 'dimgrey'; Color: TCSSAlphaColor($ff696969)),
    (Name: 'dodgerblue'; Color: TCSSAlphaColor($ff1e90ff)),
    (Name: 'firebrick'; Color: TCSSAlphaColor($ffb22222)),
    (Name: 'floralwhite'; Color: TCSSAlphaColor($fffffaf0)),
    (Name: 'forestgreen'; Color: TCSSAlphaColor($ff228b22)),
    (Name: 'fuchsia'; Color: TCSSAlphaColor($ffff00ff)),
    (Name: 'gainsboro'; Color: TCSSAlphaColor($ffdcdcdc)),
    (Name: 'ghostwhite'; Color: TCSSAlphaColor($fff8f8ff)),
    (Name: 'gold'; Color: TCSSAlphaColor($ffffd700)),
    (Name: 'goldenrod'; Color: TCSSAlphaColor($ffdaa520)),
    (Name: 'gray'; Color: TCSSAlphaColor($ff808080)),
    (Name: 'green'; Color: TCSSAlphaColor($ff008000)),
    (Name: 'greenyellow'; Color: TCSSAlphaColor($ffadff2f)),
    (Name: 'grey'; Color: TCSSAlphaColor($ff808080)),
    (Name: 'honeydew'; Color: TCSSAlphaColor($fff0fff0)),
    (Name: 'hotpink'; Color: TCSSAlphaColor($ffff69b4)),
    (Name: 'indianred'; Color: TCSSAlphaColor($ffcd5c5c)),
    (Name: 'indigo'; Color: TCSSAlphaColor($ff4b0082)),
    (Name: 'ivory'; Color: TCSSAlphaColor($fffffff0)),
    (Name: 'khaki'; Color: TCSSAlphaColor($fff0e68c)),
    (Name: 'lavender'; Color: TCSSAlphaColor($ffe6e6fa)),
    (Name: 'lavenderblush'; Color: TCSSAlphaColor($fffff0f5)),
    (Name: 'lawngreen'; Color: TCSSAlphaColor($ff7cfc00)),
    (Name: 'lemonchiffon'; Color: TCSSAlphaColor($fffffacd)),
    (Name: 'lightblue'; Color: TCSSAlphaColor($ffadd8e6)),
    (Name: 'lightcoral'; Color: TCSSAlphaColor($fff08080)),
    (Name: 'lightcyan'; Color: TCSSAlphaColor($ffe0ffff)),
    (Name: 'lightgoldenrodyellow'; Color: TCSSAlphaColor($fffafad2)),
    (Name: 'lightgray'; Color: TCSSAlphaColor($ffd3d3d3)),
    (Name: 'lightgreen'; Color: TCSSAlphaColor($ff90ee90)),
    (Name: 'lightgrey'; Color: TCSSAlphaColor($ffd3d3d3)),
    (Name: 'lightpink'; Color: TCSSAlphaColor($ffffb6c1)),
    (Name: 'lightsalmon'; Color: TCSSAlphaColor($ffffa07a)),
    (Name: 'lightseagreen'; Color: TCSSAlphaColor($ff20b2aa)),
    (Name: 'lightskyblue'; Color: TCSSAlphaColor($ff87cefa)),
    (Name: 'lightslategray'; Color: TCSSAlphaColor($ff778899)),
    (Name: 'lightslategrey'; Color: TCSSAlphaColor($ff778899)),
    (Name: 'lightsteelblue'; Color: TCSSAlphaColor($ffb0c4de)),
    (Name: 'lightyellow'; Color: TCSSAlphaColor($ffffffe0)),
    (Name: 'lime'; Color: TCSSAlphaColor($ff00ff00)),
    (Name: 'limegreen'; Color: TCSSAlphaColor($ff32cd32)),
    (Name: 'linen'; Color: TCSSAlphaColor($fffaf0e6)),
    (Name: 'magenta'; Color: TCSSAlphaColor($ffff00ff)),
    (Name: 'maroon'; Color: TCSSAlphaColor($ff800000)),
    (Name: 'mediumaquamarine'; Color: TCSSAlphaColor($ff66cdaa)),
    (Name: 'mediumblue'; Color: TCSSAlphaColor($ff0000cd)),
    (Name: 'mediumorchid'; Color: TCSSAlphaColor($ffba55d3)),
    (Name: 'mediumpurple'; Color: TCSSAlphaColor($ff9370db)),
    (Name: 'mediumseagreen'; Color: TCSSAlphaColor($ff3cb371)),
    (Name: 'mediumslateblue'; Color: TCSSAlphaColor($ff7b68ee)),
    (Name: 'mediumspringgreen'; Color: TCSSAlphaColor($ff00fa9a)),
    (Name: 'mediumturquoise'; Color: TCSSAlphaColor($ff48d1cc)),
    (Name: 'mediumvioletred'; Color: TCSSAlphaColor($ffc71585)),
    (Name: 'midnightblue'; Color: TCSSAlphaColor($ff191970)),
    (Name: 'mintcream'; Color: TCSSAlphaColor($fff5fffa)),
    (Name: 'mistyrose'; Color: TCSSAlphaColor($ffffe4e1)),
    (Name: 'moccasin'; Color: TCSSAlphaColor($ffffe4b5)),
    (Name: 'navajowhite'; Color: TCSSAlphaColor($ffffdead)),
    (Name: 'navy'; Color: TCSSAlphaColor($ff000080)),
    (Name: 'oldlace'; Color: TCSSAlphaColor($fffdf5e6)),
    (Name: 'olive'; Color: TCSSAlphaColor($ff808000)),
    (Name: 'olivedrab'; Color: TCSSAlphaColor($ff6b8e23)),
    (Name: 'orange'; Color: TCSSAlphaColor($ffffa500)),
    (Name: 'orangered'; Color: TCSSAlphaColor($ffff4500)),
    (Name: 'orchid'; Color: TCSSAlphaColor($ffda70d6)),
    (Name: 'palegoldenrod'; Color: TCSSAlphaColor($ffeee8aa)),
    (Name: 'palegreen'; Color: TCSSAlphaColor($ff98fb98)),
    (Name: 'paleturquoise'; Color: TCSSAlphaColor($ffafeeee)),
    (Name: 'palevioletred'; Color: TCSSAlphaColor($ffdb7093)),
    (Name: 'papayawhip'; Color: TCSSAlphaColor($ffffefd5)),
    (Name: 'peachpuff'; Color: TCSSAlphaColor($ffffdab9)),
    (Name: 'peru'; Color: TCSSAlphaColor($ffcd853f)),
    (Name: 'pink'; Color: TCSSAlphaColor($ffffc0cb)),
    (Name: 'plum'; Color: TCSSAlphaColor($ffdda0dd)),
    (Name: 'powderblue'; Color: TCSSAlphaColor($ffb0e0e6)),
    (Name: 'purple'; Color: TCSSAlphaColor($ff800080)),
    (Name: 'rebeccapurple'; Color: TCSSAlphaColor($ff663399)),
    (Name: 'red'; Color: TCSSAlphaColor($ffff0000)),
    (Name: 'rosybrown'; Color: TCSSAlphaColor($ffbc8f8f)),
    (Name: 'royalblue'; Color: TCSSAlphaColor($ff4169e1)),
    (Name: 'saddlebrown'; Color: TCSSAlphaColor($ff8b4513)),
    (Name: 'salmon'; Color: TCSSAlphaColor($fffa8072)),
    (Name: 'sandybrown'; Color: TCSSAlphaColor($fff4a460)),
    (Name: 'seagreen'; Color: TCSSAlphaColor($ff2e8b57)),
    (Name: 'seashell'; Color: TCSSAlphaColor($fffff5ee)),
    (Name: 'sienna'; Color: TCSSAlphaColor($ffa0522d)),
    (Name: 'silver'; Color: TCSSAlphaColor($ffc0c0c0)),
    (Name: 'skyblue'; Color: TCSSAlphaColor($ff87ceeb)),
    (Name: 'slateblue'; Color: TCSSAlphaColor($ff6a5acd)),
    (Name: 'slategray'; Color: TCSSAlphaColor($ff708090)),
    (Name: 'slategrey'; Color: TCSSAlphaColor($ff708090)),
    (Name: 'snow'; Color: TCSSAlphaColor($fffffafa)),
    (Name: 'springgreen'; Color: TCSSAlphaColor($ff00ff7f)),
    (Name: 'steelblue'; Color: TCSSAlphaColor($ff4682b4)),
    (Name: 'tan'; Color: TCSSAlphaColor($ffd2b48c)),
    (Name: 'teal'; Color: TCSSAlphaColor($ff008080)),
    (Name: 'thistle'; Color: TCSSAlphaColor($ffd8bfd8)),
    (Name: 'tomato'; Color: TCSSAlphaColor($ffff6347)),
    (Name: 'transparent'; Color: TCSSAlphaColor($ff0)),
    (Name: 'turquoise'; Color: TCSSAlphaColor($ff40e0d0)),
    (Name: 'violet'; Color: TCSSAlphaColor($ffee82ee)),
    (Name: 'wheat'; Color: TCSSAlphaColor($fff5deb3)),
    (Name: 'white'; Color: TCSSAlphaColor($ffffffff)),
    (Name: 'whitesmoke'; Color: TCSSAlphaColor($fff5f5f5)),
    (Name: 'yellow'; Color: TCSSAlphaColor($ffffff00)),
    (Name: 'yellowgreen'; Color: TCSSAlphaColor($ff9acd32))
  );


type

  { TCSSRegistryNamedItem }

  TCSSRegistryNamedItem = class
  public
    Name: TCSSString; // case sensitive
    Index: TCSSNumericalID;
  end;

  { TCSSPseudoClassDesc }

  TCSSPseudoClassDesc = class(TCSSRegistryNamedItem)
  public
  end;
  TCSSPseudoClassDescClass = class of TCSSPseudoClassDesc;
  TCSSPseudoClassDescArray = array of TCSSPseudoClassDesc;

  { TCSSTypeDesc }

  TCSSTypeDesc = class(TCSSRegistryNamedItem)
  public
  end;
  TCSSTypeDescClass = class of TCSSTypeDesc;
  TCSSTypeDescArray = array of TCSSTypeDesc;

  { TCSSAttributeKeyData }

  TCSSAttributeKeyData = class(TCSSElementOwnedData)
  public
    Invalid: boolean;
    Complete: boolean;
    Value: TCSSString;
  end;
  TCSSAttributeKeyDataClass = class of TCSSAttributeKeyData;

  TCSSBaseResolver = class;
  TCSSResolverParser = class;

  { TCSSAttributeDesc - general properties of a CSS attribute }

  TCSSAttributeDesc = class(TCSSRegistryNamedItem)
  public
    type
      TCheckEvent = function(Resolver: TCSSBaseResolver): boolean of object;
      TSplitShorthandEvent = procedure(Resolver: TCSSBaseResolver;
           var AttrIDs: TCSSNumericalIDArray; var Values: TCSSStringArray) of object;
  public
    Inherits: boolean; // true = the default value is the parent's value
    All: boolean; // true = can be changed by the 'all' attribute
    InitialValue: TCSSString;
    CompProps: array of TCSSAttributeDesc; // if this attribute is a shorthand,
      // these are the component properties (longhands + sub-shorthands like border-width)
      // used by the cascade algorithm to delete all overwritten properties
    OnCheck: TCheckEvent; // called by the parser after reading a declaration and there is no var()
      // return false if invalid, so the resolver skips this declaration
    OnSplitShorthand: TSplitShorthandEvent; // called by resolver after resolving var(), if any value is empty, the initialvalue is used
  end;
  TCSSAttributeDescClass = class of TCSSAttributeDesc;
  TCSSAttributeDescArray = array of TCSSAttributeDesc;

  { TCSSRegistry }

  TCSSRegistry = class
  private
    FAttrFunctionCount: TCSSNumericalID;
    FAttributeCount: TCSSNumericalID;
    FHashLists: array[TCSSNumericalIDKind] of TFPHashList; // name to TCSSRegistryNamedItem
    FKeywordCount: TCSSNumericalID;
    FPseudoClassCount: TCSSNumericalID;
    FPseudoFunctionCount: TCSSNumericalID;
    FStamp, FModifiedStamp: TCSSNumericalID;
    FTypeCount: TCSSNumericalID;
    function GetModified: boolean;
    procedure SetModified(const AValue: boolean);
  public
    constructor Create;
    procedure Init; virtual; // add basic items
    destructor Destroy; override;
    function FindNamedItem(Kind: TCSSNumericalIDKind; const aName: TCSSString): TCSSRegistryNamedItem; overload;
    function IndexOfNamedItem(Kind: TCSSNumericalIDKind; const aName: TCSSString): TCSSNumericalID; overload;
    procedure ConsistencyCheck; virtual;
    procedure ChangeStamp;
    property Stamp: TCSSNumericalID read FStamp; // always >0
    property Modified: boolean read GetModified write SetModified;
  public
    // attributes
    Attributes: TCSSAttributeDescArray; // Note: Attributes[0] is nil to spot bugs easily
    Attribute_ClassOf: TCSSAttributeDescClass;
    NotAllAttributes: TCSSAttributeDescArray;
    function AddAttribute(Attr: TCSSAttributeDesc): TCSSAttributeDesc; overload;
    function AddAttribute(const aName: TCSSString; const aInitialValue: TCSSString = '';
      aInherits: boolean = false; aAll: boolean = true; aClass: TCSSAttributeDescClass = nil): TCSSAttributeDesc; overload;
    function FindAttribute(const aName: TCSSString): TCSSAttributeDesc; overload;
    function IndexOfAttributeName(const aName: TCSSString): TCSSNumericalID; overload;
    procedure AddSplitLonghand(var AttrIDs: TCSSNumericalIDArray; var Values: TCSSStringArray; AttrID: TCSSNumericalID; const Value: TCSSString); overload;
    procedure AddSplitLonghandSides(var AttrIDs: TCSSNumericalIDArray; var Values: TCSSStringArray;
      TopID, RightID, BottomID, LeftID: TCSSNumericalID; const Found: TCSSStringArray); overload;
    procedure AddSplitLonghandCorners(var AttrIDs: TCSSNumericalIDArray; var Values: TCSSStringArray;
      TopLeftID, TopRightID, BottomLeftID, BottomRightID: TCSSNumericalID; const Found: TCSSStringArray); overload;
    property AttributeCount: TCSSNumericalID read FAttributeCount;
  public
    // pseudo classes
    PseudoClasses: TCSSPseudoClassDescArray; // Note: PseudoClasses[0] is nil to spot bugs easily
    PseudoClass_ClassOf: TCSSPseudoClassDescClass;
    function AddPseudoClass(aPseudo: TCSSPseudoClassDesc): TCSSPseudoClassDesc; overload;
    function AddPseudoClass(const aName: TCSSString; aClass: TCSSPseudoClassDescClass = nil): TCSSPseudoClassDesc; overload;
    function FindPseudoClass(const aName: TCSSString): TCSSPseudoClassDesc; overload;
    function IndexOfPseudoClassName(const aName: TCSSString): TCSSNumericalID; overload;
    property PseudoClassCount: TCSSNumericalID read FPseudoClassCount;
  public
    // pseudo functions lowercase (they are parsed case insensitive)
    PseudoFunctions: TCSSStringArray; // Note: PseudoFunctions[0] is nil to spot bugs easily
    function AddPseudoFunction(const aName: TCSSString): TCSSNumericalID; overload;
    function IndexOfPseudoFunction(const aName: TCSSString): TCSSNumericalID; overload;
    property PseudoFunctionCount: TCSSNumericalID read FPseudoFunctionCount;
  public
    // types
    Types: TCSSTypeDescArray; // Note: Types[0] is nil to spot bugs easily
    Type_ClassOf: TCSSTypeDescClass;
    function AddType(aType: TCSSTypeDesc): TCSSTypeDesc; overload;
    function AddType(const aName: TCSSString; aClass: TCSSTypeDescClass = nil): TCSSTypeDesc; overload;
    function FindType(const aName: TCSSString): TCSSTypeDesc; overload;
    function IndexOfTypeName(const aName: TCSSString): TCSSNumericalID; overload;
    property TypeCount: TCSSNumericalID read FTypeCount;
  public
    // keywords
    Keywords: TCSSStringArray; // Note: Keywords[0] is nil to spot bugs easily
    kwFirstColor, kwLastColor, kwTransparent: TCSSNumericalID;
    function AddKeyword(const aName: TCSSString): TCSSNumericalID; overload;
    procedure AddKeywords(const Names: TCSSStringArray; out First, Last: TCSSNumericalID); overload;
    function IndexOfKeyword(const aName: TCSSString): TCSSNumericalID; overload;
    procedure AddColorKeywords; virtual;
    function GetNamedColor(const aName: TCSSString): TCSSAlphaColor; virtual; overload;
    function GetKeywordColor(KeywordID: TCSSNumericalID): TCSSAlphaColor; virtual; overload;
    property KeywordCount: TCSSNumericalID read FKeywordCount;
  public
    // attribute functions
    AttrFunctions: TCSSStringArray; // Note: AttrFunctions[0] is nil to spot bugs easily
    const afVar = CSSAttrFuncVar;
    function AddAttrFunction(const aName: TCSSString): TCSSNumericalID; overload;
    function IndexOfAttrFunction(const aName: TCSSString): TCSSNumericalID; overload;
    property AttrFunctionCount: TCSSNumericalID read FAttrFunctionCount;
  end;

  TCSSValueParserLogEvent = procedure(MsgType: TEventType; const ID: TCSSMsgID;
                               const Msg: TCSSString; PosEl: TCSSElement) of object;

  { TCSSResolvedIdentifierElement }

  TCSSResolvedIdentifierElement = class(TCSSIdentifierElement)
  public
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
  end;

  { TCSSResolvedPseudoClassElement }

  TCSSResolvedPseudoClassElement = class(TCSSPseudoClassElement)
  public
    NumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
  end;

  { TCSSNthChildParams }

  TCSSNthChildParams = class
  public
    Modulo: integer;
    Start: integer;
    HasOf: boolean; // for nth-of-type() HasOf=true and OfSelector=nil
    OfSelector: TCSSElement;
  end;
  TCSSNthChildParamsClass = class of TCSSNthChildParams;

  { TCSSResolvedCallElement }

  TCSSResolvedCallElement = class(TCSSCallElement)
  public
    NameNumericalID: TCSSNumericalID;
    Kind: TCSSNumericalIDKind;
    Params: TObject; // e.g. TCSSNthChildParams
    destructor Destroy; override;
  end;

  { TCSSValueData }

  TCSSValueData = class(TCSSElementOwnedData)
  public
    NormValue: TCSSString; // normalized value, stripped of comments and e.g. '0.1' instead of '000.100' or '.1'
  end;

  TCSSResValueKind = (
    rvkNone,
    rvkInvalid,
    rvkSymbol,
    rvkFloat,
    rvkCalcFloat,
    rvkKeyword,
    rvkKeywordUnknown,
    rvkFunction,
    rvkFunctionUnknown,
    rvkString,
    rvkHexColor
    );

  { TCSSResCompValue }

  TCSSResCompValue = record
    Kind: TCSSResValueKind;
    StartP, EndP: PCSSChar;
    function AsString: TCSSString;
    function FloatAsString: TCSSString;
    case longint of
    1: (Float: Double; FloatUnit: TCSSUnit);
    2: (KeywordID: TCSSNumericalID);
    3: (FunctionID: TCSSNumericalID; BracketOpen: PCSSChar);
    4: (Symbol: TCSSToken);
  end;

  { TCSSCheckAttrParams_Dimension }

  TCSSCheckAttrParams_Dimension = record
  public
    AllowedUnits: TCSSUnits;
    AllowNegative, AllowFrac: boolean;
    AllowedKeywordIDs: TCSSNumericalIDArray;
    function Fits(const ResValue: TCSSResCompValue): boolean; overload;
  end;

  { TCSSBaseResolver }

  TCSSBaseResolver = class(TComponent)
  private
    FCSSRegistry: TCSSRegistry;
  protected
    procedure SetCSSRegistry(const AValue: TCSSRegistry); virtual;
  public
    CurAttrData: TCSSAttributeKeyData;
    CurDesc: TCSSAttributeDesc;
    CurValue: TCSSString;
    CurComp: TCSSResCompValue;
    function InitParseAttr(Desc: TCSSAttributeDesc; AttrData: TCSSAttributeKeyData; const Value: TCSSString): boolean; virtual; // true if parsing can start
    procedure InitParseAttr(const Value: TCSSString); virtual;
    // check whole attribute, skipping invalid values, emit warnings:
    function CheckAttribute_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    function CheckAttribute_CommaList_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    function CheckAttribute_Dimension(const Params: TCSSCheckAttrParams_Dimension): boolean; virtual;
    function CheckAttribute_Color(const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    // parse whole attribute, skipping invalid values:
    function ReadNext: boolean;
    function ReadAttribute_Keyword(out Invalid: boolean; const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    function ReadAttribute_Dimension(out Invalid: boolean; const Params: TCSSCheckAttrParams_Dimension): boolean; virtual;
    function ReadAttribute_Color(out Invalid: boolean; const AllowedKeywordIDs: TCSSNumericalIDArray): boolean; virtual;
    function IsBaseKeyword(KeywordID: TCSSNumericalID): boolean;
    function IsKeywordIn(aKeywordID: TCSSNumericalID; const KeywordIDs: TCSSNumericalIDArray): boolean; overload;
    function IsKeywordIn(const KeywordIDs: TCSSNumericalIDArray): boolean; overload;
    function IsLengthOrPercentage(AllowNegative: boolean): boolean; overload;
    function IsLengthOrPercentage(const ResValue: TCSSResCompValue; AllowNegative: boolean): boolean; overload;
    function IsSymbol(Token: TCSSToken): boolean; overload;
    function GetCompString: TCSSString; overload;
    function GetCompString(const aValue: string; const ResValue: TCSSResCompValue): TCSSString; overload;
    // low level functions to parse attribute components
    function ReadComp(var aComp: TCSSResCompValue): boolean; // true if parsing attribute can continue
    procedure ReadWordID(var aComp: TCSSResCompValue);
    class function ReadValue(var aComp: TCSSResCompValue): boolean; // true if parsing attribute can continue, not using CSSRegistry
    class function ReadNumber(var aComp: TCSSResCompValue): boolean;
    class function ReadIdentifier(var aComp: TCSSResCompValue): boolean;
    class procedure SkipToEndOfAttribute(var p: PCSSChar);
    class function SkipString(var p: PCSSChar): boolean;
    class function SkipBrackets(var p: PCSSChar; Lvl: integer = 1): boolean;
    // registry
    function GetAttributeID(const aName: TCSSString; AutoCreate: boolean = false): TCSSNumericalID; virtual;
    function GetAttributeDesc(AttrID: TCSSNumericalID): TCSSAttributeDesc; virtual;
    function GetTypeID(const aName: TCSSString): TCSSNumericalID; virtual;
    function GetPseudoClassID(const aName: TCSSString): TCSSNumericalID; virtual;
    function GetPseudoFunctionID(const aName: TCSSString): TCSSNumericalID; virtual;

    property CSSRegistry: TCSSRegistry read FCSSRegistry write SetCSSRegistry;
  end;

  { TCSSResolverParser
    - resolves identifiers to IDs
    - warns about constructs unsupported by resolver }

  TCSSResolverParser = class(TCSSParser)
  private
    FOnLog: TCSSValueParserLogEvent;
    FResolver: TCSSBaseResolver;
  protected
    function ResolveAttribute(El: TCSSResolvedIdentifierElement): TCSSNumericalID; virtual;
    function ResolveType(El: TCSSResolvedIdentifierElement): TCSSNumericalID; virtual;
    function ResolvePseudoClass(El: TCSSResolvedPseudoClassElement): TCSSNumericalID; virtual;
    function ResolvePseudoFunction(El: TCSSResolvedCallElement): TCSSNumericalID; virtual;
    function ParseCall(aName: TCSSString; IsSelector: boolean): TCSSCallElement; override;
    function ParseDeclaration(aIsAt: Boolean): TCSSDeclarationElement; override;
    function ParseSelector: TCSSElement; override;
    procedure CheckSelector(El: TCSSElement); virtual;
    procedure CheckSelectorArray(anArray: TCSSArrayElement); virtual;
    procedure CheckSelectorArrayBinary(aBinary: TCSSBinaryElement); virtual;
    procedure CheckSelectorBinary(aBinary: TCSSBinaryElement); virtual;
    procedure CheckSelectorList(aList: TCSSListElement); virtual;
    procedure CheckNthChildParams(aCall: TCSSResolvedCallElement); virtual;
    function ComputeValue(El: TCSSElement): TCSSString; virtual;
  public
    CSSNthChildParamsClass: TCSSNthChildParamsClass;
    CSSAttributeKeyDataClass: TCSSAttributeKeyDataClass;
    constructor Create(AScanner: TCSSScanner); override; overload;
    destructor Destroy; override;
    procedure Log(MsgType: TEventType; const ID: TCSSMsgID; const Msg: TCSSString; PosEl: TCSSElement); virtual;
    class function IsWhiteSpace(const s: TCSSString): boolean; virtual; overload;
    property Resolver: TCSSBaseResolver read FResolver write FResolver;
    property OnLog: TCSSValueParserLogEvent read FOnLog write FOnLog;
  end;

implementation

Const
  Alpha = ['A'..'Z','a'..'z'];
  Num   = ['0'..'9'];
  AlNum = Alpha+Num;
  AlIden = Alpha+['-'];
  AlNumIden = AlNum+['-'];
  Whitespace = [#9,#10,#13,' '];
  //WhitespaceZ = Whitespace+[#0];
  Hex = ['0'..'9','a'..'z','A'..'Z'];
  ValEnd = [#0,#10,#13,#9,' ',';',',','}',')',']']; // used for skipping a component value

{ TCSSRegistry }

procedure TCSSRegistry.SetModified(const AValue: boolean);
begin
  if AValue then
    ChangeStamp
  else
    FModifiedStamp:=FStamp;
end;

function TCSSRegistry.GetModified: boolean;
begin
  Result:=FStamp=FModifiedStamp;
end;

constructor TCSSRegistry.Create;
var
  Kind: TCSSNumericalIDKind;
  i: Integer;
begin
  for Kind in TCSSNumericalIDKind do
    FHashLists[Kind]:=TFPHashList.Create;
  if Attribute_ClassOf=nil then
    Attribute_ClassOf:=TCSSAttributeDesc;
  if PseudoClass_ClassOf=nil then
    PseudoClass_ClassOf:=TCSSPseudoClassDesc;
  if Type_ClassOf=nil then
    Type_ClassOf:=TCSSTypeDesc;

  // init attributes
  SetLength(Attributes,32);
  for i:=0 to length(Attributes)-1 do Attributes[i]:=nil;
  FAttributeCount:=1; // index 0 is CSSIDNone

  // init pseudo classes
  SetLength(PseudoClasses,32);
  for i:=0 to length(PseudoClasses)-1 do PseudoClasses[i]:=nil;
  FPseudoClassCount:=1; // index 0 is CSSIDNone

  // init pseudo functions
  SetLength(PseudoFunctions,16);
  FPseudoFunctionCount:=1; // index 0 is CSSIDNone

  // init types
  SetLength(Types,32);
  for i:=0 to length(Types)-1 do Types[i]:=nil;
  FTypeCount:=1; // index 0 is CSSIDNone

  // init keywords
  SetLength(Keywords,32);
  FKeywordCount:=1; // index 0 is CSSIDNone

  // init keywords
  SetLength(AttrFunctions,32);
  FAttrFunctionCount:=1; // index 0 is CSSIDNone
end;

procedure TCSSRegistry.Init;
begin
  // init attributes
  if AddAttribute('id').Index<>CSSAttributeID_ID then
    raise Exception.Create('20240617191749');
  if AddAttribute('class').Index<>CSSAttributeID_Class then
    raise Exception.Create('20240617191801');
  if AddAttribute('all').Index<>CSSAttributeID_All then
    raise Exception.Create('20240617191816');

  // init pseudo classes
  if AddPseudoClass('root').Index<>CSSPseudoID_Root then
    raise Exception.Create('20240623165848');
  if AddPseudoClass('empty').Index<>CSSPseudoID_Empty then
    raise Exception.Create('20240623170450');
  if AddPseudoClass('first-child').Index<>CSSPseudoID_FirstChild then
    raise Exception.Create('20240623170508');
  if AddPseudoClass('last-child').Index<>CSSPseudoID_LastChild then
    raise Exception.Create('20240623170521');
  if AddPseudoClass('only-child').Index<>CSSPseudoID_OnlyChild then
    raise Exception.Create('20240623170534');
  if AddPseudoClass('first-of-type').Index<>CSSPseudoID_FirstOfType then
    raise Exception.Create('20240623170547');
  if AddPseudoClass('last-of-type').Index<>CSSPseudoID_LastOfType then
    raise Exception.Create('20240623170558');
  if AddPseudoClass('only-of-type').Index<>CSSPseudoID_OnlyOfType then
    raise Exception.Create('20240623170609');

  // init pseudo functions
  if AddPseudoFunction('not')<>CSSCallID_Not then
    raise Exception.Create('20240625183757');
  if AddPseudoFunction('is')<>CSSCallID_Is then
    raise Exception.Create('20240625142038');
  if AddPseudoFunction('where')<>CSSCallID_Where then
    raise Exception.Create('20240625142049');
  if AddPseudoFunction('has')<>CSSCallID_Has then
    raise Exception.Create('20240625142104');
  if AddPseudoFunction('nth-child')<>CSSCallID_NthChild then
    raise Exception.Create('20240625142124');
  if AddPseudoFunction('nth-last-child')<>CSSCallID_NthLastChild then
    raise Exception.Create('20240625142136');
  if AddPseudoFunction('nth-of-type')<>CSSCallID_NthOfType then
    raise Exception.Create('20240625142156');
  if AddPseudoFunction('nth-last-of-type')<>CSSCallID_NthLastOfType then
    raise Exception.Create('20240625142212');

  // init types
  if AddType('*').Index<>CSSTypeID_Universal then
    raise Exception.Create('20240617190914');

  // init keywords
  if AddKeyword('none')<>CSSKeywordNone then
    raise Exception.Create('20240623184021');
  if AddKeyword('initial')<>CSSKeywordInitial then
    raise Exception.Create('20240623184030');
  if AddKeyword('inherit')<>CSSKeywordInherit then
    raise Exception.Create('20240623184042');
  if AddKeyword('unset')<>CSSKeywordUnset then
    raise Exception.Create('20240623184053');
  if AddKeyword('revert')<>CSSKeywordRevert then
    raise Exception.Create('20240623184104');
  if AddKeyword('revert-layer')<>CSSKeywordRevertLayer then
    raise Exception.Create('20240623184114');
  if AddKeyword('auto')<>CSSKeywordAuto then
    raise Exception.Create('20240625182731');

  // init attribute functions
  if AddAttrFunction('var')<>CSSAttrFuncVar then
    raise Exception.Create('20240716124054');
end;

destructor TCSSRegistry.Destroy;
var
  i: Integer;
  Kind: TCSSNumericalIDKind;
begin
  for Kind in TCSSNumericalIDKind do
    FreeAndNil(FHashLists[Kind]);

  // attributes
  NotAllAttributes:=nil;
  for i:=1 to AttributeCount-1 do
    FreeAndNil(Attributes[i]);
  Attributes:=nil;
  FAttributeCount:=0;

  // pseudo classes
  for i:=1 to PseudoClassCount-1 do
    FreeAndNil(PseudoClasses[i]);
  PseudoClasses:=nil;
  FPseudoClassCount:=0;

  // types
  for i:=1 to TypeCount-1 do
    FreeAndNil(Types[i]);
  Types:=nil;
  FTypeCount:=0;

  // keywords
  FKeywordCount:=0;

  inherited Destroy;
end;

function TCSSRegistry.FindNamedItem(Kind: TCSSNumericalIDKind;
  const aName: TCSSString): TCSSRegistryNamedItem;
begin
  if Kind in nikAllDescriptors then
    Result:=TCSSRegistryNamedItem(FHashLists[Kind].Find(aName))
  else
    raise Exception.Create('20240625141820');
end;

function TCSSRegistry.IndexOfNamedItem(Kind: TCSSNumericalIDKind;
  const aName: TCSSString): TCSSNumericalID;
var
  Item: TCSSRegistryNamedItem;
  p: Pointer;
begin
  if Kind in nikAllDescriptors then
  begin
    Item:=TCSSRegistryNamedItem(FHashLists[Kind].Find(aName));
    if Item<>nil then
      Result:=Item.Index
    else
      Result:=-1;
  end else begin
    p:=FHashLists[Kind].Find(aName);
    if p=nil then
      exit(CSSIDNone)
    else
      Result:={%H-}TCSSNumericalID(p);
  end;
end;

procedure TCSSRegistry.ConsistencyCheck;
var
  ID, ID2: TCSSNumericalID;
  AttrDesc, SubAttrDesc: TCSSAttributeDesc;
  PseudoClassDesc: TCSSPseudoClassDesc;
  TypeDesc: TCSSTypeDesc;
  i: Integer;
  aName: TCSSString;
begin
  if AttributeCount>length(Attributes) then
    raise Exception.Create('20240629102438');
  for ID:=1 to AttributeCount-1 do
  begin
    AttrDesc:=Attributes[ID];
    if AttrDesc=nil then
      raise Exception.Create('20240629102530 attr ID='+IntToStr(ID)+' Desc=nil');
    aName:=AttrDesc.Name;
    if aName='' then
      raise Exception.Create('20240629100056 attr ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise Exception.Create('20240629100143 attr ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise Exception.Create('20240701231211 attr ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if AttrDesc.Index<>ID then
      raise Exception.Create('20240629095849 attr ID='+IntToStr(ID)+' Desc.Index='+IntToStr(AttrDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfAttributeName(aName);
    if ID2<>ID then
      raise Exception.Create('20240629101227 attr ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));

    if length(AttrDesc.CompProps)>0 then
    begin
      // check shorthand
      for i:=0 to length(AttrDesc.CompProps)-1 do
      begin
        SubAttrDesc:=AttrDesc.CompProps[i];
        if SubAttrDesc=nil then
          raise Exception.Create('20240629102701 attr ID='+IntToStr(ID)+' Shorthand="'+aName+'" CompDesc=nil '+IntToStr(i));
        if (SubAttrDesc.Index<=0) then
          raise Exception.Create('20240629100345 attr ID='+IntToStr(ID)+' Shorthand="'+aName+'" invalid CompAttr '+IntToStr(SubAttrDesc.Index));
        if (SubAttrDesc.Index>=ID) then
          raise Exception.Create('20240629100345 attr ID='+IntToStr(ID)+' Shorthand="'+aName+'" CompAttr after Shorthand: SubID='+IntToStr(SubAttrDesc.Index)+' SubName='+SubAttrDesc.Name);
      end;
    end;
  end;

  if PseudoClassCount>length(PseudoClasses) then
    raise Exception.Create('20240629102438');
  for ID:=1 to PseudoClassCount-1 do
  begin
    PseudoClassDesc:=PseudoClasses[ID];
    if PseudoClassDesc=nil then
      raise Exception.Create('20240629102605 pseudo class ID='+IntToStr(ID)+' Desc=nil');
    aName:=PseudoClassDesc.Name;
    if aName='' then
      raise Exception.Create('20240629100652 pseudo class ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise Exception.Create('20240629100657 pseudo class ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise Exception.Create('20240701231235 pseudo class ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if PseudoClassDesc.Index<>ID then
      raise Exception.Create('20240629100659 pseudo class ID='+IntToStr(ID)+' Desc.Index='+IntToStr(PseudoClassDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfPseudoClassName(PseudoClassDesc.Name);
    if ID2<>ID then
      raise Exception.Create('20240629101227 pseudo class ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if PseudoFunctionCount>length(PseudoFunctions) then
    raise Exception.Create('20240629103430');
  for ID:=1 to PseudoFunctionCount-1 do
  begin
    aName:=PseudoFunctions[ID];
    if aName='' then
      raise Exception.Create('20240629103431 pseudo function ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise Exception.Create('20240629103433 pseudo function ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise Exception.Create('20240701231235 pseudo function ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    ID2:=IndexOfPseudoFunction(aName);
    if ID2<>ID then
      raise Exception.Create('20240629103434 pseudo function ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if TypeCount>length(Types) then
    raise Exception.Create('20240629102438');
  for ID:=1 to TypeCount-1 do
  begin
    TypeDesc:=Types[ID];
    if TypeDesc=nil then
      raise Exception.Create('20240629102620 type ID='+IntToStr(ID)+' Desc=nil');
    aName:=TypeDesc.Name;
    if aName='' then
      raise Exception.Create('20240629100812 type ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise Exception.Create('20240629100825 type ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise Exception.Create('20240701231645 type ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    if TypeDesc.Index<>ID then
      raise Exception.Create('20240629101013 type ID='+IntToStr(ID)+' Desc.Index='+IntToStr(TypeDesc.Index)+' "'+aName+'"');
    ID2:=IndexOfTypeName(aName);
    if ID2<>ID then
      raise Exception.Create('20240629101529 type ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;

  if KeywordCount>length(Keywords) then
    raise Exception.Create('20240629103200');
  for ID:=1 to KeywordCount-1 do
  begin
    aName:=Keywords[ID];
    if aName='' then
      raise Exception.Create('20240629103223 keyword ID='+IntToStr(ID)+' missing name');
    if length(aName)>255 then
      raise Exception.Create('20240629103242 keyword ID='+IntToStr(ID)+' name too long "'+aName+'"');
    if aName[1]=':' then
      raise Exception.Create('20240701231656 keyword ID='+IntToStr(ID)+' invalid name "'+aName+'"');
    ID2:=IndexOfKeyword(aName);
    if ID2<>ID then
      raise Exception.Create('20240629103303 keyword ID='+IntToStr(ID)+' "'+aName+'" IndexOf failed: '+IntToStr(ID2));
  end;
end;

procedure TCSSRegistry.ChangeStamp;
begin
  if FStamp<high(FStamp) then
    inc(FStamp)
  else
    FStamp:=1;
end;

function TCSSRegistry.AddAttribute(Attr: TCSSAttributeDesc
  ): TCSSAttributeDesc;
begin
  Result:=Attr;
  if Attr.Name='' then
    raise ECSSParser.Create('missing name');
  if FindAttribute(Attr.Name)<>nil then
    raise ECSSParser.Create('duplicate attribute "'+Attr.Name+'"');

  if AttributeCount=length(Attributes) then
  begin
    if AttributeCount<32 then
      SetLength(Attributes,32)
    else
      SetLength(Attributes,2*AttributeCount);
    FillByte(Attributes[AttributeCount],SizeOf(Pointer)*(length(Attributes)-AttributeCount),0);
  end;
  Attributes[AttributeCount]:=Attr;
  Attr.Index:=AttributeCount;
  FHashLists[nikAttribute].Add(Attr.Name,Attr);
  inc(FAttributeCount);
  ChangeStamp;
end;

function TCSSRegistry.AddAttribute(const aName: TCSSString;
  const aInitialValue: TCSSString; aInherits: boolean; aAll: boolean;
  aClass: TCSSAttributeDescClass): TCSSAttributeDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if FindAttribute(aName)<>nil then
    raise ECSSParser.Create('duplicate attribute "'+aName+'"');
  if aClass=nil then
    aClass:=Attribute_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  Result.InitialValue:=aInitialValue;
  Result.Inherits:=aInherits;
  Result.All:=aAll;
  AddAttribute(Result);

  if not aAll then
    Insert(Result,NotAllAttributes,length(NotAllAttributes));
end;

function TCSSRegistry.FindAttribute(const aName: TCSSString
  ): TCSSAttributeDesc;
begin
  Result:=TCSSAttributeDesc(FHashLists[nikAttribute].Find(aName));
end;

function TCSSRegistry.IndexOfAttributeName(const aName: TCSSString
  ): TCSSNumericalID;
var
  Attr: TCSSAttributeDesc;
begin
  Attr:=TCSSAttributeDesc(FHashLists[nikAttribute].Find(aName));
  if Attr<>nil then
    Result:=Attr.Index
  else
    Result:=-1;
end;

procedure TCSSRegistry.AddSplitLonghand(var AttrIDs: TCSSNumericalIDArray;
  var Values: TCSSStringArray; AttrID: TCSSNumericalID; const Value: TCSSString);
begin
  System.Insert(AttrID,AttrIDs,length(AttrIDs));
  System.Insert(Value,Values,length(Values));
end;

procedure TCSSRegistry.AddSplitLonghandSides(var AttrIDs: TCSSNumericalIDArray;
  var Values: TCSSStringArray; TopID, RightID, BottomID, LeftID: TCSSNumericalID;
  const Found: TCSSStringArray);
begin
  if length(Found)=0 then
    exit; // invalid

  Setlength(AttrIDs,4);
  Setlength(Values,4);

  AttrIDs[0]:=TopID;
  AttrIDs[1]:=RightID;
  AttrIDs[2]:=BottomID;
  AttrIDs[3]:=LeftID;

  // sets sides depending on how many values were passed:
  // 1: all four the same
  // 2: top and bottom | left and right
  // 3: top | left and right | bottom
  // 4: top | right | bottom | left
  case length(Found) of
  1:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[0];
      Values[2]:=Found[0];
      Values[3]:=Found[0];
    end;
  2:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[0];
      Values[3]:=Found[1];
    end;
  3:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[1];
    end;
  4:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[3];
    end;
  end;
end;

procedure TCSSRegistry.AddSplitLonghandCorners(var AttrIDs: TCSSNumericalIDArray;
  var Values: TCSSStringArray; TopLeftID, TopRightID, BottomLeftID, BottomRightID: TCSSNumericalID;
  const Found: TCSSStringArray);
begin
  if length(Found)=0 then
    exit; // invalid

  Setlength(AttrIDs,4);
  Setlength(Values,4);

  AttrIDs[0]:=TopLeftID;
  AttrIDs[1]:=TopRightID;
  AttrIDs[2]:=BottomRightID;
  AttrIDs[3]:=BottomLeftID;

  // sets corners depending on how many values were passed:
  // 1: all four the same
  // 2: top-left-and-bottom-right | top-right-and-bottom-left
  // 3: top-left | top-right-and-bottom-left | bottom-right
  // 4: top-left | top-right | bottom-right | bottom-left

  case length(Found) of
  1:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[0];
      Values[2]:=Found[0];
      Values[3]:=Found[0];
    end;
  2:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[0];
      Values[3]:=Found[1];
    end;
  3:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[1];
    end;
  4:
    begin
      Values[0]:=Found[0];
      Values[1]:=Found[1];
      Values[2]:=Found[2];
      Values[3]:=Found[3];
    end;
  end;
end;

function TCSSRegistry.AddPseudoClass(aPseudo: TCSSPseudoClassDesc
  ): TCSSPseudoClassDesc;
begin
  Result:=aPseudo;
  if aPseudo.Name='' then
    raise ECSSParser.Create('missing name');
  if FindPseudoClass(aPseudo.Name)<>nil then
    raise ECSSParser.Create('duplicate pseudo class "'+aPseudo.Name+'"');

  if PseudoClassCount=length(PseudoClasses) then
  begin
    if PseudoClassCount<32 then
      SetLength(PseudoClasses,32)
    else
      SetLength(PseudoClasses,2*PseudoClassCount);
    FillByte(PseudoClasses[PseudoClassCount],SizeOf(Pointer)*(length(PseudoClasses)-PseudoClassCount),0);
  end;
  PseudoClasses[PseudoClassCount]:=aPseudo;
  aPseudo.Index:=PseudoClassCount;
  FHashLists[nikPseudoClass].Add(aPseudo.Name,aPseudo);
  inc(FPseudoClassCount);
  ChangeStamp;
end;

function TCSSRegistry.AddPseudoClass(const aName: TCSSString;
  aClass: TCSSPseudoClassDescClass): TCSSPseudoClassDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if FindPseudoClass(aName)<>nil then
    raise ECSSParser.Create('duplicate pseudo class "'+aName+'"');
  if aClass=nil then
    aClass:=PseudoClass_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  AddPseudoClass(Result);
end;

function TCSSRegistry.FindPseudoClass(const aName: TCSSString
  ): TCSSPseudoClassDesc;
begin
  Result:=TCSSPseudoClassDesc(FHashLists[nikPseudoClass].Find(aName));
end;

function TCSSRegistry.IndexOfPseudoClassName(const aName: TCSSString
  ): TCSSNumericalID;
var
  aPseudo: TCSSPseudoClassDesc;
begin
  aPseudo:=TCSSPseudoClassDesc(FHashLists[nikPseudoClass].Find(aName));
  if aPseudo<>nil then
    Result:=aPseudo.Index
  else
    Result:=-1;
end;

function TCSSRegistry.AddPseudoFunction(const aName: TCSSString
  ): TCSSNumericalID;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if length(aName)>255 then
    raise ECSSParser.Create('pseudo function name too long');
  if aName<>LowerCase(aName) then
    raise ECSSParser.Create('pseudo function name not lowercase');
  Result:=IndexOfKeyword(aName);
  if Result>0 then
    raise ECSSParser.Create('duplicate pseudo function "'+aName+'"');

  if PseudoFunctionCount=length(PseudoFunctions) then
  begin
    if PseudoFunctionCount<32 then
      SetLength(PseudoFunctions,32)
    else
      SetLength(PseudoFunctions,2*PseudoFunctionCount);
  end;
  Result:=PseudoFunctionCount;
  PseudoFunctions[Result]:=aName;
  FHashLists[nikPseudoFunction].Add(aName,{%H-}Pointer(Result));
  inc(FPseudoFunctionCount);
  ChangeStamp;
end;

function TCSSRegistry.IndexOfPseudoFunction(const aName: TCSSString
  ): TCSSNumericalID;
var
  p: Pointer;
begin
  p:=FHashLists[nikPseudoFunction].Find(aName);
  if p=nil then
    exit(CSSIDNone)
  else
    Result:={%H-}TCSSNumericalID(p);
end;

function TCSSRegistry.AddType(aType: TCSSTypeDesc): TCSSTypeDesc;
begin
  Result:=aType;
  if aType.Name='' then
    raise ECSSParser.Create('missing name');
  if FindType(aType.Name)<>nil then
    raise ECSSParser.Create('duplicate type "'+aType.Name+'"');

  if TypeCount=length(Types) then
  begin
    if TypeCount<32 then
      SetLength(Types,32)
    else
      SetLength(Types,2*TypeCount);
    FillByte(Types[TypeCount],SizeOf(Pointer)*(length(Types)-TypeCount),0);
  end;
  Types[TypeCount]:=aType;
  aType.Index:=TypeCount;
  FHashLists[nikType].Add(aType.Name,aType);
  inc(FTypeCount);
  ChangeStamp;
end;

function TCSSRegistry.AddType(const aName: TCSSString; aClass: TCSSTypeDescClass
  ): TCSSTypeDesc;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if FindType(aName)<>nil then
    raise ECSSParser.Create('duplicate type "'+aName+'"');
  if aClass=nil then
    aClass:=Type_ClassOf;

  Result:=aClass.Create;
  Result.Name:=aName;
  AddType(Result);
end;

function TCSSRegistry.FindType(const aName: TCSSString): TCSSTypeDesc;
begin
  Result:=TCSSTypeDesc(FHashLists[nikType].Find(aName));
end;

function TCSSRegistry.IndexOfTypeName(const aName: TCSSString): TCSSNumericalID;
var
  aType: TCSSTypeDesc;
begin
  aType:=TCSSTypeDesc(FHashLists[nikType].Find(aName));
  if aType<>nil then
    Result:=aType.Index
  else
    Result:=-1;
end;

function TCSSRegistry.AddKeyword(const aName: TCSSString): TCSSNumericalID;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if length(aName)>255 then
    raise ECSSParser.Create('keyword too long');
  Result:=IndexOfKeyword(aName);
  if Result>0 then
    raise ECSSParser.Create('duplicate keyword "'+aName+'"');

  if KeywordCount=length(Keywords) then
  begin
    if KeywordCount<32 then
      SetLength(Keywords,32)
    else
      SetLength(Keywords,2*KeywordCount);
  end;
  Result:=KeywordCount;
  Keywords[Result]:=aName;
  FHashLists[nikKeyword].Add(aName,{%H-}Pointer(Result));
  inc(FKeywordCount);
  ChangeStamp;
end;

procedure TCSSRegistry.AddKeywords(const Names: TCSSStringArray; out First, Last: TCSSNumericalID);
var
  i, NewCnt: integer;
begin
  if Names=nil then begin
    First:=CSSIDNone;
    Last:=CSSIDNone;
    exit;
  end;
  for i:=0 to length(Names)-1 do
    if IndexOfKeyword(Names[i])>CSSIDNone then
      raise Exception.Create('20240712215853');

  NewCnt:=KeywordCount+length(Names);
  if NewCnt>length(Keywords) then
  begin
    NewCnt:=(NewCnt div 32 +1) *32;
    SetLength(Keywords,NewCnt);
  end;

  First:=KeywordCount;
  for i:=0 to length(Names)-1 do
  begin
    Last:=KeywordCount;
    Keywords[Last]:=Names[i];
    FHashLists[nikKeyword].Add(Names[i],{%H-}Pointer(Last));
    inc(FKeywordCount);
  end;
  ChangeStamp;
end;

function TCSSRegistry.IndexOfKeyword(const aName: TCSSString): TCSSNumericalID;
var
  p: Pointer;
begin
  p:=FHashLists[nikKeyword].Find(aName);
  if p=nil then
    exit(CSSIDNone)
  else
    Result:={%H-}TCSSNumericalID(p);
end;

procedure TCSSRegistry.AddColorKeywords;
var
  Names: TCSSStringArray;
  i: Integer;
begin
  SetLength(Names{%H-},length(CSSNamedColors));
  for i:=0 to High(CSSNamedColors) do
    Names[i]:=CSSNamedColors[i].Name;
  AddKeywords(Names,kwFirstColor,kwLastColor);
  kwTransparent:=IndexOfKeyword('transparent');
end;

function TCSSRegistry.GetNamedColor(const aName: TCSSString): TCSSAlphaColor;
begin
  Result:=GetKeywordColor(IndexOfKeyword(aName));
end;

function TCSSRegistry.GetKeywordColor(KeywordID: TCSSNumericalID): TCSSAlphaColor;
begin
  if (KeywordID<kwFirstColor) or (KeywordID>kwLastColor) then
    Result:=$ff000000
  else
    Result:=CSSNamedColors[KeywordID-kwFirstColor].Color;
end;

function TCSSRegistry.AddAttrFunction(const aName: TCSSString): TCSSNumericalID;
begin
  if aName='' then
    raise ECSSParser.Create('missing name');
  if length(aName)>255 then
    raise ECSSParser.Create('function name too long');
  Result:=IndexOfAttrFunction(aName);
  if Result>0 then
    raise ECSSParser.Create('duplicate attribute function "'+aName+'"');

  if AttrFunctionCount=length(AttrFunctions) then
  begin
    if AttrFunctionCount<32 then
      SetLength(AttrFunctions,32)
    else
      SetLength(AttrFunctions,2*AttrFunctionCount);
  end;
  Result:=AttrFunctionCount;
  AttrFunctions[Result]:=aName;
  FHashLists[nikAttrFunction].Add(aName,{%H-}Pointer(Result));
  inc(FAttrFunctionCount);
  ChangeStamp;
end;

function TCSSRegistry.IndexOfAttrFunction(const aName: TCSSString
  ): TCSSNumericalID;
var
  p: Pointer;
begin
  p:=FHashLists[nikAttrFunction].Find(aName);
  if p=nil then
    exit(CSSIDNone)
  else
    Result:={%H-}TCSSNumericalID(p);
end;

{ TCSSResolvedCallElement }

destructor TCSSResolvedCallElement.Destroy;
begin
  FreeAndNil(Params);
  inherited Destroy;
end;

{ TCSSResCompValue }

function TCSSResCompValue.AsString: TCSSString;
var
  l: integer;
begin
  if (StartP=nil) or (EndP=nil) or (EndP<=StartP) then
    exit('');
  l:=EndP-StartP;
  SetLength(Result,l);
  Move(StartP^,Result[1],l);
end;

function TCSSResCompValue.FloatAsString: TCSSString;
begin
  Result:=FloatToCSSStr(Float)+CSSUnitNames[FloatUnit];
end;

{ TCSSCheckAttrParams_Dimension }

function TCSSCheckAttrParams_Dimension.Fits(const ResValue: TCSSResCompValue): boolean;
var
  i: Integer;
begin
  Result:=false;
  case ResValue.Kind of
  rvkFloat:
    if ResValue.FloatUnit in AllowedUnits then
    begin
      if not (ResValue.FloatUnit in AllowedUnits) then exit;
      if (not AllowNegative) and (ResValue.Float<0) then exit;
      if (not AllowFrac) and (Frac(ResValue.Float)>0) then exit;
      exit(true);
    end else if (ResValue.FloatUnit=cuNone) and (ResValue.Float=0) then
      exit(true);
  rvkKeyword:
    for i:=0 to length(AllowedKeywordIDs)-1 do
      if ResValue.KeywordID=AllowedKeywordIDs[i] then
        exit(true);
  end;
end;

{ TCSSBaseResolver }

procedure TCSSBaseResolver.SetCSSRegistry(const AValue: TCSSRegistry);
begin
  if FCSSRegistry=AValue then Exit;
  FCSSRegistry:=AValue;
end;

function TCSSBaseResolver.InitParseAttr(Desc: TCSSAttributeDesc; AttrData: TCSSAttributeKeyData;
  const Value: TCSSString): boolean;
var
  p: PCSSChar;
begin
  Result:=false;
  CurAttrData:=AttrData;
  CurDesc:=Desc;
  CurValue:=Value;
  CurComp:=Default(TCSSResCompValue);
  CurComp.EndP:=PCSSChar(CurValue);
  if not ReadNext then
  begin
    if CurAttrData<>nil then
      CurAttrData.Invalid:=true;
    exit;
  end;
  if (CurAttrData<>nil) and (CurComp.Kind=rvkKeyword)
      and IsBaseKeyword(CurComp.KeywordID) then
  begin
    p:=CurComp.EndP;
    while (p^ in Whitespace) do inc(p);
    if p^>#0 then
    begin
      // "inherit" must be alone
      CurAttrData.Invalid:=true;
      exit;
    end;
    CurAttrData.Complete:=true;
  end;
  Result:=true;
end;

procedure TCSSBaseResolver.InitParseAttr(const Value: TCSSString);
begin
  CurValue:=Value;
  CurComp:=Default(TCSSResCompValue);
  CurComp.EndP:=PCSSChar(CurValue);
  ReadNext;
end;

function TCSSBaseResolver.CheckAttribute_Keyword(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
begin
  Result:=ReadAttribute_Keyword(CurAttrData.Invalid,AllowedKeywordIDs);
end;

function TCSSBaseResolver.CheckAttribute_CommaList_Keyword(
  const AllowedKeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
  Fits: Boolean;
begin
  CurAttrData.Invalid:=true;
  repeat
    Fits:=false;
    case CurComp.Kind of
    rvkKeyword:
      for i:=0 to length(AllowedKeywordIDs)-1 do
        if CurComp.KeywordID=AllowedKeywordIDs[i] then
        begin
          Fits:=true;
          break;
        end;
    rvkFunction:
      begin
        // todo: check for allowed functions
        Fits:=true;
      end;
    end;
    if not Fits then exit;

    if not ReadNext then
    begin
      // ok
      CurAttrData.Invalid:=false;
      exit(true);
    end;
    if (CurComp.Kind<>rvkSymbol) or (CurComp.Symbol=ctkCOMMA) then
      exit;
  until not ReadNext;
  Result:=false;
end;

function TCSSBaseResolver.CheckAttribute_Dimension(const Params: TCSSCheckAttrParams_Dimension
  ): boolean;
begin
  Result:=ReadAttribute_Dimension(CurAttrData.Invalid,Params);
end;

function TCSSBaseResolver.CheckAttribute_Color(const AllowedKeywordIDs: TCSSNumericalIDArray
  ): boolean;
begin
  Result:=ReadAttribute_Color(CurAttrData.Invalid,AllowedKeywordIDs);
end;

function TCSSBaseResolver.ReadNext: boolean;
begin
  Result:=ReadComp(CurComp);
end;

function TCSSBaseResolver.ReadAttribute_Keyword(out Invalid: boolean;
  const AllowedKeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
begin
  Invalid:=false;
  repeat
    case CurComp.Kind of
    rvkKeyword:
      for i:=0 to length(AllowedKeywordIDs)-1 do
        if CurComp.KeywordID=AllowedKeywordIDs[i] then
          exit(true);
    end;
    // todo: warn if invalid
  until not ReadNext;
  Invalid:=true;
  Result:=false;
end;

function TCSSBaseResolver.ReadAttribute_Dimension(out Invalid: boolean;
  const Params: TCSSCheckAttrParams_Dimension): boolean;
var
  i: Integer;
begin
  Invalid:=true;
  repeat
    case CurComp.Kind of
    rvkFloat:
      if Params.Fits(CurComp) then
        exit(true);
    rvkKeyword:
      for i:=0 to length(Params.AllowedKeywordIDs)-1 do
        if CurComp.KeywordID=Params.AllowedKeywordIDs[i] then
          exit(true);
    end;
    // todo: warn if invalid
  until not ReadNext;
  Invalid:=true;
  Result:=false;
end;

function TCSSBaseResolver.ReadAttribute_Color(out Invalid: boolean;
  const AllowedKeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
begin
  Invalid:=false;
  repeat
    case CurComp.Kind of
    rvkKeyword:
      begin
        if (CurComp.KeywordID>=CSSRegistry.kwFirstColor)
            and (CurComp.KeywordID<=CSSRegistry.kwLastColor)
        then
          exit(true);
        for i:=0 to length(AllowedKeywordIDs)-1 do
          if CurComp.KeywordID=AllowedKeywordIDs[i] then
            exit(true);
      end;
    rvkFunction:
      begin
        // todo: check for allowed functions
      end;
    rvkHexColor:
      exit(true);
    end;
    // todo: warn if invalid
  until not ReadNext;
  Invalid:=true;
  Result:=false;
end;

function TCSSBaseResolver.ReadComp(var aComp: TCSSResCompValue): boolean;
begin
  Result:=ReadValue(aComp);
  ReadWordID(aComp);
end;

procedure TCSSBaseResolver.ReadWordID(var aComp: TCSSResCompValue);
var
  Identifier: TCSSString;
  p: PCSSChar;
begin
  case aComp.Kind of
  rvkFunctionUnknown:
    begin
      p:=aComp.StartP;
      if not (p^ in AlIden) then exit;
      repeat
        inc(p);
      until not (p^ in AlNumIden);
      SetString(Identifier,aComp.StartP,p-aComp.StartP);
      aComp.FunctionID:=CSSRegistry.IndexOfAttrFunction(Identifier);
      if aComp.FunctionID>CSSIDNone then
        aComp.Kind:=rvkFunction;
    end;
  rvkKeywordUnknown:
    begin
      SetString(Identifier,aComp.StartP,aComp.EndP-aComp.StartP);
      aComp.KeywordID:=CSSRegistry.IndexOfKeyword(Identifier);
      if aComp.KeywordID>CSSIDNone then
        aComp.Kind:=rvkKeyword;
    end;
  end;
end;

class function TCSSBaseResolver.ReadValue(var aComp: TCSSResCompValue): boolean;
var
  c: TCSSChar;
  p: PCSSChar;
  l: SizeInt;

  procedure SetSymbol(s: TCSSToken);
  begin
    aComp.Kind:=rvkSymbol;
    aComp.Symbol:=s;
    aComp.EndP:=p+1;
  end;

begin
  Result:=true;
  aComp.Kind:=rvkNone;

  p:=aComp.EndP;

  // skip whitespace
  while (p^ in Whitespace) do inc(p);
  aComp.StartP:=p;
  aComp.EndP:=p;

  c:=p^;
  case c of
  #0: exit(false);
  '0'..'9':
    if ReadNumber(aComp) then exit;
  ',':
    begin
      SetSymbol(ctkCOMMA);
      exit;
    end;
  ')':
    begin
      SetSymbol(ctkRPARENTHESIS);
      exit;
    end;
  '+':
    case p[1] of
    '0'..'9','.':
      if ReadNumber(aComp) then exit;
    #0,#9,#10,#13,' ':
      begin
        SetSymbol(ctkPLUS);
        exit;
      end;
    end;
  '-':
    case p[1] of
    '0'..'9','.':
      if ReadNumber(aComp) then exit;
    'a'..'z','A'..'Z','-':
      if ReadIdentifier(aComp) then exit;
    #0,#9,#10,#13,' ':
      begin
        SetSymbol(ctkMINUS);
        exit;
      end;
    end;
  '.':
    case p[1] of
    '0'..'9':
      if ReadNumber(aComp) then exit;
    else
      SetSymbol(ctkDOT);
      exit;
    end;
  '*':
    begin
      if p[1]='=' then
      begin
        inc(p);
        SetSymbol(ctkSTAREQUAL);
      end else
        SetSymbol(ctkSTAR);
      exit;
    end;
  '/':
    begin
      SetSymbol(ctkDIV);
      exit;
    end;
  ':':
    begin
      SetSymbol(ctkCOLON);
      exit;
    end;
  ';':
    begin
      SetSymbol(ctkSEMICOLON);
      exit;
    end;
  'a'..'z','A'..'Z':
    if ReadIdentifier(aComp) then exit;
  '#':
    begin
      inc(p);
      while p^ in Hex do inc(p);
      l:=p-aComp.StartP;
      case l of
      4,5,7,9:
        begin
          // #rgb, #rgba, #rrggbb, #rrggbbaa
          aComp.Kind:=rvkHexColor;
          aComp.EndP:=p;
          exit;
        end;
      end;
    end;
  end;

  // skip unknown aComp
  aComp.Kind:=rvkInvalid;
  repeat
    if p^ in ValEnd then break;
    case p^ of
    '(','[': SkipBrackets(p);
    '''','"': SkipString(p);
    else inc(p);
    end;
  until false;
  aComp.EndP:=p;
end;

class function TCSSBaseResolver.ReadNumber(var aComp: TCSSResCompValue): boolean;
var
  Negative, HasNumber: Boolean;
  Divisor: double;
  Exponent: Integer;
  d: Float;
  U: TCSSUnit;
  StartP, p: PCSSChar;
begin
  Result:=false;
  aComp.Kind:=rvkInvalid;
  p:=aComp.StartP;

  // number: 1, 0.2, .3, 4.01, 0.0, +0.0, -0.0, .50, 2e3, -6.7E-2
  if p^='-' then
  begin
    Negative:=true;
    inc(p);
  end else begin
    Negative:=false;
    if p^='+' then
      inc(p);
  end;
  HasNumber:=false;
  aComp.Float:=0;
  if p^ in Num then
  begin
    // read significand
    HasNumber:=true;
    repeat
      aComp.Float:=aComp.Float*10+ord(p^)-ord('0');
      if aComp.Float>CSSMaxSafeIntDouble then
        exit; // loosing precision
      inc(p);
    until not (p^ in Num);
  end;
  if p^='.' then
  begin
    // read fraction
    inc(p);
    if not (p^ in Num) then exit;
    Divisor:=1;
    repeat
      Divisor:=Divisor*10;
      aComp.Float:=aComp.Float*10+ord(p^)-ord('0');
      if (Divisor>CSSMaxSafeIntDouble)
          or (aComp.Float>CSSMaxSafeIntDouble) then
        exit; // loosing precision
      inc(p);
    until not (p^ in Num);
    aComp.Float:=aComp.Float/Divisor;
  end else if not HasNumber then
    exit;
  if Negative then
    aComp.Float:=-aComp.Float;

  if (p^ in ['e','E']) and not (p[1] in ['a'..'z']) then
  begin
    inc(p);
    if p^='-' then
    begin
      Negative:=true;
      inc(p);
    end else begin
      Negative:=false;
      if p^='+' then
        inc(p);
    end;
    if not (p^ in Num) then exit;
    Exponent:=0;
    repeat
      inc(p);
      Exponent:=Exponent*10+ord(p^)-ord('0');
      if Exponent>2047 then
        exit; // out of bounds
    until not (p^ in Num);
    if Exponent>0 then
    begin
      if Negative then
        Exponent:=-Exponent;
      try
        d:=Power(10,Exponent);
        aComp.Float:=aComp.Float*d;
      except
        exit;
      end;
    end;
  end;
  aComp.Kind:=rvkFloat;

  // parse unit
  U:=cuNone;
  case p^ of
  '%':
    begin
      inc(p);
      U:=cuPercent;
    end;
  'a'..'z','A'..'Z':
    begin
      StartP:=p;
      while p^ in Alpha do inc(p);
      U:=high(TCSSUnit);
      while (U>cuNone) and not CompareMem(StartP,PChar(CSSUnitNames[U]),length(CSSUnitNames[U])) do
        U:=pred(U);
      if U=cuNone then
        exit; // unknown unit
    end;
  end;
  aComp.FloatUnit:=U;
  aComp.EndP:=p;

  Result:=true;
  //writeln('TCSSBaseResolver.ReadNumber "',p,'" Value=',FloatToCSSStr(aComp.Float),' U=',U,' Kind=',aComp.Kind,' Result=',Result);
end;

class function TCSSBaseResolver.ReadIdentifier(var aComp: TCSSResCompValue): boolean;
var
  IsFunc: Boolean;
  p: PCSSChar;
begin
  Result:=false;
  aComp.Kind:=rvkInvalid;
  p:=aComp.EndP;
  if not (p^ in AlIden) then exit;
  repeat
    inc(p);
  until not (p^ in AlNumIden);
  IsFunc:=p^='(';
  if IsFunc then
  begin
    // function call
    aComp.Kind:=rvkFunctionUnknown;
    aComp.BracketOpen:=p;
    if not SkipBrackets(p) then
    begin
      aComp.EndP:=p;
      exit;
    end;
  end else
    aComp.Kind:=rvkKeywordUnknown;
  aComp.EndP:=p;

  Result:=true;
end;

function TCSSBaseResolver.IsBaseKeyword(KeywordID: TCSSNumericalID): boolean;
begin
  Result:=(KeywordID>=CSSKeywordInitial) and (KeywordID<=CSSKeywordRevertLayer);
end;

function TCSSBaseResolver.IsKeywordIn(aKeywordID: TCSSNumericalID;
  const KeywordIDs: TCSSNumericalIDArray): boolean;
var
  i: Integer;
begin
  for i:=0 to length(KeywordIDs)-1 do
    if KeywordIDs[i]=aKeywordID then
      exit(true);
  Result:=false;
end;

function TCSSBaseResolver.IsKeywordIn(const KeywordIDs: TCSSNumericalIDArray): boolean;
var
  aKeywordID: TCSSNumericalID;
  i: Integer;
begin
  Result:=false;
  if CurComp.Kind<>rvkKeyword then exit;
  aKeywordID:=CurComp.KeywordID;
  for i:=0 to length(KeywordIDs)-1 do
    if KeywordIDs[i]=aKeywordID then
      exit(true);
end;

function TCSSBaseResolver.IsLengthOrPercentage(AllowNegative: boolean): boolean;
begin
  Result:=false;
  case CurComp.Kind of
  rvkFloat:
    if CurComp.FloatUnit in cuAllLengthsAndPercent then
    begin
      if (not AllowNegative) and (CurComp.Float<0) then exit;
      exit(true);
    end
    else if (CurComp.FloatUnit=cuNone) and (CurComp.Float=0) then
      exit(true); // 0 without unit is allowed
  end;
end;

function TCSSBaseResolver.IsLengthOrPercentage(const ResValue: TCSSResCompValue;
  AllowNegative: boolean): boolean;
begin
  Result:=false;
  case ResValue.Kind of
  rvkFloat:
    if ResValue.FloatUnit in cuAllLengthsAndPercent then
    begin
      if (not AllowNegative) and (ResValue.Float<0) then exit;
      exit(true);
    end
    else if (ResValue.FloatUnit=cuNone) and (ResValue.Float=0) then
      exit(true);
  end;
end;

function TCSSBaseResolver.IsSymbol(Token: TCSSToken): boolean;
begin
  Result:=(CurComp.Kind=rvkSymbol) and (CurComp.Symbol=Token);
end;

function TCSSBaseResolver.GetCompString: TCSSString;
var
  StartP: PCSSChar;
begin
  if CurComp.Kind=rvkKeyword then
    exit(CSSRegistry.Keywords[CurComp.KeywordID]);
  StartP:=CurComp.StartP;
  if (StartP=PCSSChar(CurValue)) and (CurComp.EndP-StartP = length(CurValue)) then
    Result:=CurValue
  else
    SetString(Result,StartP,CurComp.EndP-StartP);
end;

function TCSSBaseResolver.GetCompString(const aValue: string; const ResValue: TCSSResCompValue
  ): TCSSString;
var
  Start: PCSSChar;
begin
  if ResValue.Kind=rvkKeyword then
    exit(CSSRegistry.Keywords[ResValue.KeywordID]);
  Start:=ResValue.StartP;
  if (Start=PCSSChar(aValue)) and (ResValue.EndP-Start = length(aValue)) then
    Result:=aValue
  else
    SetString(Result,Start,ResValue.EndP-Start);
end;

class procedure TCSSBaseResolver.SkipToEndOfAttribute(var p: PCSSChar);
begin
  repeat
    case p^ of
    #0,'{','}',';': exit;
    '''','"': SkipString(p);
    else inc(p);
    end;
  until false;
end;

class function TCSSBaseResolver.SkipString(var p: PCSSChar): boolean;
var
  Delim, c: TCSSChar;
begin
  Result:=false;
  Delim:=p^;
  repeat
    inc(p);
    c:=p^;
    if c=Delim then
    begin
      inc(p);
      exit(true);
    end else if c=#0 then
      exit
    else
      inc(p);
  until false;
end;

class function TCSSBaseResolver.SkipBrackets(var p: PCSSChar; Lvl: integer): boolean;
const
  CSSMaxBracketLvl = 10;
var
  CloseBracket: TCSSChar;
begin
  Result:=false;
  if Lvl>CSSMaxBracketLvl then
  begin
    SkipToEndOfAttribute(p);
    exit;
  end;

  if p^='[' then
    CloseBracket:=']'
  else
    CloseBracket:=')';
  repeat
    inc(p);
    case p^ of
    #0,'{','}',';': exit;
    '''','"': SkipString(p);
    '(','[': SkipBrackets(p,Lvl+1);
    ')',']':
      if p^=CloseBracket then
      begin
        inc(p);
        exit(true);
      end else begin
        SkipToEndOfAttribute(p);
        exit;
      end;
    end;
  until false;
end;

function TCSSBaseResolver.GetAttributeID(const aName: TCSSString; AutoCreate: boolean
  ): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfAttributeName(aName);
  if AutoCreate then ;
end;

function TCSSBaseResolver.GetAttributeDesc(AttrID: TCSSNumericalID): TCSSAttributeDesc;
begin
  if (AttrID>0) and (AttrID<CSSRegistry.AttributeCount) then
    Result:=CSSRegistry.Attributes[AttrID]
  else
    Result:=nil;
end;

function TCSSBaseResolver.GetTypeID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfTypeName(aName);
end;

function TCSSBaseResolver.GetPseudoClassID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfPseudoClassName(aName);
end;

function TCSSBaseResolver.GetPseudoFunctionID(const aName: TCSSString): TCSSNumericalID;
begin
  Result:=CSSRegistry.IndexOfPseudoFunction(aName);
end;

{ TCSSResolverParser }

function TCSSResolverParser.ResolveAttribute(El: TCSSResolvedIdentifierElement): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NumericalID<>CSSIDNone then
    raise Exception.Create('20240701143234');
  aName:=El.Name;
  El.Kind:=nikAttribute;
  Result:=Resolver.GetAttributeID(aName,true);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20240822172823,'unknown attribute "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolveType(El: TCSSResolvedIdentifierElement): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NumericalID<>CSSIDNone then
    raise Exception.Create('20240822133813');
  aName:=El.Name;
  El.Kind:=nikType;
  Result:=Resolver.GetTypeID(aName);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20240822133816,'unknown type "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolvePseudoClass(
  El: TCSSResolvedPseudoClassElement): TCSSNumericalID;
var
  aName: TCSSString;
begin
  aName:=El.Name;
  // pseudo classes are ASCII case insensitive
  System.Delete(aName,1,1);
  aName:=lowercase(aName);

  if El.NumericalID<>CSSIDNone then
    raise Exception.Create('20240701143234');

  El.Kind:=nikPseudoClass;
  Result:=Resolver.GetPseudoClassID(aName);
  //writeln('TCSSResolverParser.ResolvePseudoClass ',aName,' ID=',Result);
  if Result<=CSSIDNone then
  begin
    El.NumericalID:=-1;
    Log(etWarning,20240822172826,'unknown pseudo class "'+aName+'"',El);
  end else
    El.NumericalID:=Result;
end;

function TCSSResolverParser.ResolvePseudoFunction(El: TCSSResolvedCallElement
  ): TCSSNumericalID;
var
  aName: TCSSString;
begin
  if El.NameNumericalID<>CSSIDNone then
    raise Exception.Create('20240701143035');
  aName:=El.Name;
  if aName[1]<>':' then
    raise Exception.Create('20240701143650');

  // pseudo functions are ASCII case insensitive
  System.Delete(aName,1,1);
  aName:=lowercase(aName);

  El.Kind:=nikPseudoFunction;
  Result:=Resolver.GetPseudoFunctionID(aName);
  //writeln('TCSSResolverParser.ResolvePseudoFunction ',aName,' ID=',Result);
  if Result<=CSSIDNone then
  begin
    El.NameNumericalID:=-1;
    Log(etWarning,20240822172830,'unknown pseudo function "'+aName+'"',El);
  end else
    El.NameNumericalID:=Result;
end;

function TCSSResolverParser.ParseCall(aName: TCSSString; IsSelector: boolean
  ): TCSSCallElement;
var
  CallID: TCSSNumericalID;
begin
  Result:=inherited ParseCall(aName, IsSelector);
  if IsSelector then
  begin
    if Result.Name[1]=':' then
    begin
      CallID:=ResolvePseudoFunction(TCSSResolvedCallElement(Result));
      case CallID of
      CSSCallID_NthChild,CSSCallID_NthLastChild,
      CSSCallID_NthOfType,CSSCallID_NthLastOfType: CheckNthChildParams(TCSSResolvedCallElement(Result));
      end;
    end
    else
      Log(etWarning,20240701222744,'invalid selector function',Result);
  end;
end;

function TCSSResolverParser.ParseDeclaration(aIsAt: Boolean
  ): TCSSDeclarationElement;
var
  aKey: TCSSElement;
  AttrId: TCSSNumericalID;
  Desc: TCSSAttributeDesc;
  AttrData: TCSSAttributeKeyData;
  i, ChildCnt: Integer;
begin
  Result:=inherited ParseDeclaration(aIsAt);
  if Result.KeyCount<>1 then
  begin
    if Result.KeyCount<1 then
      Log(etWarning,20231112135955,'missing keys in declaration',Result);
    if Result.KeyCount>1 then
      Log(etWarning,20231112140722,'too many keys in declaration',Result);
    exit;
  end;

  aKey:=Result.Keys[0];
  if aKey is TCSSResolvedIdentifierElement then
  begin
    AttrId:=ResolveAttribute(TCSSResolvedIdentifierElement(aKey));

    if aKey.CustomData<>nil then
      raise Exception.Create('20240626113536');
    AttrData:=CSSAttributeKeyDataClass.Create;
    aKey.CustomData:=AttrData;

    ChildCnt:=Result.ChildCount;
    if ChildCnt=0 then
    begin
      AttrData.Invalid:=true;
      exit;
    end;
    for i:=0 to ChildCnt-1 do
    begin
      if (i>0) then
        AttrData.Value+=', ';
      AttrData.Value+=Result.Children[i].AsString;
    end;

    if AttrId>=CSSAttributeID_All then
    begin
      Desc:=Resolver.GetAttributeDesc(AttrId);

      if Pos('var(',AttrData.Value)>0 then
      begin
        // cannot be parsed yet
      end else if AttrID<Resolver.CSSRegistry.AttributeCount then
      begin
        if Resolver.InitParseAttr(Desc,AttrData,AttrData.Value) then
        begin
          if Assigned(Desc.OnCheck) then
            AttrData.Invalid:=not Desc.OnCheck(Resolver);
        end;
      end;
      {$IFDEF VerboseCSSResolver}
      if AttrData.Invalid then
        Log(etWarning,20240710162400,'Invalid CSS attribute value: '+Result.AsString,aKey);
      {$ENDIF}
    end;
  end else begin
    Log(etWarning,20220908230855,'Expected property name, but found '+aKey.ClassName,aKey);
  end;
end;

function TCSSResolverParser.ParseSelector: TCSSElement;
begin
  Result:=inherited ParseSelector;
  CheckSelector(Result);
end;

procedure TCSSResolverParser.CheckSelector(El: TCSSElement);
var
  C: TClass;
begin
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
    // e.g. div {}
    ResolveType(TCSSResolvedIdentifierElement(El))
  else if C=TCSSHashIdentifierElement then
    // e.g. #id {}
  else if C=TCSSClassNameElement then
    // e.g. .classname {}
  else if C=TCSSResolvedPseudoClassElement then
    // e.g. :pseudoclass {}
    ResolvePseudoClass(TCSSResolvedPseudoClassElement(El))
  else if C=TCSSBinaryElement then
    CheckSelectorBinary(TCSSBinaryElement(El))
  else if C=TCSSArrayElement then
    CheckSelectorArray(TCSSArrayElement(El))
  else if C=TCSSListElement then
    CheckSelectorList(TCSSListElement(El))
  else if C=TCSSResolvedCallElement then
    // checked via ParseCall
  else
    Log(etWarning,20240625131810,'Unknown CSS selector element',El);
end;

procedure TCSSResolverParser.CheckSelectorArray(anArray: TCSSArrayElement);
var
  {$IFDEF VerboseCSSResolver}
  i: integer;
  {$ENDIF}
  El: TCSSElement;
  C: TClass;
  aValue: TCSSString;
begin
  if anArray.Prefix<>nil then
  begin
    Log(etWarning,20240625134737,'Invalid CSS attribute selector prefix',anArray.Prefix);
    exit;
  end;

  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolverParser.CheckSelectorArray Prefix=',GetCSSObj(anArray.Prefix),' ChildCount=',anArray.ChildCount);
  for i:=0 to anArray.ChildCount-1 do
    writeln('TCSSResolverParser.CheckSelectorArray ',i,' ',GetCSSObj(anArray.Children[i]));
  {$ENDIF}
  if anArray.ChildCount<1 then
  begin
    Log(etWarning,20220910154033,'Invalid CSS attribute selector',anArray);
    exit;
  end;

  if anArray.ChildCount>1 then
  begin
    El:=anArray.Children[1];
    C:=El.ClassType;
    if C=TCSSResolvedIdentifierElement then
    begin
      aValue:=TCSSResolvedIdentifierElement(El).Value;
      case aValue of
      'i','s': ;
      else
        Log(etWarning,20240625134931,'Invalid attribute modifier "'+aValue+'"',El);
        exit;
      end;
    end else begin
      Log(etWarning,20240625134940,'Invalid CSS attribute modifier',El);
      exit;
    end;
  end;
  if (anArray.ChildCount>2) then
    Log(etWarning,20240625134951,'Invalid CSS attribute modifier',anArray.Children[2]);

  El:=anArray.Children[0];
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
  begin
    // [name]  ->  has explicit attribute
    ResolveAttribute(TCSSResolvedIdentifierElement(El));
  end else if C=TCSSBinaryElement then
    CheckSelectorArrayBinary(TCSSBinaryElement(El))
  else begin
    Log(etWarning,20240625135119,'Invalid CSS array selector',El);
  end;
end;

procedure TCSSResolverParser.CheckSelectorArrayBinary(aBinary: TCSSBinaryElement
  );
var
  Left, Right: TCSSElement;
  C: TClass;
begin
  Left:=aBinary.Left;
  if Left.ClassType<>TCSSResolvedIdentifierElement then
  begin
    Log(etWarning,20240625154314,'Invalid CSS array selector, expected attribute',Left);
    exit;
  end;
  ResolveAttribute(TCSSResolvedIdentifierElement(Left));

  Right:=aBinary.Right;
  C:=Right.ClassType;
  if (C=TCSSStringElement) or (C=TCSSIntegerElement) or (C=TCSSFloatElement)
      or (C=TCSSResolvedIdentifierElement) then
    // ok
  else begin
    Log(etWarning,20240625154455,'Invalid CSS array selector, expected string',Right);
    exit;
  end;
  ComputeValue(Right);

  case aBinary.Operation of
  boEquals,
  boSquaredEqual,
  boDollarEqual,
  boPipeEqual,
  boStarEqual,
  boTildeEqual: ;
  else
    Log(etWarning,20240625154617,'Invalid CSS array selector operator',aBinary);
  end;
end;

procedure TCSSResolverParser.CheckSelectorBinary(aBinary: TCSSBinaryElement);
begin
  case aBinary.Operation of
  boGT,
  boPlus,
  boTilde,
  boWhiteSpace: ;
  else
    Log(etWarning,20240625153307,'Invalid CSS binary selector '+BinaryOperators[aBinary.Operation],aBinary);
  end;

  CheckSelector(aBinary.Left);
  CheckSelector(aBinary.Right);
end;

procedure TCSSResolverParser.CheckSelectorList(aList: TCSSListElement);
var
  i: Integer;
  El: TCSSElement;
begin
  for i:=0 to aList.ChildCount-1 do
  begin
    El:=aList.Children[i];
    {$IFDEF VerboseCSSResolver}
    writeln('TCSSResolverParser.CheckSelectorList ',i,' ',GetCSSObj(El),' AsString=',El.AsString);
    {$ENDIF}
    CheckSelector(El);
  end;
end;

procedure TCSSResolverParser.CheckNthChildParams(aCall: TCSSResolvedCallElement);

  procedure NthWarn(const ID: TCSSMsgID; const Msg: string; PosEl: TCSSElement);
  begin
    Log(etWarning,ID,CSSSelectorCallNames[aCall.NameNumericalID]+' '+Msg,PosEl);
  end;

var
  i, ArgCount, aModulo, aStart: Integer;
  Arg, OffsetEl: TCSSElement;
  Str: TCSSString;
  UnaryEl, anUnary: TCSSUnaryElement;
  Params: TCSSNthChildParams;
begin
  if aCall.Params<>nil then
    raise Exception.Create('20240625150639');
  ArgCount:=aCall.ArgCount;
  {$IFDEF VerboseCSSResolver}
  writeln('TCSSResolverParser.CheckSelectorCall_NthChild ArgCount=',aCall.ArgCount);
  for i:=0 to aCall.ArgCount-1 do
    writeln('TCSSResolverParser.CheckSelectorCall_NthChild Arg[',i,'] ',GetCSSObj(aCall.Args[i]),' AsString=',aCall.Args[i].AsString);
  {$ENDIF}

  // An, An+B, An+B of S, odd, even
  i:=0;
  aModulo:=0;
  aStart:=1;
  // check step
  if ArgCount<=i then
  begin
    NthWarn(20220915143843,'missing arguments',aCall);
    exit;
  end;
  Arg:=aCall.Args[i];
  if Arg.ClassType=TCSSIntegerElement then
  begin
    aModulo:=TCSSIntegerElement(Arg).Value;
    inc(i);
    // check n
    if ArgCount<=i then
    begin
      NthWarn(20220915143843,'missing arguments',aCall);
      exit;
    end;
    Arg:=aCall.Args[i];
    if Arg.ClassType<>TCSSResolvedIdentifierElement then
    begin
      NthWarn(20220915144312,'expected n',Arg);
      exit;
    end;
    if TCSSResolvedIdentifierElement(Arg).Value<>'n' then
    begin
      NthWarn(20220915144359,'expected n',Arg);
      exit;
    end;

  end
  else if Arg.ClassType=TCSSResolvedIdentifierElement then
  begin
    Str:=TCSSResolvedIdentifierElement(Arg).Value;
    case lowercase(Str) of
    'even':
      begin
      aModulo:=2;
      aStart:=2;
      end;
    'odd':
      begin
      aModulo:=2;
      end;
    'n':
      begin
      aModulo:=1;
      end;
    else
      NthWarn(20220915150332,'expected multiplier',Arg);
      exit;
    end
  end else if Arg.ClassType=TCSSUnaryElement then
  begin
    anUnary:=TCSSUnaryElement(Arg);
    case anUnary.Operation of
    uoMinus: aModulo:=-1;
    uoPlus: aModulo:=1;
    else
      NthWarn(20220917080309,'expected multiplier',Arg);
      exit;
    end;
    if (anUnary.Right.ClassType=TCSSResolvedIdentifierElement)
        and (SameText(TCSSResolvedIdentifierElement(anUnary.Right).Value,'n')) then
    begin
      // ok
    end else begin
      NthWarn(20220917080154,'expected multiplier',Arg);
      exit;
    end;
  end else
  begin
    NthWarn(20220915144056,'expected multiplier',Arg);
    exit;
  end;

  inc(i);
  if ArgCount>i then
  begin
    Arg:=aCall.Args[i];
    if Arg.ClassType=TCSSUnaryElement then
    begin
      UnaryEl:=TCSSUnaryElement(Arg);
      if not (UnaryEl.Operation in [uoMinus,uoPlus]) then
      begin
        NthWarn(20220915151422,'unexpected offset',UnaryEl);
        exit;
      end;
      OffsetEl:=UnaryEl.Right;
      if OffsetEl=nil then
      begin
        NthWarn(20220915151511,'unexpected offset',UnaryEl);
        exit;
      end;
      if OffsetEl.ClassType<>TCSSIntegerElement then
      begin
        NthWarn(20220915151718,'unexpected offset',OffsetEl);
        exit;
      end;
      aStart:=TCSSIntegerElement(OffsetEl).Value;
      if UnaryEl.Operation=uoMinus then
        aStart:=-aStart;
    end else
    begin
      NthWarn(20220915150851,'expected offset',Arg);
      exit;
    end;
  end;

  Params:=CSSNthChildParamsClass.Create;
  aCall.Params:=Params;
  Params.Modulo:=aModulo;
  Params.Start:=aStart;

  inc(i);
  if (i<ArgCount) then
  begin
    Arg:=aCall.Args[i];
    if (Arg.ClassType=TCSSResolvedIdentifierElement)
        and (SameText(TCSSResolvedIdentifierElement(Arg).Value,'of')) then
    begin
      // An+B of Selector
      inc(i);
      if i=ArgCount then
      begin
        NthWarn(20240711154813,'expected selector',Arg);
        exit;
      end;
      Arg:=aCall.Args[i];
      Params.HasOf:=true;
      Params.OfSelector:=Arg;
    end;
  end;

  if (aCall.NameNumericalID in [CSSCallID_NthOfType,CSSCallID_NthLastOfType]) then
    Params.HasOf:=true;
end;

function TCSSResolverParser.ComputeValue(El: TCSSElement): TCSSString;
var
  ElData: TObject;
  C: TClass;
  StrEl: TCSSStringElement;
  IntEl: TCSSIntegerElement;
  FloatEl: TCSSFloatElement;
begin
  C:=El.ClassType;
  if C=TCSSResolvedIdentifierElement then
    Result:=TCSSResolvedIdentifierElement(El).Value
  else if (C=TCSSStringElement)
      or (C=TCSSIntegerElement)
      or (C=TCSSFloatElement) then
  begin
    ElData:=El.CustomData;
    if ElData is TCSSValueData then
      exit(TCSSValueData(ElData).NormValue);
    if C=TCSSStringElement then
    begin
      StrEl:=TCSSStringElement(El);
      Result:=StrEl.Value;
    end
    else if C=TCSSIntegerElement then
    begin
      IntEl:=TCSSIntegerElement(El);
      Result:=IntEl.AsString;
    end else if C=TCSSFloatElement then
    begin
      FloatEl:=TCSSFloatElement(El);
      Result:=FloatEl.AsString;
    end;
    ElData:=TCSSValueData.Create;
    TCSSValueData(ElData).NormValue:=Result;
    El.CustomData:=ElData;
  end else begin
    Log(etWarning,20240625162632,'TCSSResolverParser.ComputeValue not supported',El);
  end;
end;

constructor TCSSResolverParser.Create(AScanner: TCSSScanner);
begin
  inherited Create(AScanner);
  CSSIdentifierElementClass:=TCSSResolvedIdentifierElement;
  CSSPseudoClassElementClass:=TCSSResolvedPseudoClassElement;
  CSSCallElementClass:=TCSSResolvedCallElement;
  CSSNthChildParamsClass:=TCSSNthChildParams;
  CSSAttributeKeyDataClass:=TCSSAttributeKeyData;
end;

destructor TCSSResolverParser.Destroy;
begin
  inherited Destroy;
end;

procedure TCSSResolverParser.Log(MsgType: TEventType; const ID: TCSSMsgID;
  const Msg: TCSSString; PosEl: TCSSElement);
begin
  if Assigned(OnLog) then
    OnLog(MsgType,ID,Msg,PosEl);
end;

class function TCSSResolverParser.IsWhiteSpace(const s: TCSSString): boolean;
var
  i: Integer;
begin
  for i:=1 to length(s) do
    if not (s[i] in [' ',#10,#13]) then
      exit(false);
  Result:=true;
end;

end.

