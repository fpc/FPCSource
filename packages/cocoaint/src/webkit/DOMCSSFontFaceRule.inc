{ Parsed from Webkit.framework DOMCSSFontFaceRule.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMCSSFONTFACERULE_PAS_T}
{$define DOMCSSFONTFACERULE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCSSFONTFACERULE_PAS_R}
{$define DOMCSSFONTFACERULE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCSSFONTFACERULE_PAS_F}
{$define DOMCSSFONTFACERULE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCSSFONTFACERULE_PAS_S}
{$define DOMCSSFONTFACERULE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMCSSFontFaceRule = objcclass;
  DOMCSSFontFaceRulePointer = ^DOMCSSFontFaceRule;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCSSFONTFACERULE_PAS_C}
{$define DOMCSSFONTFACERULE_PAS_C}

{ DOMCSSFontFaceRule }
  DOMCSSFontFaceRule = objcclass(DOMCSSRule)
    
  public
    class function alloc: DOMCSSFontFaceRule; message 'alloc';

    function style: DOMCSSStyleDeclaration; message 'style';
  end; external;

{$endif}
{$endif}
