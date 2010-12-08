{ Parsed from Webkit.framework DOMComment.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMCOMMENT_PAS_T}
{$define DOMCOMMENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMCOMMENT_PAS_R}
{$define DOMCOMMENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMCOMMENT_PAS_F}
{$define DOMCOMMENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMCOMMENT_PAS_S}
{$define DOMCOMMENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMComment = objcclass;
  DOMCommentPointer = ^DOMComment;
  DOMCommentPtr = DOMCommentPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMCOMMENT_PAS_C}
{$define DOMCOMMENT_PAS_C}

{ DOMComment }
  DOMComment = objcclass(DOMCharacterData)
    
  public
    class function alloc: DOMComment; message 'alloc';
  end; external;

{$endif}
{$endif}
