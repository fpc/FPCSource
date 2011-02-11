{ Parsed from Webkit.framework DOMComment.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:19 CET 2011 }


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
  DOMComment = objcclass external (DOMCharacterData)
    
  public
  end;

{$endif}
{$endif}
