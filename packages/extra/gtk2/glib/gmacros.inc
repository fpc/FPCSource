{$ifndef __G_MACROS_H__}
{$define __G_MACROS_H__}

  const
     NULL = nil;
     gTRUE = 1;
     gFALSE = 0;

function CLAMP(x, MinX, MaxX: integer): integer;

// Macros by analogy to GINT_TO_POINTER, GPOINTER_TO_INT
function GPOINTER_TO_SIZE(p: GPointer): GSize;
function GSIZE_TO_POINTER(s: GSize): GPointer;

{$endif}    { __G_MACROS_H__  }


