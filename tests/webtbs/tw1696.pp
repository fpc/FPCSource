{ %version=1.1 }
{ %target=win32 }

Uses Windows;
Var Font:HFONT;
Begin
   { Windows unit does not support Widechar correct }
   Font:=CreateFont(16, { Height Of Font }
                    0,   { Width Of Font  }
                    0,   { Angle Of Escapement }
                    0,   { Orientation Angle   }
                    FW_BOLD, { Font Weight }
                    0,   { Italic }
                    0,   { Underline }
                    0,   { Strikeout }
                    ANSI_CHARSET,
                    OUT_TT_PRECIS,
                    CLIP_DEFAULT_PRECIS,
                    PROOF_QUALITY,
                    FF_DONTCARE Or DEFAULT_PITCH,
                    'Verdana');
End.
