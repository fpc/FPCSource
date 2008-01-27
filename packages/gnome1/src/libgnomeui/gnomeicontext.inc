{$IfDef read_interface}

type
   PGnomeIconTextInfoRow = ^TGnomeIconTextInfoRow;
   TGnomeIconTextInfoRow = record
        thetext : Pgchar;
        width : gint;
        text_wc : PGdkWChar;
        text_length : gint;
     end;

   PGnomeIconTextInfo = ^TGnomeIconTextInfo;
   TGnomeIconTextInfo = record
        rows : PGList;
        font : PGdkFont;
        width : gint;
        height : gint;
        baseline_skip : gint;
     end;

function gnome_icon_layout_text(font:PGdkFont; theetext:Pgchar; separators:Pgchar; max_width:gint; confine:gboolean):PGnomeIconTextInfo;cdecl;external libgnomeuidll name 'gnome_icon_layout_text';
procedure gnome_icon_paint_text(ti:PGnomeIconTextInfo; drawable:PGdkDrawable; gc:PGdkGC; x:gint; y:gint;
            just:TGtkJustification);cdecl;external libgnomeuidll name 'gnome_icon_paint_text';
procedure gnome_icon_text_info_free(ti:PGnomeIconTextInfo);cdecl;external libgnomeuidll name 'gnome_icon_text_info_free';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
