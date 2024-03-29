{$PACKRECORDS C}
{$MODE OBJFPC}

{$IFNDEF FPC_DOTTEDUNITS}
unit xatom;
{$ENDIF FPC_DOTTEDUNITS}
interface
{$IFDEF FPC_DOTTEDUNITS}
uses Api.X11.X;
{$ELSE FPC_DOTTEDUNITS}
uses X;
{$ENDIF FPC_DOTTEDUNITS}

{
 THIS IS A GENERATED FILE

 Do not change!  Changing this file implies a protocol change!
}

const
        XA_PRIMARY             = TAtom ( 1);
        XA_SECONDARY           = TAtom ( 2);
        XA_ARC                 = TAtom ( 3);
        XA_ATOM                = TAtom ( 4);
        XA_BITMAP              = TAtom ( 5);
        XA_CARDINAL            = TAtom ( 6);
        XA_COLORMAP            = TAtom ( 7);
        XA_CURSOR              = TAtom ( 8);
        XA_CUT_BUFFER0         = TAtom ( 9);
        XA_CUT_BUFFER1         = TAtom (10);
        XA_CUT_BUFFER2         = TAtom (11);
        XA_CUT_BUFFER3         = TAtom (12);
        XA_CUT_BUFFER4         = TAtom (13);
        XA_CUT_BUFFER5         = TAtom (14);
        XA_CUT_BUFFER6         = TAtom (15);
        XA_CUT_BUFFER7         = TAtom (16);
        XA_DRAWABLE            = TAtom (17);
        XA_FONT                = TAtom (18);
        XA_INTEGER             = TAtom (19);
        XA_PIXMAP              = TAtom (20);
        XA_POINT               = TAtom (21);
        XA_RECTANGLE           = TAtom (22);
        XA_RESOURCE_MANAGER    = TAtom (23);
        XA_RGB_COLOR_MAP       = TAtom (24);
        XA_RGB_BEST_MAP        = TAtom (25);
        XA_RGB_BLUE_MAP        = TAtom (26);
        XA_RGB_DEFAULT_MAP     = TAtom (27);
        XA_RGB_GRAY_MAP        = TAtom (28);
        XA_RGB_GREEN_MAP       = TAtom (29);
        XA_RGB_RED_MAP         = TAtom (30);
        XA_STRING              = TAtom (31);
        XA_VISUALID            = TAtom (32);
        XA_WINDOW              = TAtom (33);
        XA_WM_COMMAND          = TAtom (34);
        XA_WM_HINTS            = TAtom (35);
        XA_WM_CLIENT_MACHINE   = TAtom (36);
        XA_WM_ICON_NAME        = TAtom (37);
        XA_WM_ICON_SIZE        = TAtom (38);
        XA_WM_NAME             = TAtom (39);
        XA_WM_NORMAL_HINTS     = TAtom (40);
        XA_WM_SIZE_HINTS       = TAtom (41);
        XA_WM_ZOOM_HINTS       = TAtom (42);
        XA_MIN_SPACE           = TAtom (43);
        XA_NORM_SPACE          = TAtom (44);
        XA_MAX_SPACE           = TAtom (45);
        XA_END_SPACE           = TAtom (46);
        XA_SUPERSCRIPT_X       = TAtom (47);
        XA_SUPERSCRIPT_Y       = TAtom (48);
        XA_SUBSCRIPT_X         = TAtom (49);
        XA_SUBSCRIPT_Y         = TAtom (50);
        XA_UNDERLINE_POSITION  = TAtom (51);
        XA_UNDERLINE_THICKNESS = TAtom (52);
        XA_STRIKEOUT_ASCENT    = TAtom (53);
        XA_STRIKEOUT_DESCENT   = TAtom (54);
        XA_ITALIC_ANGLE        = TAtom (55);
        XA_X_HEIGHT            = TAtom (56);
        XA_QUAD_WIDTH          = TAtom (57);
        XA_WEIGHT              = TAtom (58);
        XA_POINT_SIZE          = TAtom (59);
        XA_RESOLUTION          = TAtom (60);
        XA_COPYRIGHT           = TAtom (61);
        XA_NOTICE              = TAtom (62);
        XA_FONT_NAME           = TAtom (63);
        XA_FAMILY_NAME         = TAtom (64);
        XA_FULL_NAME           = TAtom (65);
        XA_CAP_HEIGHT          = TAtom (66);
        XA_WM_CLASS            = TAtom (67);
        XA_WM_TRANSIENT_FOR    = TAtom (68);

        XA_LAST_PREDEFINED     = TAtom (68);

implementation

end.
