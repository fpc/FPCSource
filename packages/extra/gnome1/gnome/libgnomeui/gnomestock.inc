{$IfDef read_interface}
const
   GNOME_STOCK_PIXMAP_NEW = 'New';
   GNOME_STOCK_PIXMAP_OPEN = 'Open';
   GNOME_STOCK_PIXMAP_CLOSE = 'Close';
   GNOME_STOCK_PIXMAP_REVERT = 'Revert';
   GNOME_STOCK_PIXMAP_SAVE = 'Save';
   GNOME_STOCK_PIXMAP_SAVE_AS = 'Save As';
   GNOME_STOCK_PIXMAP_CUT = 'Cut';
   GNOME_STOCK_PIXMAP_COPY = 'Copy';
   GNOME_STOCK_PIXMAP_PASTE = 'Paste';
   GNOME_STOCK_PIXMAP_CLEAR = 'Clear';
   GNOME_STOCK_PIXMAP_PROPERTIES = 'Properties';
   GNOME_STOCK_PIXMAP_PREFERENCES = 'Preferences';
   GNOME_STOCK_PIXMAP_HELP = 'Help';
   GNOME_STOCK_PIXMAP_SCORES = 'Scores';
   GNOME_STOCK_PIXMAP_PRINT = 'Print';
   GNOME_STOCK_PIXMAP_SEARCH = 'Search';
   GNOME_STOCK_PIXMAP_SRCHRPL = 'Search/Replace';
   GNOME_STOCK_PIXMAP_BACK = 'Back';
   GNOME_STOCK_PIXMAP_FORWARD = 'Forward';
   GNOME_STOCK_PIXMAP_FIRST = 'First';
   GNOME_STOCK_PIXMAP_LAST = 'Last';
   GNOME_STOCK_PIXMAP_HOME = 'Home';
   GNOME_STOCK_PIXMAP_STOP = 'Stop';
   GNOME_STOCK_PIXMAP_REFRESH = 'Refresh';
   GNOME_STOCK_PIXMAP_UNDO = 'Undo';
   GNOME_STOCK_PIXMAP_REDO = 'Redo';
   GNOME_STOCK_PIXMAP_TIMER = 'Timer';
   GNOME_STOCK_PIXMAP_TIMER_STOP = 'Timer Stopped';
   GNOME_STOCK_PIXMAP_MAIL = 'Mail';
   GNOME_STOCK_PIXMAP_MAIL_RCV = 'Receive Mail';
   GNOME_STOCK_PIXMAP_MAIL_SND = 'Send Mail';
   GNOME_STOCK_PIXMAP_MAIL_RPL = 'Reply to Mail';
   GNOME_STOCK_PIXMAP_MAIL_FWD = 'Forward Mail';
   GNOME_STOCK_PIXMAP_MAIL_NEW = 'New Mail';
   GNOME_STOCK_PIXMAP_TRASH = 'Trash';
   GNOME_STOCK_PIXMAP_TRASH_FULL = 'Trash Full';
   GNOME_STOCK_PIXMAP_UNDELETE = 'Undelete';
   GNOME_STOCK_PIXMAP_SPELLCHECK = 'Spellchecker';
   GNOME_STOCK_PIXMAP_MIC = 'Microphone';
   GNOME_STOCK_PIXMAP_LINE_IN = 'Line In';
   GNOME_STOCK_PIXMAP_CDROM = 'Cdrom';
   GNOME_STOCK_PIXMAP_VOLUME = 'Volume';
   GNOME_STOCK_PIXMAP_MIDI = 'Midi';
   GNOME_STOCK_PIXMAP_BOOK_RED = 'Book Red';
   GNOME_STOCK_PIXMAP_BOOK_GREEN = 'Book Green';
   GNOME_STOCK_PIXMAP_BOOK_BLUE = 'Book Blue';
   GNOME_STOCK_PIXMAP_BOOK_YELLOW = 'Book Yellow';
   GNOME_STOCK_PIXMAP_BOOK_OPEN = 'Book Open';
   GNOME_STOCK_PIXMAP_ABOUT = 'About';
   GNOME_STOCK_PIXMAP_QUIT = 'Quit';
   GNOME_STOCK_PIXMAP_MULTIPLE = 'Multiple';
   GNOME_STOCK_PIXMAP_NOT = 'Not';
   GNOME_STOCK_PIXMAP_CONVERT = 'Convert';
   GNOME_STOCK_PIXMAP_JUMP_TO = 'Jump To';
   GNOME_STOCK_PIXMAP_UP = 'Up';
   GNOME_STOCK_PIXMAP_DOWN = 'Down';
   GNOME_STOCK_PIXMAP_TOP = 'Top';
   GNOME_STOCK_PIXMAP_BOTTOM = 'Bottom';
   GNOME_STOCK_PIXMAP_ATTACH = 'Attach';
   GNOME_STOCK_PIXMAP_INDEX = 'Index';
   GNOME_STOCK_PIXMAP_FONT = 'Font';
   GNOME_STOCK_PIXMAP_EXEC = 'Exec';
   GNOME_STOCK_PIXMAP_ALIGN_LEFT = 'Left';
   GNOME_STOCK_PIXMAP_ALIGN_RIGHT = 'Right';
   GNOME_STOCK_PIXMAP_ALIGN_CENTER = 'Center';
   GNOME_STOCK_PIXMAP_ALIGN_JUSTIFY = 'Justify';
   GNOME_STOCK_PIXMAP_TEXT_BOLD = 'Bold';
   GNOME_STOCK_PIXMAP_TEXT_ITALIC = 'Italic';
   GNOME_STOCK_PIXMAP_TEXT_UNDERLINE = 'Underline';
   GNOME_STOCK_PIXMAP_TEXT_STRIKEOUT = 'Strikeout';
   GNOME_STOCK_PIXMAP_TEXT_INDENT = 'Text Indent';
   GNOME_STOCK_PIXMAP_TEXT_UNINDENT = 'Text Unindent';
   GNOME_STOCK_PIXMAP_EXIT = GNOME_STOCK_PIXMAP_QUIT;
   GNOME_STOCK_PIXMAP_COLORSELECTOR = 'Color Select';
   GNOME_STOCK_PIXMAP_ADD = 'Add';
   GNOME_STOCK_PIXMAP_REMOVE = 'Remove';
   GNOME_STOCK_PIXMAP_TABLE_BORDERS = 'Table Borders';
   GNOME_STOCK_PIXMAP_TABLE_FILL = 'Table Fill';
   GNOME_STOCK_PIXMAP_TEXT_BULLETED_LIST = 'Text Bulleted List';
   GNOME_STOCK_PIXMAP_TEXT_NUMBERED_LIST = 'Text Numbered List';
   GNOME_STOCK_PIXMAP_REGULAR = 'regular';
   GNOME_STOCK_PIXMAP_DISABLED = 'disabled';
   GNOME_STOCK_PIXMAP_FOCUSED = 'focused';

type
   TGnomeStockPixmapType = (GNOME_STOCK_PIXMAP_TYPE_NONE,GNOME_STOCK_PIXMAP_TYPE_DATA,
     GNOME_STOCK_PIXMAP_TYPE_FILE,GNOME_STOCK_PIXMAP_TYPE_PATH,
     GNOME_STOCK_PIXMAP_TYPE_WIDGET,GNOME_STOCK_PIXMAP_TYPE_IMLIB,
     GNOME_STOCK_PIXMAP_TYPE_IMLIB_SCALED,
     GNOME_STOCK_PIXMAP_TYPE_GPIXMAP);

   PGnomeStockPixmapEntryData = ^TGnomeStockPixmapEntryData;
   TGnomeStockPixmapEntryData = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        xpm_data : PPgchar;
     end;

   PGnomeStockPixmapEntryImlib = ^TGnomeStockPixmapEntryImlib;
   TGnomeStockPixmapEntryImlib = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        rgb_data : Pgchar;
        shape : TGdkImlibColor;
     end;

   PGnomeStockPixmapEntryImlibScaled = ^TGnomeStockPixmapEntryImlibScaled;
   TGnomeStockPixmapEntryImlibScaled = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        rgb_data : Pgchar;
        shape : TGdkImlibColor;
        scaled_width : longint;
        scaled_height : longint;
     end;

   PGnomeStockPixmapEntryFile = ^TGnomeStockPixmapEntryFile;
   TGnomeStockPixmapEntryFile = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        filename : Pgchar;
     end;

   PGnomeStockPixmapEntryPath = ^TGnomeStockPixmapEntryPath;
   TGnomeStockPixmapEntryPath = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        pathname : Pgchar;
     end;

   PGnomeStockPixmapEntryWidget = ^TGnomeStockPixmapEntryWidget;
   TGnomeStockPixmapEntryWidget = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        widget : PGtkWidget;
     end;

   PGnomeStockPixmapEntryGPixmap = ^TGnomeStockPixmapEntryGPixmap;
   TGnomeStockPixmapEntryGPixmap = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
        pixmap : PGnomePixmap;
     end;

   PGnomeStockPixmapEntryAny = ^TGnomeStockPixmapEntryAny;
   TGnomeStockPixmapEntryAny = record
        thetype : TGnomeStockPixmapType;
        width : longint;
        height : longint;
        thelabel : Pchar;
     end;

   PGnomeStockPixmapEntry = ^TGnomeStockPixmapEntry;
   TGnomeStockPixmapEntry = record
       case longint of
          0 : ( thetype : TGnomeStockPixmapType );
          1 : ( any : TGnomeStockPixmapEntryAny );
          2 : ( data : TGnomeStockPixmapEntryData );
          3 : ( thefile : TGnomeStockPixmapEntryFile );
          4 : ( path : TGnomeStockPixmapEntryPath );
          5 : ( widget : TGnomeStockPixmapEntryWidget );
          6 : ( imlib : TGnomeStockPixmapEntryImlib );
          7 : ( imlib_s : TGnomeStockPixmapEntryImlibScaled );
          8 : ( gpixmap : TGnomeStockPixmapEntryGPixmap );
       end;

   PGnomeStock = ^TGnomeStock;
   TGnomeStock = record
        pixmap : TGnomePixmap;
        regular : PGnomePixmap;
        disabled : PGnomePixmap;
        focused : PGnomePixmap;
        current : PGnomePixmap;
        icon : Pchar;
        c_regular : guint32;
        c_disabled : guint32;
        c_focused : guint32;
     end;
   GNOME_STOCK = TGnomeStock;

   PGnomeStockClass = ^TGnomeStockClass;
   TGnomeStockClass = record
      pixmap_class : TGnomePixmapClass;
    end;
   GNOME_STOCK_CLASS = TGnomeStockClass;

function GNOME_TYPE_STOCK : TGtkType;

function GNOME_IS_STOCK(obj : pointer) : gboolean;
function GNOME_IS_STOCK_CLASS(klass : pointer) : gboolean;
function GNOME_IS_STOCK_PIXMAP_WIDGET(obj : pointer) : gboolean;

function gnome_stock_pixmap_widget_new(window:PGtkWidget; icon:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_pixmap_widget_new';
function gnome_stock_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_stock_get_type';
function gnome_stock_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_new';
function gnome_stock_new_with_icon(icon:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_new_with_icon';
function gnome_stock_set_icon(stock:PGnomeStock; icon:Pchar):gboolean;cdecl;external libgnomeuidll name 'gnome_stock_set_icon';
function gnome_stock_pixmap(window:PGtkWidget; icon:Pchar; subtype:Pchar):PGnomePixmap;cdecl;external libgnomeuidll name 'gnome_stock_pixmap';
function gnome_stock_pixmap_widget(window:PGtkWidget; icon:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_pixmap_widget';
function gnome_stock_pixmap_widget_at_size(window:PGtkWidget; icon:Pchar; width:guint; height:guint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_pixmap_widget_at_size';
procedure gnome_stock_pixmap_widget_set_icon(widget:PGnomeStock; icon:Pchar);cdecl;external libgnomeuidll name 'gnome_stock_pixmap_widget_set_icon';
function gnome_stock_pixmap_register(icon:Pchar; subtype:Pchar; entry:PGnomeStockPixmapEntry):gint;cdecl;external libgnomeuidll name 'gnome_stock_pixmap_register';
function gnome_stock_pixmap_change(icon:Pchar; subtype:Pchar; entry:PGnomeStockPixmapEntry):gint;cdecl;external libgnomeuidll name 'gnome_stock_pixmap_change';
function gnome_stock_pixmap_checkfor(icon:Pchar; subtype:Pchar):PGnomeStockPixmapEntry;cdecl;external libgnomeuidll name 'gnome_stock_pixmap_checkfor';
function gnome_pixmap_button(pixmap:PGtkWidget; thetext:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_button';
procedure gnome_button_can_default(button:PGtkButton; can_default:gboolean);cdecl;external libgnomeuidll name 'gnome_button_can_default';

const
   GNOME_STOCK_BUTTON_OK = 'Button_Ok';
   GNOME_STOCK_BUTTON_CANCEL = 'Button_Cancel';
   GNOME_STOCK_BUTTON_YES = 'Button_Yes';
   GNOME_STOCK_BUTTON_NO = 'Button_No';
   GNOME_STOCK_BUTTON_CLOSE = 'Button_Close';
   GNOME_STOCK_BUTTON_APPLY = 'Button_Apply';
   GNOME_STOCK_BUTTON_HELP = 'Button_Help';
   GNOME_STOCK_BUTTON_NEXT = 'Button_Next';
   GNOME_STOCK_BUTTON_PREV = 'Button_Prev';
   GNOME_STOCK_BUTTON_UP = 'Button_Up';
   GNOME_STOCK_BUTTON_DOWN = 'Button_Down';
   GNOME_STOCK_BUTTON_FONT = 'Button_Font';

function gnome_stock_button(thetype:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_button';
function gnome_stock_or_ordinary_button(thetype:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_or_ordinary_button';

const
   GNOME_STOCK_MENU_BLANK = 'Menu_';
   GNOME_STOCK_MENU_NEW = 'Menu_New';
   GNOME_STOCK_MENU_SAVE = 'Menu_Save';
   GNOME_STOCK_MENU_SAVE_AS = 'Menu_Save As';
   GNOME_STOCK_MENU_REVERT = 'Menu_Revert';
   GNOME_STOCK_MENU_OPEN = 'Menu_Open';
   GNOME_STOCK_MENU_CLOSE = 'Menu_Close';
   GNOME_STOCK_MENU_QUIT = 'Menu_Quit';
   GNOME_STOCK_MENU_CUT = 'Menu_Cut';
   GNOME_STOCK_MENU_COPY = 'Menu_Copy';
   GNOME_STOCK_MENU_PASTE = 'Menu_Paste';
   GNOME_STOCK_MENU_PROP = 'Menu_Properties';
   GNOME_STOCK_MENU_PREF = 'Menu_Preferences';
   GNOME_STOCK_MENU_ABOUT = 'Menu_About';
   GNOME_STOCK_MENU_SCORES = 'Menu_Scores';
   GNOME_STOCK_MENU_UNDO = 'Menu_Undo';
   GNOME_STOCK_MENU_REDO = 'Menu_Redo';
   GNOME_STOCK_MENU_PRINT = 'Menu_Print';
   GNOME_STOCK_MENU_SEARCH = 'Menu_Search';
   GNOME_STOCK_MENU_SRCHRPL = 'Menu_Search/Replace';
   GNOME_STOCK_MENU_BACK = 'Menu_Back';
   GNOME_STOCK_MENU_FORWARD = 'Menu_Forward';
   GNOME_STOCK_MENU_FIRST = 'Menu_First';
   GNOME_STOCK_MENU_LAST = 'Menu_Last';
   GNOME_STOCK_MENU_HOME = 'Menu_Home';
   GNOME_STOCK_MENU_STOP = 'Menu_Stop';
   GNOME_STOCK_MENU_REFRESH = 'Menu_Refresh';
   GNOME_STOCK_MENU_MAIL = 'Menu_Mail';
   GNOME_STOCK_MENU_MAIL_RCV = 'Menu_Receive Mail';
   GNOME_STOCK_MENU_MAIL_SND = 'Menu_Send Mail';
   GNOME_STOCK_MENU_MAIL_RPL = 'Menu_Reply to Mail';
   GNOME_STOCK_MENU_MAIL_FWD = 'Menu_Forward Mail';
   GNOME_STOCK_MENU_MAIL_NEW = 'Menu_New Mail';
   GNOME_STOCK_MENU_TRASH = 'Menu_Trash';
   GNOME_STOCK_MENU_TRASH_FULL = 'Menu_Trash Full';
   GNOME_STOCK_MENU_UNDELETE = 'Menu_Undelete';
   GNOME_STOCK_MENU_TIMER = 'Menu_Timer';
   GNOME_STOCK_MENU_TIMER_STOP = 'Menu_Timer Stopped';
   GNOME_STOCK_MENU_SPELLCHECK = 'Menu_Spellchecker';
   GNOME_STOCK_MENU_MIC = 'Menu_Microphone';
   GNOME_STOCK_MENU_LINE_IN = 'Menu_Line In';
   GNOME_STOCK_MENU_CDROM = 'Menu_Cdrom';
   GNOME_STOCK_MENU_VOLUME = 'Menu_Volume';
   GNOME_STOCK_MENU_MIDI = 'Menu_Midi';
   GNOME_STOCK_MENU_BOOK_RED = 'Menu_Book Red';
   GNOME_STOCK_MENU_BOOK_GREEN = 'Menu_Book Green';
   GNOME_STOCK_MENU_BOOK_BLUE = 'Menu_Book Blue';
   GNOME_STOCK_MENU_BOOK_YELLOW = 'Menu_Book Yellow';
   GNOME_STOCK_MENU_BOOK_OPEN = 'Menu_Book Open';
   GNOME_STOCK_MENU_CONVERT = 'Menu_Convert';
   GNOME_STOCK_MENU_JUMP_TO = 'Menu_Jump To';
   GNOME_STOCK_MENU_UP = 'Menu_Up';
   GNOME_STOCK_MENU_DOWN = 'Menu_Down';
   GNOME_STOCK_MENU_TOP = 'Menu_Top';
   GNOME_STOCK_MENU_BOTTOM = 'Menu_Bottom';
   GNOME_STOCK_MENU_ATTACH = 'Menu_Attach';
   GNOME_STOCK_MENU_INDEX = 'Menu_Index';
   GNOME_STOCK_MENU_FONT = 'Menu_Font';
   GNOME_STOCK_MENU_EXEC = 'Menu_Exec';
   GNOME_STOCK_MENU_ALIGN_LEFT = 'Menu_Left';
   GNOME_STOCK_MENU_ALIGN_RIGHT = 'Menu_Right';
   GNOME_STOCK_MENU_ALIGN_CENTER = 'Menu_Center';
   GNOME_STOCK_MENU_ALIGN_JUSTIFY = 'Menu_Justify';
   GNOME_STOCK_MENU_TEXT_BOLD = 'Menu_Bold';
   GNOME_STOCK_MENU_TEXT_ITALIC = 'Menu_Italic';
   GNOME_STOCK_MENU_TEXT_UNDERLINE = 'Menu_Underline';
   GNOME_STOCK_MENU_TEXT_STRIKEOUT = 'Menu_Strikeout';
   GNOME_STOCK_MENU_EXIT = GNOME_STOCK_MENU_QUIT;

function gnome_stock_menu_item(thetype:Pchar; thetext:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_menu_item';
function gnome_stock_menu_accel(thetype:Pchar; key:Pguchar; mods:Pguint8):gboolean;cdecl;external libgnomeuidll name 'gnome_stock_menu_accel';
procedure gnome_stock_menu_accel_parse(section:Pchar);cdecl;external libgnomeuidll name 'gnome_stock_menu_accel_parse';
function gnome_stock_transparent_window(icon:Pchar; subtype:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_stock_transparent_window';
procedure gnome_stock_pixmap_gdk(icon:Pchar; subtype:Pchar; pixmap:PPGdkPixmap; mask:PPGdkPixmap);cdecl;external libgnomeuidll name 'gnome_stock_pixmap_gdk';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_STOCK : TGTKType;
begin
  GNOME_TYPE_STOCK:=gnome_stock_get_type;
end;

function GNOME_IS_STOCK(obj : Pointer) : gBoolean;
begin
   GNOME_IS_STOCK:=(obj<>nil) and GNOME_IS_STOCK_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_STOCK_CLASS(klass : Pointer) : gBoolean;
begin
   GNOME_IS_STOCK_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_STOCK);
end;

function GNOME_IS_STOCK_PIXMAP_WIDGET(obj : pointer) : gboolean;
begin
  GNOME_IS_STOCK_PIXMAP_WIDGET := GNOME_IS_STOCK(obj);
end;

{$Endif read_implementation}
