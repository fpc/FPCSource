{ Converted by H2Pas from richedit.h }
unit richedit;

{$mode objfpc}
{$calling stdcall}

{$ifdef FPC_OS_UNICODE}
  {$define UNICODE}
{$endif}

interface

uses Messages, Windows;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Const
  
  RICHEDIT_CLASSA = 'RichEdit20A';
  RICHEDIT_CLASSW = 'RichEdit20W';
  
{$ifdef UNICODE }
  const
     RICHEDIT_CLASS = RICHEDIT_CLASSW;
{$else}
  const
     RICHEDIT_CLASS = RICHEDIT_CLASSA;
{$endif}
  
              

  const

    WM_CONTEXTMENU     = $007B;
    WM_UNICHAR         = $0109;
    WM_PRINTCLIENT     = $0318;

//#ifndef EM_GETLIMITTEXT
    EM_GETLIMITTEXT    = (WM_USER + 37);
//#endif

//#ifndef EM_POSFROMCHAR
    EM_POSFROMCHAR     = (WM_USER + 38);
    EM_CHARFROMPOS     = (WM_USER + 39);
//#endif

//#ifndef EM_SCROLLCARET
    EM_SCROLLCARET     = (WM_USER + 49);
//#endif
    EM_CANPASTE        = (WM_USER + 50);
    EM_DISPLAYBAND     = (WM_USER + 51);
    EM_EXGETSEL        = (WM_USER + 52);
    EM_EXLIMITTEXT     = (WM_USER + 53);
    EM_EXLINEFROMCHAR  = (WM_USER + 54);
    EM_EXSETSEL        = (WM_USER + 55);
    EM_FINDTEXT        = (WM_USER + 56);
    EM_FORMATRANGE     = (WM_USER + 57);
    EM_GETCHARFORMAT   = (WM_USER + 58);
    EM_GETEVENTMASK    = (WM_USER + 59);
    EM_GETOLEINTERFACE = (WM_USER + 60);
    EM_GETPARAFORMAT   = (WM_USER + 61);
    EM_GETSELTEXT      = (WM_USER + 62);
    EM_HIDESELECTION   = (WM_USER + 63);
    EM_PASTESPECIAL    = (WM_USER + 64);
    EM_REQUESTRESIZE   = (WM_USER + 65);
    EM_SELECTIONTYPE   = (WM_USER + 66);
    EM_SETBKGNDCOLOR   = (WM_USER + 67);
    EM_SETCHARFORMAT   = (WM_USER + 68);
    EM_SETEVENTMASK    = (WM_USER + 69);
    EM_SETOLECALLBACK  = (WM_USER + 70);
    EM_SETPARAFORMAT   = (WM_USER + 71);
    EM_SETTARGETDEVICE = (WM_USER + 72);
    EM_STREAMIN        = (WM_USER + 73);
    EM_STREAMOUT       = (WM_USER + 74);
    EM_GETTEXTRANGE    = (WM_USER + 75);
    EM_FINDWORDBREAK   = (WM_USER + 76);
    EM_SETOPTIONS      = (WM_USER + 77);
    EM_GETOPTIONS      = (WM_USER + 78);
    EM_FINDTEXTEX      = (WM_USER + 79);
//#ifdef _WIN32
    EM_GETWORDBREAKPROCEX  = (WM_USER + 80);
    EM_SETWORDBREAKPROCEX  = (WM_USER + 81);
//#endif

// RichEdit 2.0 messages
    EM_SETUNDOLIMIT    = (WM_USER + 82);
    EM_REDO            = (WM_USER + 84);
    EM_CANREDO         = (WM_USER + 85);
    EM_GETUNDONAME     = (WM_USER + 86);
    EM_GETREDONAME     = (WM_USER + 87);
    EM_STOPGROUPTYPING = (WM_USER + 88);

    EM_SETTEXTMODE     = (WM_USER + 89);
    EM_GETTEXTMODE     = (WM_USER + 90);

    EM_AUTOURLDETECT   = (WM_USER + 91);
    EM_GETAUTOURLDETECT= (WM_USER + 92);
    EM_SETPALETTE      = (WM_USER + 93);
    EM_GETTEXTEX       = (WM_USER + 94);
    EM_GETTEXTLENGTHEX = (WM_USER + 95);
    EM_SHOWSCROLLBAR   = (WM_USER + 96);
    EM_SETTEXTEX       = (WM_USER + 97);

// East Asia specific messages
    EM_SETPUNCTUATION  = (WM_USER + 100);
    EM_GETPUNCTUATION  = (WM_USER + 101);
    EM_SETWORDWRAPMODE = (WM_USER + 102);
    EM_GETWORDWRAPMODE = (WM_USER + 103);
    EM_SETIMECOLOR     = (WM_USER + 104);
    EM_GETIMECOLOR     = (WM_USER + 105);
    EM_SETIMEOPTIONS   = (WM_USER + 106);
    EM_GETIMEOPTIONS   = (WM_USER + 107);
    EM_CONVPOSITION    = (WM_USER + 108);

    EM_SETLANGOPTIONS  = (WM_USER + 120);
    EM_GETLANGOPTIONS  = (WM_USER + 121);
    EM_GETIMECOMPMODE  = (WM_USER + 122);

    EM_FINDTEXTW       = (WM_USER + 123);
    EM_FINDTEXTEXW     = (WM_USER + 124);

// RE3.0 FE messages
    EM_RECONVERSION    = (WM_USER + 125);
    EM_SETIMEMODEBIAS  = (WM_USER + 126);
    EM_GETIMEMODEBIAS  = (WM_USER + 127);

// BiDi specific messages
    EM_SETBIDIOPTIONS  = (WM_USER + 200);
    EM_GETBIDIOPTIONS  = (WM_USER + 201);

    EM_SETTYPOGRAPHYOPTIONS = (WM_USER + 202);
    EM_GETTYPOGRAPHYOPTIONS = (WM_USER + 203);

// Extended edit style specific messages
    EM_SETEDITSTYLE    = (WM_USER + 204);
    EM_GETEDITSTYLE    = (WM_USER + 205);

// Pegasus outline mode messages (RE 3.0)

// Outline mode message
    EM_OUTLINE         = (WM_USER + 220);
// Message for getting and restoring scroll pos
    EM_GETSCROLLPOS    = (WM_USER + 221);
    EM_SETSCROLLPOS    = (WM_USER + 222);
// Change fontsize in current selection by wParam
    EM_SETFONTSIZE     = (WM_USER + 223);
    EM_GETZOOM         = (WM_USER + 224);
    EM_SETZOOM         = (WM_USER + 225);
    EM_GETVIEWKIND     = (WM_USER + 226);
    EM_SETVIEWKIND     = (WM_USER + 227);

// RichEdit 4.0 messages
    EM_GETPAGE         = (WM_USER + 228);
    EM_SETPAGE         = (WM_USER + 229);
    EM_GETHYPHENATEINFO= (WM_USER + 230);
    EM_SETHYPHENATEINFO= (WM_USER + 231);
    EM_INSERTTABLE     = (WM_USER + 232);
    EM_GETAUTOCORRECTPROC   = (WM_USER + 233);
    EM_SETAUTOCORRECTPROC   = (WM_USER + 234);
    EM_CALLAUTOCORRECTPROC  = (WM_USER + 255);

    EM_GETPAGEROTATE   = (WM_USER + 235);
    EM_SETPAGEROTATE   = (WM_USER + 236);
    EM_GETCTFMODEBIAS  = (WM_USER + 237);
    EM_SETCTFMODEBIAS  = (WM_USER + 238);
    EM_GETCTFOPENSTATUS= (WM_USER + 240);
    EM_SETCTFOPENSTATUS= (WM_USER + 241);
    EM_GETIMECOMPTEXT  = (WM_USER + 242);
    EM_ISIME           = (WM_USER + 243);
    EM_GETIMEPROPERTY  = (WM_USER + 244);
    EM_GETTABLEPARMS   = (WM_USER + 265);
    // These messages control what rich edit does when it comes accross
    // OLE objects during RTF stream in.  Normally rich edit queries the client
    // application only after OleLoad has been called.  With these messages it is possible to
    // set the rich edit control to a mode where it will query the client application before
    // OleLoad is called
    EM_GETQUERYRTFOBJ  = (WM_USER + 269);
    EM_SETQUERYRTFOBJ  = (WM_USER + 270);
    EM_SETEDITSTYLEEX  = (WM_USER + 275);
    EM_GETEDITSTYLEEX  = (WM_USER + 276);

    AURL_ENABLEURL          = 1;
    AURL_ENABLEEMAILADDR    = 2;
    AURL_ENABLETELNO        = 4;
    AURL_ENABLEEAURLS       = 8;
    AURL_ENABLEDRIVELETTERS = 16;
    AURL_DISABLEMIXEDLGC    = 32; // Disable mixed Latin Greek Cyrillic IDNs

// CFM_COLOR mirrors CFE_AUTOCOLOR, a little hack to easily deal with autocolor

// CHARFORMAT masks
    CFM_BOLD           = $00000001;
    CFM_ITALIC         = $00000002;
    CFM_UNDERLINE      = $00000004;
    CFM_STRIKEOUT      = $00000008;
    CFM_PROTECTED      = $00000010;
    CFM_LINK           = $00000020;      // Exchange hyperlink extension
    CFM_SIZE           = $80000000;
    CFM_COLOR          = $40000000;
    CFM_FACE           = $20000000;
    CFM_OFFSET         = $10000000;
    CFM_CHARSET        = $08000000;

// CHARFORMAT effects
    CFE_BOLD           = $00000001;
    CFE_ITALIC         = $00000002;
    CFE_UNDERLINE      = $00000004;
    CFE_STRIKEOUT      = $00000008;
    CFE_PROTECTED      = $00000010;
    CFE_LINK           = $00000020;
    CFE_AUTOCOLOR      = $40000000;           // NOTE: this corresponds to CFM_COLOR, which controls it
    
    
// Masks and effects defined for CHARFORMAT2 -- an (*) indicates
// that the data is stored by RichEdit 2.0/3.0, but not displayed
    CFM_SMALLCAPS      = $00000040;            // (*)
    CFM_ALLCAPS        = $00000080;            // Displayed by 3.0
    CFM_HIDDEN         = $00000100;            // Hidden by 3.0
    CFM_OUTLINE        = $00000200;            // (*)
    CFM_SHADOW         = $00000400;            // (*)
    CFM_EMBOSS         = $00000800;            // (*)
    CFM_IMPRINT        = $00001000;            // (*)
    CFM_DISABLED       = $00002000;
    CFM_REVISED        = $00004000;
    
    
    
    CFM_REVAUTHOR      = $00008000;
    CFE_SUBSCRIPT      = $00010000;            // Superscript and subscript are
    CFE_SUPERSCRIPT    = $00020000;            //      mutually exclusive
    CFM_ANIMATION      = $00040000;            // (*)
    CFM_STYLE          = $00080000;            // (*)
    CFM_KERNING        = $00100000;
    CFM_SPACING        = $00200000;            // Displayed by 3.0
    CFM_WEIGHT         = $00400000;
    CFM_UNDERLINETYPE  = $00800000;            // Many displayed by 3.0
//#if (_RICHEDIT_VER >=  = $0600)
    CFM_COOKIE         = $01000000;            // RE 6.0
//#endif
    CFM_LCID           = $02000000;
    CFM_BACKCOLOR      = $04000000;            // Higher mask bits defined above

    CFM_SUBSCRIPT      = (CFE_SUBSCRIPT or CFE_SUPERSCRIPT);
    CFM_SUPERSCRIPT    = CFM_SUBSCRIPT;
    
    
    CFE_ALLCAPS        = CFM_ALLCAPS;
    CFE_AUTOBACKCOLOR  = CFM_BACKCOLOR; 
    CFE_DISABLED       = CFM_DISABLED; 
    CFE_EMBOSS         = CFM_EMBOSS; 
    CFE_HIDDEN         = CFM_HIDDEN; 
    CFE_IMPRINT        = CFM_IMPRINT; 
    CFE_OUTLINE        = CFM_OUTLINE; 
    CFE_SHADOW         = CFM_SHADOW; 
    CFE_SMALLCAPS      = CFM_SMALLCAPS; 
  
   CFM_EFFECTS = 
     CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_COLOR or 
     CFM_STRIKEOUT or CFE_PROTECTED or CFM_LINK; 
     
   CFM_ALL = 
     CFM_EFFECTS or CFM_SIZE or CFM_FACE or CFM_OFFSET or CFM_CHARSET; 

   CFM_EFFECTS2 = 
     CFM_EFFECTS or CFM_DISABLED or CFM_SMALLCAPS or CFM_ALLCAPS or 
     CFM_HIDDEN  or CFM_OUTLINE or CFM_SHADOW or CFM_EMBOSS or 
     CFM_IMPRINT or CFM_DISABLED or CFM_REVISED or 
     CFM_SUBSCRIPT or CFM_SUPERSCRIPT or CFM_BACKCOLOR; 
 
   CFM_ALL2 = 
     CFM_ALL or CFM_EFFECTS2 or CFM_BACKCOLOR or CFM_LCID or 
     CFM_UNDERLINETYPE or CFM_WEIGHT or CFM_REVAUTHOR or 
     CFM_SPACING or CFM_KERNING or CFM_STYLE or CFM_ANIMATION; 
      
   CFU_CF1UNDERLINE            = $000000FF; 
   CFU_INVERT                  = $000000FE; 
   CFU_UNDERLINEDOTTED         = $00000004; 
   CFU_UNDERLINEDOUBLE         = $00000003; 
   CFU_UNDERLINEWORD           = $00000002; 
   CFU_UNDERLINE               = $00000001; 
   CFU_UNDERLINENONE           = $00000000; 
  
   GCM_RIGHTMOUSEDROP      = $8000; 
   
   
// Extended edit style masks
    SES_EMULATESYSEDIT  = 1;
    SES_BEEPONMAXTEXT   = 2;
    SES_EXTENDBACKCOLOR = 4;
    SES_MAPCPS          = 8;               // Obsolete (never used)
//#if (_RICHEDIT_VER >= = $0500)
    SES_HYPERLINKTOOLTIPS = 8;
//#endif
    SES_EMULATE10         = 16;              // Obsolete (never used)
//#if (_RICHEDIT_VER >= = $0700)
    SES_DEFAULTLATINLIGA  = 16;
//#endif
    SES_USECRLF           = 32;              // Obsolete (never used)
//#if (_RICHEDIT_VER >= = $0700)
    SES_NOFOCUSLINKNOTIFY = 32;
//#endif
    SES_USEAIMM           = 64;
    SES_NOIME             = 128;
    SES_NOXLTSYMBOLRANGE  = 32;
  
    SES_ALLOWBEEPS        = 256;
    SES_UPPERCASE         = 512;
    SES_LOWERCASE         = 1024;
    SES_NOINPUTSEQUENCECHK= 2048;
    SES_BIDI              = 4096;
    SES_SCROLLONKILLFOCUS = 8192;
    SES_XLTCRCRLFTOCR     = 16384;
    SES_DRAFTMODE         = 32768;

    SES_USECTF            = $00010000;
    SES_HIDEGRIDLINES     = $00020000;
    SES_USEATFONT         = $00040000;
    SES_CUSTOMLOOK        = $00080000;
    SES_LBSCROLLNOTIFY    = $00100000;
    SES_CTFALLOWEMBED     = $00200000;
    SES_CTFALLOWSMARTTAG  = $00400000;
    SES_CTFALLOWPROOFING  = $00800000;
//#if (_RICHEDIT_VER >= = $0500)
    SES_LOGICALCARET      = $01000000;
    SES_WORDDRAGDROP      = $02000000;
    SES_SMARTDRAGDROP     = $04000000;
    SES_MULTISELECT       = $08000000;
    SES_CTFNOLOCK         = $10000000;
    SES_NOEALINEHEIGHTADJUST = $20000000;
    SES_MAX               = $20000000;
//#endif

// Options for EM_SETLANGOPTIONS and EM_GETLANGOPTIONS
    IMF_AUTOKEYBOARD      = $0001;
    IMF_AUTOFONT          = $0002;
    IMF_IMECANCELCOMPLETE = $0004; // High completes comp string when aborting, low cancels
    IMF_IMEALWAYSSENDNOTIFY= $0008;
    IMF_AUTOFONTSIZEADJUST= $0010;
    IMF_UIFONTS           = $0020;
    IMF_NOIMPLICITLANG    = $0040;
    IMF_DUALFONT          = $0080;
    IMF_NOKBDLIDFIXUP     = $0200;
    IMF_NORTFFONTSUBSTITUTE = $0400;
    IMF_SPELLCHECKING     = $0800;
    IMF_TKBPREDICTION     = $1000;
    IMF_IMEUIINTEGRATION  = $2000;
    
    
  // Values for EM_GETIMECOMPMODE
    ICM_NOTOPEN           = $0000;
    ICM_LEVEL3            = $0001;
    ICM_LEVEL2            = $0002;
    ICM_LEVEL2_5          = $0003;
    ICM_LEVEL2_SUI        = $0004;
    ICM_CTF               = $0005;

  // Options for EM_SETTYPOGRAPHYOPTIONS
    TO_ADVANCEDTYPOGRAPHY   = $0001;
    TO_SIMPLELINEBREAK      = $0002;
    TO_DISABLECUSTOMTEXTOUT = $0004;
    TO_ADVANCEDLAYOUT       = $0008;

    // EM_SETPAGEROTATE wparam values
    EPR_0         = 0;               // Text flows left to right and top to bottom
    EPR_270       = 1;               // Text flows top to bottom and right to left
    EPR_180       = 2;               // Text flows right to left and bottom to top
    EPR_90        = 3;               // Text flows bottom to top and left to right
    //#if (_RICHEDIT_VER >= 0x0800)
    EPR_SE        = 5;               // Text flows top to bottom and left to right (Mongolian text layout)
    //#endif

    // EM_SETCTFMODEBIAS wparam values
     CTFMODEBIAS_DEFAULT               = $000;
     CTFMODEBIAS_FILENAME              = $001;
     CTFMODEBIAS_NAME                  = $002;
     CTFMODEBIAS_READING               = $003;
     CTFMODEBIAS_DATETIME              = $004;
     CTFMODEBIAS_CONVERSATION          = $005;
     CTFMODEBIAS_NUMERIC               = $006;
     CTFMODEBIAS_HIRAGANA              = $007;
     CTFMODEBIAS_KATAKANA              = $008;
     CTFMODEBIAS_HANGUL                = $009;
     CTFMODEBIAS_HALFWIDTHKATAKANA     = $00A;
     CTFMODEBIAS_FULLWIDTHALPHANUMERIC = $00B;
     CTFMODEBIAS_HALFWIDTHALPHANUMERIC = $00C;

    // EM_SETIMEMODEBIAS lparam values
     IMF_SMODE_PLAURALCLAUSE           = $001;
     IMF_SMODE_NONE                    = $002;

     ATP_NOCHANG           = 0;
     ATP_CHANGE            = 1;
     ATP_NODELIMITER       = 2;
     ATP_REPLACEALLTEXT    = 4;
     
     OLEOP_DOVERB        = 1; 

     RICHEDIT_CLASS10A = 'RICHEDIT';
     CF_RTF = 'Rich Text Format';
     CF_RTFNOOBJS = 'Rich Text Format Without Objects';
     CF_RETEXTOBJ = 'RichEdit Text and Objects';

     IMF_FORCENONE = 1;
     IMF_FORCEENABLE = 2;
     IMF_FORCEDISABLE = 4;
     IMF_CLOSESTATUSWINDOW = 8;
     IMF_VERTICAL = 32;
     IMF_FORCEACTIVE = 64;
     IMF_FORCEINACTIVE = 128;
     IMF_FORCEREMEMBER = 256;
     IMF_MULTIPLEEDIT  = $0400; 
     
     SEL_EMPTY = 0;
     SEL_TEXT = 1;
     SEL_OBJECT = 2;
     SEL_MULTICHAR = 4;
     SEL_MULTIOBJECT = 8;
     MAX_TAB_STOPS = 32;
     PFM_ALIGNMENT = 8;
     PFM_NUMBERING = 32;
     PFM_OFFSET = 4;
     PFM_OFFSETINDENT = $80000000;
     PFM_RIGHTINDENT = 2;
     PFM_STARTINDENT = 1;
     PFM_TABSTOPS = 16;
     PFM_BORDER = 2048;
     PFM_LINESPACING = 256;
     PFM_NUMBERINGSTART = 32768;
     PFM_NUMBERINGSTYLE = 8192;
     PFM_NUMBERINGTAB = 16384;
     PFM_SHADING = 4096;
     PFM_SPACEAFTER = 128;
     PFM_SPACEBEFORE = 64;
     PFM_STYLE = 1024;
     PFM_DONOTHYPHEN = 4194304;
     PFM_KEEP = 131072;
     PFM_KEEPNEXT = 262144;
     PFM_NOLINENUMBER = 1048576;
     PFM_NOWIDOWCONTROL = 2097152;
     PFM_PAGEBREAKBEFORE = 524288;
     PFM_RTLPARA = 65536;
     PFM_SIDEBYSIDE = 8388608;
     PFM_TABLE = 1073741824;
     PFM_TEXTWRAPPINGBREAK = $20000000;
     PFM_TABLEROWDELIMITER = $10000000;
     PFM_COLLAPSED = $01000000;
     PFM_OUTLINELEVEL = $02000000;
     PFM_BOX = $04000000;
  
     PFM_ALL = 
       PFM_STARTINDENT or PFM_RIGHTINDENT or PFM_OFFSET or PFM_ALIGNMENT 
       or PFM_TABSTOPS or PFM_NUMBERING or PFM_OFFSETINDENT or PFM_RTLPARA;

     PFM_EFFECTS = 
       PFM_RTLPARA or PFM_KEEP or PFM_KEEPNEXT or PFM_TABLE or PFM_PAGEBREAKBEFORE 
       or PFM_NOLINENUMBER or PFM_NOWIDOWCONTROL or PFM_DONOTHYPHEN or PFM_SIDEBYSIDE 
       or PFM_TABLE or PFM_TABLEROWDELIMITER;

     PFM_ALL2 = 
       PFM_ALL or PFM_EFFECTS or PFM_SPACEBEFORE or PFM_SPACEAFTER or 
       PFM_LINESPACING or PFM_STYLE or PFM_SHADING or PFM_BORDER or 
       PFM_NUMBERINGTAB or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE;

       
     
     PFN_BULLET = 1;
     
     PFE_DONOTHYPHEN = 64;
     PFE_KEEP = 2;
     PFE_KEEPNEXT = 4;
     PFE_NOLINENUMBER = 16;
     PFE_NOWIDOWCONTROL = 32;
     PFE_PAGEBREAKBEFORE = 8;
     PFE_RTLPARA = 1;
     PFE_SIDEBYSIDE = 128;
     PFE_TABLE = 16384;
     PFA_LEFT = 1;
     PFA_RIGHT = 2;
     PFA_CENTER = 3;
     PFA_JUSTIFY = 4;
     PFA_FULL_INTERWORD = 4;
     SF_TEXT = 1;
     SF_RTF = 2;
     SF_RTFNOOBJS = 3;
     SF_TEXTIZED = 4;
     SF_UNICODE = 16;
     SF_USECODEPAGE = 32;
     SF_NCRFORNONASCII = 64;
     SF_RTFVAL = $0700;
     SFF_PWD = $0800;
     SFF_KEEPDOCINFO = $1000;
     SFF_PERSISTVIEWSCALE = $2000;
     SFF_PLAINRTF = $4000;
     SFF_SELECTION = $8000;
     WB_CLASSIFY = 3;
     WB_MOVEWORDLEFT = 4;
     WB_MOVEWORDRIGHT = 5;
     WB_LEFTBREAK = 6;
     WB_RIGHTBREAK = 7;
     WB_MOVEWORDPREV = 4;
     WB_MOVEWORDNEXT = 5;
     WB_PREVBREAK = 6;
     WB_NEXTBREAK = 7;
     WBF_WORDWRAP = 16;
     WBF_WORDBREAK = 32;
     WBF_OVERFLOW = 64;
     WBF_LEVEL1 = 128;
     WBF_LEVEL2 = 256;
     WBF_CUSTOM = 512;
     ES_DISABLENOSCROLL = 8192;
     ES_EX_NOCALLOLEINIT = 16777216;
     ES_NOIME = 524288;
     ES_NOOLEDRAGDROP = 8;
     ES_SAVESEL = 32768;
     ES_SELECTIONBAR = 16777216;
     ES_SELFIME = 262144;
     ES_SUNKEN = 16384;
     ES_VERTICAL = 4194304;
     EN_CORRECTTEXT = 1797;
     EN_DROPFILES = 1795;
     EN_IMECHANGE = 1799;
     EN_LINK = 1803;
     EN_MSGFILTER = 1792;
     EN_OLEOPFAILED = 1801;
     EN_OBJECTPOSITIONS = $070a; 
     EN_DRAGDROPDONE = $070c; 
     EN_PROTECTED = 1796;
     EN_REQUESTRESIZE = 1793;
     EN_SAVECLIPBOARD = 1800;
     EN_SELCHANGE = 1794;
     EN_STOPNOUNDO = 1798;
     ENM_NONE = 0;
     ENM_CHANGE = 1;
     ENM_CORRECTTEXT = 4194304;
     ENM_DRAGDROPDONE = 16;
     ENM_DROPFILES = 1048576;
     ENM_IMECHANGE = 8388608;
     ENM_KEYEVENTS = 65536;
     ENM_LANGCHANGE = 16777216;
     ENM_LINK = 67108864;
     ENM_MOUSEEVENTS = 131072;
     ENM_OBJECTPOSITIONS = 33554432;
     ENM_PROTECTED = 2097152;
     ENM_REQUESTRESIZE = 262144;
     ENM_SCROLL = 4;
     ENM_SCROLLEVENTS = 8;
     ENM_SELCHANGE = 524288;
     ENM_UPDATE = 2;
     ECO_AUTOWORDSELECTION = 1;
     ECO_AUTOVSCROLL = 64;
     ECO_AUTOHSCROLL = 128;
     ECO_NOHIDESEL = 256;
     ECO_READONLY = 2048;
     ECO_WANTRETURN = 4096;
     ECO_SAVESEL = $8000;
     ECO_SELECTIONBAR = $1000000;
     ECO_VERTICAL = $400000;
     ECOOP_SET = 1;
     ECOOP_OR = 2;
     ECOOP_AND = 3;
     ECOOP_XOR = 4;
     SCF_DEFAULT = 0;
     SCF_SELECTION = 1;
     SCF_WORD = 2;
     SCF_ALL = 4;
     SCF_USEUIRULES = 8;
     TM_PLAINTEXT = 1;
     TM_RICHTEXT = 2;
     TM_SINGLELEVELUNDO = 4;
     TM_MULTILEVELUNDO = 8;
     TM_SINGLECODEPAGE = 16;
     TM_MULTICODEPAGE = 32;
     GT_DEFAULT = 0;
     GT_USECRLF = 1;
     GT_SELECTION = 2;
     GT_RAWTEXT = 4;
     GT_NOHIDDENTEXT = 8;
     
     yHeightCharPtsMost = 1638;
     lDefaultTab = 720;
     FT_MATCHCASE = 4;
     FT_WHOLEWORD = 2;
     PC_FOLLOWING = 1; 
     PC_LEADING = 2; 
     PC_OVERFLOW = 3; 
     PC_DELIMITER = 4; 
  
    PFE_TABLEROW = $c000; 
    PFE_TABLECELLEND = $8000;
    PFE_TABLECELL = $4000;
    WBF_CLASS  = $0F; 
    WBF_ISWHITE = $10; 
    WBF_BREAKLINE = $20; 
    WBF_BREAKAFTER = $40;

    WCH_EMBEDDING = $FFFC; 
   cchTextLimitDefault = 32767; 
  
  type
    UNDONAMEID = (UID_UNKNOWN, UID_TYPING, UID_DELETE, UID_DRAGDROP, UID_CUT, UID_PASTE);

     _charformat = record
          cbSize : UINT;
          dwMask : DWORD;
          dwEffects : DWORD;
          yHeight : LONG;
          yOffset : LONG;
          crTextColor : COLORREF;
          bCharSet : BYTE;
          bPitchAndFamily : BYTE;
          szFaceName : array[0..(LF_FACESIZE)-1] of char;
       end;
     CHARFORMATA = _charformat;
     TCHARFORMATA = _charformat;

     _charformatw = record
          cbSize : UINT;
          dwMask : DWORD;
          dwEffects : DWORD;
          yHeight : LONG;
          yOffset : LONG;
          crTextColor : COLORREF;
          bCharSet : BYTE;
          bPitchAndFamily : BYTE;
          szFaceName : array[0..(LF_FACESIZE)-1] of WCHAR;
       end;
     CHARFORMATW = _charformatw;
     TCHARFORMATW = _charformatw;

     _charformat2a = record
          cbSize : UINT;
          dwMask : DWORD;
          dwEffects : DWORD;
          yHeight : LONG;
          yOffset : LONG;
          crTextColor : COLORREF;
          bCharSet : BYTE;
          bPitchAndFamily : BYTE;
          szFaceName : array[0..(LF_FACESIZE)-1] of char;
          wWeight : WORD;
          sSpacing : SHORT;
          crBackColor : COLORREF;
          lcid : LCID;
          dwReserved : DWORD;
          sStyle : SHORT;
          wKerning : WORD;
          bUnderlineType : BYTE;
          bAnimation : BYTE;
          bRevAuthor : BYTE;
       end;
     CHARFORMAT2A = _charformat2a;
     TCHARFORMAT2A = _charformat2a;

     _charformat2w = record
          cbSize : UINT;
          dwMask : DWORD;
          dwEffects : DWORD;
          yHeight : LONG;
          yOffset : LONG;
          crTextColor : COLORREF;
          bCharSet : BYTE;
          bPitchAndFamily : BYTE;
          szFaceName : array[0..(LF_FACESIZE)-1] of WCHAR;
          wWeight : WORD;
          sSpacing : SHORT;
          crBackColor : COLORREF;
          lcid : LCID;
          dwReserved : DWORD;
          sStyle : SHORT;
          wKerning : WORD;
          bUnderlineType : BYTE;
          bAnimation : BYTE;
          bRevAuthor : BYTE;
       end;
     CHARFORMAT2W = _charformat2w;
     TCHARFORMAT2W = _charformat2w;

     _charrange = record
          cpMin : LONG;
          cpMax : LONG;
       end;
     CHARRANGE = _charrange;
     TCHARRANGE = _charrange;

     _compcolor = record
          crText : COLORREF;
          crBackground : COLORREF;
          dwEffects : DWORD;
       end;
     COMPCOLOR = _compcolor;
     TCOMPCOLOR = _compcolor;

     EDITSTREAMCALLBACK = function (dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD;

     _editstream = record
          dwCookie : DWORD_PTR;
          dwError : DWORD;
          pfnCallback : EDITSTREAMCALLBACK;
       end;
     EDITSTREAM = _editstream;
     TEDITSTREAM = _editstream;

     _encorrecttext = record
          nmhdr : NMHDR;
          chrg : CHARRANGE;
          seltyp : WORD;
       end;
     ENCORRECTTEXT = _encorrecttext;
     TENCORRECTTEXT = _encorrecttext;

     _endropfiles = record
          nmhdr : NMHDR;
          hDrop : HANDLE;
          cp : LONG;
          fProtected : BOOL;
       end;
     ENDROPFILES = _endropfiles;
     TENDROPFILES = _endropfiles;

     _enlink = record
          nmhdr : NMHDR;
          msg : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
          chrg : CHARRANGE;
       end;
     ENLINK = _enlink;
     TENLINK = _enlink;

     ENOLEOPFAILED = record
          nmhdr : NMHDR;
          iob : LONG;
          lOper : LONG;
          hr : HRESULT;
       end;

     _enprotected = record
          nmhdr : NMHDR;
          msg : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
          chrg : CHARRANGE;
       end;
     ENPROTECTED = _enprotected;
     TENPROTECTED = _enprotected;
     LPENPROTECTED = ^_enprotected;

     _ensaveclipboard = record
          nmhdr : NMHDR;
          cObjectCount : LONG;
          cch : LONG;
       end;
     ENSAVECLIPBOARD = _ensaveclipboard;
     TENSAVECLIPBOARD = _ensaveclipboard;

     _findtextA = record
          chrg : CHARRANGE;
          lpstrText : LPSTR;
       end;
     FINDTEXTA = _findtextA;
     TFINDTEXTA = _findtextA;

     _findtextW = record
          chrg : CHARRANGE;
          lpstrText : LPWSTR;
       end;
     FINDTEXTW = _findtextW;
     TFINDTEXTW = _findtextW;

     _findtextexA = record
          chrg : CHARRANGE;
          lpstrText : LPSTR;
          chrgText : CHARRANGE;
       end;
     FINDTEXTEXA = _findtextexA;
     TFINDTEXTEXA = _findtextexA;

     _findtextexW = record
          chrg : CHARRANGE;
          lpstrText : LPWSTR;
          chrgText : CHARRANGE;
       end;
     FINDTEXTEXW = _findtextexW;
     TFINDTEXTEXW = _findtextexW;

     _formatrange = record
          hdc : HDC;
          hdcTarget : HDC;
          rc : RECT;
          rcPage : RECT;
          chrg : CHARRANGE;
       end;
     FORMATRANGE = _formatrange;
     TFORMATRANGE = _formatrange;

     _msgfilter = record
          nmhdr : NMHDR;
          msg : UINT;
          wParam : WPARAM;
          lParam : LPARAM;
       end;
     MSGFILTER = _msgfilter;
     TMSGFILTER = _msgfilter;

     TReqSize = record
       nmhdr: TNMHdr;
       rc: TRect;
     end;
     PReqSize = ^TReqSize;

     _paraformat = record
          cbSize : UINT;
          dwMask : DWORD;
          wNumbering : WORD;
          wReserved : WORD;
          dxStartIndent : LONG;
          dxRightIndent : LONG;
          dxOffset : LONG;
          wAlignment : WORD;
          cTabCount : SHORT;
          rgxTabs : array[0..(MAX_TAB_STOPS)-1] of LONG;
       end;
     PARAFORMAT = _paraformat;
     TParaFormat = _paraformat;

     _paraformat2 = record
          cbSize : UINT;
          dwMask : DWORD;
          wNumbering : WORD;
          wEffects : WORD;
          dxStartIndent : LONG;
          dxRightIndent : LONG;
          dxOffset : LONG;
          wAlignment : WORD;
          cTabCount : SHORT;
          rgxTabs : array[0..(MAX_TAB_STOPS)-1] of LONG;
          dySpaceBefore : LONG;
          dySpaceAfter : LONG;
          dyLineSpacing : LONG;
          sStype : SHORT;
          bLineSpacingRule : BYTE;
          bOutlineLevel : BYTE;
          wShadingWeight : WORD;
          wShadingStyle : WORD;
          wNumberingStart : WORD;
          wNumberingStyle : WORD;
          wNumberingTab : WORD;
          wBorderSpace : WORD;
          wBorderWidth : WORD;
          wBorders : WORD;
       end;
     PARAFORMAT2 = _paraformat2;
     TPARAFORMAT2 = _paraformat2;

     _selchange = record
          nmhdr : NMHDR;
          chrg : CHARRANGE;
          seltyp : WORD;
       end;
     SELCHANGE = _selchange;
     TSELCHANGE = _selchange;

     _textrange = record
          chrg : CHARRANGE;
          lpstrText : LPSTR;
       end;
     TEXTRANGEA = _textrange;
     TTEXTRANGEA = _textrange;

     _textrangew = record
          chrg : CHARRANGE;
          lpstrText : LPWSTR;
       end;
     TEXTRANGEW = _textrangew;
     TTEXTRANGEW = _textrangew;

     _reqresize = record
          nmhdr : NMHDR;
          rc : RECT;
       end;
     REQRESIZE = _reqresize;
     TREQRESIZE = _reqresize;

     _repastespecial = record
          dwAspect : DWORD;
          dwParam : DWORD;
       end;
     REPASTESPECIAL = _repastespecial;
     TREPASTESPECIAL = _repastespecial;

     _punctuation = record
          iSize : UINT;
          szPunctuation : LPSTR;
       end;
     PUNCTUATION = _punctuation;
     TPUNCTUATION = _punctuation;

     _gettextex = record
          cb : DWORD;
          flags : DWORD;
          codepage : UINT;
          lpDefaultChar : LPCSTR;
          lpUsedDefaultChar : LPBOOL;
       end;
     GETTEXTEX = _gettextex;
     TGETTEXTEX = _gettextex;

     EDITWORDBREAKPROCEX = function (pchText:pchar; cchText:LONG; bCharSet:BYTE; action:LONG):LONG;
  { Defines for EM_SETTYPOGRAPHYOPTIONS  }

  const
  { Defines for GETTEXTLENGTHEX  }
     GTL_DEFAULT = 0;
     GTL_USECRLF = 1;
     GTL_PRECISE = 2;
     GTL_CLOSE = 4;
     GTL_NUMCHARS = 8;
     GTL_NUMBYTES = 16;

  type
    OBJECTPOSITIONS = record
      nmhdr: TNMHdr;
      cObjectCount: Longint;
      pcpPositions: PLongint;
    end;
    TObjectPositions = OBJECTPOSITIONS;

     _gettextlengthex = record
          flags : DWORD;
          codepage : UINT;
       end;
     GETTEXTLENGTHEX = _gettextlengthex;
{$ifdef UNICODE}

     CHARFORMAT = CHARFORMATW;
     TCHARFORMAT = CHARFORMATW;
     CHARFORMAT2 = CHARFORMAT2W;
     TCHARFORMAT2 = CHARFORMAT2W;
     FINDTEXT = FINDTEXTW;
     TFINDTEXT = FINDTEXTW;
     FINDTEXTEX = FINDTEXTEXW;
     TFINDTEXTEX = FINDTEXTEXW;
     TEXTRANGE = TEXTRANGEW;
     TTEXTRANGE = TEXTRANGEW;
{$else}

  type

     CHARFORMAT = CHARFORMATA;
     TCHARFORMAT = CHARFORMATA;
     CHARFORMAT2 = CHARFORMAT2A;
     TCHARFORMAT2 = CHARFORMAT2A;
     FINDTEXT = FINDTEXTA;
     TFINDTEXT = FINDTEXTA;
     FINDTEXTEX = FINDTEXTEXA;
     TFINDTEXTEX = FINDTEXTEXA;
     TEXTRANGE = TEXTRANGEA;
     TTEXTRANGE = TEXTRANGEA;
{$endif}


implementation


end.
