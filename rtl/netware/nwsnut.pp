{
    $Id$
    This file is part of the Free Pascal run time library
    for Netware.
    Copyright (c) 1999-2003 by the Free Pascal development team.
		
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
			
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
				    
**********************************************************************}

unit nwsnut;

interface

{$mode objfpc}

  const
    External_library='nwsnut';

  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PPByte    = ^PByte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;
    TLONG = longint;
    PLONG = ^TLONG;
    TWORD = word;
    TBYTE = byte;
    PScreenStruct = pointer;

{$PACKRECORDS C}

  type
     LONG = dword;

  const
     NULL = 0;
     
  { constants  
    the constant CURRENT_NUT_VERSION is incremented when increased
    functionality is added. An NLM can check this value which is placed
    in the NUTInfo structure, version field, to determine if the NWSNUT
    NLM contains sufficient functionality to support its requirements  }

  const
     CURRENT_NUT_VERSION = 405;
  { the constant NUT_REVISION_LEVEL is incremented when a major change
    in the behavior of NWSNUT is made. This value is not used by the calling
    NLM, but rather by NWSNUT itself to determine what is expected of it
    by the calling NLM  }
     NUT_REVISION_LEVEL = 1;
     SAVE = 1;
     NO_SAVE = 0;
     NOHEADER = 0;
     NOBORDER = 0;
     NO_HELP_CONTEXT = $ffff;
     SINGLE = 1;
     DOUBLE = 2;
     CURSOR_OFF = 0;
     CURSOR_ON = 1;
     VIRTUAL = 0;
     DIRECT = 1;
     SEVERITY_INFORM = 1;
     SEVERITY_WARNING = 2;
     SEVERITY_FATAL = 3;
  { text size minimization styles  }
     SNORMAL = 0;
     SMINWIDTH = 1;
     SMINHEIGHT = 2;
  { palettes to set screen colors.
  	background and foreground can be reversed with VREVERSE
   }
  { white and black  }
     BW_PALETTE = 0;
  { white and dark blue  }
     NORMAL_PALETTE = 1;
  { light blue and dark blue  }
     INIT_PALETTE = 2;
  { green and black  }
     HELP_PALETTE = 3;
  { red and black  }
     ERROR_PALETTE = 4;
  { pink and white  }
     WARNING_PALETTE = 5;
  { green and red  }
     OTHER_PALETTE = 6;
  { text and portal justification styles  }
     JRIGHT = 0;
     JLEFT = 1;
     JTOP = 2;
     JBOTTOM = 3;
     JCENTER = 4;
     JTOPRIGHT = 5;
     JTOPLEFT = 6;
     JBOTTOMLEFT = 7;
     JBOTTOMRIGHT = 8;
  { video constants  }
     V_UP = 6;
     V_DOWN = 7;
     LINE_OFFSET = 160;
     EXPLODE_RATE = 45;
     SCREEN_SPEED = 0;
  { video attributes  }
     VNORMAL = 0;
     VINTENSE = 1;
     VREVERSE = 2;
     VBLINK = 3;
     VIBLINK = 4;
     VRBLINK = 5;
  { header types  }
     NO_HEADER = 0;
     SMALL_HEADER = 1;
     NORMAL_HEADER = 2;
     LARGE_HEADER = 3;
  { keyboard constants  }
     KS_OFF = 0;
     KS_ON = 1;
     KS_INT = 2;
     K_NORMAL = 0;
     K_F1 = 1;
     K_F2 = 2;
     K_F3 = 3;
     K_F4 = 4;
     K_F5 = 5;
     K_F6 = 6;
     K_F7 = 7;
     K_F8 = 8;
     K_F9 = 9;
     K_F10 = 10;
     K_SF1 = 11;
     K_SF2 = 12;
     K_SF3 = 13;
     K_SF4 = 14;
     K_SF5 = 15;
     K_SF6 = 16;
     K_SF7 = 17;
     K_SF8 = 18;
     K_SF9 = 19;
     K_SF10 = 20;
     K_CF1 = 21;
     K_CF2 = 22;
     K_CF3 = 23;
     K_CF4 = 24;
     K_CF5 = 25;
     K_CF6 = 26;
     K_CF7 = 27;
     K_CF8 = 28;
     K_CF9 = 29;
     K_CF10 = 30;
     K_AF1 = 31;
     K_AF2 = 32;
     K_AF3 = 33;
     K_AF4 = 34;
     K_AF5 = 35;
     K_AF6 = 36;
     K_AF7 = 37;
     K_AF8 = 38;
     K_AF9 = 39;
     K_AF10 = 40;
     K_HELP = 1;
     K_MODIFY = 3;
     K_MARK = 5;
     K_CANCEL = 7;
     K_MODE = 9;
     K_EXIT = 40;
     K_ESCAPE = 41;
     K_BACK = 42;
     K_INSERT = 43;
     K_DELETE = 44;
     K_SELECT = 45;
     K_CYCLE = 46;
     K_UP = 47;
     K_DOWN = 48;
     K_LEFT = 49;
     K_RIGHT = 50;
     K_SUP = 51;
     K_SDOWN = 52;
     K_SLEFT = 53;
     K_SRIGHT = 54;
     K_PUP = 55;
     K_PDOWN = 56;
     K_FRIGHT = 57;
     K_FLEFT = 58;
     K_DELETE_END = 59;
  {
  For NWSUngetKey of function keys, use UGK_FUNCTION_KEY for the "type"
  parameter and "K_F1" etc. for the "value" parameter
   }
     UGK_NORMAL_KEY = $00;
     UGK_FUNCTION_KEY = $01;
  {
  For other special keys listed below, use UGK_NORMAL_KEY for the "value"
  parameter, and the UGK_xxx for the "type" parameter.
   }
     UGK_ENTER_KEY = $02;
     UGK_ESCAPE_KEY = $03;
     UGK_BACKSPACE_KEY = $04;
     UGK_DELETE_KEY = $05;
     UGK_INSERT_KEY = $06;
     UGK_CURSOR_UP_KEY = $07;
     UGK_CURSOR_DOWN_KEY = $08;
     UGK_CURSOR_RIGHT_KEY = $09;
     UGK_CURSOR_LEFT_KEY = $0a;
     UGK_CURSOR_HOME_KEY = $0b;
     UGK_CURSOR_END_KEY = $0c;
     UGK_CURSOR_PUP_KEY = $0d;
     UGK_CURSOR_PDOWN_KEY = $0e;
  {
  Added in version 403
  
  A special key type to cause LISTs to refresh. K_REFRESH_KEY may be returned
  from an action procedure passed to NWSList, or another thread that wishes
  to cause a list to refresh may call NWSUngetKey with the UGK version of this,
  and it too will cause the list to be redrawn.
  
  Use "type" = UGK_SPECIAL_KEY, and "value" = UGK_REFRESH_KEY
   }
     UGK_SPECIAL_KEY = 3;
     UGK_REFRESH_KEY = $22222222;
     K_REFRESH_KEY = UGK_REFRESH_KEY;
     OLD_REFRESH_KEY = 222;
  { available action keys for list  }
     M_ESCAPE = $0001;
     M_INSERT = $0002;
     M_DELETE = $0004;
     M_MODIFY = $0008;
     M_SELECT = $0010;
  { marked delete  }
     M_MDELETE = $0020;
     M_CYCLE = $0040;
  { marked modify  }
     M_MMODIFY = $0080;
  { marked select  }
     M_MSELECT = $0100;
  { don't sort list  }
     M_NO_SORT = $0200;
  { allow the list to be refreshed  }
     M_REFRESH = $0400;
  { return values for EditString  }
     E_ESCAPE = 1;
     E_SELECT = 2;
     E_EMPTY = 4;
     E_CHANGE = 8;
  { type values for EditString  }
     EF_ANY = $0001;
     EF_DECIMAL = $0002;
     EF_HEX = $0004;
     EF_NOSPACES = $0008;
     EF_UPPER = $0010;
     EF_DATE = $0020;
     EF_TIME = $0040;
     EF_FLOAT = $0080;
     EF_SET = $0100;
     EF_NOECHO = $0200;
     EF_FILENAME = $0400;
  { added in version 404  }
     EF_MASK = $0800;
  { and in version 405  }
     EF_NOCONFIRM_EXIT = $1000;
  { scroll bar stuff for NWSEditTextWithScrollBars, and NWSViewTextWithScrollBars  }
  { which scroll bars to show  }
     SHOW_VERTICAL_SCROLL_BAR = 2;
     SHOW_HORIZONTAL_SCROLL_BAR = 4;
  { when to show the scroll bars. Use ONLY one of these  }
     CONSTANT_SCROLL_BARS = $0200;
     TEXT_SENSITIVE_SCROLL_BARS = $0400;
     CONSIDER_LOCKED_FIELDS = $0800;
  { character and key constants  }
  { was #define dname def_expr }
  function F_H1 : longint;
      { return type might be wrong }

  { Í  }
  { was #define dname def_expr }
  function F_H2 : longint;
      { return type might be wrong }

  { ³  }
  { was #define dname def_expr }
  function F_V1 : longint;
      { return type might be wrong }

  { º  }
  { was #define dname def_expr }
  function F_V2 : longint;
      { return type might be wrong }

  { Ú  }
  { was #define dname def_expr }
  function F_UL1 : longint;
      { return type might be wrong }

  { ¿  }
  { was #define dname def_expr }
  function F_UR1 : longint;
      { return type might be wrong }

  { À  }
  { was #define dname def_expr }
  function F_LL1 : longint;
      { return type might be wrong }

  { Ù  }
  { was #define dname def_expr }
  function F_LR1 : longint;
      { return type might be wrong }

  { É  }
  { was #define dname def_expr }
  function F_UL2 : longint;
      { return type might be wrong }

  { »  }
  { was #define dname def_expr }
  function F_UR2 : longint;
      { return type might be wrong }

  { È  }
  { was #define dname def_expr }
  function F_LL2 : longint;
      { return type might be wrong }

  { ¼  }
  { was #define dname def_expr }
  function F_LR2 : longint;
      { return type might be wrong }

  { Á  }
  { was #define dname def_expr }
  function F_UT1 : longint;
      { return type might be wrong }

  { Â  }
  { was #define dname def_expr }
  function F_DT1 : longint;
      { return type might be wrong }

  { ´  }
  { was #define dname def_expr }
  function F_LT1 : longint;
      { return type might be wrong }

  { Ã  }
  { was #define dname def_expr }
  function F_RT1 : longint;
      { return type might be wrong }

  { Ê  }
  { was #define dname def_expr }
  function F_UT2 : longint;
      { return type might be wrong }

  { Ë  }
  { was #define dname def_expr }
  function F_DT2 : longint;
      { return type might be wrong }

  { ¹  }
  { was #define dname def_expr }
  function F_LT2 : longint;
      { return type might be wrong }

  { Ì  }
  { was #define dname def_expr }
  function F_RT2 : longint;
      { return type might be wrong }

  { Å  }
  { was #define dname def_expr }
  function F_X1 : longint;
      { return type might be wrong }

  { Î  }
  { was #define dname def_expr }
  function F_X2 : longint;
      { return type might be wrong }

  {   }
  { was #define dname def_expr }
  function F_UP : longint;
      { return type might be wrong }

  {   }
  { was #define dname def_expr }
  function F_DOWN : longint;
      { return type might be wrong }

  {   }
  { was #define dname def_expr }
  function F_LEFT : longint;
      { return type might be wrong }

  {   }
  { was #define dname def_expr }
  function F_RIGHT : longint;
      { return type might be wrong }

  { °  }
  { was #define dname def_expr }
  function F_BG1 : longint;
      { return type might be wrong }

  { ±  }
  { was #define dname def_expr }
  function F_BG2 : longint;
      { return type might be wrong }

  { ²  }
  { was #define dname def_expr }
  function F_BG3 : longint;
      { return type might be wrong }

  { Û  }
  { was #define dname def_expr }
  function F_BG4 : longint;
      { return type might be wrong }

  { form constants (control flags)  }

  const
     F_NOVERIFY = $00;
     F_VERIFY = $10;
     F_FORCE = $20;
  { a flag to pass if no help is desired in the form  }
     F_NO_HELP = $ffffffff;
  {    fieldFlags Type masks     }
  { normal editable field  }
     NORMAL_FIELD = $00;
  { non accessable  }
     LOCKED_FIELD = $01;
  { non editable  }
     SECURE_FIELD = $02;
  { verify field on form exit  }
     REQUIRED_FIELD = $04;
  { hidden fields are also locked  }
     HIDDEN_FIELD = $09;
  { prompt fields are also locked  }
     PROMPT_FIELD = $11;
  { field locked by user, not by NUT  }
     ULOCKED_FIELD = $0100;
  { MASKED_FIELD added in version 402  }
  { display ' ' for text   }
     MASKED_FIELD = $200;
  { flag to cause form deselection
                                             before action & verify routines
                                             are called  }
     FORM_DESELECT = $20;
  { In case old flag was used  }
     NO_FORM_DESELECT = $00;
  { normal field controlled justify  }
     DEFAULT_FORMAT = $00;
  { right justification format  }
     RIGHT_FORMAT = $40;
  { left justification format  }
     LEFT_FORMAT = $80;
  { centering format  }
     CENTER_FORMAT = $C0;
     MAXPORTALS = 50;
     MAXLISTS = 20;
     SAVELISTS = 20;
     MAXACTIONS = 60;
     MAXFUNCTIONS = MAXACTIONS;
     MAXHELP = 30;
     NO_MESSAGE = $ffff;
     DYNAMIC_MESSAGE_ONE = $fffe;
     DYNAMIC_MESSAGE_TWO = $fffd;
     DYNAMIC_MESSAGE_THREE = $fffc;
     DYNAMIC_MESSAGE_FOUR = $fffb;
     DYNAMIC_MESSAGE_FIVE = $fffa;
     DYNAMIC_MESSAGE_SIX = $fff9;
     DYNAMIC_MESSAGE_SEVEN = $fff8;
     DYNAMIC_MESSAGE_EIGHT = $fff7;
     DYNAMIC_MESSAGE_NINE = $fff6;
     DYNAMIC_MESSAGE_TEN = $fff5;
     DYNAMIC_MESSAGE_ELEVEN = $fff4;
     DYNAMIC_MESSAGE_TWELVE = $fff3;
     DYNAMIC_MESSAGE_THIRTEEN = $fff2;
     DYNAMIC_MESSAGE_FOURTEEN = $fff1;
     SYSTEM_MESSAGE = $8000;
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function IS_DYNAMIC_MESSAGE(a : longint) : boolean;


  type
     {PNUTInfo_ = ^TNUTInfo_;
     TNUTInfo_ = record
       end;}
       
       PNUTInfo_ = ^TNUTInfo_;

  { height of virtual screen  }
  { width of virtual screen  }
  { top-most line of portal  }
  { left-most column of portal  }
  { position of portal over virtual portal  }
  { position of portal over virtual portal  }
  { the following fields should never be referenced by any application  }

     PPCB_ = ^TPCB_;
     TPCB_ = record
          frameLine : TLONG;
          frameColumn : TLONG;
          frameHeight : TLONG;
          frameWidth : TLONG;
          virtualHeight : TLONG;
          virtualWidth : TLONG;
          cursorState : TLONG;
          borderType : TLONG;
          borderAttribute : TLONG;
          saveFlag : TWORD;
          secondarySaveFlag : TWORD;
          directFlag : TLONG;
          headerAttribute : TLONG;
          portalLine : TLONG;
          portalColumn : TLONG;
          portalHeight : TLONG;
          portalWidth : TLONG;
          virtualLine : TLONG;
          virtualColumn : TLONG;
          cursorLine : TLONG;
          cursorColumn : TLONG;
          firstUpdateFlag : TLONG;
          headerText : PBYTE;
          headerText2 : PBYTE;
          virtualScreen : PBYTE;
          saveScreen : PBYTE;
          screenID : PScreenStruct;
          nutInfo : PNUTInfo_;
          sequenceNumber : TLONG;
          reserved1 : TLONG;
          mtflags : TLONG;
          borderPalette : TLONG;
          showScrollBars : TLONG;
          lastLine : TLONG;
          longestLineLen : TLONG;
          verticalScroll : TLONG;
          horizontalScroll : TLONG;
          oldVertical : TLONG;
          oldHorizontal : TLONG;
          deHighlightFunction : procedure (para1:PNUTInfo_; para2:PPCB_);cdecl;
          reHighlightFunction : procedure (para1:PNUTInfo_; para2:PPCB_); cdecl;
          reportPortalUpdate : procedure (para1:PPCB_; para2:PNUTInfo_; updateType:TLONG); cdecl;
       end;
     TPCB = TPCB_;
     PPCB = ^TPCB;
  { Topmost line of frame on physical screen  }
  { Leftmost column of frame on physical screen  }
  { Height of frame on physical screen  }
  { Width of frame on physical screen  }
  { Height of virtual screen  }
  { Width of virtual screen  }
  { Is the cursor on or off (1 or 0)  }
  { Type of border to use  }
  { Attribute to use with border  }
  { TRUE = save old screen  }
  { TRUE = no virtual screen  }
  { Attribute of header text  }
  { Pointer to header text  }
  { Pointer to the help text, always assumed to  }
  { be non NULL (when empty points to a '/0' BYTE  }

     PHS_ = ^THS_;
     THS_ = record
          nextScreen : TLONG;
          previousScreen : TLONG;
          frameLine : TLONG;
          frameColumn : TLONG;
          frameHeight : TLONG;
          frameWidth : TLONG;
          virtualHeight : TLONG;
          virtualWidth : TLONG;
          cursorState : TLONG;
          borderType : TLONG;
          borderAttribute : TLONG;
          saveFlag : TLONG;
          directFlag : TLONG;
          headerAttribute : TLONG;
          headerText : PBYTE;
          text : PBYTE;
       end;
     THELP_SCREEN = THS_;
     PHELP_SCREEN = ^THELP_SCREEN;

     PLIST_STRUCT = ^TLIST_STRUCT;
     TLIST_STRUCT = record
          prev : PLIST_STRUCT;
          next : PLIST_STRUCT;
          otherInfo : pointer;
          marked : TLONG;
          flags : TWORD;
          maxSkew : TWORD;
          entryProcedure : procedure (listElement:PLIST_STRUCT; displayLine:TLONG; NUTInfoStructure:pointer);cdecl;
          extra : TLONG;
          text : array[0..0] of TBYTE;
       end;
     TLIST = TLIST_STRUCT;
     PLIST = ^TLIST;
     PPLIST= ^PLIST;

     PLP_ = ^TLP_;
     TLP_ = record
          head : pointer;
          tail : pointer;
          sortProc : function :longint;cdecl;
          freeProcedure : procedure (memoryPointer:pointer);
       end;
     TLISTPTR = TLP_;
     PLISTPTR = ^TLISTPTR;

     PMI_ = ^TMI_;
     TMI_ = record
          dynamicMessageOne : PBYTE;
          dynamicMessageTwo : PBYTE;
          dynamicMessageThree : PBYTE;
          dynamicMessageFour : PBYTE;
          dynamicMessageFive : PBYTE;
          dynamicMessageSix : PBYTE;
          dynamicMessageSeven : PBYTE;
          dynamicMessageEight : PBYTE;
          dynamicMessageNine : PBYTE;
          dynamicMessageTen : PBYTE;
          dynamicMessageEleven : PBYTE;
          dynamicMessageTwelve : PBYTE;
          dynamicMessageThirteen : PBYTE;
          dynamicMessageFourteen : PBYTE;
          messageCount : TLONG;
          programMesgTable : ^PBYTE;
       end;
     TMessageInfo = TMI_;
     PMessageInfo = ^TMessageInfo;

     PINT_ = ^TINT_;
     TINT_ = record
          interruptProc : procedure (handle:pointer);cdecl;
          key : TLONG;
       end;
     TINTERRUPT = TINT_;
     PINTERRUPT = ^TINTERRUPT;

     PMP_ = ^TMP_;
     TMP_ = record
          listAction : function (option:longint; parameter:pointer):longint;cdecl;
          parameter : pointer;
       end;
     TMENU_PARAMETER = TMP_;
     PMENU_PARAMETER = ^TMENU_PARAMETER;
  { environment structure  }
  { always leave the following fields at the end of the struct.  They
          should never be referenced directly by an application  }

     
     TNUTInfo_ = record
          portal : array[0..(MAXPORTALS)-1] of PPCB;
          currentPortal : TLONG;
          headerHeight : TLONG;
          waitFlag : TLONG;
          listStack : array[0..(MAXLISTS)-1] of TLISTPTR;
          saveStack : array[0..(SAVELISTS)-1] of TLISTPTR;
          nextAvailList : TLONG;
          head : PLIST;
          tail : PLIST;
          defaultCompareFunction : function (el1:PLIST; el2:PLIST):longint;cdecl;
          freeProcedure : procedure (memoryPointer:pointer);
          interruptTable : array[0..(MAXFUNCTIONS)-1] of procedure ;
          functionKeyStatus : array[0..(MAXACTIONS)-1] of TLONG;
          messages : TMessageInfo;
          helpContextStack : array[0..(MAXHELP)-1] of TLONG;
          currentPreHelpMessage : TLONG;
          freeHelpSlot : longint;
          redisplayFormFlag : TLONG;
          preHelpPortal : TLONG;
          helpActive : smallint;
          errorDisplayActive : smallint;
          helpPortal : TLONG;
          waitPortal : TLONG;
          errorPortal : TLONG;
          resourceTag : pointer;
          screenID : pointer;
          helpScreens : PBYTE;
          helpOffset : longint;
          helpHelp : TLONG;
          allocChain : pointer;
          version : TLONG;
          reserved : array[0..9] of TLONG;
          moduleHandle : TLONG;
          customData : pointer;
          customDataRelease : procedure (theData:pointer; thisStructure:PNUTInfo_); cdecl;
          displayErrorLabel : TLONG;
          markBuffer : PBYTE;
          markBufferLength : TLONG;
          editBuffer : PBYTE;
          editBufferLength : TLONG;
          staticFlag : TLONG;
          processID : TLONG;
          mtflags : TLONG;
          saveCurrentPortal : TLONG;
          palette : TLONG;
          nutDataHandle : pointer;
          next : PNUTInfo_;
          prev : PNUTInfo_;
          listSortFunction : procedure (head:PLIST; tail:PLIST; thisStructure:PNUTInfo_); cdecl;
          compatibilityLevel : TLONG;
       end;
     TNUTInfo = TNUTInfo_;
     PNUTInfo = ^TNUTInfo;
     PPNUTInfo= ^PNUTInfo;
     
  { menu header message number  }
  { menu center line  }
  { menu center column  }
  { len of longest menu option  }
  { menu action routine  }
  { list head for menu list  }

     PMFC_ = ^TMFC_;
     TMFC_ = record
          headernum : TLONG;
          centerLine : TLONG;
          centerColumn : TLONG;
          maxoptlen : TLONG;
          action : function (option:longint; parameter:pointer):longint;cdecl;
          arg1 : TLONG;
          arg2 : TLONG;
          arg3 : TLONG;
          arg4 : TLONG;
          arg5 : TLONG;
          arg6 : TLONG;
          menuhead : TLISTPTR;
          nutInfo : PNUTInfo;
       end;
     TMFCONTROL = TMFC_;
     PMFCONTROL = ^TMFCONTROL;
  { list element that owns the field  }
  { Control flags  }
  { Line where field is located  }
  { Column where field is located  }
  { Maximum width of field  }
  { Display attribute for field  }
  { Keys that will activate the field  }
  { Routine called when field selected  }
  { Routine to verify Input  }
  { Data & Xtra field release routine  }
  { Pointer to data  }
  { Additional control info  }
  { help context for this field  }
  { Pointer to field above  }
  { Pointer to field below  }
  { Pointer to field to left  }
  { Pointer to field to right  }
  { Pointer to previous field  }
  { Pointer to next field  }
  { if this value is set, this routine will be called upon
                  entry to each field  }
  { this allows the user to have any sort of custom data that
                  he wants attached to the field.  }
  { and this lets him release it. Note that these parameters
                  match NWSFree which allows the use of NWSAlloc for
                  this data (a further guarantee that the memory will be freed  }
  { handle to keep track of who owns the field  }

     Pfielddef = ^Tfielddef;
     Tfielddef = record
          element : PLIST;
          fieldFlags : TLONG;
          fieldLine : TLONG;
          fieldColumn : TLONG;
          fieldWidth : TLONG;
          fieldAttribute : TLONG;
          fieldActivateKeys : longint;
          fieldFormat : procedure (field:Pfielddef; text:PBYTE; buffLen:TLONG);cdecl;
          fieldControl : function (field:Pfielddef; selectKey:longint; fieldChanged:Plongint; handle:PNUTInfo):TLONG;
          fieldVerify : function (field:Pfielddef; data:PBYTE; handle:PNUTInfo):longint;
          fieldRelease : procedure (para1:Pfielddef);
          fieldData : PBYTE;
          fieldXtra : PBYTE;
          fieldHelp : longint;
          fieldAbove : Pfielddef;
          fieldBelow : Pfielddef;
          fieldLeft : Pfielddef;
          fieldRight : Pfielddef;
          fieldPrev : Pfielddef;
          fieldNext : Pfielddef;
          fieldEntry : procedure (intoField:Pfielddef; fieldData:pointer; handle:PNUTInfo);
          customData : pointer;
          customDataRelease : procedure (fieldCustomData:pointer; handle:PNUTInfo);
          nutInfo : PNUTInfo;
       end;
     TFIELD = Tfielddef;
     PFIELD = ^TFIELD;
  { Structures used for DisplayErrorCondition()  }

     PPCERR_ = ^TPCERR_;
     TPCERR_ = record
          ccodeReturned : longint;
          errorMessageNumber : longint;
       end;
     TPROCERROR = TPCERR_;
     PPROCERROR = ^TPROCERROR;

     PNA_ = ^TNA_;
     TNA_ = record
          address : pointer;
          next : pointer;
       end;
     TNUT_ALLOC = TNA_;
     PNUT_ALLOC = ^TNUT_ALLOC;
{ C++ extern C conditionnal removed }
  { CLIB screen ID  }
  { OS ResourceTagStructure  }

  function NWSInitializeNut(utility:TLONG; version:TLONG; headerType:TLONG; compatibilityLevel:TLONG; messageTable:PPBYTE; 
             helpScreens:PBYTE; screenID:longint; resourceTag:TLONG; handle:PPNUTInfo):longint;cdecl;external External_library name 'NWSInitializeNut';

  procedure NWSScreenSize(maxLines:PLONG; maxColumns:PLONG);cdecl;external External_library name 'NWSScreenSize';

  procedure NWSShowPortalLine(line:TLONG; column:TLONG; text:PBYTE; length:TLONG; portal:PPCB);cdecl;external External_library name 'NWSShowPortalLine';

  procedure NWSShowPortalLineAttribute(line:TLONG; column:TLONG; text:PBYTE; attribute:TLONG; length:TLONG; 
              portal:PPCB);cdecl;external External_library name 'NWSShowPortalLineAttribute';

  procedure NWSScrollPortalZone(line:TLONG; column:TLONG; height:TLONG; width:TLONG; attribute:TLONG; 
              count:TLONG; direction:TLONG; portal:PPCB);cdecl;external External_library name 'NWSScrollPortalZone';

  procedure NWSFillPortalZone(line:TLONG; column:TLONG; height:TLONG; width:TLONG; fillCharacter:TLONG; 
              fillAttribute:TLONG; portal:PPCB);cdecl;external External_library name 'NWSFillPortalZone';

  procedure NWSFillPortalZoneAttribute(line:TLONG; column:TLONG; height:TLONG; width:TLONG; attribute:TLONG; 
              portal:PPCB);cdecl;external External_library name 'NWSFillPortalZoneAttribute';

  function NWSGetMessage(message:TLONG; messages:PMessageInfo):PBYTE;cdecl;external External_library name 'NWSGetMessage';

  procedure NWSSetDynamicMessage(message:TLONG; text:PBYTE; messages:PMessageInfo);cdecl;external External_library name 'NWSSetDynamicMessage';

  function NWSCreatePortal(line:TLONG; column:TLONG; frameHeight:TLONG; frameWidth:TLONG; virtualHeight:TLONG; 
             virtualWidth:TLONG; saveFlag:TLONG; headerText:PBYTE; headerAttribute:TLONG; borderType:TLONG; 
             borderAttribute:TLONG; cursorFlag:TLONG; directFlag:TLONG; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSCreatePortal';

  procedure NWSDestroyPortal(portalNumber:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyPortal';

  procedure NWSPositionPortalCursor(line:TLONG; column:TLONG; portal:PPCB);cdecl;external External_library name 'NWSPositionPortalCursor';

  procedure NWSEnablePortalCursor(portal:PPCB);cdecl;external External_library name 'NWSEnablePortalCursor';

  procedure NWSDisablePortalCursor(portal:PPCB);cdecl;external External_library name 'NWSDisablePortalCursor';

  procedure NWSDeselectPortal(handle:PNUTInfo);cdecl;external External_library name 'NWSDeselectPortal';

  procedure NWSSelectPortal(portalNumber:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSSelectPortal';

  function NWSComputePortalPosition(centerLine:TLONG; centerColumn:TLONG; height:TLONG; width:TLONG; line:PLONG; 
             column:PLONG; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSComputePortalPosition';

  procedure NWSClearPortal(portal:PPCB);cdecl;external External_library name 'NWSClearPortal';

type TFreeRoutine = procedure (memoryPointer:pointer); cdecl;

  procedure NWSInitList(handle:PNUTInfo; freeRoutine:TFreeRoutine);cdecl;external External_library name 'NWSInitList';

  function NWSPushList(handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSPushList';

  function NWSPopList(handleNWS:PNUTInfo):TLONG;cdecl;external External_library name 'NWSPopList';

  function NWSSaveList(listIndex:TLONG; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSSaveList';

  function NWSRestoreList(listIndex:TLONG; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSRestoreList';

  procedure NWSDestroyList(handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyList';

  procedure NWSDestroyMenu(handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyMenu';

  procedure NWSDestroyForm(handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyForm';

  function NWSAppendToList(text:PBYTE; otherInfo:pointer; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSAppendToList';

  function NWSDeleteFromList(el:PLIST; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSDeleteFromList';

  function NWSInsertInList(text:PBYTE; otherInfo:PBYTE; atElement:PLIST; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSInsertInList';

  function NWSGetListElementText(element:PLIST):PBYTE;cdecl;external External_library name 'NWSGetListElementText';

  function NWSGetListHead(handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSGetListHead';

  function NWSGetListTail(handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSGetListTail';

  procedure NWSUnmarkList(handle:PNUTInfo);cdecl;external External_library name 'NWSUnmarkList';

  procedure NWSSetList(listPtr:PLISTPTR; handle:PNUTInfo);cdecl;external External_library name 'NWSSetList';

  procedure NWSGetList(listPtr:PLISTPTR; handle:PNUTInfo);cdecl;external External_library name 'NWSGetList';

  function NWSIsAnyMarked(handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSIsAnyMarked';

  procedure NWSPushMarks(handle:PNUTInfo);cdecl;external External_library name 'NWSPushMarks';

  procedure NWSPopMarks(handle:PNUTInfo);cdecl;external External_library name 'NWSPopMarks';

  procedure NWSSortList(handle:PNUTInfo);cdecl;external External_library name 'NWSSortList';

  procedure NWSInitMenu(handle:PNUTInfo);cdecl;external External_library name 'NWSInitMenu';

  procedure NWSInitForm(handle:PNUTInfo);cdecl;external External_library name 'NWSInitForm';

  function NWSGetSortCharacter(charIndex:TLONG):TLONG;cdecl;external External_library name 'NWSGetSortCharacter';

  function NWSGetLineDrawCharacter(charIndex:TLONG):TLONG;cdecl;external External_library name 'NWSGetLineDrawCharacter';

  function NWSStrcat(_string:PBYTE; newStuff:PBYTE):TLONG;cdecl;external External_library name 'NWSStrcat';

  procedure NWSMemmove(dest:pointer; source:pointer; len:longint);cdecl;external External_library name 'NWSMemmove';

  function NWSToupper(ch:TBYTE):TBYTE;cdecl;external External_library name 'NWSToupper';

  function NWSIsdigit(ch:TBYTE):longint;cdecl;external External_library name 'NWSIsdigit';

  function NWSIsxdigit(ch:TBYTE):longint;cdecl;external External_library name 'NWSIsxdigit';

  function NWSAsciiToInt(data:PBYTE):longint;cdecl;external External_library name 'NWSAsciiToInt';

  function NWSAsciiToLONG(data:PBYTE):TLONG;cdecl;external External_library name 'NWSAsciiToLONG';

  function NWSAsciiHexToInt(data:PBYTE):longint;cdecl;external External_library name 'NWSAsciiHexToInt';

  procedure NWSWaitForEscape(handle:PNUTInfo);cdecl;external External_library name 'NWSWaitForEscape';

  function NWSWaitForEscapeOrCancel(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSWaitForEscapeOrCancel';

  procedure NWSGetKey(_type:PLONG; value:PBYTE; handle:PNUTInfo);cdecl;external External_library name 'NWSGetKey';

  function NWSKeyStatus(handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSKeyStatus';

  function NWSUngetKey(_type:TLONG; value:TLONG; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSUngetKey';

  procedure NWSEnableFunctionKey(key:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableFunctionKey';

  procedure NWSDisableFunctionKey(key:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSDisableFunctionKey';

  procedure NWSDisableInterruptKey(key:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSDisableInterruptKey';

type TInterruptProc = procedure (handle:pointer); cdecl;

  procedure NWSEnableInterruptKey(key:TLONG; interruptProc:TInterruptProc; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableInterruptKey';

  procedure NWSSaveFunctionKeyList(keyList:PBYTE; handle:PNUTInfo);cdecl;external External_library name 'NWSSaveFunctionKeyList';

  procedure NWSEnableFunctionKeyList(keyList:PBYTE; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableFunctionKeyList';

  procedure NWSSaveInterruptList(interruptList:PINTERRUPT; handle:PNUTInfo);cdecl;external External_library name 'NWSSaveInterruptList';

  procedure NWSEnableInterruptList(interruptList:PINTERRUPT; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableInterruptList';

  procedure NWSDisableAllInterruptKeys(handle:PNUTInfo);cdecl;external External_library name 'NWSDisableAllInterruptKeys';

  procedure NWSDisableAllFunctionKeys(handle:PNUTInfo);cdecl;external External_library name 'NWSDisableAllFunctionKeys';

  procedure NWSEnableAllFunctionKeys(handle:PNUTInfo);cdecl;external External_library name 'NWSEnableAllFunctionKeys';

  function NWSDisplayTextInPortal(line:TLONG; indentLevel:TLONG; text:PBYTE; attribute:TLONG; portal:PPCB):longint;cdecl;external External_library name 'NWSDisplayTextInPortal';

  function NWSDisplayInformation(header:TLONG; pauseFlag:TLONG; centerLine:TLONG; centerColumn:TLONG; palette:TLONG; 
             attribute:TLONG; displayText:PBYTE; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSDisplayInformation';

  procedure NWSStartWait(centerLine:TLONG; centerColumn:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSStartWait';

  procedure NWSEndWait(handle:PNUTInfo);cdecl;external External_library name 'NWSEndWait';

  function NWSAlert(centerLine:TLONG; centerColumn:TLONG; handle:PNUTInfo; message:TLONG; args:array of const):TLONG;cdecl;external External_library name 'NWSAlert';

  function NWSAlert(centerLine:TLONG; centerColumn:TLONG; handle:PNUTInfo; message:TLONG):TLONG;cdecl;external External_library name 'NWSAlert';

  function NWSAlertWithHelp(centerLine:TLONG; centerColumn:TLONG; handle:PNUTInfo; message:TLONG; helpContext:TLONG; 
             args:array of const):TLONG;cdecl;external External_library name 'NWSAlertWithHelp';

  function NWSAlertWithHelp(centerLine:TLONG; centerColumn:TLONG; handle:PNUTInfo; message:TLONG; helpContext:TLONG):TLONG;cdecl;external External_library name 'NWSAlertWithHelp';

  function NWSTrace(handle:PNUTInfo; message:PBYTE; args:array of const):TLONG;cdecl;external External_library name 'NWSTrace';

  function NWSTrace(handle:PNUTInfo; message:PBYTE):TLONG;cdecl;external External_library name 'NWSTrace';

  procedure NWSDisplayErrorText(message:TLONG; severity:TLONG; handle:PNUTInfo; args:array of const);cdecl;external External_library name 'NWSDisplayErrorText';

  procedure NWSDisplayErrorText(message:TLONG; severity:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayErrorText';

  procedure NWSDisplayErrorCondition(procedureName:PBYTE; errorCode:longint; severity:TLONG; errorList:PPROCERROR; handle:PNUTInfo; 
              args:array of const);cdecl;external External_library name 'NWSDisplayErrorCondition';

  procedure NWSDisplayErrorCondition(procedureName:PBYTE; errorCode:longint; severity:TLONG; errorList:PPROCERROR; handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayErrorCondition';

  function NWSAppendToMenu(message:TLONG; option:TLONG; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSAppendToMenu';

type TActionFunc = function (option:longint; parameter:pointer) : longint; cdecl;

  function NWSMenu(header:TLONG; centerLine:TLONG; centerColumn:TLONG; defaultElement:PLIST; action:TActionFunc; 
             handle:PNUTInfo; actionParameter:pointer):longint;cdecl;external External_library name 'NWSMenu';

  function NWSConfirm(header:TLONG; centerLine:TLONG; centerColumn:TLONG; defaultChoice:TLONG; action:TActionFunc; 
             handle:PNUTInfo; actionParameter:pointer):longint;cdecl;external External_library name 'NWSConfirm';

  function NWSPushHelpContext(helpContext:TLONG; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSPushHelpContext';

  function NWSPopHelpContext(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSPopHelpContext';

type TFormatFunc=function (element:PLIST; skew:TLONG; displayLine:PBYTE; width:TLONG):TLONG; cdecl;
     TNWSListActionFunc=function (keyPressed:TLONG; elementSelected:PPLIST; itemLineNumber:PLONG; actionParameter:pointer):longint;  cdecl;
     
  function NWSList(header:TLONG; centerLine:TLONG; centerColumn:TLONG; height:TLONG; width:TLONG; 
             validKeyFlags:TLONG; element:PPLIST; handle:PNUTInfo; format:TFormatFunc; action:TNWSListActionFunc;
             actionParameter:pointer):TLONG;cdecl;external External_library name 'NWSList';

type TInsertFunc = function (text:PBYTE; otherInfo:Ppointer; parameters:pointer):longint; cdecl;
     TFreeProcedure=function (otherInfo:pointer):longint; cdecl;
     
  function NWSInsertInPortalList(currentElement:PPLIST; currentLine:PLONG; InsertProcedure:TInsertFunc; FreeProcedure:TFreeProcedure; handle:PNUTInfo; 
             parameters:pointer):longint;cdecl;external External_library name 'NWSInsertInPortalList';

type TModifyProcedure=function (text:PBYTE; parameters:pointer):longint;  cdecl;

  function NWSModifyInPortalList(currentElement:PPLIST; currentLine:PLONG; ModifyProcedure:TModifyProcedure; handle:PNUTInfo; parameters:pointer):longint;cdecl;external External_library name 'NWSModifyInPortalList';

type TDeleteFunc = function (el:PLIST; handle:PNUTInfo; parameters:pointer):PLIST; cdecl;
  function NWSDeleteFromPortalList(currentElement:PPLIST; currentLine:PLONG; DeleteProcedure:TDeleteFunc; deleteCurrentHeader:TLONG; deleteMarkedHeader:TLONG; 
             handle:PNUTInfo; parameters:pointer):longint;cdecl;external External_library name 'NWSDeleteFromPortalList';

type TNWSEditInsertFunc=function (buffer:PBYTE; maxLen:TLONG; parameters:pointer):longint; cdecl;
     TNWSEditActionFunc=function (action:TLONG; buffer:PBYTE; parameters:pointer):longint; cdecl;

  function NWSEditString(centerLine:TLONG; centerColumn:TLONG; editHeight:TLONG; editWidth:TLONG; header:TLONG; 
             prompt:TLONG; buf:PBYTE; maxLen:TLONG; _type:TLONG; handle:PNUTInfo; 
             insertProc:TNWSEditInsertFunc; actionProc:TNWSEditActionFunc; parameters:pointer):longint;cdecl;external External_library name 'NWSEditString';

  function NWSAppendIntegerField(line:TLONG; column:TLONG; fflag:TLONG; data:Plongint; minimum:longint; 
             maximum:longint; help:TLONG; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendIntegerField';

  function NWSAppendUnsignedIntegerField(line:TLONG; column:TLONG; fflag:TLONG; data:PLONG; minimum:TLONG; 
             maximum:TLONG; help:TLONG; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendUnsignedIntegerField';

  function NWSAppendHexField(line:TLONG; column:TLONG; fflag:TLONG; data:Plongint; minimum:longint; 
             maximum:longint; help:TLONG; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendHexField';

  procedure NWSDisplayPreHelp(line:TLONG; column:TLONG; message:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayPreHelp';

  procedure NWSRemovePreHelp(handle:PNUTInfo);cdecl;external External_library name 'NWSRemovePreHelp';

  function NWSGetADisk(volName:PBYTE; prompt:PBYTE; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSGetADisk';

  procedure NWSInitListPtr(listPtr:PLISTPTR);cdecl;external External_library name 'NWSInitListPtr';

  function NWSEditForm(headernum:TLONG; line:TLONG; col:TLONG; portalHeight:TLONG; portalWidth:TLONG; 
             virtualHeight:TLONG; virtualWidth:TLONG; ESCverify:TLONG; forceverify:TLONG; confirmMessage:TLONG; 
             handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditForm';

  function NWSEditPortalFormField(header:TLONG; cline:TLONG; ccol:TLONG; formHeight:TLONG; formWidth:TLONG; 
             controlFlags:TLONG; formHelp:TLONG; confirmMessage:TLONG; startField:PFIELD; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditPortalFormField';

  function NWSEditPortalForm(header:TLONG; centerLine:TLONG; centerColumn:TLONG; formHeight:TLONG; formWidth:TLONG; 
             controlFlags:TLONG; formHelp:TLONG; confirmMessage:TLONG; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditPortalForm';

type TfFormat  = procedure (field:Pfielddef; text:PBYTE; buffLen:TLONG); cdecl;
     TfControl = function (field:Pfielddef; selectKey:longint; fieldChanged:Plongint; handle:PNUTInfo):TLONG; cdecl;
     TfVerify  = function (field:Pfielddef; data:PBYTE; handle:PNUTInfo):longint; cdecl;
     TfRelease = procedure (field:Pfielddef); cdecl;

  { Data & Xtra field release routine  }
  function NWSAppendToForm(
             fline:TLONG; 
	     fcol:TLONG; 
	     fwidth:TLONG; 
	     fattr:TLONG; 
	     fFormat:TfFormat;
             fControl:TfControl;
	     fVerify:TfVerify;
	     fRelease:TfRelease;
	     fData:PBYTE; 
	     fXtra:PBYTE; 
             fflags:TLONG; 
	     fActivateKeys:TLONG; 
	     fhelp:TLONG; 
	     handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendToForm';

  function NWSAppendPromptField(line:TLONG; column:TLONG; promptnum:TLONG; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendPromptField';

  function NWSAppendCommentField(line:TLONG; column:TLONG; prompt:PBYTE; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendCommentField';

  function NWSAppendStringField(line:TLONG; column:TLONG; width:TLONG; fflag:TLONG; data:PBYTE; 
             cset:PBYTE; help:TLONG; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendStringField';

  function NWSAppendBoolField(line:TLONG; column:TLONG; fflag:TLONG; data:PBYTE; help:TLONG; 
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendBoolField';

  function NWSAppendGenericBoolField(line:TLONG; column:TLONG; fflag:TLONG; data:PBYTE; help:TLONG; 
             yesString:PBYTE; noString:PBYTE; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendGenericBoolField';

type TSpotActionFunc = function (fp:PFIELD; selectKey:longint; changedField:Plongint; handle:PNUTInfo):TLONG; cdecl;
  function NWSAppendHotSpotField(line:TLONG; column:TLONG; fflag:TLONG; 
             displayString:PBYTE; 
	     SpotAction:TSpotActionFunc;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendHotSpotField';

  function NWSInitMenuField(headermsg:TLONG; cLine:TLONG; cCol:TLONG; 
             action:TActionFunc ;
	     nutInfo:PNUTInfo; 
             args:array of const):PMFCONTROL;cdecl;external External_library name 'NWSInitMenuField';

  function NWSInitMenuField(headermsg:TLONG; cLine:TLONG; cCol:TLONG; 
              action:TActionFunc; nutInfo:PNUTInfo):PMFCONTROL;cdecl;external External_library name 'NWSInitMenuField';

  function NWSAppendToMenuField(m:PMFCONTROL; optiontext:TLONG; option:longint; nutInfo:PNUTInfo):longint;cdecl;external External_library name 'NWSAppendToMenuField';

  function NWSAppendMenuField(line:TLONG; column:TLONG; fflag:TLONG; data:Plongint; m:PMFCONTROL; 
             help:TLONG; nutInfo:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendMenuField';

  { length of document  }
  function NWSEditText(centerLine:TLONG; centerColumn:TLONG; height:TLONG; width:TLONG; headerNumber:TLONG; 
             textBuffer:PBYTE; maxBufferLength:TLONG; confirmMessage:TLONG; forceConfirm:TLONG; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditText';

  function NWSViewText(centerLine:TLONG; centerColumn:TLONG; height:TLONG; width:TLONG; headerNumber:TLONG; 
             textBuffer:PBYTE; maxBufferLength:TLONG; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSViewText';

  procedure NWSDisplayHelpScreen(offset:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayHelpScreen';

  function NWSAlloc(numberOfBytes:TLONG; handle:PNUTInfo):pointer;cdecl;external External_library name 'NWSAlloc';

  procedure NWSFree(address:pointer; handle:PNUTInfo);cdecl;external External_library name 'NWSFree';

  function NWSDisplayTextJustifiedInPortal(justify:TLONG; line:TLONG; column:TLONG; textWidth:TLONG; text:PBYTE; 
             attribute:TLONG; portal:PPCB):longint;cdecl;external External_library name 'NWSDisplayTextJustifiedInPortal';

  function NWSDisplayInformationInPortal(header:TLONG; portalJustifyLine:TLONG; portalJustifyColumn:TLONG; portalJustifyType:TLONG; portalPalette:TLONG; 
             portalBorderType:TLONG; portalMaxWidth:TLONG; portalMaxHeight:TLONG; portalMinWidth:TLONG; portalMinHeight:TLONG; 
             textLRJustifyType:TLONG; textLRIndent:TLONG; textTBJustifyType:TLONG; textTBIndent:TLONG; textAttribute:TLONG; 
             textMinimizeStyle:TLONG; text:PBYTE; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSDisplayInformationInPortal';

  procedure NWSRestoreNut(handle:PNUTInfo);cdecl;external External_library name 'NWSRestoreNut';

  procedure NWSDrawPortalBorder(portal:PPCB);cdecl;external External_library name 'NWSDrawPortalBorder';

  procedure NWSUpdatePortal(portal:PPCB);cdecl;external External_library name 'NWSUpdatePortal';

type TSSFEntryProc = procedure (para1:PFIELD; para2:pointer; para3:PNUTInfo); cdecl;
     TSSFCustomDataReleaseProc = procedure (para1:pointer; para2:PNUTInfo); cdecl;
     TSSFFormat=procedure (para1:PFIELD; text:PBYTE; para3:TLONG); cdecl;
     TSSFControlFunc = function (para1:PFIELD; para2:longint; para3:Plongint; para4:PNUTInfo):TLONG; cdecl;
     TSSFVerifyFunc  = function (para1:PFIELD; para2:PBYTE; para3:PNUTInfo):longint; cdecl;
     TSSFReleaseProc = procedure (para1:PFIELD); cdecl;
     
  procedure NWSSetFieldFunctionPtr(fp:PFIELD; 
              Format : TSSFFormat;
	      Control: TSSFControlFunc;
	      Verify : TSSFVerifyFunc;
	      Release: TSSFReleaseProc;
              Entry  : TSSFEntryProc; 
	      customDataRelease
	             : TSSFCustomDataReleaseProc);cdecl;external External_library name 'NWSSetFieldFunctionPtr';

  procedure NWSGetFieldFunctionPtr(
                fp:PFIELD; 
		var Format  : TSSFFormat;
		var Control : TSSFControlFunc;
		var Verify  : TSSFVerifyFunc;
		var Release : TSSFReleaseProc;
                var Entry   : TSSFEntryProc;
		var customDataRelease:TSSFCustomDataReleaseProc);cdecl;external External_library name 'NWSGetFieldFunctionPtr';

type TCompareFunc = function (el1:PLIST; el2:PLIST):longint; cdecl;
  procedure NWSSetDefaultCompare(handle:PNUTInfo; 
                                 defaultCompareFunction:TCompareFunc);cdecl;external External_library name 'NWSSetDefaultCompare';

  procedure NWSGetDefaultCompare(handle:PNUTInfo; var defaultCompareFunction:TCompareFunc);cdecl;external External_library name 'NWSGetDefaultCompare';

type TlistSortFunction = procedure (head:PLIST; tail:PLIST; handle:PNUTInfo); cdecl;
  { added in version 402  }
  procedure NWSSetListSortFunction(handle:PNUTInfo; 
                   listSortFunction:TlistSortFunction);cdecl;external External_library name 'NWSSetListSortFunction';

  { added in version 402  }
  procedure NWSGetListSortFunction(handle:PNUTInfo; 
                            var listSortFunction:TlistSortFunction);cdecl;external External_library name 'NWSGetListSortFunction';

  procedure NWSSetScreenPalette(newPalette:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSSetScreenPalette';

  function NWSGetScreenPalette(handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSGetScreenPalette';

  procedure NWSGetPCB(var _pPcb:PPCB; portalNumber:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSGetPCB';

type TentryProcedure = procedure (element:PLIST; displayLine:TLONG; handle:PNUTInfo); cdecl;
  procedure NWSSetListNotifyProcedure(el:PLIST; 
              entryProcedure:TentryProcedure);cdecl;external External_library name 'NWSSetListNotifyProcedure';

  procedure NWSGetListNotifyProcedure(el:PLIST; 
                                      var entryProcedure:TentryProcedure);cdecl;external External_library name 'NWSGetListNotifyProcedure';

type TcdReleaseProc = procedure (theData:pointer; handle:PNUTInfo); cdecl;
  procedure NWSSetHandleCustomData(handle:PNUTInfo; 
                                   customData:pointer; 
				   customDataRelease:TcdReleaseProc);cdecl;external External_library name 'NWSSetHandleCustomData';

  procedure NWSGetHandleCustomData(handle:PNUTInfo; 
                                   customData:Ppointer; 
				   customDataRelease:TcdReleaseProc);cdecl;external External_library name 'NWSGetHandleCustomData';

  procedure NWSSetErrorLabelDisplayFlag(flag:TLONG; 
                                        handle:PNUTInfo);cdecl;external External_library name 'NWSSetErrorLabelDisplayFlag';

  procedure NWSSetHelpHelp(helpIndex:TLONG; 
                           handle:PNUTInfo);cdecl;external External_library name 'NWSSetHelpHelp';

  { max length of passwordString, including NULL  }
  function NWSPromptForPassword(passwordHeader:TLONG; line:TLONG; column:TLONG; maxPasswordLen:TLONG; passwordString:PBYTE; 
             verifyEntry:TLONG; handle:PNUTInfo):TLONG;cdecl;external External_library name 'NWSPromptForPassword';

  { field flags  }
  { ptr to field text  }
  { including null  }
  { help for field  }
  { force password verification  }
  { fill character for field  }
  function NWSAppendPasswordField(line:TLONG; column:TLONG; width:TLONG; fflag:TLONG; data:PBYTE; 
             maxDataLen:TLONG; help:TLONG; verifyEntry:TLONG; passwordPortalHeader:TLONG; maskCharacter:TLONG; 
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendPasswordField';

  { field display width in form  }
  { field flags (NORMAL_FIELD, etc.)  }
  { ptr to field text  }
  { max len of data, allowing for null terminator  }
  { valid characters, if using EF_SET  }
  { NWSEditString flags (EF_UPPER etc.)  }
  function NWSAppendScrollableStringField(line:TLONG; column:TLONG; width:TLONG; fflag:TLONG; data:PBYTE; 
             maxLen:TLONG; cset:PBYTE; editFlags:TLONG; help:TLONG; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendScrollableStringField';

type TSSFInsertFunc = function (_string:PBYTE; maxLen:TLONG; parameters:pointer):longint; cdecl;
  procedure NWSSetScrollableFieldInsertProc(fp:PFIELD; insertProc:TSSFInsertFunc);cdecl;external External_library name 'NWSSetScrollableFieldInsertProc';

  { Returns 0 for success, -1 if none selected  }
  { if not NULL, returns portal number  }
  function NWSGetCurrentPortal(nutInfo:PNUTInfo; portalNumber:PLONG; var portal:PPCB):TLONG;cdecl;external External_library name 'NWSGetCurrentPortal';

  { if not NULL, returns PCB pointer  }
  function NWSWaitForKeyAndValue(handle:PNUTInfo; 
                                        nKeys:TLONG; 
					keyType:array of TLONG; 
					keyValue:array of TLONG):longint;cdecl;external External_library name 'NWSWaitForKeyAndValue';

  procedure NWSShowLineAttribute(line:TLONG; 
                                 column:TLONG; 
				 text:PBYTE; 
				 attribute:TLONG; 
				 length:TLONG; 
                                 screenID:PScreenStruct);cdecl;external External_library name 'NWSShowLineAttribute';

  procedure NWSShowLine(line:TLONG; column:TLONG; text:PBYTE; length:TLONG; screenID:PScreenStruct);cdecl;external External_library name 'NWSShowLine';

  procedure NWSScrollZone(line:TLONG; column:TLONG; height:TLONG; width:TLONG; attribute:TLONG; 
              count:TLONG; direction:TLONG; screenID:PScreenStruct);cdecl;external External_library name 'NWSScrollZone';

  procedure NWSSaveZone(line:TLONG; column:TLONG; height:TLONG; width:TLONG; buffer:PBYTE; 
              screenID:PScreenStruct);cdecl;external External_library name 'NWSSaveZone';

  procedure NWSRestoreZone(line:TLONG; column:TLONG; height:TLONG; width:TLONG; buffer:PBYTE; 
              screenID:PScreenStruct);cdecl;external External_library name 'NWSRestoreZone';

  procedure NWSRestoreDisplay(screenID:PScreenStruct);cdecl;external External_library name 'NWSRestoreDisplay';

  procedure NWSPositionCursor(line:TLONG; column:TLONG; screenID:PScreenStruct);cdecl;external External_library name 'NWSPositionCursor';

  procedure NWSGetNUTVersion(majorVersion:PLONG; minorVersion:PLONG; revision:PLONG);cdecl;external External_library name 'NWSGetNUTVersion';

  procedure NWSSetFormRepaintFlag(value:TLONG; handle:PNUTInfo);cdecl;external External_library name 'NWSSetFormRepaintFlag';

  procedure NWSSetFormNoWrap(handle:PNUTInfo);cdecl;external External_library name 'NWSSetFormNoWrap';

  function NWSViewTextWithScrollBars(centerLine:TLONG; centerColumn:TLONG; height:TLONG; width:TLONG; headerNumber:TLONG; 
             textBuffer:PBYTE; maxBufferLength:TLONG; scrollBarFlag:TLONG; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSViewTextWithScrollBars';

  { length of document  }
  function NWSEditTextWithScrollBars(centerLine:TLONG; centerColumn:TLONG; height:TLONG; width:TLONG; headerNumber:TLONG; 
             textBuffer:PBYTE; maxBufferLength:TLONG; confirmMessage:TLONG; forceConfirm:TLONG; scrollBarFlag:TLONG; 
             handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditTextWithScrollBars';


implementation

  { was #define dname def_expr }
  function F_H1 : longint;
      { return type might be wrong }
      begin
         F_H1:=NWSGetLineDrawCharacter(0);
      end;

  { was #define dname def_expr }
  function F_H2 : longint;
      { return type might be wrong }
      begin
         F_H2:=NWSGetLineDrawCharacter(1);
      end;

  { was #define dname def_expr }
  function F_V1 : longint;
      { return type might be wrong }
      begin
         F_V1:=NWSGetLineDrawCharacter(2);
      end;

  { was #define dname def_expr }
  function F_V2 : longint;
      { return type might be wrong }
      begin
         F_V2:=NWSGetLineDrawCharacter(3);
      end;

  { was #define dname def_expr }
  function F_UL1 : longint;
      { return type might be wrong }
      begin
         F_UL1:=NWSGetLineDrawCharacter(4);
      end;

  { was #define dname def_expr }
  function F_UR1 : longint;
      { return type might be wrong }
      begin
         F_UR1:=NWSGetLineDrawCharacter(5);
      end;

  { was #define dname def_expr }
  function F_LL1 : longint;
      { return type might be wrong }
      begin
         F_LL1:=NWSGetLineDrawCharacter(6);
      end;

  { was #define dname def_expr }
  function F_LR1 : longint;
      { return type might be wrong }
      begin
         F_LR1:=NWSGetLineDrawCharacter(7);
      end;

  { was #define dname def_expr }
  function F_UL2 : longint;
      { return type might be wrong }
      begin
         F_UL2:=NWSGetLineDrawCharacter(8);
      end;

  { was #define dname def_expr }
  function F_UR2 : longint;
      { return type might be wrong }
      begin
         F_UR2:=NWSGetLineDrawCharacter(9);
      end;

  { was #define dname def_expr }
  function F_LL2 : longint;
      { return type might be wrong }
      begin
         F_LL2:=NWSGetLineDrawCharacter(10);
      end;

  { was #define dname def_expr }
  function F_LR2 : longint;
      { return type might be wrong }
      begin
         F_LR2:=NWSGetLineDrawCharacter(11);
      end;

  { was #define dname def_expr }
  function F_UT1 : longint;
      { return type might be wrong }
      begin
         F_UT1:=NWSGetLineDrawCharacter(12);
      end;

  { was #define dname def_expr }
  function F_DT1 : longint;
      { return type might be wrong }
      begin
         F_DT1:=NWSGetLineDrawCharacter(13);
      end;

  { was #define dname def_expr }
  function F_LT1 : longint;
      { return type might be wrong }
      begin
         F_LT1:=NWSGetLineDrawCharacter(14);
      end;

  { was #define dname def_expr }
  function F_RT1 : longint;
      { return type might be wrong }
      begin
         F_RT1:=NWSGetLineDrawCharacter(15);
      end;

  { was #define dname def_expr }
  function F_UT2 : longint;
      { return type might be wrong }
      begin
         F_UT2:=NWSGetLineDrawCharacter(24);
      end;

  { was #define dname def_expr }
  function F_DT2 : longint;
      { return type might be wrong }
      begin
         F_DT2:=NWSGetLineDrawCharacter(25);
      end;

  { was #define dname def_expr }
  function F_LT2 : longint;
      { return type might be wrong }
      begin
         F_LT2:=NWSGetLineDrawCharacter(26);
      end;

  { was #define dname def_expr }
  function F_RT2 : longint;
      { return type might be wrong }
      begin
         F_RT2:=NWSGetLineDrawCharacter(27);
      end;

  { was #define dname def_expr }
  function F_X1 : longint;
      { return type might be wrong }
      begin
         F_X1:=NWSGetLineDrawCharacter(36);
      end;

  { was #define dname def_expr }
  function F_X2 : longint;
      { return type might be wrong }
      begin
         F_X2:=NWSGetLineDrawCharacter(39);
      end;

  { was #define dname def_expr }
  function F_UP : longint;
      { return type might be wrong }
      begin
         F_UP:=NWSGetLineDrawCharacter(40);
      end;

  { was #define dname def_expr }
  function F_DOWN : longint;
      { return type might be wrong }
      begin
         F_DOWN:=NWSGetLineDrawCharacter(41);
      end;

  { was #define dname def_expr }
  function F_LEFT : longint;
      { return type might be wrong }
      begin
         F_LEFT:=NWSGetLineDrawCharacter(42);
      end;

  { was #define dname def_expr }
  function F_RIGHT : longint;
      { return type might be wrong }
      begin
         F_RIGHT:=NWSGetLineDrawCharacter(43);
      end;

  { was #define dname def_expr }
  function F_BG1 : longint;
      { return type might be wrong }
      begin
         F_BG1:=NWSGetLineDrawCharacter(44);
      end;

  { was #define dname def_expr }
  function F_BG2 : longint;
      { return type might be wrong }
      begin
         F_BG2:=NWSGetLineDrawCharacter(45);
      end;

  { was #define dname def_expr }
  function F_BG3 : longint;
      { return type might be wrong }
      begin
         F_BG3:=NWSGetLineDrawCharacter(46);
      end;

  { was #define dname def_expr }
  function F_BG4 : longint;
      { return type might be wrong }
      begin
         F_BG4:=NWSGetLineDrawCharacter(47);
      end;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }   
  function IS_DYNAMIC_MESSAGE(a : longint) : boolean;
    begin
       IS_DYNAMIC_MESSAGE:=(a > $fff0) and (a < $ffff);
    end;


end.

{
  $Log$
  Revision 1.1  2003-02-16 17:45:08  armin
  * added nwsnut, nwconio and nwthreads for netware

  
}