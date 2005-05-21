{
    This file is part of the Free Pascal run time library
    for Netware.
    Copyright (c) 1999-2005 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit nwsnut;

interface

{$mode objfpc}
{$if defined (netware_clib)}
uses nwserv;
{$else}
uses libc;
{$endif}

{$PACKRECORDS C}

  const
    External_library='nwsnut';

  { constants
    the constant CURRENT_NUT_VERSION is incremented when increased
    functionality is added. An NLM can check this value which is placed
    in the NUTInfo structure, version field, to determine if the NWSNUT
    NLM contains sufficient functionality to support its requirements  }


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
     SEVERITY_INFORM  = 1;
     SEVERITY_WARNING = 2;
     SEVERITY_FATAL   = 3;
  { text size minimization styles  }
     SNORMAL    = 0;
     SMINWIDTH  = 1;
     SMINHEIGHT = 2;
  { palettes to set screen colors.
        background and foreground can be reversed with VREVERSE }
     BW_PALETTE      = 0;  // white and black
     NORMAL_PALETTE  = 1;  // white and dark blue
     INIT_PALETTE    = 2;  // light blue and dark blue
     HELP_PALETTE    = 3;  // green and black
     ERROR_PALETTE   = 4;  // red and black
     WARNING_PALETTE = 5;  // pink and white
     OTHER_PALETTE   = 6;  // green and red
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

          function F_H1 : longint;
  { Í  }  function F_H2 : longint;
  { ³  }  function F_V1 : longint;
  { º  }  function F_V2 : longint;
  { Ú  }  function F_UL1 : longint;
  { ¿  }  function F_UR1 : longint;
  { À  }  function F_LL1 : longint;
  { Ù  }  function F_LR1 : longint;
  { É  }  function F_UL2 : longint;
  { »  }  function F_UR2 : longint;
  { È  }  function F_LL2 : longint;
  { ¼  }  function F_LR2 : longint;
  { Á  }  function F_UT1 : longint;
  { Â  }  function F_DT1 : longint;
  { ´  }  function F_LT1 : longint;
  { Ã  }  function F_RT1 : longint;
  { Ê  }  function F_UT2 : longint;
  { Ë  }  function F_DT2 : longint;
  { ¹  }  function F_LT2 : longint;
  { Ì  }  function F_RT2 : longint;
  { Å  }  function F_X1 : longint;
  { Î  }  function F_X2 : longint;
  {   }  function F_UP : longint;
  {   }  function F_DOWN : longint;
  {   }  function F_LEFT : longint;
  {   }  function F_RIGHT : longint;
  { °  }  function F_BG1 : longint;
  { ±  }  function F_BG2 : longint;
  { ²  }  function F_BG3 : longint;
  { Û  }  function F_BG4 : longint;


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

  function IS_DYNAMIC_MESSAGE(a : longint) : boolean;


  type
    PNUTInfo_ = ^TNUTInfo_;

     PPCB_ = ^TPCB_;
     TPCB_ = record
          frameLine : longint;
          frameColumn : longint;
          frameHeight : longint;
          frameWidth : longint;
          virtualHeight : longint;
          virtualWidth : longint;
          cursorState : longint;
          borderType : longint;
          borderAttribute : longint;
          saveFlag : word;
          secondarySaveFlag : word;
          directFlag : longint;
          headerAttribute : longint;
          portalLine : longint;
          portalColumn : longint;
          portalHeight : longint;
          portalWidth : longint;
          virtualLine : longint;
          virtualColumn : longint;
          cursorLine : longint;
          cursorColumn : longint;
          firstUpdateFlag : longint;
          headerText : pchar;
          headerText2 : pchar;
          virtualScreen : pointer;
          saveScreen : pointer;
          screenID : TScr;
          nutInfo : PNUTInfo_;
          sequenceNumber : longint;
          reserved1 : longint;
          mtflags : longint;
          borderPalette : longint;
          showScrollBars : longint;
          lastLine : longint;
          longestLineLen : longint;
          verticalScroll : longint;
          horizontalScroll : longint;
          oldVertical : longint;
          oldHorizontal : longint;
          deHighlightFunction : procedure (para1:PNUTInfo_; para2:PPCB_);cdecl;
          reHighlightFunction : procedure (para1:PNUTInfo_; para2:PPCB_); cdecl;
          reportPortalUpdate : procedure (para1:PPCB_; para2:PNUTInfo_; updateType:longint); cdecl;
       end;
     TPCB = TPCB_;
     PPCB = ^TPCB;

     PHS_ = ^THS_;
     THS_ = record
          nextScreen      : longint;
          previousScreen  : longint;
          frameLine       : longint;
          frameColumn     : longint;
          frameHeight     : longint;
          frameWidth      : longint;
          virtualHeight   : longint;
          virtualWidth    : longint;
          cursorState     : longint;
          borderType      : longint;
          borderAttribute : longint;
          saveFlag        : longint;
          directFlag      : longint;
          headerAttribute : longint;
          headerText      : pchar;
          text            : pchar;
       end;
     THELP_SCREEN = THS_;
     PHELP_SCREEN = ^THELP_SCREEN;

     PLIST_STRUCT = ^TLIST_STRUCT;
     TLIST_STRUCT = record
          prev           : PLIST_STRUCT;
          next           : PLIST_STRUCT;
          otherInfo      : pointer;
          marked         : longint;
          flags          : word;
          maxSkew        : word;
          entryProcedure : procedure (listElement:PLIST_STRUCT; displayLine:longint; NUTInfoStructure:pointer);cdecl;
          extra          : longint;
          text           : array[0..0] of char;
       end;
     TLIST = TLIST_STRUCT;
     PLIST = ^TLIST;
     PPLIST= ^PLIST;

     PLP_ = ^TLP_;
     TLP_ = record
          head          : pointer;
          tail          : pointer;
          sortProc      : function :longint;cdecl;
          freeProcedure : procedure (memoryPointer:pointer);
       end;
     TLISTPTR = TLP_;
     PLISTPTR = ^TLISTPTR;

     PMI_ = ^TMI_;
     TMI_ = record
          dynamicMessageOne      : pchar;
          dynamicMessageTwo      : pchar;
          dynamicMessageThree    : pchar;
          dynamicMessageFour     : pchar;
          dynamicMessageFive     : pchar;
          dynamicMessageSix      : pchar;
          dynamicMessageSeven    : pchar;
          dynamicMessageEight    : pchar;
          dynamicMessageNine     : pchar;
          dynamicMessageTen      : pchar;
          dynamicMessageEleven   : pchar;
          dynamicMessageTwelve   : pchar;
          dynamicMessageThirteen : pchar;
          dynamicMessageFourteen : pchar;
          messageCount : longint;
          programMesgTable : ppchar;
       end;
     TMessageInfo = TMI_;
     PMessageInfo = ^TMessageInfo;

     PINT_ = ^TINT_;
     TINT_ = record
          interruptProc : procedure (handle:pointer);cdecl;
          key : longint;
       end;
     TINTERRUPT = TINT_;
     PINTERRUPT = ^TINTERRUPT;

     PMP_ = ^TMP_;
     TMP_ = record
          listAction : function (option:longint; parameter:pointer):longint;cdecl;
          parameter  : pointer;
       end;
     TMENU_PARAMETER = TMP_;
     PMENU_PARAMETER = ^TMENU_PARAMETER;
  { environment structure  }
  { always leave the following fields at the end of the struct.  They
          should never be referenced directly by an application  }


     TNUTInfo_ = record
          portal : array[0..(MAXPORTALS)-1] of PPCB;
          currentPortal : longint;
          headerHeight : longint;
          waitFlag : longint;
          listStack : array[0..(MAXLISTS)-1] of TLISTPTR;
          saveStack : array[0..(SAVELISTS)-1] of TLISTPTR;
          nextAvailList : longint;
          head : PLIST;
          tail : PLIST;
          defaultCompareFunction : function (el1:PLIST; el2:PLIST):longint;cdecl;
          freeProcedure : procedure (memoryPointer:pointer);
          interruptTable : array[0..(MAXFUNCTIONS)-1] of procedure ;
          functionKeyStatus : array[0..(MAXACTIONS)-1] of longint;
          messages : TMessageInfo;
          helpContextStack : array[0..(MAXHELP)-1] of longint;
          currentPreHelpMessage : longint;
          freeHelpSlot : longint;
          redisplayFormFlag : longint;
          preHelpPortal : longint;
          helpActive : smallint;
          errorDisplayActive : smallint;
          helpPortal : longint;
          waitPortal : longint;
          errorPortal : longint;
          resourceTag : pointer;
          screenID : pointer;
          helpScreens : pointer;
          helpOffset : longint;
          helpHelp : longint;
          allocChain : pointer;
          version : longint;
          reserved : array[0..9] of longint;
          moduleHandle : longint;
          customData : pointer;
          customDataRelease : procedure (theData:pointer; thisStructure:PNUTInfo_); cdecl;
          displayErrorLabel : longint;
          markBuffer : pchar;
          markBufferLength : longint;
          editBuffer : pchar;
          editBufferLength : longint;
          staticFlag : longint;
          processID : longint;
          mtflags : longint;
          saveCurrentPortal : longint;
          palette : longint;
          nutDataHandle : pointer;
          next : PNUTInfo_;
          prev : PNUTInfo_;
          listSortFunction : procedure (head:PLIST; tail:PLIST; thisStructure:PNUTInfo_); cdecl;
          compatibilityLevel : longint;
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
          headernum : longint;
          centerLine : longint;
          centerColumn : longint;
          maxoptlen : longint;
          action : function (option:longint; parameter:pointer):longint;cdecl;
          arg1 : longint;
          arg2 : longint;
          arg3 : longint;
          arg4 : longint;
          arg5 : longint;
          arg6 : longint;
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
          fieldFlags : longint;
          fieldLine : longint;
          fieldColumn : longint;
          fieldWidth : longint;
          fieldAttribute : longint;
          fieldActivateKeys : longint;
          fieldFormat : procedure (field:Pfielddef; text:pchar; buffLen:longint);cdecl;
          fieldControl : function (field:Pfielddef; selectKey:longint; fieldChanged:Plongint; handle:PNUTInfo):longint;
          fieldVerify : function (field:Pfielddef; data:pchar; handle:PNUTInfo):longint;
          fieldRelease : procedure (para1:Pfielddef);
          fieldData : pchar;
          fieldXtra : pchar;
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
  { Structures used for DisplayErrorCondition }

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

  function NWSInitializeNut
                (utility,
                 version,
                 headerType,
                 compatibilityLevel : longint;
                 messageTable       : PPchar;
                 helpScreens        : pchar;
                 screenID           : TScr;     // Clib/OS Screen Id
                 resourceTag        : TRtag;    // OS ResourceTagStructure
             var handle             : PNUTInfo) : longint;cdecl;external External_library name 'NWSInitializeNut';

  procedure NWSScreenSize(maxLines,maxColumns:plongint);cdecl;external External_library name 'NWSScreenSize';
  procedure NWSScreenSize(var maxLines,maxColumns:longint);cdecl;external External_library name 'NWSScreenSize';

  procedure NWSShowPortalLine(line,column:longint; text:pchar; length:longint; portal:PPCB);cdecl;external External_library name 'NWSShowPortalLine';

  procedure NWSShowPortalLineAttribute(line,column:longint; text:pchar; attribute,length:longint;
              portal:PPCB);cdecl;external External_library name 'NWSShowPortalLineAttribute';

  procedure NWSScrollPortalZone(line,column,height,width,attribute,
              count,direction:longint; portal:PPCB);cdecl;external External_library name 'NWSScrollPortalZone';

  procedure NWSFillPortalZone(line,column,height,width,fillCharacter,
              fillAttribute:longint; portal:PPCB);cdecl;external External_library name 'NWSFillPortalZone';

  procedure NWSFillPortalZoneAttribute(line,column,height,width,attribute:longint;
              portal:PPCB);cdecl;external External_library name 'NWSFillPortalZoneAttribute';

  function NWSGetMessage(message:longint; messages:PMessageInfo):pchar;cdecl;external External_library name 'NWSGetMessage';

  procedure NWSSetDynamicMessage(message:longint; text:pchar; messages:PMessageInfo);cdecl;external External_library name 'NWSSetDynamicMessage';
  procedure NWSSetDynamicMessage(message:longint; text:pchar; var messages:TMessageInfo);cdecl;external External_library name 'NWSSetDynamicMessage';

  function NWSCreatePortal
                (line,
                 column,
                 frameHeight,
                 frameWidth,
                 virtualHeight,
                 virtualWidth,
                 saveFlag:longint;
                 headerText:pchar;
                 headerAttribute,
                 borderType,
                 borderAttribute,
                 cursorFlag,
                 directFlag : longint;
                 handle     : PNUTInfo) : longint;cdecl;external External_library name 'NWSCreatePortal';

  procedure NWSDestroyPortal(portalNumber:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyPortal';

  procedure NWSPositionPortalCursor(line:longint; column:longint; portal:PPCB);cdecl;external External_library name 'NWSPositionPortalCursor';

  procedure NWSEnablePortalCursor(portal:PPCB);cdecl;external External_library name 'NWSEnablePortalCursor';

  procedure NWSDisablePortalCursor(portal:PPCB);cdecl;external External_library name 'NWSDisablePortalCursor';

  procedure NWSDeselectPortal(handle:PNUTInfo);cdecl;external External_library name 'NWSDeselectPortal';

  procedure NWSSelectPortal(portalNumber:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSSelectPortal';

  function NWSComputePortalPosition(centerLine:longint; centerColumn:longint; height:longint; width:longint; line:plongint;
             column:plongint; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSComputePortalPosition';

  procedure NWSClearPortal(portal:PPCB);cdecl;external External_library name 'NWSClearPortal';

type TFreeRoutine = procedure (memoryPointer:pointer); cdecl;

  procedure NWSInitList(handle:PNUTInfo; freeRoutine:TFreeRoutine);cdecl;external External_library name 'NWSInitList';

  function NWSPushList(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSPushList';

  function NWSPopList(handleNWS:PNUTInfo):longint;cdecl;external External_library name 'NWSPopList';

  function NWSSaveList(listIndex:longint; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSSaveList';

  function NWSRestoreList(listIndex:longint; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSRestoreList';

  procedure NWSDestroyList(handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyList';

  procedure NWSDestroyMenu(handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyMenu';

  procedure NWSDestroyForm(handle:PNUTInfo);cdecl;external External_library name 'NWSDestroyForm';

  function NWSAppendToList(text:pchar; otherInfo:pointer; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSAppendToList';

  function NWSDeleteFromList(el:PLIST; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSDeleteFromList';

  function NWSInsertInList(text:pchar; otherInfo:pointer; atElement:PLIST; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSInsertInList';

  function NWSGetListElementText(element:PLIST):pchar;cdecl;external External_library name 'NWSGetListElementText';

  function NWSGetListHead(handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSGetListHead';

  function NWSGetListTail(handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSGetListTail';

  procedure NWSUnmarkList(handle:PNUTInfo);cdecl;external External_library name 'NWSUnmarkList';

  procedure NWSSetList(listPtr:PLISTPTR; handle:PNUTInfo);cdecl;external External_library name 'NWSSetList';

  procedure NWSGetList(listPtr:PLISTPTR; handle:PNUTInfo);cdecl;external External_library name 'NWSGetList';

  function NWSIsAnyMarked(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSIsAnyMarked';

  procedure NWSPushMarks(handle:PNUTInfo);cdecl;external External_library name 'NWSPushMarks';

  procedure NWSPopMarks(handle:PNUTInfo);cdecl;external External_library name 'NWSPopMarks';

  procedure NWSSortList(handle:PNUTInfo);cdecl;external External_library name 'NWSSortList';

  procedure NWSInitMenu(handle:PNUTInfo);cdecl;external External_library name 'NWSInitMenu';

  procedure NWSInitForm(handle:PNUTInfo);cdecl;external External_library name 'NWSInitForm';

  function NWSGetSortCharacter(charIndex:longint):longint;cdecl;external External_library name 'NWSGetSortCharacter';

  function NWSGetLineDrawCharacter(charIndex:longint):longint;cdecl;external External_library name 'NWSGetLineDrawCharacter';

  function NWSStrcat(_string, newStuff:pchar):longint;cdecl;external External_library name 'NWSStrcat';

  procedure NWSMemmove(dest:pointer; source:pointer; len:longint);cdecl;external External_library name 'NWSMemmove';

  function NWSToupper(ch:char):char;cdecl;external External_library name 'NWSToupper';

  function NWSIsdigit(ch:char):longbool;cdecl;external External_library name 'NWSIsdigit';

  function NWSIsxdigit(ch:char):longbool;cdecl;external External_library name 'NWSIsxdigit';

  function NWSAsciiToInt(data:pchar):longint;cdecl;external External_library name 'NWSAsciiToInt';

  function NWSAsciiToLONG(data:pchar):longint;cdecl;external External_library name 'NWSAsciiToLONG';

  function NWSAsciiHexToInt(data:pchar):longint;cdecl;external External_library name 'NWSAsciiHexToInt';

  procedure NWSWaitForEscape(handle:PNUTInfo);cdecl;external External_library name 'NWSWaitForEscape';

  function NWSWaitForEscapeOrCancel(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSWaitForEscapeOrCancel';

  procedure NWSGetKey(_type:plongint; value:pchar; handle:PNUTInfo);cdecl;external External_library name 'NWSGetKey';
  procedure NWSGetKey(var _type:longint; value:pchar; handle:PNUTInfo);cdecl;external External_library name 'NWSGetKey';

  function NWSKeyStatus(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSKeyStatus';

  function NWSUngetKey(_type:longint; value:longint; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSUngetKey';

  procedure NWSEnableFunctionKey(key:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableFunctionKey';

  procedure NWSDisableFunctionKey(key:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSDisableFunctionKey';

  procedure NWSDisableInterruptKey(key:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSDisableInterruptKey';

type TInterruptProc = procedure (handle:pointer); cdecl;

  procedure NWSEnableInterruptKey(key:longint; interruptProc:TInterruptProc; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableInterruptKey';

  procedure NWSSaveFunctionKeyList(keyList:pchar; handle:PNUTInfo);cdecl;external External_library name 'NWSSaveFunctionKeyList';

  procedure NWSEnableFunctionKeyList(keyList:pchar; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableFunctionKeyList';

  procedure NWSSaveInterruptList(interruptList:PINTERRUPT; handle:PNUTInfo);cdecl;external External_library name 'NWSSaveInterruptList';

  procedure NWSEnableInterruptList(interruptList:PINTERRUPT; handle:PNUTInfo);cdecl;external External_library name 'NWSEnableInterruptList';

  procedure NWSDisableAllInterruptKeys(handle:PNUTInfo);cdecl;external External_library name 'NWSDisableAllInterruptKeys';

  procedure NWSDisableAllFunctionKeys(handle:PNUTInfo);cdecl;external External_library name 'NWSDisableAllFunctionKeys';

  procedure NWSEnableAllFunctionKeys(handle:PNUTInfo);cdecl;external External_library name 'NWSEnableAllFunctionKeys';

  function NWSDisplayTextInPortal(line,indentLevel:longint; text:pchar; attribute:longint; portal:PPCB):longint;cdecl;external External_library name 'NWSDisplayTextInPortal';

  function NWSDisplayInformation(header,pauseFlag,centerLine,centerColumn,palette,
             attribute:longint; displayText:pchar; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSDisplayInformation';

  procedure NWSStartWait(centerLine,centerColumn:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSStartWait';

  procedure NWSEndWait(handle:PNUTInfo);cdecl;external External_library name 'NWSEndWait';

  function NWSAlert(centerLine,centerColumn:longint; handle:PNUTInfo; message:longint; args:array of const):longint;cdecl;external External_library name 'NWSAlert';

  function NWSAlert(centerLine,centerColumn:longint; handle:PNUTInfo; message:longint):longint;cdecl;external External_library name 'NWSAlert';

  function NWSAlertWithHelp(centerLine,centerColumn:longint; handle:PNUTInfo; message,helpContext:longint;
             args:array of const):longint;cdecl;external External_library name 'NWSAlertWithHelp';

  function NWSAlertWithHelp(centerLine,centerColumn:longint; handle:PNUTInfo; message:longint; helpContext:longint):longint;cdecl;external External_library name 'NWSAlertWithHelp';

  function NWSTrace(handle:PNUTInfo; message:pchar; args:array of const):longint;cdecl;external External_library name 'NWSTrace';
  function NWSTrace(handle:PNUTInfo; message:pchar):longint;cdecl;external External_library name 'NWSTrace';

  procedure NWSDisplayErrorText(message:longint; severity:longint; handle:PNUTInfo; args:array of const);cdecl;external External_library name 'NWSDisplayErrorText';

  procedure NWSDisplayErrorText(message:longint; severity:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayErrorText';

  procedure NWSDisplayErrorCondition(procedureName:pchar; errorCode:longint; severity:longint; errorList:PPROCERROR; handle:PNUTInfo;
              args:array of const);cdecl;external External_library name 'NWSDisplayErrorCondition';

  procedure NWSDisplayErrorCondition(procedureName:pchar; errorCode:longint; severity:longint; errorList:PPROCERROR; handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayErrorCondition';

  function NWSAppendToMenu(message:longint; option:longint; handle:PNUTInfo):PLIST;cdecl;external External_library name 'NWSAppendToMenu';

type TActionFunc = function (option:longint; parameter:pointer) : longint; cdecl;

  function NWSMenu(header,
                   centerLine,
                   centerColumn:longint;
                   defaultElement:PLIST;
                   action:TActionFunc;
                   handle:PNUTInfo;
                   actionParameter:pointer):longint;cdecl;external External_library name 'NWSMenu';

  function NWSConfirm(header,centerLine,centerColumn,defaultChoice:longint;
                      action:TActionFunc;
                      handle:PNUTInfo;
                      actionParameter:pointer):longint;cdecl;external External_library name 'NWSConfirm';

  function NWSPushHelpContext(helpContext:longint; handle:PNUTInfo):longint;cdecl;external External_library name 'NWSPushHelpContext';

  function NWSPopHelpContext(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSPopHelpContext';

type TFormatFunc=function (element:PLIST; skew:longint; displayLine:pchar; width:longint):longint; cdecl;
     TNWSListActionFunc=function (keyPressed:longint; elementSelected:PPLIST; itemLineNumber:plongint; actionParameter:pointer):longint;  cdecl;

  function NWSList(header:longint; centerLine:longint; centerColumn:longint; height:longint; width:longint;
             validKeyFlags:longint; element:PPLIST; handle:PNUTInfo; format:TFormatFunc; action:TNWSListActionFunc;
             actionParameter:pointer):longint;cdecl;external External_library name 'NWSList';

type TInsertFunc = function (text:pchar; otherInfo:Ppointer; parameters:pointer):longint; cdecl;
     TFreeProcedure=function (otherInfo:pointer):longint; cdecl;

  function NWSInsertInPortalList(currentElement:PPLIST; currentLine:plongint; InsertProcedure:TInsertFunc; FreeProcedure:TFreeProcedure; handle:PNUTInfo;
             parameters:pointer):longint;cdecl;external External_library name 'NWSInsertInPortalList';

type TModifyProcedure=function (text:pchar; parameters:pointer):longint;  cdecl;

  function NWSModifyInPortalList(currentElement:PPLIST; currentLine:plongint; ModifyProcedure:TModifyProcedure; handle:PNUTInfo; parameters:pointer):longint;cdecl;external External_library name 'NWSModifyInPortalList';

type TDeleteFunc = function (el:PLIST; handle:PNUTInfo; parameters:pointer):PLIST; cdecl;
  function NWSDeleteFromPortalList(currentElement:PPLIST; currentLine:plongint; DeleteProcedure:TDeleteFunc; deleteCurrentHeader:longint; deleteMarkedHeader:longint;
             handle:PNUTInfo; parameters:pointer):longint;cdecl;external External_library name 'NWSDeleteFromPortalList';

type TNWSEditInsertFunc=function (buffer:pchar; maxLen:longint; parameters:pointer):longint; cdecl;
     TNWSEditActionFunc=function (action:longint; buffer:pchar; parameters:pointer):longint; cdecl;

  function NWSEditString(
             centerLine, centerColumn, editHeight, editWidth, header,
             prompt :longint;
             buf:pchar;
             maxLen, _type:longint; handle:PNUTInfo;
             insertProc:TNWSEditInsertFunc;
             actionProc:TNWSEditActionFunc;
             parameters:pointer):longint;cdecl;external External_library name 'NWSEditString';

  function NWSAppendIntegerField
             (line, column, fflag:longint; data:Plongint;
              minimum, maximum, help:longint;
              handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendIntegerField';

  function NWSAppendIntegerField
             (line, column, fflag:longint; var data:longint;
              minimum, maximum, help:longint;
              handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendIntegerField';

  function NWSAppendUnsignedIntegerField
              (line,
               column,
               fflag:longint;
               data:plongint;
               minimum, maximum, help:longint;
               handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendUnsignedIntegerField';

  function NWSAppendUnsignedIntegerField
              (line,
               column,
               fflag:longint;
               var data:cardinal;
               minimum, maximum, help:longint;
               handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendUnsignedIntegerField';

  function NWSAppendHexField
              (line,column,fflag:longint;
               data:Plongint;
               minimum, maximum, help:longint;
               handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendHexField';

  procedure NWSDisplayPreHelp
              (line, column, message:longint;
               handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayPreHelp';

  procedure NWSRemovePreHelp
              (handle:PNUTInfo);cdecl;external External_library name 'NWSRemovePreHelp';

  function  NWSGetADisk
              (volName,prompt:pchar;
               handle:PNUTInfo):longint;cdecl;external External_library name 'NWSGetADisk';

  procedure NWSInitListPtr(listPtr:PLISTPTR);cdecl;external External_library name 'NWSInitListPtr';

  function NWSEditForm
              (headernum,
               line,
               col,
               portalHeight,
               portalWidth,
               virtualHeight,
               virtualWidth,
               ESCverify,
               forceverify,
               confirmMessage : longint;
               handle         : PNUTInfo):longint;cdecl;external External_library name 'NWSEditForm';

  function NWSEditPortalFormField
              (header,
               cline,
               ccol,
               formHeight,
               formWidth,
               controlFlags:longint;
               formHelp:CARDINAL;
               confirmMessage:longint;
               startField:PFIELD;
               handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditPortalFormField';

  function NWSEditPortalForm
              (header,
               centerLine,
               centerColumn,
               formHeight,
               formWidth,
               controlFlags:longint;
               formHelp:CARDINAL;
               confirmMessage:longint;
               handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditPortalForm';

type TfFormat  = procedure (field:Pfielddef; text:pchar; buffLen:longint); cdecl;
     TfControl = function (field:Pfielddef; selectKey:longint; var fieldChanged:longint; handle:PNUTInfo):longint; cdecl;
     TfVerify  = function (field:Pfielddef; data:pointer; handle:PNUTInfo):longint; cdecl;
     TfRelease = procedure (field:Pfielddef); cdecl;

  { Data & Xtra field release routine  }
  function NWSAppendToForm(
             fline,
             fcol,
             fwidth,
             fattr:longint;
             fFormat:TfFormat;
             fControl:TfControl;
             fVerify:TfVerify;
             fRelease:TfRelease;
             fData:pointer;
             fXtra:pointer;
             fflags:longint;
             fActivateKeys:longint;
             fhelp:longint;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendToForm';

  function NWSAppendPromptField(line,column,promptnum:longint; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendPromptField';

  function NWSAppendCommentField(line,column:longint; prompt:pchar; handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendCommentField';

  function NWSAppendStringField
            (line,
             column,
             width,
             fflag:longint;
             data,cset:pchar;
             help:longint;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendStringField';

  function NWSAppendBoolField
            (line,
             column,
             fflag:longint;
             data:pointer;
             help:longint;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendBoolField';

  function NWSAppendBoolField
            (line,
             column,
             fflag:longint;
         var data:longbool;
             help:longint;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendBoolField';

  function NWSAppendGenericBoolField
            (line,
             column,
             fflag:longint;
             data:pointer;
             help:longint;
             yesString, noString:pchar;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendGenericBoolField';

  function NWSAppendGenericBoolField
            (line,
             column,
             fflag:longint;
         var data:longbool;
             help:longint;
             yesString, noString:pchar;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendGenericBoolField';

type TSpotActionFunc = function (fp:PFIELD; selectKey:longint; var changedField:longint; handle:PNUTInfo):longint; cdecl;
  function NWSAppendHotSpotField
            (line,
             column,
             fflag:longint;
             displayString:pchar;
             SpotAction:TSpotActionFunc;
             handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendHotSpotField';

  function NWSInitMenuField
            (headermsg,
             cLine,
             cCol:longint;
             action:TActionFunc;
             nutInfo:PNUTInfo;
             args:array of const):PMFCONTROL;cdecl;external External_library name 'NWSInitMenuField';

  function NWSInitMenuField
            (headermsg,
             cLine,
             cCol:longint;
             action:TActionFunc;
             nutInfo:PNUTInfo):PMFCONTROL;cdecl;external External_library name 'NWSInitMenuField';

  function NWSAppendToMenuField
            (m:PMFCONTROL;
             optiontext:longint;
             option:longint;
             nutInfo:PNUTInfo):longint;cdecl;external External_library name 'NWSAppendToMenuField';

  function NWSAppendMenuField
            (line,
             column,
             fflag:longint;
             data:Plongint;
             m:PMFCONTROL;
             help:longint;
             nutInfo:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendMenuField';

   function NWSAppendMenuField
            (line,
             column,
             fflag:longint;
         var data:longint;
             m:PMFCONTROL;
             help:longint;
             nutInfo:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendMenuField';

  function NWSEditText
            (centerLine,
             centerColumn,
             height,
             width,
             headerNumber:longint;
             textBuffer:pchar;
             maxBufferLength,
             confirmMessage,
             forceConfirm:longint;
             handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditText';

  function NWSViewText
            (centerLine,
             centerColumn,
             height,
             width,
             headerNumber:longint;
             textBuffer:pchar;
             maxBufferLength:longint;
             handle:PNUTInfo):longint;cdecl;external External_library name 'NWSViewText';

  procedure NWSDisplayHelpScreen
            (offset:longint;
             handle:PNUTInfo);cdecl;external External_library name 'NWSDisplayHelpScreen';

  // Allocates memory for NWSNUT purposes
  function NWSAlloc
            (numberOfBytes:longint;
             handle:PNUTInfo):pointer;cdecl;external External_library name 'NWSAlloc';

  procedure NWSFree
            (address:pointer;
             handle:PNUTInfo);cdecl;external External_library name 'NWSFree';

  // Displays justified text in an existing portal
  function NWSDisplayTextJustifiedInPortal
            (justify,
             line:longint; column:longint; textWidth:longint; text:pchar;
             attribute:longint; portal:PPCB):longint;cdecl;external External_library name 'NWSDisplayTextJustifiedInPortal';

  function NWSDisplayInformationInPortal
            (header,
             portalJustifyLine,
             portalJustifyColumn,
             portalJustifyType,
             portalPalette,
             portalBorderType,
             portalMaxWidth,
             portalMaxHeight,
             portalMinWidth,
             portalMinHeight,
             textLRJustifyType,
             textLRIndent,
             textTBJustifyType,
             textTBIndent,
             textAttribute,
             textMinimizeStyle:longint;
             text:pchar;
             handle:PNUTInfo):longint;cdecl;external External_library name 'NWSDisplayInformationInPortal';

  procedure NWSRestoreNut(handle:PNUTInfo);cdecl;external External_library name 'NWSRestoreNut';

  procedure NWSDrawPortalBorder(portal:PPCB);cdecl;external External_library name 'NWSDrawPortalBorder';

  procedure NWSUpdatePortal(portal:PPCB);cdecl;external External_library name 'NWSUpdatePortal';

type TSSFEntryProc = procedure (para1:PFIELD; para2:pointer; para3:PNUTInfo); cdecl;
     TSSFCustomDataReleaseProc = procedure (para1:pointer; para2:PNUTInfo); cdecl;
     TSSFFormat=procedure (para1:PFIELD; text:pchar; para3:longint); cdecl;
     TSSFControlFunc = function (para1:PFIELD; para2:longint; para3:Plongint; para4:PNUTInfo):longint; cdecl;
     TSSFVerifyFunc  = function (para1:PFIELD; para2:pointer; para3:PNUTInfo):longint; cdecl;
     TSSFReleaseProc = procedure (para1:PFIELD); cdecl;

  procedure NWSSetFieldFunctionPtr(fp:PFIELD;
              Format : TSSFFormat;
              Control: TSSFControlFunc;
              Verify : TSSFVerifyFunc;
              Release: TSSFReleaseProc;
              Entry  : TSSFEntryProc;
              customDataRelease
                     : TSSFCustomDataReleaseProc);cdecl;external External_library name 'NWSSetFieldFunctionPtr';


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

  procedure NWSSetScreenPalette(newPalette:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSSetScreenPalette';

  function NWSGetScreenPalette(handle:PNUTInfo):longint;cdecl;external External_library name 'NWSGetScreenPalette';

  procedure NWSGetPCB(var _pPcb:PPCB; portalNumber:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSGetPCB';

type TentryProcedure = procedure (element:PLIST; displayLine:longint; handle:PNUTInfo); cdecl;
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

  procedure NWSSetErrorLabelDisplayFlag(flag:longint;
                                        handle:PNUTInfo);cdecl;external External_library name 'NWSSetErrorLabelDisplayFlag';

  procedure NWSSetHelpHelp(helpIndex:longint;
                           handle:PNUTInfo);cdecl;external External_library name 'NWSSetHelpHelp';

  { max length of passwordString, including NULL  }
  function NWSPromptForPassword
               (passwordHeader,
                line,
                column,
                maxPasswordLen : longint;
                passwordString:pchar;
                verifyEntry:longint;
                handle:PNUTInfo):longint;cdecl;external External_library name 'NWSPromptForPassword';

  function NWSAppendPasswordField
               (line,
                column,
                width,
                fflag:longint;            // field flags
                data:pchar;             // ptr to field text
                maxDataLen,             // including null
                help,                   // help for field
                verifyEntry,            // force password verification
                passwordPortalHeader,
                maskCharacter:longint;    // fill character for field
                nhandle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendPasswordField';

  function NWSAppendScrollableStringField
               (line,
                column,
                width,
                fflag:longint;
                data:pchar;
                maxLen:longint;           // max len of data, allowing for null terminator
                cset:pointer;           // valid characters, if using EF_SET
                editFlags,              // NWSEditString flags (EF_UPPER etc.)
                help:longint;
                handle:PNUTInfo):PFIELD;cdecl;external External_library name 'NWSAppendScrollableStringField';

type TSSFInsertFunc = function (_string:pchar; maxLen:longint; parameters:pointer):longint; cdecl;
  procedure NWSSetScrollableFieldInsertProc(fp:PFIELD; insertProc:TSSFInsertFunc);cdecl;external External_library name 'NWSSetScrollableFieldInsertProc';

  { Returns 0 for success, -1 if none selected  }
  { if not NULL, returns portal number  }
  function NWSGetCurrentPortal(nutInfo:PNUTInfo; portalNumber:plongint; var portal:PPCB):longint;cdecl;external External_library name 'NWSGetCurrentPortal';

  { if not NULL, returns PCB pointer  }
  function NWSWaitForKeyAndValue(handle:PNUTInfo;
                                 nKeys:longint;
                                 keyType:array of longint;
                                 keyValue:array of longint):longint;cdecl;external External_library name 'NWSWaitForKeyAndValue';

  procedure NWSShowLineAttribute(line,
                                 column:longint;
                                 text:pchar;
                                 attribute,
                                 length:longint;
                                 screenID:TScr);cdecl;external External_library name 'NWSShowLineAttribute';

  procedure NWSShowLine(line,column:longint; text:pchar; length:longint; screenID:TScr);cdecl;external External_library name 'NWSShowLine';

  procedure NWSScrollZone
                   (line,
                    column,
                    height,
                    width,
                    attribute,
                    count,
                    direction:longint;
                    screenID:TScr);cdecl;external External_library name 'NWSScrollZone';

  procedure NWSSaveZone
                   (line,
                    column,
                    height,
                    width:longint;
                    buffer:pointer;
                    screenID:TScr);cdecl;external External_library name 'NWSSaveZone';

  procedure NWSSaveZone
                   (line,
                    column,
                    height,
                    width:longint;
                var buffer;
                    screenID:TScr);cdecl;external External_library name 'NWSSaveZone';

  procedure NWSRestoreZone
                   (line,
                    column,
                    height,
                    width:longint;
                    buffer:pointer;
                    screenID:TScr);cdecl;external External_library name 'NWSRestoreZone';

  procedure NWSRestoreZone
                   (line,
                    column,
                    height,
                    width:longint;
                var buffer;
                    screenID:TScr);cdecl;external External_library name 'NWSRestoreZone';

  procedure NWSRestoreDisplay(screenID:TScr);cdecl;external External_library name 'NWSRestoreDisplay';

  procedure NWSPositionCursor(line, column:longint; screenID:TScr);cdecl;external External_library name 'NWSPositionCursor';

  procedure NWSGetNUTVersion(majorVersion, minorVersion, revision:plongint);cdecl;external External_library name 'NWSGetNUTVersion';
  procedure NWSGetNUTVersion(var majorVersion, minorVersion, revision:longint);cdecl;external External_library name 'NWSGetNUTVersion';

  procedure NWSSetFormRepaintFlag(value:longint; handle:PNUTInfo);cdecl;external External_library name 'NWSSetFormRepaintFlag';

  procedure NWSSetFormNoWrap(handle:PNUTInfo);cdecl;external External_library name 'NWSSetFormNoWrap';

  function NWSViewTextWithScrollBars
                (centerLine,
                 centerColumn,
                 height,
                 width,
                 headerNumber:longint;
                 textBuffer:pchar;
                 maxBufferLength,
                 scrollBarFlag:longint;
                 handle:PNUTInfo):longint;cdecl;external External_library name 'NWSViewTextWithScrollBars';

  { length of document  }
  function NWSEditTextWithScrollBars
                (centerLine,
                 centerColumn,
                 height,
                 width,
                 headerNumber:longint;
                 textBuffer:pchar;
                 maxBufferLength,
                 confirmMessage,
                 forceConfirm,
                 scrollBarFlag:longint;
                 handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditTextWithScrollBars';

   function NWSEditTextWithScrollBars
                (centerLine,
                 centerColumn,
                 height,
                 width,
                 headerNumber:longint;
                 textBuffer:pchar;
                 maxBufferLength,
                 confirmMessage : longint;
                 forceConfirm   : longbool;
                 scrollBarFlag  : longint;
                 handle:PNUTInfo):longint;cdecl;external External_library name 'NWSEditTextWithScrollBars';


implementation

  function F_H1 : longint;
  begin
    F_H1:=NWSGetLineDrawCharacter(0);
  end;

  function F_H2 : longint;
  begin
    F_H2:=NWSGetLineDrawCharacter(1);
  end;

  function F_V1 : longint;
  begin
    F_V1:=NWSGetLineDrawCharacter(2);
  end;

  function F_V2 : longint;
  begin
    F_V2:=NWSGetLineDrawCharacter(3);
  end;

  function F_UL1 : longint;
  begin
    F_UL1:=NWSGetLineDrawCharacter(4);
  end;

  function F_UR1 : longint;
  begin
    F_UR1:=NWSGetLineDrawCharacter(5);
  end;

  function F_LL1 : longint;
  begin
    F_LL1:=NWSGetLineDrawCharacter(6);
  end;

  function F_LR1 : longint;
  begin
    F_LR1:=NWSGetLineDrawCharacter(7);
  end;

  function F_UL2 : longint;
  begin
    F_UL2:=NWSGetLineDrawCharacter(8);
  end;

  function F_UR2 : longint;
  begin
    F_UR2:=NWSGetLineDrawCharacter(9);
  end;

  function F_LL2 : longint;
  begin
    F_LL2:=NWSGetLineDrawCharacter(10);
  end;

  function F_LR2 : longint;
  begin
    F_LR2:=NWSGetLineDrawCharacter(11);
  end;

  function F_UT1 : longint;
  begin
    F_UT1:=NWSGetLineDrawCharacter(12);
  end;

  function F_DT1 : longint;
  begin
    F_DT1:=NWSGetLineDrawCharacter(13);
  end;

  function F_LT1 : longint;
  begin
    F_LT1:=NWSGetLineDrawCharacter(14);
  end;

  function F_RT1 : longint;
  begin
    F_RT1:=NWSGetLineDrawCharacter(15);
  end;

  function F_UT2 : longint;
  begin
    F_UT2:=NWSGetLineDrawCharacter(24);
  end;

  function F_DT2 : longint;
  begin
    F_DT2:=NWSGetLineDrawCharacter(25);
  end;

  function F_LT2 : longint;
  begin
    F_LT2:=NWSGetLineDrawCharacter(26);
  end;

  function F_RT2 : longint;
  begin
    F_RT2:=NWSGetLineDrawCharacter(27);
  end;

  function F_X1 : longint;
  begin
    F_X1:=NWSGetLineDrawCharacter(36);
  end;

  function F_X2 : longint;
  begin
    F_X2:=NWSGetLineDrawCharacter(39);
  end;

  function F_UP : longint;
  begin
    F_UP:=NWSGetLineDrawCharacter(40);
  end;

  function F_DOWN : longint;
  begin
    F_DOWN:=NWSGetLineDrawCharacter(41);
  end;

  function F_LEFT : longint;
  begin
    F_LEFT:=NWSGetLineDrawCharacter(42);
  end;

  function F_RIGHT : longint;
  begin
    F_RIGHT:=NWSGetLineDrawCharacter(43);
  end;

  function F_BG1 : longint;
  begin
    F_BG1:=NWSGetLineDrawCharacter(44);
  end;

  function F_BG2 : longint;
  begin
    F_BG2:=NWSGetLineDrawCharacter(45);
  end;

  function F_BG3 : longint;
  begin
    F_BG3:=NWSGetLineDrawCharacter(46);
  end;

  function F_BG4 : longint;
  begin
    F_BG4:=NWSGetLineDrawCharacter(47);
  end;

  function IS_DYNAMIC_MESSAGE(a : longint) : boolean;
  begin
    IS_DYNAMIC_MESSAGE:=(a > $fff0) and (a < $ffff);
  end;


end.
