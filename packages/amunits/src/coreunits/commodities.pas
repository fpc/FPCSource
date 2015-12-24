{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:
    Added overlay functions for Pchar->Strings, functions
    and procedures.
    14 Jul 2000.

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening
    of the library.
    13 Jan 2003.

    changed integer > smallint,
            cardinal > longword.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se
}
{$PACKRECORDS 2}
unit commodities;

INTERFACE


uses exec, inputevent, keymap;



{    **************
 * Broker stuff
 **************}

CONST
{     buffer sizes   }
      CBD_NAMELEN   =  24;
      CBD_TITLELEN  =  40;
      CBD_DESCRLEN  =  40;

{     CxBroker errors   }
      CBERR_OK      =  0;        {     No error                         }
      CBERR_SYSERR  =  1;        {     System error , no memory, etc    }
      CBERR_DUP     =  2;        {     uniqueness violation             }
      CBERR_VERSION =  3;        {     didn't understand nb_VERSION     }

      NB_VERSION    =  5;        {     Version of NewBroker structure   }

Type
  pNewBroker = ^tNewBroker;
  tNewBroker = record
   nb_Version   : Shortint;  {     set to NB_VERSION                }
   nb_Name,
   nb_Title,
   nb_Descr     : STRPTR;
   nb_Unique,
   nb_Flags     : smallint;
   nb_Pri       : Shortint;
   {     new in V5   }
   nb_Port      : pMsgPort;
   nb_ReservedChannel  : smallint;  {     plans for later port sharing     }
  END;

CONST
{     Flags for nb_Unique }
      NBU_DUPLICATE  = 0;
      NBU_UNIQUE     = 1;        {     will not allow duplicates        }
      NBU_NOTIFY     = 2;        {     sends CXM_UNIQUE to existing broker }

{     Flags for nb_Flags }
        COF_SHOW_HIDE = 4;

{    *******
 * cxusr
 *******}

{    * Fake data types for system private objects   }
Type
  CxObj = Longint;
  pCxObj = ^CxObj;
  CxMsg = Longint;
  pCXMsg = ^CxMsg;


CONST
{    ******************************}
{    * Commodities Object Types   *}
{    ******************************}
      CX_INVALID     = 0;     {     not a valid object (probably null)  }
      CX_FILTER      = 1;     {     input event messages only           }
      CX_TYPEFILTER  = 2;     {     filter on message type      }
      CX_SEND        = 3;     {     sends a message                     }
      CX_SIGNAL      = 4;     {     sends a signal              }
      CX_TRANSLATE   = 5;     {     translates IE into chain            }
      CX_BROKER      = 6;     {     application representative          }
      CX_DEBUG       = 7;     {     dumps kprintf to serial port        }
      CX_CUSTOM      = 8;     {     application provids function        }
      CX_ZERO        = 9;     {     system terminator node      }

{    ***************}
{    * CxMsg types *}
{    ***************}
      CXM_UNIQUE     = 16;    {     sent down broker by CxBroker()      }
{     Obsolete: subsumed by CXM_COMMAND (below)   }

{     Messages of this type rattle around the Commodities input network.
 * They will be sent to you by a Sender object, and passed to you
 * as a synchronous function call by a Custom object.
 *
 * The message port or function entry point is stored in the object,
 * and the ID field of the message will be set to what you arrange
 * issuing object.
 *
 * The Data field will point to the input event triggering the
 * message.
 }
      CXM_IEVENT     = 32;

{     These messages are sent to a port attached to your Broker.
 * They are sent to you when the controller program wants your
 * program to do something.  The ID field identifies the command.
 *
 * The Data field will be used later.
 }
      CXM_COMMAND    = 64;

{     ID values   }
      CXCMD_DISABLE   = (15);   {     please disable yourself       }
      CXCMD_ENABLE    = (17);   {     please enable yourself        }
      CXCMD_APPEAR    = (19);   {     open your window, if you can  }
      CXCMD_DISAPPEAR = (21);   {     go dormant                    }
      CXCMD_KILL      = (23);   {     go away for good              }
      CXCMD_UNIQUE    = (25);   {     someone tried to create a broker
                               * with your name.  Suggest you Appear.
                               }
      CXCMD_LIST_CHG  = (27);  {     Used by Exchange program. Someone }
                              {     has changed the broker list       }

{     return values for BrokerCommand(): }
      CMDE_OK        = (0);
      CMDE_NOBROKER  = (-1);
      CMDE_NOPORT    = (-2);
      CMDE_NOMEM     = (-3);

{     IMPORTANT NOTE: for V5:
 * Only CXM_IEVENT messages are passed through the input network.
 *
 * Other types of messages are sent to an optional port in your broker.
 *
 * This means that you must test the message type in your message handling,
 * if input messages and command messages come to the same port.
 *
 * Older programs have no broker port, so processing loops which
 * make assumptions about type won't encounter the new message types.
 *
 * The TypeFilter CxObject is hereby obsolete.
 *
 * It is less convenient for the application, but eliminates testing
 * for type of input messages.
 }

{    ********************************************************}
{    * CxObj Error Flags (return values from CxObjError())  *}
{    ********************************************************}
      COERR_ISNULL      = 1;  {     you called CxError(NULL)            }
      COERR_NULLATTACH  = 2;  {     someone attached NULL to my list    }
      COERR_BADFILTER   = 4;  {     a bad filter description was given  }
      COERR_BADTYPE     = 8;  {     unmatched type-specific operation   }


{    ****************************}
{     Input Expression structure }
{    ****************************}

      IX_VERSION        = 2;

Type
  pInputXpression = ^tInputXpression;
  tInputXpression = record
   ix_Version,               {     must be set to IX_VERSION  }
   ix_Class    : Byte;       {     class must match exactly   }

   ix_Code     : Word;      {     Bits that we want  }

   ix_CodeMask : Word;      {     Set bits here to indicate  }
                             {     which bits in ix_Code are  }
                             {     don't care bits.           }

   ix_Qualifier: Word;      {     Bits that we want  }

   ix_QualMask : Word;      {     Set bits here to indicate  }
                           {     which bits in ix_Qualifier }
                                                   {     are don't care bits        }

   ix_QualSame : Word;    {     synonyms in qualifier      }
  END;

   IX = tInputXpression;
   pIX = ^IX;

CONST
{     QualSame identifiers }
      IXSYM_SHIFT = 1;     {     left- and right- shift are equivalent     }
      IXSYM_CAPS  = 2;     {     either shift or caps lock are equivalent  }
      IXSYM_ALT   = 4;     {     left- and right- alt are equivalent       }

{     corresponding QualSame masks }
      IXSYM_SHIFTMASK = (IEQUALIFIER_LSHIFT + IEQUALIFIER_RSHIFT);
      IXSYM_CAPSMASK  = (IXSYM_SHIFTMASK    + IEQUALIFIER_CAPSLOCK);
      IXSYM_ALTMASK   = (IEQUALIFIER_LALT   + IEQUALIFIER_RALT);

      IX_NORMALQUALS  = $7FFF;   {     for QualMask field: avoid RELATIVEMOUSE }


VAR CxBase : pLibrary;

const
    COMMODITIESNAME : PChar = 'commodities.library';


FUNCTION ActivateCxObj(co : pCxObj location 'a0'; tru : LONGINT location 'd0') : LONGINT; syscall CxBase 042;
PROCEDURE AddIEvents(events : pInputEvent location 'a0'); syscall CxBase 180;
PROCEDURE AttachCxObj(headObj : pCxObj location 'a0'; co : pCxObj location 'a1'); syscall CxBase 084;
PROCEDURE ClearCxObjError(co : pCxObj location 'a0'); syscall CxBase 072;
FUNCTION CreateCxObj(typ : ULONG location 'd0'; arg1 : LONGINT location 'a0'; arg2 : LONGINT location 'a1') : pCxObj; syscall CxBase 030;
FUNCTION CxBroker(nb : pNewBroker location 'a0'; error : pCxObj location 'd0') : pCxObj; syscall CxBase 036;
FUNCTION CxMsgData(cxm : pCxMsg location 'a0') : POINTER; syscall CxBase 144;
FUNCTION CxMsgID(cxm : pCxMsg location 'a0') : LONGINT; syscall CxBase 150;
FUNCTION CxMsgType(cxm : pCxMsg location 'a0') : ULONG; syscall CxBase 138;
FUNCTION CxObjError(co : pCxObj location 'a0') : LONGINT; syscall CxBase 066;
FUNCTION CxObjType(co : pCxObj location 'a0') : ULONG; syscall CxBase 060;
PROCEDURE DeleteCxObj(co : pCxObj location 'a0'); syscall CxBase 048;
PROCEDURE DeleteCxObjAll(co : pCxObj location 'a0'); syscall CxBase 054;
PROCEDURE DisposeCxMsg(cxm : pCxMsg location 'a0'); syscall CxBase 168;
PROCEDURE DivertCxMsg(cxm : pCxMsg location 'a0'; headObj : pCxObj location 'a1'; returnObj : pCxObj location 'a2'); syscall CxBase 156;
PROCEDURE EnqueueCxObj(headObj : pCxObj location 'a0'; co : pCxObj location 'a1'); syscall CxBase 090;
PROCEDURE InsertCxObj(headObj : pCxObj location 'a0'; co : pCxObj location 'a1'; pred : pCxObj location 'a2'); syscall CxBase 096;
FUNCTION InvertKeyMap(ansiCode : ULONG location 'd0'; event : pInputEvent location 'a0'; km : pKeyMap location 'a1') : LongBool; syscall CxBase 174;
FUNCTION MatchIX(event : pInputEvent location 'a0'; ix : pInputXpression location 'a1') : LongBool; syscall CxBase 204;
FUNCTION ParseIX(description : pCHAR location 'a0'; ix : pInputXpression location 'a1') : LONGINT; syscall CxBase 132;
PROCEDURE RemoveCxObj(co : pCxObj location 'a0'); syscall CxBase 102;
PROCEDURE RouteCxMsg(cxm : pCxMsg location 'a0'; co : pCxObj location 'a1'); syscall CxBase 162;
FUNCTION SetCxObjPri(co : pCxObj location 'a0'; pri : LONGINT location 'd0') : LONGINT; syscall CxBase 078;
PROCEDURE SetFilter(filter : pCxObj location 'a0'; text : pCHAR location 'a1'); syscall CxBase 120;
PROCEDURE SetFilterIX(filter : pCxObj location 'a0'; ix : pInputXpression location 'a1'); syscall CxBase 126;
PROCEDURE SetTranslate(translator : pCxObj location 'a0'; events : pInputEvent location 'a1'); syscall CxBase 114;

{ overlay functions}

FUNCTION ParseIX(description : rawbytestring; ix : pInputXpression) : LONGINT;
PROCEDURE SetFilter(filter : pCxObj; text : rawbytestring);


IMPLEMENTATION

uses amsgbox;

FUNCTION ParseIX(description : rawbytestring; ix : pInputXpression) : LONGINT;
begin
  ParseIX := ParseIX(pchar(description),ix);
end;

PROCEDURE SetFilter(filter : pCxObj; text : rawbytestring);
begin
  SetFilter(filter,pchar(text));
end;

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of commodities.library}

var
    commodities_exit : Pointer;

procedure ClosecommoditiesLibrary;
begin
    ExitProc := commodities_exit;
    if CxBase <> nil then begin
        CloseLibrary(CxBase);
        CxBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

begin
    CxBase := nil;
    CxBase := OpenLibrary(COMMODITIESNAME,LIBVERSION);
    if CxBase <> nil then begin
        commodities_exit := ExitProc;
        ExitProc := @ClosecommoditiesLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open commodities.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of commodities.library compiled}
   {$Info Make sure you open commodities.library yourself}
{$endif use_auto_openlib}

END. (* UNIT COMMODITIES *)



