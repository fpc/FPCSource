{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    Keymap.resource definitions and console.device key map definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit Keymap;

INTERFACE

{$mode objfpc}

uses exec, inputevent;

Type

    PKeyMap = ^TKeyMap;
    TKeyMap = record
        km_LoKeyMapTypes        : Pointer;
        km_LoKeyMap             : Pointer;
        km_LoCapsable           : Pointer;
        km_LoRepeatable         : Pointer;
        km_HiKeyMapTypes        : Pointer;
        km_HiKeyMap             : Pointer;
        km_HiCapsable           : Pointer;
        km_HiRepeatable         : Pointer;
    end;


    pKeymapNode = ^TKeyMapNode;
    TKeyMapNode = record
        kn_Node         : TNode;         { including name of Keymap }
        kn_KeyMap       : TKeyMap;
    end;

{ the structure of Keymap.resource }

    pKeyMapResource = ^TKeyMapResource;
    TKeyMapResource = record
        kr_Node         : TNode;
        kr_List         : tList;         { a list of KeyMapNodes }
    end;


Const

{ Key Map Types }

    KC_NOQUAL           = 0;
    KC_VANILLA          = 7;    { note that SHIFT+ALT+CTRL is VANILLA }
    KCB_SHIFT           = 0;
    KCF_SHIFT           = $01;
    KCB_ALT             = 1;
    KCF_ALT             = $02;
    KCB_CONTROL         = 2;
    KCF_CONTROL         = $04;
    KCB_DOWNUP          = 3;
    KCF_DOWNUP          = $08;

    KCB_DEAD            = 5;    { may be dead or modified by dead key:  }
    KCF_DEAD            = $20;  {   use dead prefix bytes               }

    KCB_STRING          = 6;
    KCF_STRING          = $40;

    KCB_NOP             = 7;
    KCF_NOP             = $80;


{ Dead Prefix Bytes }

    DPB_MOD             = 0;
    DPF_MOD             = $01;
    DPB_DEAD            = 3;
    DPF_DEAD            = $08;

    DP_2DINDEXMASK      = $0f;  { mask for index for 1st of two dead keys }
    DP_2DFACSHIFT       = 4;    { shift for factor for 1st of two dead keys }

var KeyMapBase : pLibrary;

const
    KEYMAPNAME : PChar = 'keymap.library';

function AskKeyMapDefault : PKeyMap; syscall KeyMapBase 6;
function MapANSI(TheString : PChar; Count : LongInt; Buffer : PChar; Length : LongInt; KeyMap : PKeyMap) : LongInt;  syscall KeyMapBase 8;
function MapRawKey(Event : PInputEvent; Buffer : PChar; Length : LongInt; KeyMap : PKeyMap) : SmallInt;  syscall KeyMapBase 7;
procedure SetKeyMapDefault(KeyMap : PKeyMap);  syscall KeyMapBase 5;

implementation

initialization
  KeyMapBase := OpenLibrary(KEYMAPNAME, 0);

finalization
  CloseLibrary(KeyMapBase);

end. (* UNIT KEYMAP *)



