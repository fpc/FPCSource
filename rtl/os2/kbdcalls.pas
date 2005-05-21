{Set tabsize to 4.}
{****************************************************************************


                           KBDCALLS interface unit
                     Free Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Kl„mpfl
                    Copyright (c) 1999-2000 by Daniel Mantione
                      Copyright (c) 1999-2000 by Tomas Hajny

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This is an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit KbdCalls;

{ Interface library to KBDCALLS.DLL (through EMXWRAP.DLL)

Variant records and aliases for some record types created to maintain highest
possible level of compatibility with other existing OS/2 compilers.

Changelog:

    People:

        TH - Tomas Hajny (xhajt03@mbox.vol.cz on Internet)

    Date:           Description of change:              Changed by:

     -              First released version 1.0          TH

Coding style:

    I have tried to use the same coding style as Daniel Mantione in unit
    DOSCALLS, although I can't say I would write it the same way otherwise
    (I would write much more spaces myself, at least). Try to use it as well,
    please. Original note by Daniel Mantione follows:


    It may be well possible that coding style feels a bit strange to you.
    Nevertheless I friendly ask you to try to make your changes not look all
    to different. To make life easier, set your IDE to use tab characters,
    turn optimal fill, autoindent and backspace unindents on and set a
    tabsize of 4.}

{***************************************************************************}
interface
{***************************************************************************}

{$IFDEF FPC}
    {$PACKRECORDS 1}
{$ENDIF FPC}

const
{return codes / error constants (those marked with * shouldn't occur under
normal conditions)}
    No_Error                        =  0;
    Error_Invalid_Parameter         = 87;
    Error_Sem_TimeOut               =121;
    Error_Kbd_Parameter             =373;
    Error_Kbd_No_Device             =374; {*}
    Error_Kbd_Invalid_IOWait        =375; {*}
    Error_Kbd_Invalid_Length        =376;
    Error_Kbd_Invalid_Echo_Mask     =377;
    Error_Kbd_Invalid_Input_Mask    =378;
    Error_Kbd_Smg_Only              =407; {*}
    Error_Kbd_Invalid_ASCIIZ        =408;
    Error_Kbd_Invalid_Mask          =409;
    Error_Kbd_Register              =410;
    Error_Kbd_Deregister            =411;
    Error_Kbd_Invalid_Handle        =439;
    Error_Kbd_No_more_Handle        =440;
    Error_Kbd_Cannot_Create_KCB     =441;
    Error_Kbd_Codepage_Load_Incompl =442; {*}
    Error_Kbd_Invalid_CodePage_ID   =443; {*}
    Error_Kbd_No_CodePage_Support   =444; {*}
    Error_Kbd_Focus_Required        =445;
    Error_Kbd_Focus_Already_Active  =446; {*}
    Error_Kbd_Keyboard_Busy         =447;
    Error_Kbd_Invalid_CodePage      =448;
    Error_Kbd_Unable_To_Focus       =449; {*}
    Error_Kbd_Detached              =464;
    Error_Kbd_No_Console            =500; {*}
    Error_Kbd_Extended_SG           =504;

{FnMask}
    kr_KbdCharIn        =$00000001;
    kr_KbdPeek          =$00000002;
    kr_KbdFlushBuffer   =$00000004;
    kr_KbdGetStatus     =$00000008;
    kr_KbdSetStatus     =$00000010;
    kr_KbdStringIn      =$00000020;
    kr_KbdOpen          =$00000040;
    kr_KbdClose         =$00000080;
    kr_KbdGetFocus      =$00000100;
    kr_KbdFreeFocus     =$00000200;
    kr_KbdGetCP         =$00000400;
    kr_KbdSetCP         =$00000800;
    kr_KbdXLate         =$00001000;
    kr_KbdSetCustXT     =$00002000;

{WaitFlag}
    IO_Wait     =0;
        {KbdCharIn: wait for a character if one is not available}
        {KbdGetFocus: wait for the focus}
        {KbdStringIn: in binary input mode, wait until CharBuf is full, in    }
        {             ASCII input mode wait until a carriage return is pressed}
    IO_NoWait   =1;
        {KbdCharIn: immediate return if no character is available}
        {KbdGetFocus: do not wait for the focus}
        {KbdStringIn: send an immediate return if no characters are available,}
        {             if characters available, send them (up to the maximum   }
        {             length); not supported in ASCII input mode              }

{TKbdInfo.fsMask}
    Keyboard_Echo_On            =$0001; {echo is on}
    Keyboard_Echo_Off           =$0002; {echo is off}
    Keyboard_Binary_Mode        =$0004; {binary mode is on}
    Keyboard_ASCII_Mode         =$0008; {ASCII mode is on}
    Keyboard_Modify_State       =$0010; {shift state is modified}
    Keyboard_Modify_Interim     =$0020; {interim character flags are modified}
    Keyboard_Modify_TurnAround  =$0040; {turn-around character is modified}
    Keyboard_2B_TurnAround      =$0080; {length of the turn-around character  }
                                        {(meaningful only if                  }
                                        {Keyboard_Modify_TurnAround bit is on)}
    Keyboard_Shift_Report       =$0100; {shift return is on}

{TKbdInfo.fsState/TKbdKeyInfo.fsState/TKbdTrans.fsState}
    KbdStF_RightShift           =$0001;
    KbdStF_LeftShift            =$0002;
    KbdStF_Control              =$0004;
    KbdStF_Alt                  =$0008;
    KbdStF_ScrollLock_On        =$0010;
    KbdStF_Numlock_On           =$0020;
    KbdStF_Capslock_On          =$0040;
    KbdStF_Insert_On            =$0080;
    KbdStF_LeftControl          =$0100;
    KbdStF_LeftAlt              =$0200;
    KbdStF_RightControl         =$0400;
    KbdStF_RightAlt             =$0800;
    KbdStF_ScrollLock           =$1000;
    KbdStF_NumLock              =$2000;
    KbdStF_CapsLock             =$4000;
    KbdStF_SysReq               =$8000;

{TKbdTrans.fbStatus}
    KbdTrF_Shift_Key_In         =$01;   {shift status returned}
                                        {without character    }
    KbdTrF_Extended_Key_In      =$02;   {extended key code }
                                        {from the keyboard,}
                                        {not a character   }
    KbdTrF_Conversion_Request   =$20;   {immediate conversion}
                                        {requested           }
    KbdTrF_Final_Char_In        =$40;   {either $40 or $80 or both}
    KbdTrF_Interim_Char_In      =$80;   {must be present          }

{TKbdHWID.idKbd}
    Keyboard_Undetermined   =$0000; {undetermined keyboard type}
    Keyboard_AT_Compatible  =$0001; {PC-AT Standard Keyboard}
    Keyboard_Enhanced_101   =$AB41; {101 Key Enhanced Keyboard}
    Keyboard_Enhanced_102   =$AB41; {102 Key Enhanced Keyboard}
    Keyboard_Enhanced_88_89 =$AB54; {88 and 89 Key Enhanced Keyboards}
    Keyboard_Enhanced_122   =$AB85; {122 Key Enhanced Keyboard}
    Keyboard_AT_Compatable=Keyboard_AT_Compatible;
    Keyboard_SpaceSaver=Keyboard_Enhanced_88_89;


type
{TKbdKeyInfo - record type for character data for KbdCharIn and KbdPeek}
(*   #pragma pack(2) ??? *)
    TKbdKeyInfo=record
        chChar:char;    {ASCII character code; the scan code received}
                        {from the keyboard is translated to the ASCII}
                        {character code                              }
        case boolean of
        false:(
        chScan:byte;    {scan Code received from the keyboard}
        fbStatus:byte;  {state of the keystroke event, see KbdTrF_* constants}
        bNlsShift:byte; {NLS shift status (always 0?)}
        fsState:word;   {shift key status, see KbdStF_* constants}
        Time:cardinal); {time stamp indicating when a key was pressed,}
                        {specified in milliseconds from the time      }
                        {the system was started                       }
        true:(
        chScan2:char;   (* should be chScan, fbStatus and bNlsShift,   *)
        fbStatus2:byte; (* but this construct is unsupported currently *)
        bNlsShift2:char);
    end;
    PKbdKeyInfo=^TKbdKeyInfo;
    KbdKeyInfo=TKbdKeyInfo; {for better compatibility with other compilers}

{record type for KbdStringIn}
    TStringInBuf=record
        cb:word;    {length of the input buffer, maximum length is 255}
        cchIn:word; {number of bytes actually read into the buffer}
    end;
    PStringInBuf=^TStringInBuf;
    StringInBuf=TStringInBuf;

{TKbdInfo record type, for KbdSet/GetStatus}
    TKbdInfo=record
        cb,             {total length in bytes, 10 is the only valid value  }
        fsMask,         {see TKbdInfo.fsMask constants, higher bits reserved}
                        {and set to 0                                       }
        chTurnAround,   {definition of the turn-around character, in ASCII   }
                        {and extended-ASCII format, the turn-around character}
                        {is defined as the carriage return, in ASCII format  }
                        {only, the turn-around character is defined in the   }
                        {low-order byte; usually $000D                                }
        fsInterim,      {interim character flags: bits 0-4 and 6 - reserved   }
                        {                                        and set to 0,}
                        {                         bit 5          - application}
                        {                                        requested    }
                        {                                        immediate    }
                        {                                        conversion   }
                        {                         bit 7          - interim    }
                        {                                        character    }
                        {                                        flag is on   }
                        {                         bits 8-15      - NLS shift  }
                        {                                        state        }
        fsState:word;   {shift state, see TKbdInfo.fsState constants}
    end;
    PKbdInfo=^TKbdInfo;
    KbdInfo=TKbdInfo;

{record type for KbdGetHWID}
    TKbdHWID=record
        cb,                 {length in bytes, on input length of the TKbdHWID}
                            {record (at least 2), on output the actual number}
                            {of bytes returned                               }
        idKbd,              {keyboard type: $0000 = undetermined keyboard type}
                            {               $0001 = PC-AT Standard Keyboard   }
                            {               $AB41 = 101 Key Enhanced Keyboard }
                            {               $AB41 = 102 Key Enhanced Keyboard }
                            {               $AB54 = 88 and 89 Key Enhanced    }
                            {                        Keyboards                }
                            {               $AB85 = 122 Key Enhanced Keyboard }
                            {- see Keyboard_* constants                       }
        usReserved1,        {reserved, returned set to zero (secondary ID?)}
        usReserved2:word;   {reserved, returned set to zero}
    end;
    PKbdHWID=^TKbdHWID;
    KbdHWID=TKbdHWID;

{record type for KbdXlate}
(*   #pragma pack(2) ???*)
    TKbdTrans=record
        case boolean of
        false:(
            CharData:TKbdKeyInfo);
        true:(
            chChar:char;    {ASCII character code; the scan code received}
                            {from the keyboard is translated to the ASCII}
                            {character code                              }
            case boolean of
            false:(
            chScan,         {scan Code received from the keyboard}
            fbStatus,       {state of the keystroke event,}
                            {see KbdTrF_* constants       }
            bNlsShift:byte; {NLS shift status (always 0?)}
            fsState:word;   {shift key status, see KbdStF_* constants}
            Time:cardinal;  {time stamp indicating when a key was pressed,}
                            {specified in milliseconds from the time      }
                            {the system was started                       }
            fsDD:word;      {device driver returned flag, }
                            {see KbdDDFlagWord notes below}
            fsXlate:word;   {translation flag: 0 - translation incomplete,}
                            {                  1 - translation complete   }
            fsShift:word;   {identifies the state of translation across    }
                            {successive calls, initially the value should  }
                            {be zero; it may take several calls to this    }
                            {function to complete a character, the value   }
                            {should not be changed unless a new translation}
                            {is required (that is, reset value to zero)    }
            sZero:word);    {reserved, set to 0}
            true:(
            chScan2,        (* should be chScan, fbStatus and bNlsShift,   *)
            fbStatus2,      (* but this construct is unsupported currently *)
            bNlsShift2:char));
    end;
    PKbdTrans=^TKbdTrans;
    KbdTrans=TKbdTrans;

{KbdDDFlagWord notes:
  bits 15-14    Available. These bits are available for communication between
                monitors; they are not used by the physical device driver. The
                monitor applications coordinate the use of these flags.
  Bits 13-10    Reserved, set to zero. Monitors must pass these flags as is.
                They must set these flags to 0 in packets they create.
  Bit 9         Accented. This key is translated using the previous key passed,
                which is an accent key. Where an accent key is pressed, and the
                following key does not use the accent, a packet containing the
                accent character itself is first passed with this bit set. The
                scan code field of MonFlagWord (see above) would be 0,
                indicating a non-key generated record. A valid packet
                containing that following keystroke is then passed without this
                bit set.
  Bit 8         Multimake. The translation process sees this scan code as
                a typematic repeat of a toggle key or a shift key. Because
                toggle and shift keys only change state on the first make after
                each key-break, no state information is changed. For example,
                the NumLock toggle bit in the shift status word is not changed,
                even though this can be the NumLock key. If this key is a valid
                character, it does not go into the Keyboard Input Buffer (KIB)
                once this bit is set.
  Bit 7         Secondary. The scan code prior to the one in this packet was
                the Secondary Key Prefix (see below).
  Bit 6         Key break. This record is generated by the release (the break)
                of the key involved.
  Bits 5-0      Key type. This numeric field flags the physical device driver
                and reports that this is a key that requires action. The number
                in this field is filled in during the translation of the scan
                code. The value allows the driver to act on keystrokes without
                regard for what scan codes the keyboard uses or character codes
                that the current translation process may be using. The
                following values are currently defined:

                  -  Value for keys that are always placed in the KIB.
                     Zero = no special action, always place in KIB.

                  -  Values acted on prior to passing packet to monitors.
                     Except for the final keystroke of the DUMP key sequences,
                     all of these values are passed on to the monitors. They
                     are not placed in the KIB. The XlatedChar and XlatedScan
                     fields are undefined for these values:
                     01h    ACK. This scan code is a keyboard acknowledge.
                            Personal Computer IBM* AT* attached keyboards
                            set this value on an FAh scan code.
                     02h    Secondary key prefix. This scan code is a prefix
                            generated by the Enhanced Keyboard. It indicates
                            that the next scan code coming is one of the
                            secondary keys that exists on that keyboard.
                            Usually set on an E0h scan code or an E1h scan
                            code.
                     03h    Kbd overrun. This scan code is an overrun
                            indication from the keyboard. On an IBM Personal
                            Computer AT-attached keyboard, this value would be
                            set on an FFh scan code.
                     04h    Resend. This scan code is a resend request from the
                            keyboard. On an IBM Personal Computer AT-attached
                            keyboard, this value would be set on an FEh scan
                            code.
                     05h    Reboot key. This scan code completes the multi-key
                            restart sequence. On an IBM Personal Computer AT
                            attached-keyboard, this value would be used when
                            the Ctrl+Alt+Delete sequence is used.
                     06h    Dump key. This scan code completes the multi-key
                            Stand Alone Dump request sequence. On an IBM
                            Personal Computer AT-attached keyboard, this value
                            would be used on completion of the second
                            consecutive press of Ctrl+Alt+NumLock or
                            Ctrl+Alt+F10 without other keystrokes between the
                            two presses.
                     07h-
                     0Ah    See entries below.

                     0Bh    Invalid accent combination. This scan code follows
                            an accent scan code but the combination is not
                            valid, and neither key is put in the KIB.
                            (Note: This is set if the Canadian-French code
                            pages are in use.)
                     0Ch    System-defined hot keys.
                     0Dh
                     -0Fh   Reserved. Treated as undefined. See entry 3Fh.
                  -  Values acted on after passing packet to monitors. Except
                     where noted, these values are placed in the KIB when the
                     physical device driver is in binary mode; they are not
                     placed in the KIB when the physical device driver is in
                     ASCII mode. (Also listed are those that never get placed
                     in the KIB.)
                     07h    Shift key. This scan code translates as a shift key
                            and affects the shift status fields of the CharData
                            record, but does not generate a defined character.
                            It is not placed in the KIB. The XlatedChar field
                            is undefined. The scan code field is 0.
                     08h    Pause key. This scan code is translated as the key
                            sequence meaning pause. On an IBM Personal Computer
                            AT-attached keyboard, this value is used when the
                            Ctrl+NumLock sequence is used. The key itself is
                            not placed in the KIB.
                     09h    Pseudo-Pause key. This scan code is translated into
                            the value that is treated as the Pause key when the
                            physical device driver is in ASCII mode. On most
                            keyboards, this would be when the Ctrl+S
                            combination is used. The key itself is not placed
                            in the KIB.
                     0Ah    Wake-up key. This scan code follows a Pause key or
                            Pseudo-Pause key, which causes the Pause state to
                            end. The key itself is not placed in the KIB.
                     10h    Accent key. This scan code is translated and used
                            as a key to alter the translation of the next key
                            to come in. The packet containing this value is
                            passed when the accent key is pressed, but it is
                            not put into the KIB, unless the Accented bit is
                            ON. The next key determines this decision. If the
                            next key is one that can be accented, then it is
                            passed by itself with the Accented bit ON. If that
                            next key cannot be accented by this accent, then
                            two packets are passed. The first contains the
                            character to print for the accent itself. It has
                            the Accent key value and the Accented flag (which
                            allows the packet to be put in the KIB). The second
                            packet contains a regular translation of that
                            following key.
                            (Note: The two packets get passed for every
                            language except Canadian-French - see entry 0Bh.)
                     11h    Break key. This scan code is translated as the key
                            sequence meaning break. On the IBM Personal
                            Computer AT-attached keyboard, this value is used
                            where the Ctrl+Break sequence is used.
                     12h    Pseudo-Break key. This scan code is translated into
                            the value that is treated as the Break key when the
                            physical device driver is in ASCII mode. On most
                            keyboards, this would be when the Ctrl+C
                            combination is used. Notice that the event
                            generated by this key is separate from the one
                            generated by the Break key when in the binary mode.
                     13h    Print Screen key. This scan code is translated as
                            the key sequence meaning Print Screen. On an IBM
                            Personal Computer AT-attached keyboard, this value
                            is used where the Shift+PrtSc sequence is used.
                     14h    Print Echo key. This scan code is translated as the
                            key sequence meaning Print Echo. This value is used
                            where the Ctrl+PrtSc sequence is used.
                     15h    Pseudo-Print Echo key. This scan code is translated
                            into the value that is treated as the Print Echo
                            key when the physical device driver is in ASCII
                            mode. On most keyboards, this would show as the
                            Ctrl+P combination.
                     16h    Print-Flush key. This scan code is translated into
                            the key sequence Print-Flush. This value is used
                            where the Ctrl+Alt+PrtSc sequence is used.
                     17h
                     -2Fh   Reserved, set to zero. Treated as undefined. See
                            entry 3Fh.
                  -  Values for packets not generated by a keystroke:
                     30h
                     -37h   Reserved.
                     38h
                     -3Eh   Reserved. Treated as undefined. See entry 3Fh.
                  -  Value for keys the translation process does not recognize:
                     3Fh    Undefined. This scan code, or its combination with
                            the current shift state, is not recognized in the
                            translation process.
}

{header of TXLateTbl}
    TXHeader=record
        XTableID:word;      {code page number}
        XTableFlags1:word;  {bits 0-2 determine which shift key or key      }
                            {combination affects Char3 of each TXLateKeyDef }
                            {element, bits 7-10 determine which shift key or}
                            {key combination causes Char5 to be used in each}
                            {TXLateKeyDef element                           }
                            {bit 0 - ShiftAlt (use Shift+Alt instead of     }
                            {           Ctrl+Alt)                           }
                            {bit 1 - AltGrafL (use left Alt key as          }
                            {           Alt+Graphics)                       }
                            {bit 2 - AltGrafR (use right Alt key as         }
                            {           Alt+Graphics)                       }
                            {bit 3 - ShiftLock (treat Caps Lock as          }
                            {           ShiftLock)                          }
                            {bit 4 - DefaultTable (default table for the    }
                            {           language)                           }
                            {bit 5 - ShiftToggle (1 = toggle ShiftLock,     }
                            {           0 = latch it)                       }
                            {bit 6 - AccentPass (pass accent and non-accent }
                            {           key through; 1 = pass on accent keys}
                            {           and beep, 0 = beep only             }
                            {bit 7 - CapsShift (Caps+Shift uses Char5)      }
                            {bit 8 - MachDep (machine-dependent table)      }
                            {bits  9-10 reserved                            }
                            {bits 11-15 reserved                            }
        XTableFlags2:word;  {reserved, set to zero}
        KbdType:word;       {keyboard type, 1 for extended (all common types)}
        KbdSubType:word;    {reserved}
        XtableLen:word;     {length of table}
        EntryCount:word;    {number of KeyDef entries}
        EntryWidth:word;    {width of KeyDef entries}
        Country:word;       {language ID}
        TableTypeID:word;   {the table type; 1st byte (type):     01X 00X     }
                            {                2nd byte (sub-type): 00X reserved}
        SubCountryID:cardinal;
                            {sub-language identifier}
        Reserved:array[1..8] of word;
    end;
    PXHeader=^TXHeader;

{element of TXLateTbl, not all entries are used (unused entries are zero)}
    TXLateKeyDef=record
        XlateOp:word;   {translate operation specifier;             }
                        {bits 0- 6 - AccentFlags (see Notes 1 and 8)}
                        {bits 7-15 - KeyType (see Note 2)           }
        Char1:char;
        Char2:char;
        Char3:char;
        Char4:char;
        Char5:char;
    end;
    PXLateKeyDef=^TXLateKeyDef;

{record type for character definition in TAccentEntry}
    TKeyCode=record
        CharCode:char;
        ScanCode:byte;
    end;

{accent entry definitions for TAccentTable, see Notes 1 and 9}
    TAccentEntry=record
        NonAccent:TKeyCode;             {char/scan code when}
                                        {not used as accent }
        CtlAccent:TKeyCode;             {char/scan code when}
                                        {used with Ctrl key }
        AltAccent:TKeyCode;             {char/scan code when}
                                        {used with Alt key  }
        Maps:array[1..20] of TKeyCode;  {from char-to-char for translation}
    end;
    PAccentEntry=^TAccentEntry;

{table of accent key definitions for TXLateTbl}
    TAccentTable=array[1..7] of TAccentEntry;
    PAccentTable=^TAccentTable;

{record type for SetCustXT, one element for each possible scan code
(entries are in scan code order, based on the remapped scan codes
returned by the keyboard controller)}
    TXLateTbl=record
        XHeader:TXHeader;
        KeyDefs:array [1..127] of TXLateKeyDef;
        AccentTbl:TAccentTable;
    end;
    PXLateTbl=^TXLateTbl;

{Remarks for TXLateTbl record type:

  The request changes the device driver resident code page for the system
  and updates the zero entry of the Code Page Control Block.

* Note 1
  The AccentFlags field of the KeyDef record has seven flags that are
  individually set if a corresponding entry in the accent table applies to this
  scan code. If the key pressed immediately before the current one was an
  accent key and the bit for that accent is set in the AccentFlags field for
  the current key, the corresponding AccentTable entry is searched for the
  replacement character value to use.  If no replacement is found and bit 6 of
  the XlateFlags1 field is set, the not-an-accent beep is sounded and the
  accent character and current character are passed as two separate
  characters.  Also see Note 8.

* Note 2

  The KeyType field of the KeyDef record currently has the following values
  defined. The remaining values up to 1Fh are undefined. The effect of each
  type of shift is defined below. Except where otherwise noted, when no
  shifts are active, Char1 is the translated character. (See Note 3.) Notice
  that any of the Alt, Alt+Char, Alt+Shift, or Alt+Gr keys (or all of them) can
  be present on a keyboard based on the AltGrafL and AltGrafR bits in the
  XTableFlags1 flag word in the table header.

  01h   AlphaKey. Alphabetical character key:

        Shift            Uses Char2. If Caps Lock, uses Char1.
        Caps Lock        Uses Char2. If Shift, uses Char1.
        Ctrl             Set standard control code for this key's Char1 value.
                         See Note 4.
        Alt              Standard extended code.  See Note 7.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  02h   SpecKey. Special nonalphabetic character key, no Caps Lock or Alt:

        Shift            Uses Char2.
        Caps Lock        No effect, only depends on Shift, or Ctrl.
        Ctrl             See Note 4.
        Alt              Marked undefined.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  03h   SpecKeyC. Special nonalphabetic character key with Caps Lock. See
        Note 15.

        Shift            Uses Char2. If Caps Lock, uses Char1.
        Caps Lock        Uses Char2. If Shift, uses Char1.
        Ctrl             See Note 4.
        Alt              Uses Char4, if not zero.  See Note 7.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  04h   SpecKeyA. Special nonalphabetic character key with Alt (no Caps
        Lock):

        Shift            Uses Char2.
        Caps Lock        No effect; depends on Shift, Ctrl, or Alt only.
        Ctrl             See Notes 5 and 9.
        Alt              See Notes 7 and 10.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  05h   SpecKeyCA. Special nonalphabetic character key with Caps Lock and
        Alt:

        Shift            Uses Char2. If Caps Lock, uses Char1.
        Caps Lock        Uses Char2. If Shift, uses Char1.
        Ctrl             See Note 4.
        Alt              See Note 7.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  06h   FuncKey. Function keys. Char1 = n in Fn; Char2 ignored. Sets
        extended codes 58+Char1, if no shift; if F11 or F12, uses 139 and 140.

        Shift            Sets extended codes 83+Char1. F11 and F12 use 141
                         and 142, respectively.
        Caps Lock        No effect on function keys.
        Ctrl             Sets extended codes 93+Char1. F11 and F12 use 143
                         and 144, respectively.
        Alt              Sets extended codes 103+Char1. F11 and F12 use 145
                         and 146, respectively.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  07h   PadKey. Keypad keys (see Note 5 for definition of Char1). Note that
        nonshifted use of these keys is fixed to the extended codes:

        Shift            Uses Char2, unless Num Lock. See Note 5.
        Caps Lock        No effect on pad keys, unless Num Lock. See Note 5.
        Ctrl             Sets extended codes. See Note 5.
        Alt              Used to build a character. See Note 5.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  08h   SpecCtlKey. Special action keys, when used with Ctrl pressed:

        Shift            No effect on these keys.
        Caps Lock        No effect on these keys.
        Ctrl             Uses Char2.
        Alt              See Note 7.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  09h   PrtSc. Print Screen key; sets Char1 normally (see Note 17):

        Shift            Signal the Print Screen function.
        Caps Lock        No effect on this key.
        Ctrl             Sets extended code and signals the Print Echo function.
        Alt              Marked undefined.
        Alt+Char         Uses Char3, if it is not 0.
        Alt+Shift        Uses Char3, if it is not 0.
        Alt+Gr           Uses Char3, if it is not 0.

  0Ah   SysReq. System Request key; treated like a shift key. See Note 6.

  0Bh   AccentKey. Keys that affect the next key pressed (also known as
        dead keys). Char1 is an index into the AccentTbl field of the
        XlateTable, selecting the AccentEntry that corresponds to this key.
        Char2 and Char3 do the same for the shifted Accent character. See
        Note 15.

        Shift            Uses Char2 to index to applicable AccentEntry.
        Caps Lock        No effect on this key.
        Ctrl             Uses CtlAccent character from AccentEntry. See Note 8.
        Alt              Uses AltAccent character from AccentEntry. See Note 8.
        Alt+Char         Uses Char3 to index to applicable AccentEntry.
        Alt+Shift        Uses Char3 to index to applicable AccentEntry.
        Alt+Gr           Uses Char3 to index to applicable AccentEntry.

  Note: Key types 0Ch - 13h set Char1 and Char2 to mask values as defined
        in Note 6.

  0Ch   ShiftKeys. Shift or Ctrl key, sets and clears flags. Char1 holds the
        bits in the lower byte of the shift status word to set when the
        key is down and clear when the key is released. Char2 does the
        same thing for the upper byte of the shift status word unless the
        secondary key prefix (hex E0) is seen immediately prior to this key,
        in which case Char3 is used in place of Char2.

  0Dh   ToggleKey. General toggle key (like Caps Lock). Char1 holds the
        bits in the lower byte of the shift status word to toggle on the
        first make of the key after it is pressed. Char2 holds the bits in
        the upper byte of the shift status word to set when the key is
        down and clear when the key is released unless the secondary key
        prefix (hex E0) is seen immediately prior to this key, in which case
        Char3 is used in place of Char2.

  0Eh   AltKey. Treated just like ShiftKeys above, but has its own key
        type, because when seen, the accumulator used for Alt+PadKey
        entry is zeroed to prepare such entry (see Note 5). Sometimes this
        key is treated as the AltC/S/G key (that is, either Alt+Char,
        Alt+Shift, or Alt+Gr) if one of the AltGraf bits is on in XTableFlags1.

  0Fh   Num Lock. Normally behaves like ToggleKey, but the physical
        keyboard device driver sets a pause screen indication when this
        key is used with the Ctrl key pressed. The pause is cleared on the
        following keystroke if that stroke is a character-generating key.

  10h   Caps Lock. This key is treated as a type 0Dh toggle key. It has a
        separate entry here so that it can be processed like a Shift Lock
        key when that flag is set in the XTableFlags1 word in the header.
        When treated as a Shift Lock, the Caps Lock flag in the shift
        status word is set on on any make of this key, and only cleared
        when the left or right shift key is pressed. Char2 and Char3 are
        processed the same as ToggleKey.

  11h   Scroll Lock. Normally behaves like ToggleKey but has a separate
        entry here. When used with Ctrl, it can be recognized as
        Ctrl+Break.

  12h   XShiftKey. Extended Shift Key (for Country support). See Note 9.

  13h   XToggleKey. Extended Toggle Key (for Country support). See Note 9.

  14h   SpecKeyCS. Special key 1 for country keyboard processing. See Note 15.

        Shift            Uses Char2.
        Caps Lock        Uses Char4.
        Ctrl             See Note 4.
        Alt              See Note 7.
        Alt+Char         Uses Char3.
        Alt+Shift        Uses Char3.
        Alt+Gr           Uses Char3.
        Caps+Shift       Uses Char5.

  15h   SpecKeyAS.  Special key 2 for country keyboard processing. See Note 15.

        Shift            Uses Char2.
        Caps Lock        No effect on this key.
        Ctrl             See Note 4.
        Alt              Uses Char 4. See Note 14.
        Alt+Char         Uses Char 3. See Note 14.
        Alt+Shift        Uses Char 3. See Note 14.
        Alt+Gr           Uses Char 3. See Note 14.

  1Ah   Extended Extended key. This corresponds to the BIOS level support
        provided for INT 16h, Functions 20h, 21h, and 22h.

        Shift            Uses Char2.
        Caps Lock        No effect on this key.
        Ctrl             Uses Char4.
        Alt              Uses Char5.
        Alt+Char         Uses Char 3, if not 0.
        Alt+Shift        Uses Char 3, if not 0.
        Alt+Gr           Uses Char 3, if not 0.
  16h-
  1FFh  Reserved, except for 1Ah, the Extended Extended key (see above).

* Note 3

  Undefined Character Code.  Any key combination that does not fall into any
  of the defined categories.  For example, the Ctrl key pressed along with a
  key that has no defined control mapping is mapped to the value 0, and the
  key type is set in the KeyPacket record indicating undefined translation.
  The KeyPacket record passed to the monitors, if installed, contain the
  original scan code in the ScanCode field and the 0 in the Character field for
  this key.  Notice that no character data records with an undefined character
  code are placed in the keyboard input buffer.

* Note 4

  Ctrl Key. The six possible situations that can occur when a key is pressed
  with only the Ctrl+shift key are shown below:

    -  The key pressed is an AlphaKey character. In this case, the Ctrl plus
       Char1 combination defines one of the standard defined control codes
       (all numbers are decimal):

           Ctrl-   Mapping   Code Name        Ctrl-   Mapping   Code Name
           -----   -------   ---------        -----   -------   ---------
             a        1         SOH             n       14         SO
             b        2         STX             o       15         SI
             c        3         ETX             p       16         DLE
             d        4         EOT             q       17         DC1
             e        5         ENQ             r       18         DC2
             f        6         ACK             s       19         DC3
             g        7         BEL             t       20         DC4
             h        8         BS              u       21         NAK
             i        9         HT              v       22         SYN
             j       10         LF              w       23         ETB
             k       11         VT              x       24         CAN
             l       12         FF              y       25         EM
             m       13         CR              z       26         SUB

       Notice that any key defined as AlphaKey uses the Char1 code value
       minus 96 (ASCII code for a) plus 1 to set the mapping shown above.
       Any scan code defined as AlphaKey must assign to Char1 one of the
       allowed lower case letters.

    -  The key pressed is a nonalpha character, such as [, but is not an action
       key, such as Enter, Backspace, or an arrow key. This is a
       SpecKey[C][A] in the list of key types in the previous example. In this
       case, with one exception, the mapping is based on the scan code of the
       key. Though the key can be relabeled, the Ctrl+Char combination is
       always mapped based on the scan code of the key using the following
       table (all numbers are decimal):
}(*
            Scan     US Kbd     Mapped     Name of
            Code     Legend     Value      New Code
            ----     ------     ------     --------
              3       2 @          0         Null
              7       6 ^         30         RS
             12       - _         31         US   (see Note below)
             26       [ {         27         Esc
             27       ] }         29         GS
             43       \ |         28         FS
*){
       Note:  The mapping for the hyphen character (-) is the one exception.
              The scan code for it is ignored; only the ASCII code for hyphen
              (decimal 45) is looked for in Char1 when mapping the Ctrl+-
              combination. This is because there can be more than one
              occurrence of the hyphen (-) key on the keyboard.  The Ctrl+-
              (PadKey minus) combination produces character/scan code values
              of 00/8Eh, respectively.

    -  The key pressed is an action key such as Enter, Backspace, or an arrow
       key. These keys generate special values when used in conjunction with
       the Ctrl key. Those actions are defined in other notes where they
       apply. Two particular keys in this category are:

           Ctrl+Enter     = LF(010)
           Ctrl+Backspace = Del(127)

    -  The key pressed is a function key, F1 - F12. See the FuncKey
       description in Note 2.

    -  The key pressed is an accent key. See Note 8.

    -  The key is not defined in conjunction with Ctrl. In this case, the key
       is treated as undefined, as described in Note 3.

* Note 5

  PadKey. The pad keys have several uses that depend on various shift
  states. Some of them are based on their position on the keyboard. Because
  keyboard layouts change, the hard-coded assumed positions of the keypad
  keys, with the offset value that must be coded into Char1, are defined
  below. Any remapping must use the Char1 values shown below for the keys
  that correspond to the pad keys given by the Legend or Char2 values:

       US Kbd     Scan     Char1        Char2
       Legend     Code     Required     US Kbd     With Ctrl
       -------    ----     ---------    -------    -----------
       Home  7     71      Decimal 0    ASCII 7    Decimal 119
       Up    8     72          "   1      "   8      "     141
       PgUp  9     73          "   2      "   9      "     132
             -     74          "   3      "   -      "     142
       Left  4     75          "   4      "   4      "     115
             5     76          "   5      "   5      "     143
       Right 6     77          "   6      "   6      "     116
             +     78          "   7      "   +      "     144
       End   1     79          "   8      "   1      "     117
       Down  2     80          "   9      "   2      "     145
       PgDn  3     81          "  10      "   3      "     118
       Ins   0     82          "  11      "   0      "     146
       Del   .     83          "  12      "   .      "     147

  Notice that when Num Lock is off, or if Shift is active and Num Lock on, the
  code returned is the extended code. The code returned corresponds to the
  Legends above (Home, PgUp, and so forth). When Num Lock is on, or if
  Shift is active and Num Lock is off, the code returned is Char2. Notice that
  the + and - keys also return Char2 when the shift key is down.

  When the Alt key is used with the PadKeys, the absolute value of the
  pressed key (looked up using the required Char1 value) is added to the
  accumulated value of any of the previous numeric keys pressed, without
  releasing the Alt key. Before adding the new number to the accumulated
  value, that accumulation is multiplied by ten, with overflow beyond 255
  ignored. When Alt is released, the accumulation becomes a Character code
  and is passed along with a scan code of zero. Notice that if any key other
  than the 10 numeric keys is pressed, the accumulated value is reset to zero.
  When the keypad *, -, or + keys are pressed while the Alt key is down, the
  extended characters 55, 74, and 78 (decimal) are returned, respectively.

  When AltGraphics is used with the PadKeys, the Char3 value is returned if it
  is nonzero, and if an AltGraf bit is set in XTableFlags1; otherwise, it is
  treated the same as the Alt key.

  On the Enhanced keyboard, the secondary keypad keys return, as an
  extended character, the scan code of the key plus 80 (decimal) when
  pressed in conjunction with the Alt key.  The secondary / key returns an
  extended character of 164, when pressed in conjunction with the Alt key.

* Note 6

  State Key. Each state key entry has Char1, Char2, and Char3 defined as
  follows:

    -  Char1. A mask to set the appropriate bit in the low byte of the
       keyboard Shift Flags when the state key is pressed. When the state
       key is a toggle key, the set bit is toggled each additional time the key
       is pressed. When the state key is not a toggle key, the set bit is
       cleared when the key is released.

    -  Char2. A mask to set the appropriate bit in the high byte of the
       Keyboard Shift Flags when the key is pressed.

    -  Char3. Used in place of Char2 when the secondary key prefix is seen
       immediately prior to this key.

  The masks are shown below (numbers are in hex):

       Key            Char1    Char2    Char3
       -----------    -----    -----    -----
       Right Shift      01       00       00
       Left Shift       02       00       00
       Ctrl Shift       04       01       04
       Alt Shift        08       02       08
       Scroll Lock      10       10       10
       Num Lock         20       20       20
       Caps Lock        40       40       40
       SysReq           00       80       80

  Notice that the INS key is not treated as a state key, but as a pad key.
  Also, SysReq is included here because it is treated as a shift key.

* Note 7

  Alt Character. Most of the keys defined in a category that allows the Alt
  key (AlphaKey, SpecKeyA, SpecKeyCA) return a value called an extended
  character. This value is a character code of 00H or E0H, with a second byte
  (using the ScanCode field of the CharData record) defining the extended
  code. In most cases, this value is the scan code of the key. Since the
  legend on these keys can be remapped on a foreign language keyboard, the
  Alt-based extended code is hard to define in a general sense. The following
  rules are used:

    -  AlphaKey. The extended code is derived from Char1 (the lower-case
       character) as it was originally mapped on the PC keyboard. The
       original scan code value is the extended code that a character returns.
       These keys can be moved and will still return their original Alt
       extended codes.

    -  SpecKeyA and SpecKeyCA. This category is used for all keys that are
       not an alphabetic character or an action code (like Enter or Backspace,
       the only exception being the Tab key, which is treated as a character).
       On foreign keyboards, these keys can be moved around and can have
       new values assigned to them, such as special punctuation symbols.
       Therefore, the Alt mappings must be based on the real scan code as
       any keys defined by the SpecKey_ classification will have only an Alt
       mapping, if it is in one of the positions defined below. In that case,
       the Alt extended code is as shown:
}(*
           Scan    US Kbd    Alt          Scan    US Kbd    Alt
           Code    Legend    Value        Code    Legend    Value
           ----    ------    -----        ----    ------    -----
             2      1 !       120          15      Tab       165
             3      2 @       121          26      [ {        26
             4      3 #       122          27      ] }        27
             5      4 $       123          28      Enter      28
             6      5 %       124          39      ; :        39
             7      6 ^       125          40      ' "        40
             8      7 &       126          41      ' ~        41
             9      8 *       127          43      \ |        43  (equals W.T.C. key number 42)
            10      9 (       128          51      , <        51
            11      0 )       129          52      . >        52
            12      - _       130          53      / ?        53
            13      = +       131
*){
       The secondary / key returns an extended character of 164 when
       pressed while Alt is down.

    -  FuncKey. Defined in Note 2.

    -  SpecCtlKey. The Alt+ values of the Escape, Backspace, and Enter keys
       are extended characters equaling 1, 14, and 28 (decimal), respectively.

  When AltGraphics is used, the Char3 value is returned if it is nonzero and if
  an AltGraf bit is set in XTableFlags1. Otherwise, it is treated the same as
  the Alt key.

* Note 8

  Accent Key. When an accent key is pressed with Ctrl or Alt, it is treated as
  a regular key. The character it translates to is the one in the CtlAccent or
  AltAccent field of the AccentEntry pointed to by the Char5 value of the
  KeyDef. If the key being defined has no defined value with Ctrl or Alt, it
  should have zeros in the field of the undefined combination.

  When an accent key is pressed by itself (or with Right Shift, Left Shift, or
  AltGraphics), it is not translated immediately. The Char1 (or Char2, when
  Left or Right Shift or AltGraphics is used) index in the KeyDef record is
  used with the next key received to check if the next key has an accent
  mapping. If that next key has no mapping for this accent (that is, if it has
  no bit set in its AccentFlags), or if that next key is not found in this
  accent's AccentEntry, then the character value in the NonAccent field of the
  AccentEntry is used as the character to display. It is followed by the
  translation of that next key after the not-an-accent beep is sounded.

  Notice that if a key doesn't change when a Left or Right Shift key is
  pressed, it should use the same value for Char1 and Char2 so the accent
  applies in both the shifted and nonshifted cases. If the accent value is
  undefined when used with a shift key or AltGraphics, the value in Char2 or
  Char3 should be 0.

  Any accent key that doesn't have an Alt or Ctrl mapping should put zeros in
  the AltAccent and CtlAccent fields of its AccentEntry. If the value in the
  table is between 1 and 7, then the key is considered an accent key and
  further accent key processing is indicated. See Note 1 for more information.

* Note 9

  Extended State Key. For special Country support, the keyboard device
  driver maintains another byte of shift status. Key types 12h and 13h are
  provided for manipulation of that byte. The other fields of the KeyDef are:

    -  Char1. A mask in which bits that are on define the field being used for
       the Char2 value. Only bits in the NLS shift status byte that correspond
       to the bits in this byte are altered by the Char2 value.

    -  Char2. For KeyType 12h (Extended Shift), the value to OR into the
       byte when the make code is seen. Also, the inverted value is ANDed
       when the break code is seen. For KeyType 13h (Extended Toggle), the
       value XORed into the byte on each make code seen (break code
       ignored).

    -  Char3. Use in place of the Char2 when the secondary key prefix (hex
       E0) is seen immediately prior to this key.

  For example, Char1 or Char2 can define single shift status bits to
  set/clear/toggle. Char2 can be a set of coded bits, delineated by Char1, that
  are set to a numeric value when the key is pressed and cleared to zero
  when released (or on the next press, if toggled). The whole byte can be
  set to Char2 when Char1 has all bits on.

* Note 10

  Space Key. The key treated as the space character should have a flag set
  in its AccentFlags field for each possible accent (that is, for each defined
  AccentEntry in the AccentTable). And each AccentEntry should have the
  Space character defined as one of its accented characters, with the
  translation having the same value as the accent character itself. The reason
  for this is that, by definition, an Accent Key followed by the space
  character maps to the accent character alone. If the table is not set up as
  just described, a not-an-accent beep is sounded whenever the accent key
  followed by a space is pressed.

  Notice that the space key is defined as a SpecKeyA (type 4) because its
  use, in conjunction with the Alt key, is allowed. In this case, and when
  used with the Ctrl key, it returns the ASCII space character. This works
  correctly, except in the case of the diaresis accent (double-dot) in code
  page 437. The space is treated as an invalid character and the beep result
  occurs, with the diaresis represented by double quotation marks. The
  characters displayed depend upon the language in effect when the invalid
  diaresis is encountered. For some languages, the character substituted is
  the double-quotation marks; for others, the character used is the F9h
  character.

* Note 11

  KbdType identifies the hardware-specific keyboard used by this table. The
  values and allowable types are the same as those specified in IOCTL call
  KBD_GETKEYBDTYPE (1 means extended keyboard, which is used for all common
  keyboard types).

* Note 12

  The DefaultTable flag in XtableFlags1 is used by the KEYB utility in loading
  code pages when changing from one language to another. It identifies the
  default code page to KEYB, should KEYB not find one or both CODEPAGE=
  defined code pages.

* Note 13

  The Language IDs and Subcountry IDs used are as follows:

  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
  ³Keyboard Layout ³Keyboard Layout ³Country             ³
  ³Country Code    ³SubCountry Code ³                    ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       AR       ³       785      ³Arabic-speaking     ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       BE       ³       120      ³Belgium             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       CF       ³       058      ³Canadian-French     ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       CS       ³       243      ³Czech Republic      ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       CS       ³       245      ³Czech Republic      ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       DK       ³       159      ³Denmark             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       SU       ³       153      ³Finland             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       FR       ³       120      ³France              ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       FR       ³       189      ³France              ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       GR       ³       129      ³Germany             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       HE       ³       972      ³Hebrew-speaking     ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       HU       ³       208      ³Hungary             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       IS       ³       197      ³Iceland             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       IT       ³       141      ³Italy               ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       IT       ³       142      ³Italy               ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       LA       ³       171      ³Latin-American      ³
  ³                ³                ³Spanish             ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       NL       ³       143      ³Netherlands         ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       NO       ³       155      ³Norway              ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       PL       ³       214      ³Poland              ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       PO       ³       163      ³Portugal            ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       SP       ³       172      ³Spain               ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       SV       ³       153      ³Sweden              ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       SF       ³       150F     ³Swiss-French        ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       SG       ³       150G     ³Swiss-German        ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       TR       ³       179      ³Turkey              ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       UK       ³       166      ³United Kingdom      ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       UK       ³       168      ³United Kingdom      ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       US       ³       103      ³United States       ³
  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
  ³       YU       ³       234      ³Former Yugoslavia   ³
  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

* Note 14

  Keytype 15. When the Alt or Alt+Shift keys are pressed, both XlatedChar
  and XlatedScan in the CharData record will have the same value.

* Note 15

  If the Charx value is in the range of 1-7, then Charx identifies an accent
  key. Otherwise, Charx is treated as a valid ASCII character. This does not
  apply to Ctrl+Charx sequences.

  Note 16

  If Alt+Gr, Alt+Shift, or Alt+Ctrl are pressed, and Char3 is 0, the Alt key is
  used to translate to a valid result.

  Note 17

  The * key on the keypad of the Enhanced keyboard, although producing the
  same scan code/character as that of the IBM Personal Computer AT*
  keyboard, is treated differently because a dedicated Print Screen key exists
  on the Enhanced keyboard. The following scan codes/characters are
  returned by the physical keyboard device driver for the Enhanced keyboard
  * key on the keypad:

  Unshifted    37H/2AH
  Shifted      37H/2AH
  Ctrl         96H/00
  Alt          37H/00

* Note 18

  Size. The code page described here has the following dimensions:

      Xlate Header                =    40
      127 KeyDefs @ 7 bytes       =   889
      7 AccentEntries @ 46 bytes  =   322
                                     ----
                                     1251 bytes

  If more than 6 AccentEntries are needed, then the following format is used:
  In the first 6 AccentEntries, the length is set at 20, with unused elements
  set to zero. For each AccentEntry of 7 and greater, up to 120 element pairs
  may exist, and the length is dynamic.

  For each AccentEntry of 7 and greater, the first byte in the record will
  contain the LENGTH of the AccentEntry record. The LENGTH value is defined
  as the total length in bytes of the AccentEntry record including the LENGTH
  byte.

  The record is defined as follows:

    AccEnt <l,a,b,c,d,e,f,c1,s1,c2,s2..c120,s120>
    where....
    l is the total length in bytes of the AccEnt including itself.
    a &b are the scan code &char to use when the key following this accent
    is not affected by the accent so the accent itself must be used.
    c &d are the scan code &char to use when Ctl+[accent] is pressed.
    e &f do the same for Alt+[accent].
    c1,s1 - c120,s120 are the char/scan code mapping for accented translation.

  Adding more than 7 accents will make the standard 1251-byte table an
  extended variable size.
}



{Register a keyboard subsystem within a session.}
{ModuleName - dynamic link module name, maximum length is 8 bytes, ProcName is
a dynamic link entry point name of a routine that receives control when any of
the registered functions are called. Maximum length is 32 bytes, FnMask - see
KR_* constants}
{Possible return codes:
    0         No_Error
    408       Error_Kbd_Invalid_ASCIIZ
    409       Error_Kbd_Invalid_Mask
    410       Error_Kbd_Register
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* There can be only one KbdRegister call outstanding for each session without
  an intervening KbdDeRegister. KbdDeRegister must be issued by the same
  process that issued the KbdRegister.}
function KbdRegister(ModuleName,ProcName:PChar;FnMask:cardinal):word; cdecl;
function KbdRegister(ModuleName,ProcName:string;FnMask:cardinal):word;

{Deregister a keyboard subsystem previously registered within a session - only
the process that issued the KbdRegister may issue KbdDeRegister.}
{Possible return codes:
    0         No_Error
    411       Error_Kbd_Deregister
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
function KbdDeRegister:word; cdecl;

{Return a character data record from the keyboard.}
{Key - see TKbdKeyInfo record type, WaitFlag - see IO_Wait and IO_NoWait
constants, KbdHandle is the default keyboard (0) or a logical keyboard.}
{Possible return codes are:
    0         No_Error
    375       Error_Kbd_Invalid_IOWait
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* On an enhanced keyboard, the secondary enter key returns the normal
  character 0Dh and a scan code of E0h.
* Double-byte character codes (DBCS) require two function calls to obtain the
  entire code.
* If shift report is set with KbdSetStatus, the CharData record returned
  reflects changed shift information only.
* Extended ASCII codes are identified with the status byte, bit 1 on and the
  ASCII character code being either 00h or E0h. Both conditions must be
  satisfied for the character to be an extended keystroke.  For extended
  ASCII codes, the scan code byte returned is the second code (extended
  code). Usually the extended ASCII code is the scan code of the primary key
  that was pressed.
* A thread in the foreground session that repeatedly polls the keyboard with
  KbdCharIn (with no wait), can prevent all regular priority class threads
  from executing.  If polling must be used and a minimal amount of other
  processing is being performed, the thread should periodically yield to the
  CPU by issuing a DosSleep call for an interval of at least 5 milliseconds.}
function KbdCharIn(var Key:TKbdKeyInfo;WaitFlag,KbdHandle:word):word; cdecl;

{Return any available character data record from the keyboard
without removing it from the buffer.}
{Key - see TKbdKeyInfo record type, KbdHandle is the default keyboard (0)
or a logical keyboard.}
{Possible return codes are:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* On an enhanced keyboard, the secondary enter key returns the normal
  character 0Dh and a scan code of E0h.
* Double-byte character codes (DBCS) require two function calls to obtain the
  entire code.
* If shift report is set with KbdSetStatus the CharData record returned,
  reflects changed shift information only.
* Extended ASCII codes are identified with the status byte, bit 1 on and the
  ASCII character code being either 00h or E0h. Both conditions must be
  satisfied for the character to be an extended keystroke. For extended
  ASCII codes, the scan code byte returned is the second code (extended
  code). Usually the extended ASCII code is the scan code of the primary key
  that was pressed.
* A thread in the foreground session that repeatedly polls the keyboard with
  KbdCharIn (with no wait), can prevent all regular priority class threads
  from executing. If polling must be used and a minimal amount of other
  processing is being performed, the thread should periodically yield the CPU
  by issuing a DosSleep call for an interval of at least 5 milliseconds.}
function KbdPeek(var Key:TKbdKeyInfo;KbdHandle:word):word; cdecl;

{Read a character string (character codes only) from the keyboard.}
{CharBuf is a buffer for returned characters, LenInOut - see TStringInBuf
record type, WaitFlag - see IO_Wait and IO_NoWait constants, KbdHandle is the
default keyboard (0) or a logical keyboard.}
{Possible return codes are:
    0         No_Error
    375       Error_Kbd_Invalid_IOWait
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* The character strings may be optionally echoed on the display if echo mode
  is set. When echo is on each character is echoed as it is read from the
  keyboard. Echo mode and binary mode are mutually exclusive. Reference
  KbdSetStatus and KbdGetStatus for more information.
* The default input mode is ASCII. In ASCII mode, 2-byte character codes only
  return in complete form. An extended ASCII code is returned in a 2-byte
  string. The first byte is 0Dh or E0h and the next byte is an extended code.
* In input mode (binary, ASCII), the following returns can be set and
  retrieved with KbdSetStatus and KbdGetStatus:
    Turnaround Character
    Echo Mode
    Interim Character Flag
    Shift State
* The received input length is also used by the KbdStringIn line edit
  functions for re-displaying and entering a caller specified string. On the
  next KbdStringIn call the received input length indicates the length of the
  input buffer that may be recalled by the user using the line editing keys.
  A value of 0 inhibits the line editing function for the current KbdStringIn
  request.
* KbdStringIn completes when the handle has access to the physical keyboard
  (focus), or is equal to zero and no other handle has the focus.}
function KbdStringIn(var CharBuf;var LenInOut:TStringInBuf;WaitFlag:word;
                                                   KbdHandle:word):word; cdecl;
function KbdStringIn(CharBuf:PChar;LenInOutP:PStringInBuf;WaitFlag:word;
                                                   KbdHandle:word):word; cdecl;

{Clear the keystroke buffer.}
{KbdHandle is the default keyboard (0) or a logical keyboard.}
{Possible return codes are:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* KbdFlushBuffer completes when the handle has access to the physical
  keyboard (focus), or is equal to zero and no other handle has the focus.}
function KbdFlushBuffer(KbdHandle:word):word; cdecl;

{Set the characteristics of the keyboard.}
{Status - see TKbdInfo record type, KbdHandle is the default keyboard (0) or
a logical keyboard.}
{Possible return codes are:
    0         No_Error
    376       Error_Kbd_Invalid_length
    377       Error_Kbd_Invalid_Echo_Mask
    378       Error_Kbd_Invalid_Input_Mask
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* Shift return (bit 8 in sysstate) must be disabled in ASCII mode.
* KbdSetStatus is ignored for a Vio-windowed application.}
function KbdSetStatus(var Status:TKbdInfo;KbdHandle:word):word; cdecl;

{Get the current state of the keyboard.}
{Status - see TKbdInfo record type, KbdHandle is the default keyboard (0) or
a logical keyboard.}
{Possible return codes:
    0         No_Error
    376       Error_Kbd_Invalid_Length
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* The initial state of the keyboard is established by the system at
  application load time. Some default states may be modified by the
  application through KbdSetStatus. KbdGetStatus returns only those keyboard
  parameters initially set by KbdSetStatus. The returned parameters are:
  Input Mode, Interim Character Flags, Shift State, Echo State, TurnAround
  Character
* KbdGetStatus completes only when the handle has access to the physical
  keyboard (focus) or the handle is 0 and no other handle has the focus.}
function KbdGetStatus(var Status:TKbdInfo;KbdHandle:word):word; cdecl;

{Set the code page used to translate key strokes received from the keyboard for
current process.}
{Reserved - reserved, must be set to 0, CodePage - code-page ID in the
application's data area, must be equivalent to one of the code-page IDs
specified on the CONFIG.SYS CODEPAGE= statement or 0, an error results
otherwise, KbdHandle is the default keyboard (0) or a logical keyboard.}
{Possible return codes:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    448       Error_Kbd_Invalid_CodePage
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* Keyboard code page support is not available without the DEVINFO=KBD
  statement in the CONFIG.SYS file.}
function KbdSetCp(Reserved,CodePage,KbdHandle:word):word; cdecl;

{Query the code page being used to translate scan codes to ASCII characters.}
{Reserved must be set to 0. The keyboard support returns the current code
page for a specified keyboard handle in CodePage, it is one of the code page
IDs specified in the CONFIG.SYS CODEPAGE= statement or 0000. KbdHandle is
the default keyboard (0) or a logical keyboard.}
{Possible return codes:
    0         No_Error
    373       Error_Kbd_Parameter
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* CodePage is set to the currently active keyboard code page. A value of 0
  indicates the code page translation table in use is the ROM code page
  translation table provided by the hardware.}
function KbdGetCp(Reserved:cardinal;var CodePage:word;KbdHandle:word):word;
                                                                         cdecl;

{Create a new logical keyboard.}
{Handle for the new logical keyboard returned in KbdHandle.}
{Possible return codes:
    0         No_Error
    440       Error_Kbd_No_More_Handle
    441       Error_Kbd_Cannot_Create_KCB
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* KbdOpen blocks while another thread has the keyboard focus (by way of
  KbdGetFocus) until the thread with the focus issues KbdFreeFocus.
  Therefore, to prevent KbdOpen from blocking, it is recommended that KbdOpen
  be issued only while the current thread has the focus. For example:
    KbdGetFocus     wait until focus available on handle 0
    KbdOpen         get a logical keyboard handle
    KbdFreeFocus    give up the focus on handle 0}
function KbdOpen(var KbdHandle:word):word; cdecl;

{Close the existing logical keyboard identified by the keyboard handle}
{KbdHandle is the default keyboard (0) or a logical keyboard}
{Possible return codes:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* KbdClose blocks while another thread has the keyboard focus (by way of
  KbdGetFocus) until the thread with the focus issues KbdFreeFocus.
  Therefore, to prevent KbdClose from blocking, it is recommended that
  KbdClose be issued only while the current thread has the focus.  For
  example:
    KbdGetFocus     wait until focus available on handle 0
    KbdClose        close a logical keyboard handle
    KbdFreeFocus    give up the focus on handle 0}
function KbdClose(KbdHandle:word):word; cdecl;

{Bind the logical keyboard to the physical keyboard.}
{KbdHandle is the default keyboard (0) or a logical keyboard}
{Possible return codes:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
function KbdGetFocus(WaitFlag,KbdHandle:word):word; cdecl;

{Free the logical-to-physical keyboard bond created by KbdGetFocus.}
{KbdHandle is the default keyboard (0) or a logical keyboard}
{Possible return codes:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* KbdFreeFocus may be replaced by issuing KbdRegister. Unlike other keyboard
  subsystem functions, the replaced KbdFreeFocus is called only if there is
  an outstanding focus.}
function KbdFreeFocus(KbdHandle:word):word; cdecl;

{Synchronize access from a keyboard subsystem to the keyboard device driver.}
{WaitFlag - see IO_Wait and IO_NoWait constants (wait / don't wait for access
to the device driver.}
{Possible return codes:
    0         No_Error
    121       Error_Sem_TimeOut}
{Remarks:
* KbdSynch blocks all other threads within a session until return from the
  subsystem to the router. To ensure proper synchronization, KbdSynch should
  be issued by a keyboard subsystem if it intends to issue a DosDevIOCtl or
  access dynamically shared data. KbdSynch does not protect globally shared
  data from threads in other sessions.}
function KbdSynch (WaitFlag:word):word; cdecl;

{Raise the priority of the foreground keyboard's thread.}
{Possible return codes:
    0         No_Error
    447       Error_Kbd_Keyboard_Busy
    504       Error_Kbd_Extended_SG}
{Remarks:
* KbdSetFgnd marks the current process that owns the keyboard. Threads in
  this process receive a priority boost. The previous foreground keyboard
  threads lose their priority boost.
* This function should only be issued by a Keyboard Subsystem during
  KbdCharIn or KbdStringIn processing.}
function KbdSetFgnd:word; cdecl;

{Return the attached keyboard's hardware-generated identification value.}
{HWID is a pointer to the caller's data area, see TKbdHWID, KbdHandle is the
default keyboard (0) or a logical keyboard.}
{Possible return codes:
    0         No_Error
    373       Error_Kbd_Parameter
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* In past OS/2 releases, all keyboards could be supported by knowing the
  hardware family information available with keyboard IOCTL 77h. However,
  with the addition of the 122-key keyboard, recognition was not containable
  by hardware family information alone. The 122-key keyboard has a number of
  differences from other keyboards. Therefore, applications performing
  keystroke specific functions may need to determine specifically which
  keyboard is attached.
* This function is of particular usefulness for applications providing Custom
  Translate Tables and mapping keyboard layouts.}
function KbdGetHWID(var HWID:TKbdHWID;KbdHandle:word):word; cdecl;

{Undocumented in official IBM documentation}
function KbdSetHWID(var HWID:TKbdHWID;KbdHandle:word):word; cdecl;
function KbdSetHWID(HWIDP:PKbdHWID;KbdHandle:word):word; cdecl;

{Translate scan codes with shift states into ASCII codes.}
{TransData - see TKbdTransData, KbdHandle is the default keyboard (0) or a
logical keyboard.}
{Possible return codes:
    0         No_Error
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* It may take several calls to complete a translation because of accent key
  combinations, or other complex operations.
* The fsShift and sZero are for use by the keyboard translation routines.
  These fields are reserved and must only be accessed by the caller prior
  to starting a translation sequence and then they must be set to zero.
  The KbdXlate function is intended to be used for translating a particular
  scan code for a given shift state. The KbdXlate function is not intended
  to be a replacement for the OS/2 system keystroke translation function.}
function KbdXlate(var TransData:TKbdTrans;KbdHandle:word):word; cdecl;

{Install, on the specified handle, the translate table which this call points
to. This translate table affects only this handle.}
{XLateTbl is the translation table used to translate scan code to ASCII code
for a specified handle (the format of the translation table is documented in
the Set Code Page IOCTL 50h), KbdHandle is the default keyboard (0) or a
logical keyboard.}
{Possible return codes:
    0         No_Error
    377       Error_Kbd_Invalid_Echo_Mask
    378       Error_Kbd_Invalid_Input_Mask
    439       Error_Kbd_Invalid_Handle
    445       Error_Kbd_Focus_Required
    447       Error_Kbd_Keyboard_Busy
    464       Error_Kbd_Detached
    504       Error_Kbd_Extended_SG}
{Remarks:
* The translate table must be maintained in the caller's memory. No copy of
  the translate table is made by KbdSetCustXt.
* KbdSetCp reverses the action of KbdSetCustXt and sets the handle equal to
  one of the system translate tables. If memory is dynamically allocated by
  the caller for the translate table and is freed before the KbdSetCp is
  performed, KbdSetCp and future translations may fail.}
function KbdSetCustXt(var XLateTbl:TXLateTbl;KbdHandle:word):word; cdecl;
function KbdSetCustXt(var CodePage:word;KbdHandle:word):word; cdecl;
function KbdSetCustXt(var XLateTblP:pointer;KbdHandle:word):word; cdecl;


(* Following routines are not supported
   (just have a look in some C header
   file - you probably won't find it there either).
KbdInit (index 2)
KbdLoadInstance (index 6)
KbdSwitchFgnd (index 15)
KbdShellInit (index 16)
KbdFree (index 19)
*)


{***************************************************************************}
implementation
{***************************************************************************}


function KbdRegister(ModuleName,ProcName:PChar;FnMask:cardinal):word; cdecl;
external 'EMXWRAP' index 208;
{external 'KBDCALLS' index 8;}

function KbdRegister(ModuleName,ProcName:string;FnMask:cardinal):word;
begin
    if byte(ModuleName[0])>8 then byte(ModuleName[0]):=8;
    ModuleName[Succ(byte(ModuleName[0]))]:=#0;
    if byte(ProcName[0])>32 then byte(ProcName[0]):=32;
    ProcName[Succ(byte(ProcName[0]))]:=#0;
    KbdRegister:=KbdRegister(@ModuleName[1],@ProcName[1],FnMask);
end;

function KbdDeRegister:word; cdecl;
external 'EMXWRAP' index 220;
{external 'KBDCALLS' index 20;}

function KbdCharIn(var Key:TKbdKeyInfo;WaitFlag,KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 204;
{external 'KBDCALLS' index 4;}

function KbdPeek(var Key:TKbdKeyInfo;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 222;
{external 'KBDCALLS' index 22;}

function KbdStringIn(var CharBuf;var LenInOut:TStringInBuf;WaitFlag:word;
                                                   KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 209;
{external 'KBDCALLS' index 9;}

function KbdStringIn(CharBuf:PChar;LenInOutP:PStringInBuf;WaitFlag:word;
                                                   KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 209;
{external 'KBDCALLS' index 9;}

function KbdFlushBuffer(KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 213;
{external 'KBDCALLS' index 13;}

function KbdSetStatus(var Status:TKbdInfo;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 211;
{external 'KBDCALLS' index 11;}

function KbdGetStatus(var Status:TKbdInfo;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 210;
{external 'KBDCALLS' index 10;}

function KbdSetCp(Reserved,CodePage,KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 205;
{external 'KBDCALLS' index 5;}

function KbdGetCp(Reserved:cardinal;var CodePage:word;KbdHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 203;
{external 'KBDCALLS' index 3;}

function KbdOpen(var KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 223;
{external 'KBDCALLS' index 23;}

function KbdClose(KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 217;
{external 'KBDCALLS' index 17;}

function KbdGetFocus(WaitFlag,KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 212;
{external 'KBDCALLS' index 12;}

function KbdFreeFocus(KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 218;
{external 'KBDCALLS' index 18;}

function KbdSynch (WaitFlag:word):word; cdecl;
external 'EMXWRAP' index 207;
{external 'KBDCALLS' index 7;}

function KbdSetFgnd:word; cdecl;
external 'EMXWRAP' index 221;
{external 'KBDCALLS' index 21;}

function KbdGetHWID(var HWID:TKbdHWID;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 224;
{external 'KBDCALLS' index 24;}

function KbdSetHWID(var HWID:TKbdHWID;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 225;
{external 'KBDCALLS' index 25;}

function KbdSetHWID(HWIDP:PKbdHWID;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 225;
{external 'KBDCALLS' index 25;}

function KbdXlate(var TransData:TKbdTrans;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 214;
{external 'KBDCALLS' index 14;}

function KbdSetCustXt(var XLateTbl:TXLateTbl;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 201;
{external 'KBDCALLS' index 1;}

function KbdSetCustXt(var CodePage:word;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 201;
{external 'KBDCALLS' index 1;}

function KbdSetCustXt(var XLateTblP:pointer;KbdHandle:word):word; cdecl;
external 'EMXWRAP' index 201;
{external 'KBDCALLS' index 1;}


end.
