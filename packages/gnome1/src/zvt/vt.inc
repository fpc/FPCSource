{$IfDef read_interface}

const
   UPDATE_CHANGES = $00;
   UPDATE_REFRESH = $01;
   UPDATE_SCROLLBACK = $02;

type
   Puint32 = ^uint32;
   uint32 = dword;

const
   VTPARAM_MAXARGS = 5;
   VTPARAM_ARGMAX = 20;
   VTPARAM_INTARGS = 20;

type
   Pvt_line = ^Tvt_line;
   Tvt_line = record
        next : Pvt_line;
        prev : Pvt_line;
        line : longint;
        width : longint;
        modcount : longint;
        data : array[0..0] of uint32;
     end;

     PVTTITLE_TYPE = ^TVTTITLE_TYPE;
     TVTTITLE_TYPE = (VTTITLE_WINDOWICON := 0,VTTITLE_ICON,
       VTTITLE_WINDOW,VTTITLE_XPROPERTY);

    const
       VTATTR_BOLD = $40000000;
       VTATTR_UNDERLINE = $20000000;
       VTATTR_BLINK = $10000000;
       VTATTR_REVERSE = $08000000;
       VTATTR_CONCEALED = $04000000;
       VTATTR_MULTIBYTE = $80000000;
       VTATTR_MASK = $ffff0000;
       VTATTR_DATAMASK =  not (VTATTR_MASK);
       VTATTR_CLEARMASK =  not (((VTATTR_BOLD or VTATTR_UNDERLINE) or VTATTR_BLINK) or VTATTR_REVERSE);
       VTATTR_FORECOLOURM = $03e00000;
       VTATTR_BACKCOLOURM = $001f0000;
       VTATTR_FORECOLOURB = 21;
       VTATTR_BACKCOLOURB = 16;

    type
       Pvt_em = ^Tvt_em;
       Tvt_em = record
            cursorx : longint;
            cursory : longint;
            width : longint;
            height : longint;
            scrolltop : longint;
            scrollbottom : longint;
            childpid : pid_t;
            childfd : longint;
            keyfd : longint;
            pty_tag : pointer;
            msgfd : longint;
            savex : longint;
            savey : longint;
            savemode : uint32;
            saveattr : uint32;
            saveremaptable : Pbyte;
            savethis : Pvt_line;
            cx : longint;
            cy : longint;
            sx : longint;
            sy : longint;
            remaptable : Pbyte;
            Gx : longint;
            G : array[0..3] of Pbyte;
            attr : uint32;
            mode : uint32;
            arg : record
                case longint of
                   0 : ( txt : record
                        args_dummy : array[0..(VTPARAM_MAXARGS)-1] of Pbyte;
                        args_mem : array[0..(VTPARAM_MAXARGS * VTPARAM_ARGMAX)-1] of char;
                        argptr_dummy : ^Pbyte;
                        outptr : Pchar;
                        outend_dummy : Pchar;
                     end );
                   1 : ( num : record
                        intargs : array[0..(VTPARAM_INTARGS)-1] of dword;
                        intarg : dword;
                     end );
                end;
            argcnt : longint;
            state : longint;
            this_line : Pvt_line;
            lines : Tvt_list;
            lines_back : Tvt_list;
            lines_alt : Tvt_list;
            scrollback : Tvt_list;
            scrollbacklines : longint;
            scrollbackoffset : longint;
            scrollbackold : longint;
            scrollbackmax : longint;
            ring_my_bell : procedure (user_data:pointer);cdecl;
            change_my_name : procedure (user_data:pointer; _type:TVTTITLE_TYPE; name:Pchar);cdecl;
            dtterm_seq : procedure (user_data:pointer);cdecl;
            user_data : pointer;
            decode : record
                case longint of
                   0 : ( utf8 : record
                        wchar : uint32;
                        shiftchar : longint;
                        shift : longint;
                     end );
                end;
            coding : longint;
         end;


    const
       ZVT_CODE_ISOLATIN1 = 0;
       ZVT_CODE_UTF8 = 1;
       VTMODE_INSERT = $00000001;
       VTMODE_SEND_MOUSE = $02;
       VTMODE_WRAPOFF = $04;
       VTMODE_APP_CURSOR = $00000008;
       VTMODE_RELATIVE = $10;
       VTMODE_APP_KEYPAD = $20;
       VTMODE_SEND_MOUSE_PRESS = $42;
       VTMODE_SEND_MOUSE_BOTH = $82;
       VTMODE_SEND_MOUSE_MASK = $c2;
       VTMODE_BLANK_CURSOR = $100;
       VTMODE_ALTSCREEN = $80000000;

    const
       VT_THRESHHOLD = 4;

function vt_init(vt:Pvt_em; width:longint; height:longint):Pvt_em;cdecl;external libzvtdll name 'vt_init';
procedure vt_destroy(vt:Pvt_em);cdecl;external libzvtdll name 'vt_destroy';
procedure vt_resize(vt:Pvt_em; width:longint; height:longint; pixwidth:longint; pixheight:longint);cdecl;external libzvtdll name 'vt_resize';
procedure vt_parse_vt(vt:Pvt_em; ptr:Pchar; length:longint);cdecl;external libzvtdll name 'vt_parse_vt';
procedure vt_swap_buffers(vt:Pvt_em);cdecl;external libzvtdll name 'vt_swap_buffers';
function vt_forkpty(vt:Pvt_em; do_uwtmp_log:longint):pid_t;cdecl;external libzvtdll name 'vt_forkpty';
function vt_readchild(vt:Pvt_em; buffer:Pchar; len:longint):longint;cdecl;external libzvtdll name 'vt_readchild';
function vt_writechild(vt:Pvt_em; buffer:Pchar; len:longint):longint;cdecl;external libzvtdll name 'vt_writechild';
function vt_report_button(vt:Pvt_em; down:longint; button:longint; qual:longint; x:longint;
               y:longint):longint;cdecl;external libzvtdll name 'vt_report_button';
procedure vt_scrollback_set(vt:Pvt_em; lines:longint);cdecl;external libzvtdll name 'vt_scrollback_set';
function vt_killchild(vt:Pvt_em; signal:longint):longint;cdecl;external libzvtdll name 'vt_killchild';
function vt_closepty(vt:Pvt_em):longint;cdecl;external libzvtdll name 'vt_closepty';
procedure vt_reset_terminal(vt:Pvt_em; hard:longint);cdecl;external libzvtdll name 'vt_reset_terminal';
function vt_line_mblen(x:longint; line:Pvt_line):longint;cdecl;external libzvtdll name 'vt_line_mblen';
function vt_query_line_mbchar(x:longint; line:Pvt_line):longint;cdecl;external libzvtdll name 'vt_query_line_mbchar';

Function VT_LINE_SIZE(width : Longint) : Longint;
Function VTATTR_CLEAR : Longint;
Function VT_BLANK(n : Longint) : Boolean;
Function VT_ASCII(n : Longint) : Longint;
function VT_BMASK(n : longint) : longint;

{$EndIf read_interface}

{$Ifdef read_implementation}

Function VT_LINE_SIZE(width : Longint) : Longint;
begin
  VT_LINE_SIZE := sizeof(Tvt_line) + (sizeof(uint32) * (width));
end;

Function VTATTR_CLEAR : Longint;
begin
  VTATTR_CLEAR := (16 shl VTATTR_FORECOLOURB) or (17 shl VTATTR_BACKCOLOURB) or 0;
end;

Function VT_BLANK(n : Longint) : Boolean;
begin
  VT_BLANK := (n = 0) or (n = 9) or (n = 32);
end;

Function VT_ASCII(n : Longint) : Longint;
begin
  If ((n and VTATTR_DATAMASK) = 0) or ((n and VTATTR_DATAMASK) = 9) then
    VT_ASCII := 32
  else
    VT_ASCII := n and VTATTR_DATAMASK;
end;

function VT_BMASK(n : longint) : longint;
begin
  VT_BMASK := N and (VTATTR_FORECOLOURM or VTATTR_BACKCOLOURM or VTATTR_REVERSE or VTATTR_UNDERLINE);
end;

{$Endif read_implementation}
