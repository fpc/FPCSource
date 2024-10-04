{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    This is unit implements some of the crt functionality
    for the gui win32 graph unit implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit WinCRT;
{$ENDIF FPC_DOTTEDUNITS}

  interface

    function readkey : AnsiChar;
    function keypressed : boolean;
    procedure delay(ms : word);

    { dummy }
    procedure textmode(mode : integer);

    { plays the windows standard sound }
    { hz is ignored (at least on win95 }
    procedure sound(hz : word);

    { dummy }
    procedure nosound;


  var
     directvideo : boolean;

     { dummy }
     lastmode : word;

  implementation

{$IFDEF FPC_DOTTEDUNITS}
    uses
       WinApi.Windows, TP.Graph;
{$ELSE FPC_DOTTEDUNITS}
    uses
       windows,graph;
{$ENDIF FPC_DOTTEDUNITS}

    const
       keybuffersize = 32;

    var
       keyboardhandling : TCriticalSection;
       keybuffer : array[1..keybuffersize] of AnsiChar;
       nextfree,nexttoread : longint;

    procedure inccyclic(var i : longint);

      begin
         inc(i);
         if i>keybuffersize then
           i:=1;
      end;

    procedure addchar(c : AnsiChar);

      begin
         EnterCriticalSection(keyboardhandling);
         keybuffer[nextfree]:=c;
         inccyclic(nextfree);
         { skip old chars }
         if nexttoread=nextfree then
           begin
              // special keys are started by #0
              // so we've to remove two chars
              if keybuffer[nexttoread]=#0 then
                inccyclic(nexttoread);
              inccyclic(nexttoread);
           end;
         LeaveCriticalSection(keyboardhandling);
      end;

    function readkey : AnsiChar;

      begin
         while true do
           begin
              EnterCriticalSection(keyboardhandling);
              if nexttoread<>nextfree then
                begin
                   readkey:=keybuffer[nexttoread];
                   inccyclic(nexttoread);
                   LeaveCriticalSection(keyboardhandling);
                   exit;
                end;
              LeaveCriticalSection(keyboardhandling);
              { give other threads a chance }
              {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.Sleep(10);
           end;
      end;

    function keypressed : boolean;

      begin
         EnterCriticalSection(keyboardhandling);
         keypressed:=nexttoread<>nextfree;
         LeaveCriticalSection(keyboardhandling);
      end;

    procedure delay(ms : word);

      begin
         Sleep(ms);
      end;

    procedure textmode(mode : integer);

      begin
      end;

    procedure sound(hz : word);

      begin
         {$IFDEF FPC_DOTTEDUNITS}WinApi.{$ENDIF}Windows.Beep(hz,500);
      end;

    procedure nosound;

      begin
      end;

    procedure addextchar(c : AnsiChar);

      begin
         addchar(#0);
         addchar(c);
      end;

    const
       altkey : boolean = false;
       ctrlkey : boolean = false;
       shiftkey : boolean = false;

    function msghandler(Window: HWnd; AMessage:UInt; WParam : WParam; LParam: LParam): LResult; stdcall;

      begin
         case AMessage of
           WM_CHAR:
             begin
               if ctrlkey then
               begin
                 case wparam of
                   VK_BACK:  addchar(chr(127));
                   42,43,45,47: ; { num+,num-,num*,num/ ignore here }
                   0:; {ignore, we can't have char 0}
                   27:;{conflict with NumLock on, numpad3}
                   28,29:; {ignore here, taken care in wm_keydown }
                   30,31,46,48,49,57,127:; {ignore, invalid combination}
                 else
                   addchar(chr(wparam));
                 end;
               end else
               if shiftkey then
               begin
                 case wparam of
                   VK_TAB:  addextchar(#15);
                 else
                   addchar(chr(wparam));
                 end;
               end else
                 addchar(chr(wparam));
             end;
           WM_KEYDOWN,WM_SYSKEYDOWN:
             begin
               if altkey then
               begin   {-------------  ATL  -------------}
                 case wparam of
                    VK_ESCAPE:      addextchar(#1);
                    VK_OEM_3:       addextchar(#41);  { ~ }
                    VK_1:           addextchar(#120);
                    VK_2:           addextchar(#121);
                    VK_3:           addextchar(#122);
                    VK_4:           addextchar(#123);
                    VK_5:           addextchar(#124);
                    VK_6:           addextchar(#125);
                    VK_7:           addextchar(#126);
                    VK_8:           addextchar(#127);
                    VK_9:           addextchar(#128);
                    VK_0:           addextchar(#129);
                    VK_OEM_MINUS:   addextchar(#130);
                    VK_OEM_PLUS:    addextchar(#131);
                    VK_BACK:        addextchar(#14);

                    VK_Q:           addextchar(#16);
                    VK_W:           addextchar(#17);
                    VK_E:           addextchar(#18);
                    VK_R:           addextchar(#19);
                    VK_T:           addextchar(#20);
                    VK_Y:           addextchar(#21);
                    VK_U:           addextchar(#22);
                    VK_I:           addextchar(#23);
                    VK_O:           addextchar(#24);
                    VK_P:           addextchar(#25);
                    VK_OEM_4:       addextchar(#24);  { [ }
                    VK_OEM_6:       addextchar(#25);  { ] }
                    VK_RETURN:      addextchar(#28);

                    VK_A:           addextchar(#30);
                    VK_S:           addextchar(#31);
                    VK_D:           addextchar(#32);
                    VK_F:           addextchar(#33);
                    VK_G:           addextchar(#34);
                    VK_H:           addextchar(#35);
                    VK_J:           addextchar(#36);
                    VK_K:           addextchar(#37);
                    VK_L:           addextchar(#38);
                    VK_OEM_1:       addextchar(#39);  { ; }
                    VK_OEM_7:       addextchar(#40);  { ' }
                    VK_OEM_5:       addextchar(#43);  { \ }

                    VK_Z:           addextchar(#44);
                    VK_X:           addextchar(#45);
                    VK_C:           addextchar(#46);
                    VK_V:           addextchar(#47);
                    VK_B:           addextchar(#48);
                    VK_N:           addextchar(#49);
                    VK_M:           addextchar(#50);
                    VK_OEM_COMMA:   addextchar(#51);
                    VK_OEM_PERIOD:  addextchar(#52);
                    VK_OEM_2:       addextchar(#164); { / }
                    VK_OEM_102:     addextchar(#86);  { < }

                    VK_CLEAR:       addextchar(#154); { alt + numpad 5 }
                    VK_ADD:         addextchar(#78);
                    VK_SUBTRACT:    addextchar(#74);
                    VK_MULTIPLY:    addextchar(#55);
                    VK_DIVIDE:      addextchar(#164);

                    {in dos/win32 this is Numeric ASCII entry, but add in anyway }
                    VK_NUMPAD0:     addextchar(#162);
                    VK_NUMPAD1:     addextchar(#159);
                    VK_NUMPAD2:     addextchar(#160);
                    VK_NUMPAD3:     addextchar(#161);
                    VK_NUMPAD4:     addextchar(#155);
                    VK_NUMPAD5:     addextchar(#154); { alt + numpad 5 }
                    VK_NUMPAD6:     addextchar(#167);
                    VK_NUMPAD7:     addextchar(#151);
                    VK_NUMPAD8:     addextchar(#152);
                    VK_NUMPAD9:     addextchar(#153);

                    VK_DECIMAL:     addextchar(#163);
                    VK_LEFT:        addextchar(#155);
                    VK_RIGHT:       addextchar(#167);
                    VK_DOWN:        addextchar(#160);
                    VK_UP:          addextchar(#152);
                    VK_INSERT:      addextchar(#162);
                    VK_DELETE:      addextchar(#163);
                    VK_END:         addextchar(#159);
                    VK_HOME:        addextchar(#151);
                    VK_PRIOR:       addextchar(#153);
                    VK_NEXT:        addextchar(#161);
                    VK_F1..VK_F10:  addextchar(chr(wparam-8));
                    VK_F11..VK_F12: addextchar(chr(wparam+17));
                  end;
               end else
               if ctrlkey then
               begin   {-------------  CTRL  -------------}
                 case wparam of
                    49..57: addextchar(chr(wparam-47));
                     48: addextchar(#11);
                    189: addextchar(#12);
                    187: addextchar(#13);
                    222: addextchar(#40);
                    186: addextchar(#39);
                    192: addextchar(#41);
                    188: addextchar(#51);
                    190: addextchar(#52);
                    191,111: addextchar(#149);
                    106: addextchar(#150);
                    109: addextchar(#142);
                    107: addextchar(#144);
                    110: addextchar(#147);
                    VK_NUMPAD0:     addextchar(#146);
                    VK_NUMPAD1:     addextchar(#117);
                    VK_NUMPAD2:     addextchar(#145);
                    VK_NUMPAD3:     addextchar(#118);
                    VK_NUMPAD4:     addextchar(#115);
                    VK_NUMPAD5:     addextchar(#143);
                    VK_NUMPAD6:     addextchar(#116);
                    VK_NUMPAD7:     addextchar(#119);
                    VK_NUMPAD8:     addextchar(#141);
                    VK_NUMPAD9:     addextchar(#132);

                    VK_OEM_4:       addchar(#27);  { [ }
                    VK_OEM_6:       addchar(#28);  { ] }
                    VK_OEM_5:       addchar(#29);  { \ }
                    VK_OEM_102:     addextchar(#86);  { < }

                    VK_TAB:         addextchar(#148);
                    VK_CLEAR:       addextchar(#143); { ctrl + numpad 5 }
                    VK_LEFT:        addextchar(#115);
                    VK_RIGHT:       addextchar(#116);
                    VK_DOWN:        addextchar(#145);
                    VK_UP:          addextchar(#141);
                    VK_INSERT:      addextchar(#146);
                    VK_DELETE:      addextchar(#147);
                    VK_END:         addextchar(#117);
                    VK_HOME:        addextchar(#119);
                    VK_PRIOR:       addextchar(#132);
                    VK_NEXT:        addextchar(#118);
                    VK_F1..VK_F10:  addextchar(chr(wparam-18));
                    VK_F11..VK_F12: addextchar(chr(wparam+15));
                 end;
               end else
               if shiftkey then
               begin   {-------------  SHIFT  -------------}
                  case wparam of
                    VK_CLEAR:       addextchar(#76); { shift + numpad 5 }
                    VK_LEFT:        addextchar(#75);
                    VK_RIGHT:       addextchar(#77);
                    VK_DOWN:        addextchar(#80);
                    VK_UP:          addextchar(#72);
                    VK_INSERT:      addextchar(#82);
                    VK_DELETE:      addextchar(#83);
                    VK_END:         addextchar(#79);
                    VK_HOME:        addextchar(#71);
                    VK_PRIOR:       addextchar(#73);
                    VK_NEXT:        addextchar(#81);
                    VK_F1..VK_F10:  addextchar(chr(wparam-28));
                    VK_F11..VK_F12: addextchar(chr(wparam+13));
                  end;
               end else
               begin   {------------- no modif -------------}
                 case wparam of
                    VK_CLEAR:       addextchar(#76); {  numpad 5 }
                    VK_LEFT:        addextchar(#75);
                    VK_RIGHT:       addextchar(#77);
                    VK_DOWN:        addextchar(#80);
                    VK_UP:          addextchar(#72);
                    VK_INSERT:      addextchar(#82);
                    VK_DELETE:      addextchar(#83);
                    VK_END:         addextchar(#79);
                    VK_HOME:        addextchar(#71);
                    VK_PRIOR:       addextchar(#73);
                    VK_NEXT:        addextchar(#81);
                    VK_F1..VK_F10:  addextchar(chr(wparam-53));
                    VK_F11..VK_F12: addextchar(chr(wparam+11));
                  end;
               end;
               {------------- common -------------}
               case wparam of
                 VK_CONTROL: ctrlkey:=true;
                 VK_MENU:    altkey:=true;
                 VK_SHIFT:   shiftkey:=true;
               end;
             end;
           WM_KEYUP,WM_SYSKEYUP:
             begin
               case wparam of
                 VK_CONTROL: ctrlkey:=false;
                 VK_MENU:    altkey:=false;
                 VK_SHIFT:   shiftkey:=false;
               end;
             end;
         end;
         msghandler:=0;
      end;

    var
       oldexitproc : pointer;

    procedure myexitproc;

      begin
         exitproc:=oldexitproc;
         charmessagehandler:=nil;
         DeleteCriticalSection(keyboardhandling);
      end;

begin
   charmessagehandler:=@msghandler;
   nextfree:=1;
   nexttoread:=1;
   InitializeCriticalSection(keyboardhandling);
   oldexitproc:=exitproc;
   exitproc:=@myexitproc;
   lastmode:=0;
end.
