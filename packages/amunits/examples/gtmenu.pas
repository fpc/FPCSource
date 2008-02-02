Program GadtoolsMenu;

{* gadtoolsmenu.p
** Example showing the basic usage of the menu system with a window.
** Menu layout is done with GadTools, as is recommended for applications.
**
*}

{
   Changed to use TAGS and pas2c.
   1 Nov 1998.

   Updated for systemvartags.
   28 Nov 2002.

   nils.sjoholm@mailbox.swipnet.se
}

uses Exec, Intuition, Utility, GadTools, systemvartags;



const

    mynewmenu : array[0..15] of tNewMenu = (
    (nm_Type: NM_TITLE; nm_Label:'Project';   nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:'Open...';   nm_CommKey:'O';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:'Save';      nm_CommKey:'S';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:nil;         nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),

    (nm_Type: NM_ITEM;  nm_Label:'Print';     nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_SUB;   nm_Label:'Draft';     nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_SUB;   nm_Label:'NLQ';       nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:nil;         nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),

    (nm_Type: NM_ITEM;  nm_Label:'Quit...';   nm_CommKey:'Q';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),

    (nm_Type: NM_TITLE; nm_Label:'Edit';      nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:'Cut';       nm_CommKey:'X';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:'Copy';      nm_CommKey:'C';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:'Paste';     nm_CommKey:'V';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),
    (nm_Type: NM_ITEM;  nm_Label:nil;         nm_CommKey: NIL;  nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),

    (nm_Type: NM_ITEM;  nm_Label:'Undo';      nm_CommKey:'Z';   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL),

    (nm_Type:   NM_END; nm_Label:NIL;         nm_CommKey:NIL;   nm_Flags:0;
nm_MutualExclude:0; nm_UserData:NIL));

var
   win : pWindow;
   myVisualInfo : Pointer;
   menuStrip : pMenu;
   msg  : pMessage;
   done : boolean;

Procedure Die;
begin
    if assigned(MenuStrip) then begin
       ClearMenuStrip(win);
       FreeMenus(MenuStrip);
    end;
    if assigned(myVisualInfo) then FreeVisualInfo(myVisualInfo);
    if assigned(win) then CloseWindow(win);
    Halt(0);
end;




{*
** Watch the menus and wait for the user to select the close gadget
** or quit from the menus.
*}
PROCEDURE ProcessIDCMP;
VAR
    IMessage    : tIntuiMessage;
    IPtr    : pIntuiMessage;

    Procedure ProcessMenu;
    var
    MenuNumber  : Word;
    ItemNumber  : Word;
    SubItemNumber   : Word;

    begin
    if IMessage.Code = MENUNULL then
        Exit;

    MenuNumber := MenuNum(IMessage.Code);
    ItemNumber := ItemNum(IMessage.Code);
    SubItemNumber := SubNum(IMessage.Code);

    if (MenuNumber = 0) and (ItemNumber = 5) then done := true;
    end;

begin
    IPtr := pIntuiMessage(Msg);
    IMessage := IPtr^;
    ReplyMsg(Msg);

    case IMessage.IClass of
      IDCMP_MENUPICK    : ProcessMenu;
      IDCMP_CLOSEWINDOW : done := True;
    end;
end;

{*
** Open all of the required libraries and set-up the menus.
*}

begin

    win := OpenWindowTags(NIL, [
                             WA_Width,  400,
                             WA_Activate,    ltrue,
                             WA_Height, 100,
                             WA_CloseGadget, ltrue,
                             WA_Title,  'Menu Test Window',
                             WA_IDCMP,  IDCMP_CLOSEWINDOW or IDCMP_MENUPICK,
                             TAG_END]);

    if win = nil then die;

    myVisualInfo := GetVisualInfoA(win^.WScreen,nil);
    if myVisualInfo = nil then die;

    {
      make the barlabels
    }
    mynewmenu[3].nm_Label := PChar(NM_BARLABEL);
    mynewmenu[7].nm_Label := PChar(NM_BARLABEL);
    mynewmenu[13].nm_Label := PChar(NM_BARLABEL);

    if pExecBase(_ExecBase)^.LibNode.Lib_Version >= 39 then begin
        MenuStrip := CreateMenus(@mynewmenu, [
                                 GTMN_FrontPen, 1,
                                 TAG_END]);
    end else MenuStrip := CreateMenusA(@mynewmenu,NIL);

    if menuStrip = nil then die;

    if not LayoutMenusA(menuStrip, myVisualInfo,nil) then die;

    if not SetMenuStrip(win,menuStrip) then die;

    repeat
    Msg := WaitPort(win^.UserPort);
    Msg := GetMsg(win^.UserPort);
       ProcessIDCMP;
    until done;
    die;
end.
