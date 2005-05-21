Program nuttest;

{$if not defined(netware)}
{$error Sorry, this Demo is for netware and netwlibc only}
{$endif}
{$mode objfpc}
{$if defined (netware_libc)}
{$description FreePascal NUT Demo - libc}
{$Screenname FreePascal NWSNUT Demo - libc}
{$else}
{$description FreePascal NUT Demo - clib}
{$Screenname default}
{$endif}
{$Copyright 2005 Armin Diehl <armin@freepascal.org>}
{$Version 1,0,0}



{$if defined(netware_clib)}
uses nwserv,nwsnut,sysutils;
{$else}
uses libc,nwsnut,sysutils;
{$endif}

var
  gExiting : boolean  = FALSE;
  gThreadCount : integer = 0;
  gNUTHandle : PNUTInfo = NIL;

const
  gMyName = 'NUT Demo';
  gMessageTable : array [0..26] of pchar = (
        gMyName,
        '1.00',
        'Exit NUT Demo?',
        'NUT Demo',
        'NUTDEMO Tag',
        'Main Menu',
        'Sub-menu option #1',
        'Unsorted Sub-menu Example',
        'Program Trace Portal',
        'Bobby',
        'Sub-menu option #2',
        'Sub-Menu',
        'Bravo',
        'Tango',
        'Alpha',
        'Zulu',
        'Unsorted Menu',
        'Sorted List',
        'Sub-menu Example',
        'Item List Example',
        'Form Example',
        'Menu In Form',
        'Option 1',
        'Option 2',
        'Save Data?',
        'Edit String Example',
        'Edit Text Exampl');

// keep in sync with above...
        PROGRAM_NAME                    = $0000;
        PROGRAM_VERSION                 = $0001;
        PROGRAM_EXIT                    = $0002;
        SCREEN_NAME                     = $0003;
        RS_TAG_NAME                     = $0004;
        MENU_MAIN__HDR                  = $0005;
        MENU_SUB_OPTION1                = $0006;
        MENU_MAIN_NOSORT                = $0007;
        TRACE_PORTAL__HDR               = $0008;
        MENU_NOSORT_OPTION_A            = $0009;
        MENU_SUB_OPTION2                = $000A;
        MENU_SUB__HDR                   = $000B;
        MENU_NOSORT_OPTION_B            = $000C;
        MENU_NOSORT_OPTION_C            = $000D;
        MENU_NOSORT_OPTION_D            = $000E;
        MENU_NOSORT_OPTION_E            = $000F;
        MENU_NOSORT__HDR                = $0010;
        LIST_SUBLIST__HDR               = $0011;
        MENU_MAIN_SUBMENU               = $0012;
        MENU_MAIN_LIST                  = $0013;
        MENU_MAIN_FORM                  = $0014;
        MENU_IN_FORM_TITLE              = $0015;
        FORM_MENU_OPT1                  = $0016;
        FORM_MENU_OPT2                  = $0017;
        EXIT_FORM_MSG                   = $0018;
        MENU_MAIN_EDIT_STRING           = $0019;
        MENU_MAIN_EDIT_TEXT             = $001A;


function NLM_VerifyProgramExit : longint; cdecl;
var res : integer;
begin
  res := NWSConfirm (PROGRAM_EXIT,   // Header
                     0,              // centerLine
                     0,              // Center Column
                     1,              // Default Choice
                     nil,            // Action Procedure
                     gNUTHandle,     // Handle
                     nil);           // Action Parameter

  // Escape(-1) means No(0).
  if (res = -1) then inc (res);
  Result := res;
end;


(****************************************************************************
 * Edit a string
 ****************************************************************************)
procedure NLM_EditStringSub; cdecl;
const
   maxLen = 40;
var err : integer;
    str : ansistring;
begin
   //------------------------------------------------------------------------
   // Generate dynamic messages - this allows you to call NUT functions
   // and specify messages on the fly
   //*/
   NWSSetDynamicMessage(DYNAMIC_MESSAGE_ONE, 'String Edit Function',gNUTHandle^.messages);
   NWSSetDynamicMessage(DYNAMIC_MESSAGE_TWO, ' Editing can be fun: ',gNUTHandle^.messages);

   str := 'String to edit';
   setlength (str, maxLen);

   err := NWSEditString(
                10,   // center line
                40,   // center column
                1,    // edit height
                40,   // edit width
                DYNAMIC_MESSAGE_ONE, // header msg
                DYNAMIC_MESSAGE_TWO, // prompt msg
                pchar(str),          // buffer
                maxLen,              // max length of string
                EF_ANY OR EF_UPPER,  // acceptable chars
                gNUTHandle,          // nut handle
                nil,                 // insert-key procedure
                nil,                 // action procedure
                nil);                // parameters

   // if escape key was pressed
  if (err = 1) then
    NWSTrace(gNUTHandle,'String was not saved');
end;

(****************************************************************************
 * Edit text in a window
 ****************************************************************************)
procedure NLM_EditTextSub;
const maxLen = 1024;
var err : integer;
    str : ansistring;
begin
  // Generate dynamic messages - this allows you to call NUT functions
  // and specify messages on the fly
  NWSSetDynamicMessage(DYNAMIC_MESSAGE_ONE,'Text Edit Function', gNUTHandle^.messages);
  NWSSetDynamicMessage(DYNAMIC_MESSAGE_TWO,'Save changes?', gNUTHandle^.messages);

  str := 'This could be any kind of text'#13'that you might have.';
  setlength (str,maxLen+1);

  // Edit the text in a portal with scroll bars that appear only when the
  // text goes beyond the portal bounderies
  err := NWSEditTextWithScrollBars (
                10,                     // center line
                40,                     // center column
                4,                      // edit height
                40,                     // edit width
                DYNAMIC_MESSAGE_ONE,    // header msg
                pchar(str),             // buffer
                maxLen,                 // max length of string
                DYNAMIC_MESSAGE_TWO,    // confirm msg
                true,                   // force confirm
                SHOW_VERTICAL_SCROLL_BAR OR    // // scroll bar props
                SHOW_HORIZONTAL_SCROLL_BAR OR
                CONSTANT_SCROLL_BARS,
                gNUTHandle);

  // escape key was pressed
  if err = 1 then
    NWSTrace(gNUTHandle,'Text was not saved');
end;

function NLM_FormMenuAction (option : longint; param : pointer):longint; cdecl;
begin
// Do anything that might be needed by the selection of a given menu option
// and the value returned will indicate which data item is to be displayed
// in the menu field on the form.
   result := option;
end;


function NLM_HotSpotAction (fp : PField; selectKey : longint; var changedField : longint; Handle : PNUTInfo) : longint; cdecl;
begin
  // do the work here. . .

  NWSTrace(handle, 'This is your hot spot routine');

  result :=K_RIGHT;  // send us to the next field...
end;

(****************************************************************************
 * Form display with various fields
 ****************************************************************************)
procedure NLM_FormSub;
var
  line,
  formSaved,
  menuChoice,
  myInteger,
  myHexInteger : longint;
  MyOtherInteger : cardinal;
  myBoolean : longbool;
  myString : ansistring;
  mfctl : PMFCONTROL;
begin
  myInteger := 600;
  myHexInteger := $2ffc;
  myOtherInteger := 900;

  // Don't do this list if we should be exiting.
  if gExiting then exit;

  // At this point, the current list is the Main Menu.  If we begin adding
  // new items to the current list, it would mess up the Main menu (to say
  // the least).  So, we will save the Main Menu List on the List stack
  // (PushList) and then initialize a new form (set head and tail to NULL)
  // by calling NWSInitForm().
  NWSPushList(gNUTHandle);
  NWSInitForm(gNUTHandle);

  // Define the fields in the form
  line := 0;
  NWSAppendCommentField (line, 1, 'Boolean Field:', gNUTHandle);
  NWSAppendBoolField (line, 25, NORMAL_FIELD, myBoolean, 0, gNUTHandle);

  line += 2;
  NWSAppendCommentField (line, 1, 'Integer Field:', gNUTHandle);
  NWSAppendIntegerField (line, 25, NORMAL_FIELD, myInteger, 0, 9999, 0, gNUTHandle);

  line += 2;
  NWSAppendCommentField (line, 1, 'String Field:', gNUTHandle);
  myString := 'Data String';
  setLength (myString,30);
  NWSAppendStringField (line, 25, 30, NORMAL_FIELD, pchar(myString), 'A..Za..z ',0, gNUTHandle);

  line += 2;
  NWSAppendCommentField (line, 1, 'Unsigned Integer Field:', gNUTHandle);
  NWSAppendUnsignedIntegerField (line, 25, NORMAL_FIELD, @myOtherInteger, 0, 99999, 0, gNUTHandle);

  line += 2;
  NWSAppendCommentField (line, 1, 'Hex Field:', gNUTHandle);
  NWSAppendHexField (line, 25, NORMAL_FIELD, @myHexInteger, 0, 99999, 0, gNUTHandle);

  line += 2;
  NWSAppendCommentField (line, 1, 'Comment Field:', gNUTHandle);
  NWSAppendCommentField (line, 25, 'A comment', gNUTHandle);

  line += 2;
  NWSAppendCommentField (line, 1, 'Hot Spot Field:', gNUTHandle);
  NWSAppendHotSpotField (line, 25, NORMAL_FIELD, 'Hot Field', @NLM_HotSpotAction, gNUTHandle);

  mfctl := NWSInitMenuField (MENU_IN_FORM_TITLE, 10, 40, @NLM_FormMenuAction, gNUTHandle);

  NWSAppendToMenuField (mfctl, FORM_MENU_OPT1, 1, gNUTHandle);
  NWSAppendToMenuField (mfctl, FORM_MENU_OPT2, 2, gNUTHandle);

  menuChoice := 1;       // display the text for option one

  line += 2;
  NWSAppendCommentField (line, 1, 'Menu Field:', gNUTHandle);
  NWSAppendMenuField (line, 25, NORMAL_FIELD, @menuChoice, mfctl, 0, gNUTHandle);

  // Edit the form
  formSaved := NWSEditPortalForm (
                MENU_MAIN_FORM,    // I- header
                11,                // I- center line
                40,                // I- center col
                16,                // I- form height
                50,                // I- form width
                F_VERIFY,          // I- ctl flags
                F_NO_HELP,         // I- form help
                EXIT_FORM_MSG,     // I- confirm msg
                gNUTHandle);

  // This function returns TRUE if the form was saved, FALSE if not.
  // If the form was not saved you must restore all variables to their
  // original values manually
  if longbool (formSaved) then
    NWSTrace(gNUTHandle,'The form data was not saved');

  // cleanup and discard this form
  NWSDestroyForm(gNUTHandle);
  NWSPopList(gNUTHandle);
end;


(****************************************************************************
 * Display information in a portal given a selection from the list
 ****************************************************************************)
procedure NLM_DisplayPortalInformation (selectedItem : pchar);
var
  portal : longint;
  szTemp : ansistring;  //char  szTemp[80+1];
  portalPCB : PPCB;
begin
  // Dim the current portal
  NWSDeselectPortal(gNUTHandle);

  // Create a portal in which we will display the connection information.
  // (A portal is a window).
  portal := NWSCreatePortal(
                5,              // I- line
                2,              // I- column
                10,             // I- frameHeight
                76,             // I- frameWidth
                6,              // I- virtualHeight
                76,             // I- virtualWidth
                SAVE,           // I- saveFlag
                selectedItem,   // I- headerText
                VNORMAL,        // I- headerAttribute
                SINGLE,         // I- borderType
                VINTENSE,       // I- borderAttribute
                CURSOR_OFF,     // I- cursorFlag
                VIRTUAL,        // I- directflag
                gNUTHandle);

  case cardinal(portal) of
    $FFFFFFFE : begin
                  NWSTrace(gNUTHandle, 'NWSCreatePortal reports: Unable to allocate memory for PCB, virtual screen, or save area.');
                  exit;
                end;
    $FFFFFFFF : begin
                  NWSTrace(gNUTHandle, 'NWSCreatePortal reports: Maximum number of portals already defined.');
                  exit;
                end;
  end;

  // Get portal's PCB.
  NWSGetPCB (portalPCB, portal, gNUTHandle);

  // Make our portal current and clear it.
  NWSSelectPortal(portal, gNUTHandle);
  NWSClearPortal(portalPCB);

  // Place information on portal.
  NWSDisplayTextInPortal(1,0,'This is data displayed in a portal',VINTENSE,portalPCB);

  szTemp := format ('Item selected: %s',[selectedItem]);
  NWSDisplayTextInPortal(3,0,pchar(szTemp),VNORMAL,portalPCB);

  NWSDisplayTextInPortal(5,0,'<Press ESCAPE to exit>',VINTENSE,portalPCB);

  // Update portal content to user screen.
  NWSUpdatePortal(portalPCB);

  // Wait for user to press ESCAPE.
  NWSWaitForEscape(gNUTHandle);

  // Trash portal.
  NWSDestroyPortal(portal, gNUTHandle);
end;

(****************************************************************************
 * Action procedure for the list
 ****************************************************************************)
function NLM_ListSubAction (keyPressed : longint;
                            elementSelected:PPLIST;
                            itemLineNumber:plongint;
                            actionParameter:pointer) : longint; cdecl;
begin
  result := -1;
  case keyPressed of
     M_ESCAPE : result := 0;
     M_SELECT : begin
                  NLM_DisplayPortalInformation(@elementSelected^^.text);
                  result := -1;
                end;
  end;
end;


(****************************************************************************
 * Build a list of items
 ****************************************************************************)
procedure NLM_ListSubBuild;
var i : integer;
    s : ansistring;
begin
  for i := 1 to 50 do
  begin
    s := format ('Item number %02d',[i]);
    NWSAppendToList (pchar(s),nil,gNUTHandle);
  end;
end;


(****************************************************************************
 * Create and display the list
 ****************************************************************************)
procedure NLM_DisplaySubList;
begin
  if gExiting then exit;

  // At this point, the current list is the Main Menu.  If we begin adding
  // new items to the current list, it would mess up the Main menu (to say
  // the least).  So, we will save the Main Menu List on the List stack
  // (PushList) and then initialize a new list (set head and tail to NULL)
  // by calling InitList().  Note that Lists use NWInitList() and Menus use
  // NWInitMenu().
  NWSPushList(gNUTHandle);
  NWSInitList(gNUTHandle, nil);

  // Build a list
  NLM_ListSubBuild;

  // Display the list and allow user interaction.
  NWSList(
           LIST_SUBLIST__HDR,     // I- header
           0,                     // I- centerLine
           0,                     // I- centerColumn
           10,                    // I- height
           72,                    // I- width
           M_ESCAPE OR M_SELECT,  // I- validKeyFlags
           nil,                   // IO element
           gNUTHandle,            // I- handle
           nil,                   // I- formatProcedure
           @NLM_ListSubAction,    // I- actionProcedure
           nil);                  // I- actionParameter

  // Before returning, we must free the list items allocated by
  // NLM_ListSubBuild...().  Then the Main Menu list context
  // must be restored.  Note that Lists use NWDestroyList() and
  // Menus use NWDestroyMenu().
  NWSDestroyList(gNUTHandle);
  NWSPopList(gNUTHandle);
end;


(****************************************************************************
 * Unsorted sub-menu (NWSLIST) action procedure.  Note that the parameters
 * for an NWSList() action procedure are very different from the parameters
 * passed to an NWSMenu() action procedure.
 ****************************************************************************)

function NLM_MenuNoSortAct (keyPressed:longint; elementSelected:PPLIST; itemLineNumber:plongint; actionParameter:pointer):longint;  cdecl;
var index : integer;
begin
  // Setup index variable to be the same as it would be in a NWSMenu()
  // action procedure.
  if keypressed = M_ESCAPE then
    index := -1
  else
    index := integer(elementSelected^^.otherInfo^);

  // Perform the user-selected action.
  // (Just like a normal NWSMenu() action procedure...)
  case index of
    -1: begin
          result := 0; exit;
        end;
    MENU_NOSORT_OPTION_A :
          NWSTrace(gNUTHandle,'Insert no-sort sub-menu option #A here.');
    MENU_NOSORT_OPTION_B :
          NWSTrace(gNUTHandle,'Insert no-sort sub-menu option #B here.');
    MENU_NOSORT_OPTION_C :
          NWSTrace(gNUTHandle,'Insert no-sort sub-menu option #C here.');
    MENU_NOSORT_OPTION_D :
          NWSTrace(gNUTHandle,'Insert no-sort sub-menu option #D here.');
    MENU_NOSORT_OPTION_E :
          NWSTrace(gNUTHandle,'Insert no-sort sub-menu option #E here.')
    else
          NWSTrace(gNUTHandle,pchar(format('Option %d not implemented.',[index])));
  end;

  // If we should be exiting, pretend that ESCAPE was pressed.
  if gExiting then
    result := 0
  else
    result := -1;
end;


(****************************************************************************
 * Unsorted sub-menu.
 *
 * There are times when you would like to display a menu, but you don't want
 * the elements to be sorted.  NWSMenu() automatically sorts the list of menu
 * items and there is no way to disable this feature.
 *
 * The NWSList() function has an M_NO_SORT flag that is not available to the
 * NWSMenu() function; however, using NWSList to display a menu can be scary
 * if you don't know how.
 *
 * The following code demonstrates how to build a menu and then display it as
 * a list.  The action procedure (above) is specific to NWSList() and is not
 * a suitable action procedure for NWSMenu().
 ***************************************************************************)
procedure NLM_MenuNoSort;
var defItem : PLIST;
begin
  if gExiting then exit;

  // At this point, the current list is the Main Menu.  If we begin adding
  // new items to the current list, it would mess up the Main menu (to say
  // the least).  So, we will save the Main Menu List on the List stack
  // (PushList) and then initialize a new list (set head and tail to NULL)
  // by calling InitMenu().  Note that Lists use NWInitList() and Menus use
  // NWInitMenu().
  NWSPushList(gNUTHandle);
  NWSInitMenu(gNUTHandle);

  // Insert menu items in the order they will be displayed.
  NWSAppendToMenu(MENU_NOSORT_OPTION_B, MENU_NOSORT_OPTION_B, gNUTHandle);
  NWSAppendToMenu(MENU_NOSORT_OPTION_A, MENU_NOSORT_OPTION_A, gNUTHandle);
  defItem := NWSAppendToMenu(MENU_NOSORT_OPTION_C, MENU_NOSORT_OPTION_C, gNUTHandle);
  NWSAppendToMenu(MENU_NOSORT_OPTION_E, MENU_NOSORT_OPTION_E, gNUTHandle);
  NWSAppendToMenu(MENU_NOSORT_OPTION_D, MENU_NOSORT_OPTION_D, gNUTHandle);

  // Display the menu (as though it were a list) and allow user interaction.
        NWSList(
                MENU_NOSORT__HDR,   // header
                0,                  // centerLine
                65,                 // centerColumn
                5,                  // height
                20,                 // width
                M_ESCAPE OR M_SELECT OR
                M_NO_SORT,          // validKeyFlags
                @defItem,           // element
                gNUTHandle,         // handle
                nil,                // formatProcedure
                @NLM_MenuNoSortAct, // actionProcedure
                nil);               // actionParameter

  // Before returning, we must free the list items allocated by
  // NWSAppendToMenu().  Then the Main Menu list context must be restored.
  // Note that Lists use NWDestroyList() and Menus use NWDestroyMenu().
  NWSDestroyMenu(gNUTHandle);
  NWSPopList(gNUTHandle);
end;


(****************************************************************************
 * Sub menu (sorted) action procedure.
 ****************************************************************************)
function NLM_MenuSubAction (index:longint; parm:pointer):longint; cdecl;
begin
  // Perform the user-selected action.
  case index of
    -1 : begin
           result := 0; exit;
         end;
    MENU_SUB_OPTION1: NWSTrace(gNUTHandle,'Insert sub-menu option #1 here.');
    MENU_SUB_OPTION2: NWSTrace(gNUTHandle,'Insert sub-menu option #2 here.')
  else
    NWSTrace(gNUTHandle,'Option not implemented.');
  end;

  // If we should be exiting, pretend that ESCAPE was pressed.
  if gExiting then
    result := 0
  else
    result := -1;
end;


procedure NLM_MenuSub;
begin
  if gExiting then exit;

  // At this point, the current list is the Main Menu.  If we begin adding
  // new items to the current list, it would mess up the Main menu (to say
  // the least).  So, we will save the Main Menu List on the List stack
  // (PushList) and then initialize a new list (set head and tail to NULL)
  // by calling InitMenu().  Note that Lists use NWInitList() and Menus use
  // NWInitMenu().

  NWSPushList(gNUTHandle);
  NWSInitMenu(gNUTHandle);

  // Insert menu items.  Note that the insertion order does not matter being
  // that NWSMenu() will always sort the Menu selections automatically.

  NWSAppendToMenu(MENU_SUB_OPTION1, MENU_SUB_OPTION1, gNUTHandle);
  NWSAppendToMenu(MENU_SUB_OPTION2, MENU_SUB_OPTION2, gNUTHandle);

  // Display the menu and allow user interaction.
  NWSMenu(MENU_SUB__HDR,                // Header
          0,                            // centerLine
          15,                           // centerColumn
          nil,                          // defaultElement
          @NLM_MenuSubAction,           // actionProcedure
          gNUTHandle,
          nil);                         // actionParameter

  // Before returning, we must free the list items allocated by
  // NWSAppendToMenu().  Then the Main Menu list context must be restored.
  // Note that Lists use NWDestroyList() and Menus use NWDestroyMenu().

  NWSDestroyMenu(gNUTHandle);
  NWSPopList(gNUTHandle);
end;



function NLM_MenuMainAction (index:longint; parm:pointer):longint; cdecl;
begin
  case index of
    -1: if longbool(NLM_VerifyProgramExit) then  // ESC pressed
        begin
          result := 0;
          exit;
        end;
    MENU_MAIN_SUBMENU     : NLM_MenuSub;
    MENU_MAIN_NOSORT      : NLM_MenuNoSort;
    MENU_MAIN_LIST        : NLM_DisplaySubList;
    MENU_MAIN_FORM        : NLM_FormSub;
    MENU_MAIN_EDIT_STRING : NLM_EditStringSub;
    MENU_MAIN_EDIT_TEXT   : NLM_EditTextSub
  else
    NWSTrace(gNUTHandle,'Option not implemented.');
  end;

  if gExiting then
    result := 0
  else
    result := -1;
end;


procedure DoMainMenu;
var defaultOption : PLIST;
begin
  if gExiting then exit;

  // At this point, the current list is uninitialized (being that it is the
  // first list of the program.)  Before using the current list it must be
  // initialized (set head and tail to NULL) by calling InitMenu().
  // Note that Lists use NWInitList() and Menus use NWInitMenu().

  NWSInitMenu(gNUTHandle);

  // Insert menu items.  Note that the insertion order does not matter being
  // that NWSMenu() will always sort the Menu selections automatically.
  // The defaultOption stores a pointer to the menu item which we wish to be
  // highlighed by default.

  NWSAppendToMenu(MENU_MAIN_SUBMENU, MENU_MAIN_SUBMENU, gNUTHandle);
  NWSAppendToMenu(MENU_MAIN_NOSORT, MENU_MAIN_NOSORT, gNUTHandle);
  NWSAppendToMenu(MENU_MAIN_LIST, MENU_MAIN_LIST, gNUTHandle);
  NWSAppendToMenu(MENU_MAIN_FORM, MENU_MAIN_FORM, gNUTHandle);
  defaultOption :=
    NWSAppendToMenu(MENU_MAIN_EDIT_STRING, MENU_MAIN_EDIT_STRING, gNUTHandle);
  NWSAppendToMenu(MENU_MAIN_EDIT_TEXT, MENU_MAIN_EDIT_TEXT, gNUTHandle);

  // Display the menu and allow user interaction.

  NWSMenu(MENU_MAIN__HDR,       // Header
          0,                    // centerLine
          0,                    // centerColumn
          defaultOption,        // defaultElement
          @NLM_MenuMainAction,  // procedure to handle events
          gNUTHandle,
          nil);                 // actionParameter

  // Before returning, we must free the list items allocated by
  // NWSAppendToMenu(). Note that Lists use NWDestroyList() and Menus use
  // NWDestroyMenu().

  NWSDestroyMenu(gNUTHandle);
end;


procedure DeinitializeNUT;
begin
  if gNUTHandle <> nil then
    NWSRestoreNut(gNUTHandle);
end;

var oldNetwareUnloadProc : pointer = nil;

procedure onUnload;
var i : integer;
begin
  gExiting := TRUE;

  // Wait for main() to terminate.
  // If main() has not terminateded within a 1/2 second, ungetch an
  // escape key. This will "trick" a blocking NWSList() or NWSMenu()
  // function and wake it up.
  i := 0;
  while (gThreadCount > 0) do
  begin
    delay (100);
    inc(i);
    if i = 5 then
      ungetcharacter(ESCAPE);
    {$if defined (netware_libc)}
    pthread_yield;
    {$else}
    ThreadSwitchWithDelay;
    {$endif}
  end;
  System.NetwareUnloadProc := oldNetwareUnloadProc;
end;


procedure InitializeNUT;
var err : integer;
    NLMHandle : TNLMHandle;
    screen    : TScr;
    allocTag  : TRtag;
begin
  // use the SIGTERM handler defined in system.pp to facilitate a console UNLOAD command.
  oldNetwareUnloadProc := System.NetwareUnloadProc;
  NetwareUnloadProc := @onUnload;

  NLMHandle := getnlmhandle;
  {$if defined(netware_clib)}
  screen := CreateScreen ('FreePascal NWSNUT Demo - clib',AUTO_DESTROY_SCREEN);
  if screen <> nil then
    DisplayScreen (screen);
  {$else}
  screen    := getscreenhandle();
  {$endif}

  if ((pointer(NLMHandle) = nil) or (pointer(screen) = nil)) then
  begin
    gExiting := TRUE;
    Exit;
  end;

  // Fire up NWSNUT on our screen which was set up via the linker. LibC
  // doesn't have a great deal of flexibility with screens. Setting up your
  // own, additional screen may prove challenging, however, it should be
  // possible.

  allocTag := AllocateResourceTag(NLMHandle, gMyName, AllocSignature);
  if pointer(allocTag) = nil then
  begin
    gExiting := TRUE;
    Exit;
  end;

  err := NWSInitializeNut(PROGRAM_NAME, PROGRAM_VERSION, NORMAL_HEADER,
                          NUT_REVISION_LEVEL, gMessageTable, nil, screen, allocTag,
                          gNUTHandle);
  if err <> 0 then
    gExiting := TRUE;
end;


begin
  inc (gThreadCount);

  InitializeNUT;
  DoMainMenu;
  DeinitializeNUT;

  dec (gThreadCount);
end.
