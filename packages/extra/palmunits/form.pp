(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Form.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines form structures and routines.
 *
 * History:
 *    09/06/94 art   Created by Art Lamb.
 *    08/28/00 kwk   Added FrmGetActiveField (4.0)
 *
 *****************************************************************************)

unit form;

interface

uses palmos, coretraps, rect, font, window, control, field, list, scrollbar, table, event_;

const
  noFocus = $ffff;

  frmInvalidObjectId = $ffff;
  frmNoSelectedControl = $ff;

// Update code send as part of a frmUpdate event.
  frmRedrawUpdateCode = $8000;

// Magic button IDs used by FrmCustomResponseAlert callbacks
  frmResponseCreate = 1974;
  frmResponseQuit = $BEEF;

// Alert constants and structures
type
  alertTypes = Enum;

const
  informationAlert = 0;
  confirmationAlert = Succ(informationAlert);
  warningAlert = Succ(confirmationAlert);
  errorAlert = Succ(warningAlert);

type
  AlertType = alertTypes;

  AlertTemplateType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    alertType: UInt16;
    helpRscID: UInt16;
    numButtons: UInt16;
    defaultButton: UInt16;
  {$endif}
  end;
  AlertTemplateTag = AlertTemplateType;
// Types of object in a dialog box
  formObjects = Enum;

const
  frmFieldObj = 0;
  frmControlObj = Succ(frmFieldObj);
  frmListObj = Succ(frmControlObj);
  frmTableObj = Succ(frmListObj);
  frmBitmapObj = Succ(frmTableObj);
  frmLineObj = Succ(frmBitmapObj);
  frmFrameObj = Succ(frmLineObj);
  frmRectangleObj = Succ(frmFrameObj);
  frmLabelObj = Succ(frmRectangleObj);
  frmTitleObj = Succ(frmLabelObj);
  frmPopupObj = Succ(frmTitleObj);
  frmGraffitiStateObj = Succ(frmPopupObj);
  frmGadgetObj = Succ(frmGraffitiStateObj);
  frmScrollBarObj = Succ(frmGadgetObj);

type
  FormObjectKind = formObjects;

  FormObjAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
    UInt16 usable    :1; // Set if part of ui
    UInt16 reserved  :15; // pad it out
}
  {$endif}
  end;

  FormObjAttrTag = FormObjAttrType;

// Gadget support:
const
  formGadgetDrawCmd = 0; // paramP is unspecified
  formGadgetEraseCmd = 1; // paramP is unspecified
  formGadgetHandleEventCmd = 2; // paramP is an EventType *for the relevant event.
  formGadgetDeleteCmd = 3; // paramP is unspecified.

type
  FormGadgetAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
    UInt16 usable   :1; // Set if part of ui - "should be drawn"
    UInt16 extended  :1; // Set if the structure is an "Extended" gadget (i.e., the 'handler' field is present)
    UInt16 visible   :1; // Set if drawn - "has been drawn" or "must do work to erase"
    UInt16 reserved  :13; // pad it out
}
  {$endif}
  end;
  FormGadgetAttrTag = FormGadgetAttrType;

  FormGadgetPtr = ^FormGadgetType;

  FormGadgetHandlerType = function(gadgetP: FormGadgetPtr; cmd: UInt16; paramP: Pointer): Boolean;

  FormGadgetType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    id: UInt16;
    attr: FormGadgetAttrType;
    rect: RectangleType;
    data: Pointer;
    handler: FormGadgetHandlerType;
  {$endif}
  end;

// All of the smaller form objects:

  FormBitmapType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    attr: FormObjAttrType;
    pos: PointType;
    rscID: UInt16;
  {$endif}
  end;
  FormBitmapTag = FormBitmapType;
  FormBitmapPtr = ^FormBitmapType;

  FormLineType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    attr: FormObjAttrType;
    point1: PointType;
    point2: PointType;
  {$endif}
  end;
  FormLineTag = FormLineType;

  FormFrameType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    id: UInt16;
    attr: FormObjAttrType;
    rect: RectangleType;
    frameType: UInt16;
  {$endif}
  end;
  FormFrameTag = FormFrameType;

  FormRectangleType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    attr: FormObjAttrType;
    rect: RectangleType;
  {$endif}
  end;
  FormRectangleTag = FormRectangleType;

  FormLabelType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    id: UInt16;
    pos: PointType;
    attr: FormObjAttrType;
    fontID: FontID;
    reserved: UInt8;
    text: PChar;
  {$endif}
  end;
  FormLabelTag = FormLabelType;
  FormLabelPtr = ^FormLabelType;

  FormTitleType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    rect: RectangleType;
    text: PChar;
  {$endif}
  end;
  FormTitleTag = FormTitleType;

  FormPopupType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    controlID: UInt16;
    listID: UInt16;
  {$endif}
  end;
  FormPopupTag = FormPopupType;

  FrmGraffitiStateType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    pos: PointType;
  {$endif}
  end;
  FormGraffitiStateTag = FrmGraffitiStateType;
  FrmGraffitiStatePtr = ^FrmGraffitiStateType;

  FormObjectType = record
    case Integer of
      1: (ptr: Pointer);
      2: (field: ^FieldType);
      3: (control: ^ControlType);
      4: (graphicControl: ^GraphicControlType);
      5: (sliderControl: ^SliderControlType);
      6: (list: ^ListType);
      7: (table: ^TableType);
      8: (bitmap: ^FormBitmapType);
        // FormLineType *    line;
        // FormFrameType *   frame;
        // FormRectangleType *  rectangle;
      9: (label_: ^FormLabelType);
      10: (title: ^FormTitleType);
      11: (popup: ^FormPopupType);
      12: (grfState: ^FrmGraffitiStateType);
      13: (gadget: ^FormGadgetType);
      14: (scrollBar: ^ScrollBarType);
  end;
  FormObjectTag = FormObjectType;

// typedef FormObjectType *FormObjectPtr;

  FormObjListType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    objectType: FormObjectKind;
    reserved: UInt8;
    object_: FormObjectType;
  {$endif}
  end;
  FormObjListTag = FormObjListType;

  FormAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
 UInt16 usable   :1; // Set if part of ui
 UInt16 enabled   :1; // Set if interactable (not grayed out)
 UInt16 visible   :1; // Set if drawn, used internally
 UInt16 dirty   :1; // Set if dialog has been modified
 UInt16 saveBehind  :1; // Set if bits behind form are save when form ids drawn
 UInt16 graffitiShift :1;   // Set if graffiti shift indicator is supported
 UInt16 globalsAvailable:1; // Set by Palm OS if globals are available for the
          // form event handler
 UInt16 doingDialog :1; // FrmDoDialog is using for nested event loop
 UInt16 exitDialog  :1; // tells FrmDoDialog to bail out and stop using this form
 UInt16 attnIndicator :1;   // Set if attention indicator is supported
 UInt16 reserved      :6;   // pad to 16
}
    reserved2: UInt16;    // FormAttrType now explicitly 32-bits wide.
  {$endif}
  end;
  FormAttrTag = FormAttrType;

  FormEventHandlerType = function(var eventP: EventType): Boolean;
  FormEventHandlerPtr = FormEventHandlerType;

  FormType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FORMS} // These fields will not be available in the next OS release!
    window: WindowType;
    formId: UInt16;
    attr: FormAttrType;
    bitsBehindForm: WinHandle;
    handler: FormEventHandlerType;
    focus: UInt16;
    defaultButton: UInt16;
    helpRscId: UInt16;
    menuRscId: UInt16;
    numObjects: UInt16;
    objects: ^FormObjListType;
  {$endif}
  end;

  FormPtr = ^FormType;
  FieldPtr = ^FieldType;
// FormActiveStateType: this structure is passed to FrmActiveState for
// saving and restoring active form/window state; this structure's
// contents are abstracted because the contents will differ significantly
// as PalmOS evolves
// Added for PalmOS 3.0
  FormActiveStateType = record
    data: array [0..10] of UInt16;
  end;

// FrmCustomResponseAlert callback routine prototype
  FormCheckResponseFuncType = function(button: Int16; attempt: PChar): Boolean;
  FormCheckResponseFuncPtr = FormCheckResponseFuncType;

//--------------------------------------------------------------------
//
// Form Function
//
//--------------------------------------------------------------------

function FrmInitForm(rscID: UInt16): FormPtr; syscall sysTrapFrmInitForm;

procedure FrmDeleteForm(formP: FormPtr); syscall sysTrapFrmDeleteForm;

procedure FrmDrawForm(formP: FormPtr); syscall sysTrapFrmDrawForm;

procedure FrmEraseForm(formP: FormPtr); syscall sysTrapFrmEraseForm;

function FrmGetActiveForm: FormPtr; syscall sysTrapFrmGetActiveForm;

procedure FrmSetActiveForm(formP: FormPtr); syscall sysTrapFrmSetActiveForm;

function FrmGetActiveFormID: UInt16; syscall sysTrapFrmGetActiveFormID;

function FrmGetActiveField(const formP: FormPtr): FieldPtr; syscall sysTrapFrmGetActiveField;

function FrmGetUserModifiedState(const formP: FormPtr): Boolean; syscall sysTrapFrmGetUserModifiedState;

procedure FrmSetNotUserModified(formP: FormPtr); syscall sysTrapFrmSetNotUserModified;

function FrmGetFocus(const formP: FormPtr): UInt16; syscall sysTrapFrmGetFocus;

procedure FrmSetFocus(formP: FormPtr; fieldIndex: UInt16); syscall sysTrapFrmSetFocus;

function FrmHandleEvent(formP: FormPtr; eventP: EventPtr): Boolean; syscall sysTrapFrmHandleEvent;

procedure FrmGetFormBounds(const formP: FormPtr; var rP: RectangleType); syscall sysTrapFrmGetFormBounds;

function FrmGetWindowHandle(const formP: FormPtr): WinHandle; syscall sysTrapFrmGetWindowHandle;

function FrmGetFormId(const formP: FormPtr): UInt16; syscall sysTrapFrmGetFormId;

function FrmGetFormPtr(formId: UInt16): FormPtr; syscall sysTrapFrmGetFormPtr;

function FrmGetFirstForm: FormPtr; syscall sysTrapFrmGetFirstForm;

function FrmGetNumberOfObjects(const formP: FormPtr): UInt16; syscall sysTrapFrmGetNumberOfObjects;

function FrmGetObjectIndex(const formP: FormPtr; objID: UInt16): UInt16; syscall sysTrapFrmGetObjectIndex;

function FrmGetObjectIndexFromPtr(const formP: FormPtr; objP: Pointer): UInt16; syscall sysTrapFrmGetObjectIndexFromPtr;

function FrmGetObjectId(const formP: FormPtr; objIndex: UInt16): UInt16; syscall sysTrapFrmGetObjectId;

function FrmGetObjectType(const formP: FormPtr; objIndex: UInt16): FormObjectKind; syscall sysTrapFrmGetObjectType;

function FrmGetObjectPtr(const formP: FormPtr; objIndex: UInt16): Pointer; syscall sysTrapFrmGetObjectPtr;

procedure FrmGetObjectBounds(const formP: FormPtr; objIndex: UInt16; var rP: RectangleType); syscall sysTrapFrmGetObjectBounds;

procedure FrmHideObject(formP: FormPtr; objIndex: UInt16); syscall sysTrapFrmHideObject;

procedure FrmShowObject(formP: FormPtr; objIndex: UInt16); syscall sysTrapFrmShowObject;

procedure FrmGetObjectPosition(const formP: FormPtr; objIndex: UInt16; var x, y: Coord); syscall sysTrapFrmGetObjectPosition;

procedure FrmSetObjectPosition(formP: FormPtr; objIndex: UInt16; x, y: Coord); syscall sysTrapFrmSetObjectPosition;

procedure FrmSetObjectBounds(formP: FormPtr; objIndex: UInt16; {const} var bounds: RectangleType); syscall sysTrapFrmSetObjectBounds;

function FrmGetControlValue(const formP: FormPtr; objIndex: UInt16): Int16; syscall sysTrapFrmGetControlValue;

procedure FrmSetControlValue(const formP: FormPtr; objIndex, newValue: Int16); syscall sysTrapFrmSetControlValue;

function FrmGetControlGroupSelection(const formP: FormPtr; groupNum: UInt8): UInt16; syscall sysTrapFrmGetControlGroupSelection;

procedure FrmSetControlGroupSelection(const formP: FormPtr; groupNum: UInt8; controlID: UInt16); syscall sysTrapFrmSetControlGroupSelection;

procedure FrmCopyLabel(formP: FormPtr; labelID: UInt16; const newLabel: PChar); syscall sysTrapFrmCopyLabel;

function FrmGetLabel(const formP: FormPtr; labelID: UInt16): {const} PChar; syscall sysTrapFrmGetLabel;

procedure FrmSetCategoryLabel(const formP: FormPtr; objIndex: UInt16; newLabel: PChar); syscall sysTrapFrmSetCategoryLabel;

function FrmGetTitle(const formP: FormPtr): {const} PChar; syscall sysTrapFrmGetTitle;

procedure FrmSetTitle(formP: FormPtr; newTitle: PChar); syscall sysTrapFrmSetTitle;

procedure FrmCopyTitle(formP: FormPtr; const newTitle: PChar); syscall sysTrapFrmCopyTitle;

function FrmGetGadgetData(const formP: FormPtr; objIndex: UInt16): Pointer; syscall sysTrapFrmGetGadgetData;

procedure FrmSetGadgetData(formP: FormPtr; objIndex: UInt16; const data: Pointer); syscall sysTrapFrmSetGadgetData;

procedure FrmSetGadgetHandler(formP: FormPtr; objIndex: UInt16; attrP: FormGadgetHandlerType); syscall sysTrapFrmSetGadgetHandler;

function FrmDoDialog(formP: FormPtr): UInt16; syscall sysTrapFrmDoDialog;

function FrmAlert(alertId: UInt16): UInt16; syscall sysTrapFrmAlert;

function FrmCustomAlert(alertId: UInt16; const s1, s2, s3: PChar): UInt16; syscall sysTrapFrmCustomAlert;

procedure FrmHelp(helpMsgId: UInt16); syscall sysTrapFrmHelp;

procedure FrmUpdateScrollers(formP: FormPtr; upIndex, downIndex: UInt16; scrollableUp, scrollableDown: Boolean); syscall sysTrapFrmUpdateScrollers;

function FrmVisible(const formP: FormPtr): Boolean; syscall sysTrapFrmVisible;

procedure FrmSetEventHandler(formP: FormPtr; handler: FormEventHandlerType); syscall sysTrapFrmSetEventHandler;

function FrmDispatchEvent(var eventP: EventType): Boolean; syscall sysTrapFrmDispatchEvent;

procedure FrmPopupForm(formId: UInt16); syscall sysTrapFrmPopupForm;

procedure FrmGotoForm(formId: UInt16); syscall sysTrapFrmGotoForm;

procedure FrmUpdateForm(formId, updateCode: UInt16); syscall sysTrapFrmUpdateForm;

procedure FrmReturnToForm(formId: UInt16); syscall sysTrapFrmReturnToForm;

procedure FrmCloseAllForms; syscall sysTrapFrmCloseAllForms;

procedure FrmSaveAllForms; syscall sysTrapFrmSaveAllForms;

function FrmPointInTitle(const formP: FormPtr; x, y: Coord): Boolean; syscall sysTrapFrmPointInTitle;

procedure FrmSetMenu(formP: FormPtr; menuRscID: UInt16); syscall sysTrapFrmSetMenu;

function FrmValidatePtr(const formP: FormPtr): Boolean; syscall sysTrapFrmValidatePtr;

function FrmAddSpaceForObject(var formPP: FormPtr; var objectPP: MemPtr; objectKind: FormObjectKind; objectSize: UInt16): Err; syscall sysTrapFrmAddSpaceForObject;

function FrmRemoveObject(var formPP: FormPtr; objIndex: UInt16): Err; syscall sysTrapFrmRemoveObject;

function FrmNewForm(formID: UInt16; const titleStrP: PChar; x, y, width, height: Coord; modal: Boolean;
                    defaultButton, helpRscID, menuRscID: UInt16): FormPtr; syscall sysTrapFrmNewForm;

function FrmNewLabel(var formPP: FormPtr; ID: UInt16; const textP: PChar; x, y: Coord; font: FontID): FormLabelPtr; syscall sysTrapFrmNewLabel;

function FrmNewBitmap(var formPP: FormPtr; ID, rscID: UInt16; x, y: Coord): FormBitmapPtr; syscall sysTrapFrmNewBitmap;

function FrmNewGadget(var formPP: FormPtr; id: UInt16; x, y, width, height: Coord): FormGadgetPtr; syscall sysTrapFrmNewGadget;

function FrmActiveState(var stateP: FormActiveStateType; save: Boolean): Err; syscall sysTrapFrmActiveState;

function FrmCustomResponseAlert(alertId: UInt16; const s1, s2, s3: PChar; entryStringBuf: PChar;
                                entryStringBufLength: Int16; callback: FormCheckResponseFuncType): UInt16; syscall sysTrapFrmCustomResponseAlert;

function FrmNewGsi(var formPP: FormPtr; x, y: Coord): FrmGraffitiStatePtr; syscall sysTrapFrmNewGsi;

function FrmSaveActiveState(var stateP: FormActiveStateType): Err;
function FrmRestoreActiveState(var stateP: FormActiveStateType): Err;

implementation

function FrmSaveActiveState(var stateP: FormActiveStateType): Err;
begin
  FrmSaveActiveState := FrmActiveState(stateP, True);
end;

function FrmRestoreActiveState(var stateP: FormActiveStateType): Err;
begin
  FrmRestoreActiveState := FrmActiveState(stateP, False);
end;

end.
