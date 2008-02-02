{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1996-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Progress.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This header file defines a generic progress dialog interface
 *
 * History:
 *    6/4/97 from Ron Marianetti's net dialog stuff   Created by Gavin Peacock
 *
 *****************************************************************************)

unit progress;

interface

uses palmos, coretraps, control;

const
  progressMaxMessage    = 128;
  progressMaxTitle      = 31;  // max size for title of progress dialog
  progressMaxButtonText = 7;   // max size of text in OK/Cancel button

// Progress callback function
// The progress dialog will call this function to get the text to display for the
// current status.
// stage - the current stage of progess as defined by your app
// message - text that can be sent from the protocol
// cancel - true if the dialog is in cancel mode
// error - current error (func should return an error message in this case...
type
  PrgCallbackData = record
    stage: UInt16;              // <= current stage
    textP: PChar;               // => buffer to hold text to display
    textLen: UInt16;            // <= length of text buffer
    message: PChar;             // <= additional text for display
    error: Err;                 // <= current error
    bitmapId: UInt16;           // => resource ID of bitmap to display
    Bits: UInt16;
{
    UInt16 canceled:1;          // <= true if user has pressed the cancel button
    UInt16 showDetails:1;       // <= true if user pressed down arrow for more details
    UInt16 textChanged:1;       // => if true then update text (defaults to true)
    UInt16 timedOut:1;          // <= true if update caused by a timeout
}
    timeout: UInt32;            // <> timeout in ticks to force next update (for animation)

    //progress bar info (Not Implemented)
    barMaxValue: UInt32;        // the maximum value for the progress bar, if = 0 then the bar is
                                // not visible
    barCurValue: UInt32;        // the current value of the progress bar, the bar will be drawn
                                // filled the percentage of maxValue \ value
    barMessage: PChar;          // additional text for display below the progress bar.
    barFlags: UInt16;           // reserved for future use.

    //
    // *** The following fields were added in PalmOS 3.2 ***
    //

   delay_bits: UInt16;
{
    UInt16  delay:1;            // => if true delay 1 second after updating form icon/msg
}
    userDataP: Pointer;         // <= context pointer that caller passed to PrgStartDialog
  end;

  PrgCallbackDataPtr = ^PrgCallbackData;

//typedef Boolean (*PrgCallbackFunc)  (UInt16 stage,Boolean showDetails,Char *message,Boolean cancel,UInt16 error,Char *textP, UInt16 maxtextLen,UInt16 *bitmapID);

  PrgCallbackFunc = function(cbP: PrgCallbackDataPtr): Boolean;

//---------------------------------------------------------------------------
// Structure of the Progress Info structure. This structure should be stored
//  in the interface's globals. Each of the routines in SerNetIFCommon.c
//  take a pointer to this structure.
//---------------------------------------------------------------------------

  ProgressType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_PROGRESS}

    // This field contains a pointer to the open progress dialog
    frmP: FormPtr;                       // Our progress dialog ptr

    // This field is set a maximum time for the action to take place. A cancel
    // will be generated if this timeout is reached
    timeout: UInt32;                    // max time to wait in ticks


    Bits: UInt16;
{
    // This boolean is set by either the protocol (through PrgUpdateDialog()) or UI
    //  task to inform the UI that it needs to update it's progress dialog with new
    //  information as stored in the error, stage, and message fields.
    UInt16              needUpdate:1;           // true if UI update required.


    // The following boolean is set by the UI task when the user hits the cancel button.
    // When the user cancels, the UI changes to display "Cancelling..." and then waits
    // for the protocol task to notice the user cancel and set the error field to
    //  netErrUserCancel before disposing the dialog. The SerIFUserCancel() which is
    //  called from the protocol task checks this boolean.
    UInt16              cancel:1;               // true if cancelling


    // This boolean is set by PrvCheckEvents() after we've displayed an error message
    //  in the progress dialog and changed the "cancel" button to an "OK" button.
    //  This tells the dialog event handling code in PrvCheckEvents() that it should
    //  dispose of the dialog on the next hit of the cancel/OK button.
    UInt16              waitingForOK:1;     // true if waiting for OK button hit.


    // This boolean gets set if the user hits the down button while the UI is up. It
    //  causes more detailed progress to be shown
    UInt16              showDetails:1;          // show progress details.

    // This is set to true whenever the message text is changed. This allows the
    // display to be more efficient by not redrawing when not needed
    UInt16                  messageChanged: 1;
}

    //-----------------------------------------------------------------------
    // The following fields are set by PrgUpdateDialog() and used by PrgHandleEvent()
    //  to figure out what to display in the progress dialog
    //-----------------------------------------------------------------------

    // This word is set by the protocol task (through PrgUpdateDialog()) when an
    //  error occurs during connection establishment. If this error is non-nil
    //  and not equal to netErrUserCancel, the UI task will display the appropriate
    //  error message and change the cancel button to an OK button, set the waitingForOK
    //  boolean and wait for the user to  hit the OK button before disposing
    //  the dialog.
    error: UInt16;                  // error set by interface

    // This enum is set by the protocol task (through PrgUpdateDialog()) as it
    //  progresses through the  connection establishment and is checked by
    //  PrgHandleEvent() when needUpate is true. It is used to determine what
    //  string to display in the progress dialog.
    stage: UInt16;                  // which stage of the connection we're in


    // This is an additional string that is displayed in the progress dialog for
    //  certain stages. The netConStageSending stage for example uses this string
    //  for holding the text string that it is sending. It is set by
    //  PrgUpdateDialog().
    message: array [0..progressMaxMessage] of Char; // connection stage message.

    reserved1: UInt8;

    // Used to cache current icon number so we don't unnecessarily redraw it
    lastBitmapID: UInt16;

    // Text array used to hold control title for the OK/Cancel button. This
    //  must be kept around while the control is present in case of updates.
    ctlLabel: array [0..progressMaxButtonText] of Char;

    serviceNameP: PChar;

    //progress bar stuff (Not implemented)
    lastBarMaxValue: UInt32;
    lastBarCurValue: UInt32;

    // stuff for saving old window state
    oldDrawWinH: WinHandle;
    oldActiveWinH: WinHandle;
    oldFrmP: FormPtr;
    oldInsPtState: Boolean;
    reserved2: UInt8;
    oldInsPtPos: PointType;

    textCallback: PrgCallbackFunc;

    title: array [0..progressMaxTitle] of Char;

    //
    // *** The following field was added in PalmOS 3.2 ***
    //

    userDataP: Pointer;
  {$else}
    opaque1: UInt32;
    opaque2: UInt32;
    Bits: UInt16;

   {!!!
       UInt16     opaque3:1;

   // The following boolean is set by the UI task when the user hits the cancel button.
   // When the user cancels, the UI changes to display "Cancelling..." and then waits
   // for the protocol task to notice the user cancel and set the error field to
   //  netErrUserCancel before disposing the dialog. The SerIFUserCancel() which is
   //  called from the protocol task checks this boolean.
   UInt16         cancel:1;            // true if cancelling

   UInt16         opaque4:14;
    !!!}
   // This word is set by the protocol task (through PrgUpdateDialog()) when an
   //  error occurs during connection establishment. If this error is non-nil
   //  and not equal to netErrUserCancel, the UI task will display the appropriate
   //  error message and change the cancel button to an OK button, set the waitingForOK
   //  boolean and wait for the user to  hit the OK button before disposing
   //  the dialog.
           error: UInt16;               // error set by interface

   // This enum is set by the protocol task (through PrgUpdateDialog()) as it
   //  progresses through the  connection establishment and is checked by
   //  PrgHandleEvent() when needUpate is true. It is used to determine what
   //  string to display in the progress dialog.
           stage: UInt16;               // which stage of the connection we're in
  {$endif}
  end;

  ProgressPtr = ^ProgressType;

// Warning:  In the future, the ProgressType will be opaque.  So, please don't
// write code that depends on its internals; you'll just pass it around as a
// "cookie," and that's how you should be treating it now.

// macro to test if the user has canceled

function PrgUserCancel(prgP: ProgressPtr): Boolean;

//-----------------------------------------------------------------------
// Prototypes
//-----------------------------------------------------------------------

function PrgStartDialogV31(const title: PChar; textCallback: PrgCallbackFunc): ProgressPtr; syscall sysTrapPrgStartDialogV31;

function PrgStartDialog(const title: PChar; textCallback: PrgCallbackFunc; userDataP: Pointer): ProgressPtr; syscall sysTrapPrgStartDialog;

procedure PrgStopDialog(prgP: ProgressPtr; force: Boolean); syscall sysTrapPrgStopDialog;

procedure PrgUpdateDialog(prgGP: ProgressPtr; err, stage: UInt16; const messageP: PChar; updateNow: Boolean); syscall sysTrapPrgUpdateDialog;

function PrgHandleEvent(prgGP: ProgressPtr; eventP: EventPtr): Boolean; syscall sysTrapPrgHandleEvent;

implementation

function PrgUserCancel(prgP: ProgressPtr): Boolean;
begin
  PrgUserCancel := (prgP^.Bits and $4000) <> 0; // cancel
end;

end.
