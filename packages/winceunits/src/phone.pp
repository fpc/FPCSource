{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//
// phone.h
//
// Phone API
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit phone;

{$CALLING cdecl}

interface

uses Windows;

type
     CALLERIDTYPE = (CALLERIDTYPE_UNAVAILABLE,
                     CALLERIDTYPE_BLOCKED,
                     CALLERIDTYPE_AVAILABLE);

type
     IOM = (IOM_MISSED,
            IOM_INCOMING,
            IOM_OUTGOING);

type
     CALLLOGSEEK = (CALLLOGSEEK_BEGINNING := 2,
                    CALLLOGSEEK_END := 4);

// Thought out by me specially for CALLLOGENTRY._flags field.
const
      CALLLOGENTRY_FLAG_INCOMING  = 0; // direction of call.  (Missed calls are incoming.)
      CALLLOGENTRY_FLAG_OUTGOING  = 1; // direction of call.  (Missed calls are incoming.)
      CALLLOGENTRY_FLAG_CONNECTED = 2; // Did the call connect? (as opposed to busy, no answer)
      CALLLOGENTRY_FLAG_ENDED     = 4; // Was the call ended? (as opposed to dropped)
      CALLLOGENTRY_FLAG_DROPPED   = 0; // Was the call dropped? (as opposed to ended)
      CALLLOGENTRY_FLAG_ROAMING   = 8; // Roaming (vs. local)
      CALLLOGENTRY_FLAG_LOCAL     = 0; // Local

type
     CALLLOGENTRY = record
       cbSize:DWORD;  // sizeof CALLLOGENTRY
       ftStartTime:FILETIME;
       ftEndTime:FILETIME;
       _iom:IOM;
       _flags:UINT;
{
       BOOL fOutgoing:1;        // direction of call.  (Missed calls are incoming.)
       BOOL fConnected:1;     // Did the call connect? (as opposed to busy, no answer)
       BOOL fEnded:1;        // Was the call ended? (as opposed to dropped)
       BOOL fRoam:1;        // Roaming (vs. local)
}
       cidt:CALLERIDTYPE;
       pszNumber:PTSTR;
       pszName:PTSTR;
       pszNameType:PTSTR;    // "w" for work tel, "h" for home tel, for example
       pszNote:PTSTR;    // filename of associated Notes file
     end;
     PCALLLOGENTRY = ^CALLLOGENTRY;

const
      PhoneDLL = 'phone.dll';

// Open the call log and set the seek pointer to the beginning.
function PhoneOpenCallLog(ph:LPHANDLE):HRESULT; external PhoneDLL name 'PhoneOpenCallLog';

// PhoneGetCallLogEntry returns one call log entry and advances the seek
// pointer.
// pentry->cbSize must be set to "sizeof CALLLOGENTRY" before this function is
// called.
// The entries are returned in order of start time, the most recent call first.
// It is the responsibility of the caller to LocalFree any strings
// which are returned.
// A return value of S_FALSE indicates there are no more entries.
function PhoneGetCallLogEntry(h:HANDLE; pentry:PCALLLOGENTRY):HRESULT; external PhoneDLL name 'PhoneGetCallLogEntry';

// PhoneSeekCallLog seeks to a record in the call log.
// iRecord is the zero-based index of the  record, starting at the beginning
// if seek == CALLLOGSEEK_BEGINNING, at the end if CALLLOGSEEK_END.
// piRecord returns the zero-based index (from the beginning) of the seek
// pointer after the seek is performed.
// PhoneSeekCallLog(h, CALLLOGSEEK_END, 0, &count) will return the number of
// records.
function PhoneSeekCallLog(hdb:HANDLE;
                          _seek:CALLLOGSEEK;
                          iRecord:DWORD;
                          piRecord:LPDWORD):HRESULT; external PhoneDLL name 'PhoneGetCallLogEntry';

// Close the call log
function PhoneCloseCallLog(h:HANDLE):HRESULT; external PhoneDLL name 'PhoneCloseCallLog';


// The Call Log application supports the context menu extensibility mechanism.
// The Context name is "Phone" and the Class name is "Log".
// The IUnknown pointer passed to the context menu handler supports IPropertyBag,
// and the set of properties supported is as follows:
//
// property name:   type:   value:
// --------------   -----   ------
// PropSet          BSTR    "CallLog"
// Number           BSTR    the phone number of the other person on the call
// NumberType       BSTR    the type of phone number (e.g. "h" for home)
// Name             BSTR    person's name
// Year             I2      the year of the beginning time of the call
// Month            I2      the month of the beginning time of the call
// Day              I2      the day of the beginning time of the call
// Hour             I2      the hour of the beginning time of the call
// Minute           I2      the minute of the beginning time of the call
// Second           I2      the second of the beginning time of the call
// DayOfWeek        I2      the day of the week of the beginning time of the call
// Duration         UI4     the duration of the call in seconds
// CallerIDType     UI4     a CALLERIDTYPE value (see above)
// Connected        BOOL    Did the call connect? (as opposed to busy, no answer)
// Ended            BOOL    Was the call ended? (as opposed to dropped)
// Outgoing         BOOL    Was the call outgoing?  (Missed calls are incoming.)
// Roaming          BOOL    Roaming?
// IOM              UI4     an IOM value (see above)


// PhoneShowCallLog
//
// Show the Call Log, and filter it
type
     CALLLOGFILTER = (CALLLOGFILTER_ALL_CALLS,
                      CALLLOGFILTER_MISSED,
                      CALLLOGFILTER_INCOMING,
                      CALLLOGFILTER_OUTGOING);

function PhoneShowCallLog(iCallLogFilter:CALLLOGFILTER):HRESULT; external PhoneDLL name 'PhoneShowCallLog';

//++++++
//
//  PhoneMakeCall
//
//       Dials a number
type
     tagPHONEMAKECALLINFO = record
       cbSize:DWORD;
       dwFlags:DWORD;

      //Params to tapiRequestMakeCall
       pszDestAddress:PCWSTR;
       pszAppName:PCWSTR;
       pszCalledParty:PCWSTR;
       pszComment:PCWSTR;

      // owner window for dialog box that appears when the PMCF_EDITBEFORECALLING flag is passed into PhoneMakeCall
       hwndOwner:HWND;
     end;
     PHONEMAKECALLINFO = tagPHONEMAKECALLINFO;
     PPHONEMAKECALLINFO = ^tagPHONEMAKECALLINFO;

// PhoneMakeCall flags
const
      PMCF_DEFAULT                = $00000001;
      PMCF_PROMPTBEFORECALLING    = $00000002;
      PMCF_EDITBEFORECALLING      = $00000020;

function PhoneMakeCall(ppmci:PPHONEMAKECALLINFO):LONG; external PhoneDLL name 'PhoneMakeCall';

//
// End PhoneMakeCall
//
//------


//++++++
//
//  PhoneAddSpeedDial
//
//       adds a speed dial entry
//
// dwFlags - currently unused, set to 0
//
// piKey - in/out: the key sequence which will invoke the speed dial.
//           currently an entry between 2 and 99 (inclusive) is valid.
//            the API will cause existing entries to be overwritten
//
// pszDisplayName - the non-empty display name for the speed dial
//
// pszTelNumber - the non-empty tel# to be dialed
//

function PhoneAddSpeedDial(dwFlags:DWORD;
                           piKey:LPDWORD;
                           pszDisplayName:PTCHAR;
                           pszTelNumber:PTCHAR):HRESULT; external PhoneDLL name 'PhoneAddSpeedDial';
                           
//
// End PhoneAddSpeedDial
//
//------




//++++++
//
//  PhoneSendDTMFStart
//
//  Allows an application to begin sending a DTMF tone using the
//  current phone call.
//
//  Parameters:
//    chDTMF - DTMF character to send. Must be '0'-'9', 
//             'A'-'D', '*', or '#'.
//
//  Return Value:
//    -----------------------------------------------------------
//    Value                        Description
//    ------------------------------------------------------------
//    S_OK                         The operation completed successfully.
//    E_FAIL                       An unknown error occurred.
//    E_INVALIDARG                 chDTMF is invalid.
//    E_NOTIMPL                    No phone is present on the device, the 
//                                 phone service is not running, or no 
//                                 calls are currently active.
//    
//    Other custom failure codes where the facility code is
//    FACILITY_WINDOWS_CE may be retrieved using the HRESULT_CODE
//    macro:
//    -----------------------------------------------------------
//    Error Code                   Description
//    ------------------------------------------------------------
//    ERROR_DEVICE_NOT_CONNECTED   No phone is present on the device, 
//                                 the phone service is not running, or 
//                                 no calls are currently active.
//
//  Remarks:
//    Caller must call PhoneSendDTMFStop to stop the DTMF tone.
//
//    This function will fail if no phone is present on the 
//    device, the phone service is not running, or no calls are  
//    currently active. If a subsequent call to this API is made 
//    by the caller or any other application before the DTMF tone  
//    is finished the original DTMF tone will cease and the new 
//    DTMF will be sent.
//

function PhoneSendDTMFStart(chDTMF:TCHAR):HRESULT; external PhoneDLL name 'PhoneSendDTMFStart';

//
// End PhoneSendDTMFStart
//
//------



//++++++
//
//  PhoneSendDTMFStop
//
//  Stops an existing DTMF tone.
//
//  Remarks:
//    This function stops all DTMF tones. Applications
//    may use PhoneSendDTMFStart to begin a DTMF tone.
//

procedure PhoneSendDTMFStop; external PhoneDLL name 'PhoneSendDTMFStop';

//
// End PhoneSendDTMFStop
//
//------


//++++++
//
//  PhoneIsEmergencyNumber
//
//  Determines if a string contains an emergency phone number.
//
//  Parameters:
//    fTailMatchOnly
//        [in] If this argument is set to TRUE, the function only 
//        attempts to match emergency numbers at the end of 
//        pszNumber. If this value is FALSE, the function attempts 
//        to match the emergency string with the full pszNumber 
///       argument exactly.
//    pszNumber
//        [in] String to match. If this pointer is NULL or points 
//        to an empty string, the function returns FALSE.
//    ppszEmergencyNumber
//        [out] On success, constant pointer to the emergency phone 
//        number matched in pszNumber. Caller must not free. On 
//        failure, this pointer is undefined. Optional, may be NULL.
//
//  Return Value:
//    TRUE if pszNumber contains an emergency number; otherwise FALSE.
//
//  Remarks:
//    The system merges the list of emergency phone numbers from 
//    the registry and the SIM, if available. The list of emergency 
//    numbers can vary by device.
//

function PhoneIsEmergencyNumber(fTailMatchOnly:BOOL;
                                pszNumber:LPCTSTR;
                                ppszEmergencyNumber:PLPWStr):BOOL;external PhoneDLL name 'PhoneIsEmergencyNumber';

//
// End PhoneIsEmergencyNumber
//
//------


implementation

end.