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
// Module: pimstore.h
//

//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit pimstore;

{$MODE OBJFPC}

interface

uses Windows, ActiveX, windbase;

const
// Flags for receiving notifications
      PIMFOLDERNOTIFICATION_REMOTE = $01;    // Notification for changes from other processes
      PIMFOLDERNOTIFICATION_LOCAL  = $02;    // Notification for changes from this process
      PIMFOLDERNOTIFICATION_ALL    = PIMFOLDERNOTIFICATION_REMOTE or PIMFOLDERNOTIFICATION_LOCAL;

// Notification window messages for changes in the local process
      PIM_ITEM_CREATED_LOCAL        = WM_APP + $0100;
      PIM_ITEM_DELETED_LOCAL        = WM_APP + $0101;
      PIM_ITEM_CHANGED_LOCAL        = WM_APP + $0102;

      PIM_ITEM_CREATED_REMOTE       = WM_APP + $0105;
      PIM_ITEM_DELETED_REMOTE       = WM_APP + $0106;
      PIM_ITEM_CHANGED_REMOTE       = WM_APP + $0107;

type
     PDATE = ^TDateTime;

const
      LIBID_PocketOutlook:TIID = '{4E130E40-7DBE-11D2-8F23-0000F87A4335}';
      
// Interfaces IDs.
const
      IID_IException:TIID              = '{B47398D0-3B73-11d2-8F1B-0000F87A4335}';
      IID_IExceptions:TIID             = '{B47398D1-3B73-11d2-8F1B-0000F87A4335}';
      IID_ITimeZone:TIID               = '{78B27290-5256-11d2-8F1B-0000F87A4335}';
      IID_IRecurrencePattern:TIID      = '{38F47300-270F-11d2-8F18-0000F87A4335}';
      IID_IRecipient:TIID              = '{7E136BE0-5240-11d2-8F1B-0000F87A4335}';
      IID_IPOlRecipient:TIID           = '{A11C6E30-51B5-11d3-8F39-0000F87A4335}';
      IID_IRecipient2:TIID             = '{13A73D92-C9CE-4904-A348-CBA3246F2F8C}';
      IID_IRecipients:TIID             = '{76065AE0-2347-11d2-8F18-0000F87A4335}';
      IID_IPOutlookItemCollection:TIID = '{F06748C0-21A5-11d2-8F18-0000F87A4335}';
      IID_IPOlItems:TIID               = '{6E3DBE90-5411-11d3-8F39-0000F87A4335}';
      IID_IContact:TIID                = '{7F804E40-2010-11d2-8F18-0000F87A4335}';
      IID_ITask:TIID                   = '{37C78CE0-202C-11d2-8F18-0000F87A4335}';
      IID_IAppointment:TIID            = '{5B43F691-202C-11d2-8F18-0000F87A4335}';
      IID_ICity:TIID                   = '{C83C5E90-3D1B-11d2-8F1B-0000F87A4335}';
      IID_IFolder:TIID                 = '{05058F20-20BE-11d2-8F18-0000F87A4335}';
      IID_IPOutlookApp:TIID            = '{05058F22-20BE-11d2-8F18-0000F87A4335}';
      IID_IPOutlookApp2:TIID           = '{AF7D0DC7-3D35-424b-AA60-27A38B8B629E}';
      IID_IItem:TIID                   = '{FB8998D0-38F0-4d12-AC56-4EC8FCE9F3D5}';
      IID_IPOlItems2:TIID              = '{CB52F880-4CB0-4a23-AA09-82326C98929F}';

      IID_IPimSrcContactSummaryCard:TIID = '{95932f0a-e03a-412c-85ee-045365277e60}';
      IID_IPimSrcContactNew:TIID         = '{FD76819E-4BDC-4e98-880C-15DED1FDA30D}';
      IID_IPimSrcContactListIcon:TIID    = '{95932f0a-e03a-412c-85ee-045365277e62}';

const
      CLSID_Exception:CLSID         = '{b47398d2-3b73-11d2-8f1b-0000f87a4335}';
      CLSID_Exceptions:CLSID        = '{b47398d3-3b73-11d2-8f1b-0000f87a4335}';
      CLSID_TimeZone:CLSID          = '{78b27291-5256-11d2-8f1b-0000f87a4335}';
      CLSID_RecurrencePattern:CLSID = '{38f47301-270f-11d2-8f18-0000f87a4335}';
      CLSID_Recipient:CLSID         = '{7e136be1-5240-11d2-8f1b-0000f87a4335}';
      CLSID_Recipients:CLSID        = '{76065ae1-2347-11d2-8f18-0000f87a4335}';
      CLSID_Items:CLSID             = '{f06748c1-21a5-11d2-8f18-0000f87a4335}';
      CLSID_ContactItem:CLSID       = '{430539d0-2017-11d2-8f18-0000f87a4335}';
      CLSID_TaskItem:CLSID          = '{5b43f690-202c-11d2-8f18-0000f87a4335}';
      CLSID_AppointmentItem:CLSID   = '{5b43f692-202c-11d2-8f18-0000f87a4335}';
      CLSID_CityItem:CLSID          = '{c83c5e91-3d1b-11d2-8f1b-0000f87a4335}';
      CLSID_Folder:CLSID            = '{05058f21-20be-11d2-8f18-0000f87a4335}';
      CLSID_Application:CLSID       = '{05058F23-20BE-11d2-8F18-0000F87A4335}';

type
// Forward declarations
      IAppointment = interface;
      IPOutlookApp = interface;
      IItem = interface;

{****************************************
 * Generated header for interface: IException
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

      IException = interface(IDispatch)
       ['{B47398D0-3B73-11d2-8F1B-0000F87A4335}']
        function get_AppointmentItem(out ppAppt:IAppointment):HRESULT; stdcall;
        function get_OriginalDate(_pdate:PDATE):HRESULT; stdcall;
        function get_Deleted(pfDeleted:PVariant):HRESULT; stdcall;
        function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
      end;

{****************************************
 * Generated header for interface: IExceptions
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

    IExceptions = interface(IDispatch)
     ['{B47398D1-3B73-11d2-8F1B-0000F87A4335}']
      function Item(nIndex:longint; out ppExcept:IException):HRESULT; stdcall;
      function get_Count(pnCount:PLongint):HRESULT; stdcall;
      function get__NewEnum(out ppEnumerator:IUnknown):HRESULT; stdcall; // ppEnumerator is a reference to the IEnumVARIANT
                                                                         // interface for an enumerator object for the collection.
      function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
    end;

{****************************************
 * Generated header for interface: ITimeZone
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

    ITimeZone = interface(IDispatch)
     ['{78B27290-5256-11d2-8F1B-0000F87A4335}']
      function get_Bias(plBias:LPLONG):HRESULT; stdcall;
      function get_SupportsDST(pfSupportsDST:PVARIANT_BOOL):HRESULT; stdcall;
      function get_IsStandardAbsoluteDate(pfAbsolute:PVARIANT_BOOL):HRESULT; stdcall;
      function get_IsDaylightAbsoluteDate(pfAbsolute:PVARIANT_BOOL):HRESULT; stdcall;
      function get_StandardBias(plBias:LPLONG):HRESULT; stdcall;
      function get_StandardName(out ppwsz:BSTR):HRESULT; stdcall;
      function get_StandardDate(_pDate:PDATE):HRESULT; stdcall;
      function get_StandardDayOfWeekMask(plMask:LPLONG):HRESULT; stdcall;
      function get_StandardInstance(plInstance:LPLONG):HRESULT; stdcall;
      function get_StandardMonthOfYear(plMonth:LPLONG):HRESULT; stdcall;
      function get_DaylightBias(plBias:LPLONG):HRESULT; stdcall;
      function get_DaylightName(out ppwsz:BSTR):HRESULT; stdcall;
      function get_DaylightDate(_pDate:PDATE):HRESULT; stdcall;
      function get_DaylightDayOfWeekMask(plMask:LPLONG):HRESULT; stdcall;
      function get_DaylightInstance(plInstance:LPLONG):HRESULT; stdcall;
      function get_DaylightMonthOfYear(plMonth:LPLONG):HRESULT; stdcall;
      function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
    end;

{****************************************
 * Generated header for interface: IRecurrencePattern
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IRecurrencePattern = interface(IDispatch)
      ['{38F47300-270F-11d2-8F18-0000F87A4335}']
       function get_RecurrenceType(plRecType:LPLONG):HRESULT; stdcall;
       function get_PatternStartDate(pst:PDATE):HRESULT; stdcall;
       function get_StartTime(pst:PDATE):HRESULT; stdcall;
       function get_EndTime(pst:PDATE):HRESULT; stdcall;
       function get_PatternEndDate(pst:PDATE):HRESULT; stdcall;
       function get_NoEndDate(pfNoEndDate:PVARIANT_BOOL):HRESULT; stdcall;
       function get_Occurrences(plOccurrences:LPLONG):HRESULT; stdcall;
       function get_Interval(plInterval:LPLONG):HRESULT; stdcall;
       function get_DayOfWeekMask(plMask:LPLONG):HRESULT; stdcall;
       function get_DayOfMonth(plDay:LPLONG):HRESULT; stdcall;
       function get_Instance(plInstance:LPLONG):HRESULT; stdcall;
       function get_Duration(plDuration:LPLONG):HRESULT; stdcall;
       function get_MonthOfYear(plMask:LPLONG):HRESULT; stdcall;
       function put_RecurrenceType(lRecType:LONG):HRESULT; stdcall;
       function put_PatternStartDate(st:TDateTime):HRESULT; stdcall;
       function put_StartTime(st:TDateTime):HRESULT; stdcall;
       function put_EndTime(st:TDateTime):HRESULT; stdcall;
       function put_PatternEndDate(st:TDateTime):HRESULT; stdcall;
       function put_NoEndDate(fNoEndDate:VARIANT_BOOL):HRESULT; stdcall;
       function put_Occurrences(lOccurrences:LONG):HRESULT; stdcall;
       function put_Interval(lInterval:LONG):HRESULT; stdcall;
       function put_DayOfWeekMask(lMask:LONG):HRESULT; stdcall;
       function put_DayOfMonth(lDay:LONG):HRESULT; stdcall;
       function put_Instance(lInstance:LONG):HRESULT; stdcall;
       function put_Duration(lDuration:LONG):HRESULT; stdcall;
       function put_MonthOfYear(lMask:LONG):HRESULT; stdcall;
       function get_Exceptions(out ppExceptions:IExceptions):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
       function GetOccurrence(_date:TDateTime; out ppAppt:IAppointment):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IRecipient
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IRecipient = interface(IDispatch)
      ['{7E136BE0-5240-11d2-8F1B-0000F87A4335}']
       function put_Address(pwsz:BSTR):HRESULT; stdcall;
       function get_Address(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Name(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IPOlRecipient
 * at Fri Aug 13 12:48:03 1999
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IPOlRecipient = interface(IRecipient)
      ['{A11C6E30-51B5-11d3-8F39-0000F87A4335}']
       function Resolve(fShowDialog:VARIANT_BOOL; pfResolved:PVARIANT_BOOL):HRESULT; stdcall;
     end;

// Recipient types
     OlRecipientType = (olRecipientTypeUnknown  := 0,
                        olRecipientTypeRequired := 1,
                        olRecipientTypeOptional := 2,
                        olRecipientTypeResource := 3);

// Recipient status
     OlRecipientStatus = (olRecipientStatusUnknown    := 0,
                          olRecipientStatusAccepted   := 1,
                          olRecipientStatusDeclined   := 2,
                          olRecipientStatusTentative  := 3,
                          olRecipientStatusNoResponse := 4);

     IRecipient2 = interface(IPOlRecipient)
      ['{13A73D92-C9CE-4904-A348-CBA3246F2F8C}']
       function put_Type(rtType:OlRecipientType):HRESULT; stdcall;
       function put_Status(rsStatus:OlRecipientStatus):HRESULT; stdcall;
       function get_Type(out prtType:OlRecipientType):HRESULT; stdcall;
       function get_Status(out prsStatus:OlRecipientStatus):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IRecipients
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IRecipients = interface(IDispatch)
      ['{76065AE0-2347-11d2-8F18-0000F87A4335}']
       function Add(pwszName:BSTR; out pRecip:IRecipient):HRESULT; stdcall;
       function Item(iIndex:longint; out pRecip:IRecipient):HRESULT; stdcall;
       function Remove(iIndex:longint):HRESULT; stdcall;
       function get_Count(pnCount:PLongint):HRESULT; stdcall;
       function get__NewEnum(out ppEnumerator:IUnknown):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IPOutlookItemCollection
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IPOutlookItemCollection = interface(IDispatch)
      ['{F06748C0-21A5-11d2-8F18-0000F87A4335}']
       function Add(out ppolItem:IDispatch):HRESULT; stdcall;
       function get_Count(pnCount:PLongint):HRESULT; stdcall;
       function Find(pwszRestriction:BSTR; out ppItem:IDispatch):HRESULT; stdcall;
       function FindNext(out ppItem:IDispatch):HRESULT; stdcall;
       function Item(iIndex:longint; out ppolItem:IDispatch):HRESULT; stdcall;
       function Remove(iIndex:longint):HRESULT; stdcall;
       function Restrict(pwszRestriction:BSTR; out ppolItems:IPOutlookItemCollection):HRESULT; stdcall;
       function Sort(pwszProperty:BSTR; fDescending:VARIANT_BOOL):HRESULT; stdcall;
       function get_IncludeRecurrences(pfIncludeRecurrences:PVARIANT_BOOL):HRESULT; stdcall;
       function put_IncludeRecurrences(pfIncludeRecurrences:VARIANT_BOOL):HRESULT; stdcall;
       function get__NewEnum(out ppEnumerator:IUnknown):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IPOlItems
 * at Mon Aug 16 12:34:34 1999
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IPOlItems = interface(IPOutlookItemCollection)
      ['{6E3DBE90-5411-11d3-8F39-0000F87A4335}']
       function SetColumns(Columns:BSTR):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IContact
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IContact = interface(IDispatch)
      ['{7F804E40-2010-11d2-8F18-0000F87A4335}']
       function get_Birthday(pst:PDATE):HRESULT; stdcall;
       function get_Anniversary(pst:PDATE):HRESULT; stdcall;
       function get_BusinessFaxNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_CompanyName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Department(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Email1Address(out ppwsz:BSTR):HRESULT; stdcall;
       function get_MobileTelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_OfficeLocation(out ppwsz:BSTR):HRESULT; stdcall;
       function get_PagerNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_BusinessTelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_JobTitle(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeTelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Email2Address(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Spouse(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Email3Address(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Home2TelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeFaxNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_CarTelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_AssistantName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_AssistantTelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Children(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Categories(out ppwsz:BSTR):HRESULT; stdcall;
       function get_WebPage(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Business2TelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Title(out ppwsz:BSTR):HRESULT; stdcall;
       function get_FirstName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_MiddleName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_LastName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Suffix(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeAddressStreet(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeAddressCity(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeAddressState(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeAddressPostalCode(out ppwsz:BSTR):HRESULT; stdcall;
       function get_HomeAddressCountry(out ppwsz:BSTR):HRESULT; stdcall;
       function get_OtherAddressStreet(out ppwsz:BSTR):HRESULT; stdcall;
       function get_OtherAddressCity(out ppwsz:BSTR):HRESULT; stdcall;
       function get_OtherAddressState(out ppwsz:BSTR):HRESULT; stdcall;
       function get_OtherAddressPostalCode(out ppwsz:BSTR):HRESULT; stdcall;
       function get_OtherAddressCountry(out ppwsz:BSTR):HRESULT; stdcall;
       function get_BusinessAddressStreet(out ppwsz:BSTR):HRESULT; stdcall;
       function get_BusinessAddressCity(out ppwsz:BSTR):HRESULT; stdcall;
       function get_BusinessAddressState(out ppwsz:BSTR):HRESULT; stdcall;
       function get_BusinessAddressPostalCode(out ppwsz:BSTR):HRESULT; stdcall;
       function get_BusinessAddressCountry(out ppwsz:BSTR):HRESULT; stdcall;
       function get_RadioTelephoneNumber(out ppwsz:BSTR):HRESULT; stdcall;
       function get_FileAs(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Body(out ppwsz:BSTR):HRESULT; stdcall;
       function get_YomiCompanyName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_YomiFirstName(out ppwsz:BSTR):HRESULT; stdcall;
       function get_YomiLastName(out ppwsz:BSTR):HRESULT; stdcall;

       function put_Birthday(st:TDateTime):HRESULT; stdcall;
       function put_Anniversary(st:TDateTime):HRESULT; stdcall;
       function put_BusinessFaxNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_CompanyName(pwsz:BSTR):HRESULT; stdcall;
       function put_Department(pwsz:BSTR):HRESULT; stdcall;
       function put_Email1Address(pwsz:BSTR):HRESULT; stdcall;
       function put_MobileTelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_OfficeLocation(pwsz:BSTR):HRESULT; stdcall;
       function put_PagerNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_BusinessTelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_JobTitle(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeTelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_Email2Address(pwsz:BSTR):HRESULT; stdcall;
       function put_Spouse(pwsz:BSTR):HRESULT; stdcall;
       function put_Email3Address(pwsz:BSTR):HRESULT; stdcall;
       function put_Home2TelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeFaxNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_CarTelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_AssistantName(pwsz:BSTR):HRESULT; stdcall;
       function put_AssistantTelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_Children(pwsz:BSTR):HRESULT; stdcall;
       function put_Categories(pwsz:BSTR):HRESULT; stdcall;
       function put_WebPage(pwsz:BSTR):HRESULT; stdcall;
       function put_Business2TelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_Title(pwsz:BSTR):HRESULT; stdcall;
       function put_FirstName(pwsz:BSTR):HRESULT; stdcall;
       function put_MiddleName(pwsz:BSTR):HRESULT; stdcall;
       function put_LastName(pwsz:BSTR):HRESULT; stdcall;
       function put_Suffix(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeAddressStreet(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeAddressCity(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeAddressState(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeAddressPostalCode(pwsz:BSTR):HRESULT; stdcall;
       function put_HomeAddressCountry(pwsz:BSTR):HRESULT; stdcall;
       function put_OtherAddressStreet(pwsz:BSTR):HRESULT; stdcall;
       function put_OtherAddressCity(pwsz:BSTR):HRESULT; stdcall;
       function put_OtherAddressState(pwsz:BSTR):HRESULT; stdcall;
       function put_OtherAddressPostalCode(pwsz:BSTR):HRESULT; stdcall;
       function put_OtherAddressCountry(pwsz:BSTR):HRESULT; stdcall;
       function put_BusinessAddressStreet(pwsz:BSTR):HRESULT; stdcall;
       function put_BusinessAddressCity(pwsz:BSTR):HRESULT; stdcall;
       function put_BusinessAddressState(pwsz:BSTR):HRESULT; stdcall;
       function put_BusinessAddressPostalCode(pwsz:BSTR):HRESULT; stdcall;
       function put_BusinessAddressCountry(pwsz:BSTR):HRESULT; stdcall;
       function put_RadioTelephoneNumber(pwsz:BSTR):HRESULT; stdcall;
       function put_FileAs(pwsz:BSTR):HRESULT; stdcall;
       function put_Body(pwsz:BSTR):HRESULT; stdcall;
       function put_YomiCompanyName(pwsz:BSTR):HRESULT; stdcall;
       function put_YomiFirstName(pwsz:BSTR):HRESULT; stdcall;
       function put_YomiLastName(pwsz:BSTR):HRESULT; stdcall;

       function Save:HRESULT; stdcall;
       function Delete:HRESULT; stdcall;
       function Copy(out ppolCopy:IContact):HRESULT; stdcall;
       function Display:HRESULT; stdcall;
       function get_Oid(poid:LPLONG):HRESULT; stdcall;
       function put_BodyInk(pcebl:PCEBLOB):HRESULT; stdcall;
       function get_BodyInk(pcebl:PCEBLOB):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: ITask
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     ITask = interface(IDispatch)
      ['{37C78CE0-202C-11d2-8F18-0000F87A4335}']
       function ClearRecurrencePattern:HRESULT; stdcall;
       function GetRecurrencePattern(out ppRecPattern:IRecurrencePattern):HRESULT; stdcall;
       function get_IsRecurring(pfIsRecurring:PVARIANT_BOOL):HRESULT; stdcall;
       function get_Subject(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Categories(out ppwsz:BSTR):HRESULT; stdcall;
       function get_StartDate(pst:PDATE):HRESULT; stdcall;
       function get_DueDate(pst:PDATE):HRESULT; stdcall;
       function get_DateCompleted(pst:PDATE):HRESULT; stdcall;
       function get_Importance(pdwPriority:LPLONG):HRESULT; stdcall;
       function get_Complete(pfCompleted:PVARIANT_BOOL):HRESULT; stdcall;
       function get_Sensitivity(plSensitivity:LPLONG):HRESULT; stdcall;
       function get_TeamTask(pfTeamTask:PVARIANT_BOOL):HRESULT; stdcall;
       function get_Body(out ppwsz:BSTR):HRESULT; stdcall;
       function get_ReminderSet(pfReminderSet:PVARIANT_BOOL):HRESULT; stdcall;
       function get_ReminderSoundFile(out ppwsz:BSTR):HRESULT; stdcall;
       function get_ReminderTime(pst:PDATE):HRESULT; stdcall;
       function get_ReminderOptions(pdwOptions:LPLONG):HRESULT; stdcall;
       function put_Subject(pwsz:BSTR):HRESULT; stdcall;
       function put_Categories(pwsz:BSTR):HRESULT; stdcall;
       function put_StartDate(st:TDateTime):HRESULT; stdcall;
       function put_DueDate(st:TDateTime):HRESULT; stdcall;
       function put_Importance(dwPriority:LONG):HRESULT; stdcall;
       function put_Complete(fCompleted:VARIANT_BOOL):HRESULT; stdcall;
       function put_Sensitivity(lSensitivity:LONG):HRESULT; stdcall;
       function put_TeamTask(fTeamTask:VARIANT_BOOL):HRESULT; stdcall;
       function put_Body(pwsz:BSTR):HRESULT; stdcall;
       function put_ReminderSet(fReminderSet:VARIANT_BOOL):HRESULT; stdcall;
       function put_ReminderSoundFile(pwsz:BSTR):HRESULT; stdcall;
       function put_ReminderTime(st:TDateTime):HRESULT; stdcall;
       function put_ReminderOptions(dwOptions:LONG):HRESULT; stdcall;

       function Save:HRESULT; stdcall;
       function Delete:HRESULT; stdcall;
       function SkipRecurrence:HRESULT; stdcall;
       function Copy(out ppolCopy:ITask):HRESULT; stdcall;
       function Display:HRESULT; stdcall;
       function get_Oid(poid:LPLONG):HRESULT; stdcall;
       function put_BodyInk(pcebl:PCEBLOB):HRESULT; stdcall;
       function get_BodyInk(pcebl:PCEBLOB):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IAppointment
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IAppointment = interface(IDispatch)
      ['{5B43F691-202C-11d2-8F18-0000F87A4335}']
       function ClearRecurrencePattern:HRESULT; stdcall;
       function GetRecurrencePattern(ppRecPattern:IRecurrencePattern):HRESULT; stdcall;
       function get_IsRecurring(pfIsRecurring:PVARIANT_BOOL):HRESULT; stdcall;
       function get_Subject(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Location(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Categories(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Start(pst:PDATE):HRESULT; stdcall;
       function get_Duration(pnLen:LPLONG):HRESULT; stdcall;
       function get_End(pst:PDATE):HRESULT; stdcall;
       function get_AllDayEvent(pfAllDay:PVARIANT_BOOL):HRESULT; stdcall;
       function get_BusyStatus(pnState:LPLONG):HRESULT; stdcall;
       function get_Sensitivity(plSensitivity:LPLONG):HRESULT; stdcall;
       function get_Body(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Recipients(pRecipients:IRecipients):HRESULT; stdcall;
       function get_MeetingStatus(pnStatus:LPLONG):HRESULT; stdcall;
       function get_ReminderSet(pfReminderSet:PVARIANT_BOOL):HRESULT; stdcall;
       function get_ReminderSoundFile(out ppwsz:BSTR):HRESULT; stdcall;
       function get_ReminderMinutesBeforeStart(plMinutes:LPLONG):HRESULT; stdcall;
       function get_ReminderOptions(pdwOptions:LPLONG):HRESULT; stdcall;

       function put_Subject(pwsz:BSTR):HRESULT; stdcall;
       function put_Location(pwsz:BSTR):HRESULT; stdcall;
       function put_Categories(pwsz:BSTR):HRESULT; stdcall;
       function put_Start(st:TDateTime):HRESULT; stdcall;
       function put_Duration(nLen:LONG):HRESULT; stdcall;
       function put_End(st:TDateTime):HRESULT; stdcall;
       function put_AllDayEvent(fAllDay:VARIANT_BOOL):HRESULT; stdcall;
       function put_BusyStatus(nState:LONG):HRESULT; stdcall;
       function put_Sensitivity(lSensitivity:LONG):HRESULT; stdcall;
       function put_Body(pwsz:BSTR):HRESULT; stdcall;
       function put_ReminderSet(fReminderSet:VARIANT_BOOL):HRESULT; stdcall;
       function put_ReminderSoundFile(pwsz:BSTR):HRESULT; stdcall;
       function put_ReminderMinutesBeforeStart(lMinutes:LONG):HRESULT; stdcall;
       function put_ReminderOptions(dwOptions:LONG):HRESULT; stdcall;

       function Save:HRESULT; stdcall;
       function Send:HRESULT; stdcall;
       function Delete:HRESULT; stdcall;
       function Cancel:HRESULT; stdcall;
       function Copy(out ppolCopy:IAppointment):HRESULT; stdcall;
       function Display:HRESULT; stdcall;

       function get_Oid(poid:LPLONG):HRESULT; stdcall;
       function put_BodyInk(pcebl:PCEBLOB):HRESULT; stdcall;
       function get_BodyInk(pcebl:PCEBLOB):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: ICity
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     ICity = interface(IDispatch)
      ['{C83C5E90-3D1B-11d2-8F1B-0000F87A4335}']
       function get_Longitude(pcLongitude:LPLONG):HRESULT; stdcall;
       function get_Latitude(pcLatitude:LPLONG):HRESULT; stdcall;
       function get_TimezoneIndex(pcTimezone:LPLONG):HRESULT; stdcall;
       function get_AirportCode(out ppwsz:BSTR):HRESULT; stdcall;
       function get_CountryPhoneCode(out ppwsz:BSTR):HRESULT; stdcall;
       function get_AreaCode(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Name(out ppwsz:BSTR):HRESULT; stdcall;
       function get_Country(out ppwsz:BSTR):HRESULT; stdcall;
       function get_InROM(pfInROM:PVARIANT_BOOL):HRESULT; stdcall;

       function put_Longitude(cLongitude:LONG):HRESULT; stdcall;
       function put_Latitude(cLatitude:LONG):HRESULT; stdcall;
       function put_TimezoneIndex(cTimezone:LONG):HRESULT; stdcall;
       function put_AirportCode(pwsz:BSTR):HRESULT; stdcall;
       function put_CountryPhoneCode(pwsz:BSTR):HRESULT; stdcall;
       function put_AreaCode(pwsz:BSTR):HRESULT; stdcall;
       function put_Name(pwsz:BSTR):HRESULT; stdcall;
       function put_Country(pwsz:BSTR):HRESULT; stdcall;

       function Save:HRESULT; stdcall;
       function Delete:HRESULT; stdcall;
       function Copy(out ppolCopy:ICity):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IFolder
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object]

     IFolder = interface(IDispatch)
      ['{05058F20-20BE-11d2-8F18-0000F87A4335}']
       function get_Items(out ppolItems:IPOutlookItemCollection):HRESULT; stdcall;
       function get_DefaultItemType(polItem:PLongint):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
       function AddItemToInfraredFolder(olItem:longint; polItem:IDispatch):HRESULT; stdcall;
       function SendToInfrared:HRESULT; stdcall;
       function ReceiveFromInfrared(out ppItems:IPOutlookItemCollection):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for interface: IPOutlookApp
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [dual][full][helpstring][uuid][object] 

     IPOutlookApp = interface(IDispatch)
      ['{05058F22-20BE-11d2-8F18-0000F87A4335}']
       function Logon(_hWnd:LONG):HRESULT; stdcall;
       function Logoff:HRESULT; stdcall;
       function get_Version(out ppwszVersion:BSTR):HRESULT; stdcall;
       function GetDefaultFolder(olFolder:longint; out ppIFolder:IFolder):HRESULT; stdcall;
       function CreateItem(olItem:longint; out ppPOutlookItem:IDispatch):HRESULT; stdcall;
       function GetItemFromOid(oid:LONG; out ppPOutlookItem:IDispatch):HRESULT; stdcall;
       function get_HomeCity(out ppHomeCity:ICity):HRESULT; stdcall;
       function put_HomeCity(pHomeCity:ICity):HRESULT; stdcall;
       function get_VisitingCity(out ppVisitingCity:ICity):HRESULT; stdcall;
       function put_VisitingCity(pVisitingCity:ICity):HRESULT; stdcall;
       function get_CurrentCityIndex(pnolCity:LPLONG):HRESULT; stdcall;
       function put_CurrentCityIndex(olCity:LONG):HRESULT; stdcall;
       function ReceiveFromInfrared:HRESULT; stdcall;
       function get_OutlookCompatible(pfCompat:PVARIANT_BOOL):HRESULT; stdcall;
       function GetTimeZoneFromIndex(cTimezone:longint; out ppTz:ITimeZone):HRESULT; stdcall;
       function GetTimeZoneInformationFromIndex(cTimezone:longint; ptzInfo:LPTIME_ZONE_INFORMATION):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
       function SysFreeString(_bstr:BSTR):HRESULT; stdcall;
       function VariantTimeToSystemTime(_date:TDateTime; pst:PSYSTEMTIME):HRESULT; stdcall;
       function SystemTimeToVariantTime(pst:PSYSTEMTIME; _pdate:PDATE):HRESULT; stdcall;
     end;


     IPOutlookApp2 = interface(IPOutlookApp)
      ['{AF7D0DC7-3D35-424b-AA60-27A38B8B629E}']
       function GetIDsFromNames(cPropNames:ULONG; rgszPropNames:pointer{^LPCWSTR}; ulFlags:ULONG; rgPropIDs:PCEPROPID):HRESULT; stdcall;
       function GetItemFromOidEx(oidPIM:ULONG; ulFlags:ULONG; out ppItem:IItem):HRESULT; stdcall;
     end;

     IItem = interface(IUnknown)
      ['{FB8998D0-38F0-4d12-AC56-4EC8FCE9F3D5}']
       function GetProps(rgPropID:PCEPROPID;
                         ulFlags:ULONG;
                         cProps:word;
                         prgVals:pointer{^PCEPROPVAL};
                         pcbBuffer:PULONG;
                         hHeap:HANDLE):HRESULT; stdcall;
       function SetProps(ulFlags:ULONG; cProps:word; rgVals:PCEPROPVAL):HRESULT; stdcall;
       function Save:HRESULT; stdcall;
       function Delete:HRESULT; stdcall;
       function Display(hwndParent:HWND):HRESULT; stdcall;
       function Edit(hwndParent:HWND):HRESULT; stdcall;
       function Copy(out ppolCopy:IItem):HRESULT; stdcall;
       function get_Oid(poid:LPLONG):HRESULT; stdcall;
       function OpenProperty(propID:CEPROPID; dwMode:DWORD; out ppStream:IStream):HRESULT; stdcall;
       function get_DefaultItemType(polItem:PLongint):HRESULT; stdcall;
       function get_Application(out polApp:IPOutlookApp):HRESULT; stdcall;
       function get_Parent(out ppIFolder:IFolder):HRESULT; stdcall;
       function AddCategory(pszwCategory:LPCWSTR):HRESULT; stdcall;
       function RemoveCategory(pszwCategory:LPCWSTR):HRESULT; stdcall;
     end;

     IPOlItems2 = interface(IPOlItems)
      ['{CB52F880-4CB0-4a23-AA09-82326C98929F}']
      // Get props for the item at a given index without instantiating
      // an object for the item
       function GetProps(iIndex:longint;
                         rgPropID:PCEPROPID;
                         ulFlags:ULONG;
                         cProps:word;
                         prgVals:pointer{^PCEPROPVAL};
                         pcbBuffer:PULONG;
                         hHeap:HANDLE):HRESULT; stdcall;
     end;

{****************************************
 * Generated header for library: PocketOutlook
 * at Mon Dec 07 11:02:04 1998
 * using MIDL 3.02.88
 ****************************************}
// [helpstring][version][uuid]

// Maximum/min dates for calendar entries
const
      CAL_MAXDATE     = TDateTime(401768.0);  // 12/31/2999
      CAL_MINDATE     = TDateTime(0.0);       // 12/30/1899
      DATE_NONE       = TDateTime(949998.0);  // 1/1/4501

// Custom error codes
const
      E_CLOCKRUNNING          = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 100;
      E_CITYINROM             = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 101;
      E_FIELDTOOLARGE         = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 102;
      E_INVALIDREMINDERTIME   = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 103;
      E_INVALIDDATES          = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 104;
      E_ALLDAYMEETING         = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 105;
      E_OVERLAPPINGEXCEPTION  = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 106;
      E_CANTCHANGEDATE        = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 107;
      E_EXCEPTIONSAMEDAY      = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 108;
      E_UNWANTEDITEM          = (SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16) or 109;
      S_AUTO_CLOSED           = (SEVERITY_SUCCESS shl 31) or (FACILITY_ITF shl 16) or 150;

type
     OlImportance = (olImportanceLow     := 0,
                     olImportanceNormal  := 1,
                     olImportanceHigh    := 2);

     OlDefaultFolders = (olFolderCalendar    := 9,
                         olFolderContacts    := 10,
                         olFolderTasks       := 13,
                         olFolderCities      := 101,
                         olFolderInfrared    := 102);

     OlItemType = (olAppointmentItem   := 1,
                   olContactItem       := 2,
                   olTaskItem          := 3,
                   olCityItem          := 102);

     OlReminderOptions = (olLED               := 1,
                          olVibrate           := 2,
                          olDialog            := 4,
                          olSound             := 8,
                          olRepeat            := 16);

     OlBusyStatus = (olFree              := 0,
                     olTentative         := 1,
                     olBusy              := 2,
                     olOutOfOffice       := 3);

      OlMeetingStatusFlags = (olMeetingFlag       := 1,
                              olReceivedFlag      := 2,
                              olCanceledFlag      := 4,
                              olForwardedFlag     := 8);

     // This type was declared in pimstore.h as a enumerated one. And its values
     // are olNonMeeting, olMeeting, olMeetingAccepted, olMeetingCanceled.
      OlMeetingStatus = longint;

const
      olNonMeeting        = OlMeetingStatus(0);
      olMeeting           = OlMeetingStatus(olMeetingFlag);
      olMeetingAccepted   = OlMeetingStatus(olMeetingFlag) or
                            OlMeetingStatus(olReceivedFlag);
      olMeetingCanceled   = OlMeetingStatus(olMeetingFlag) or
                            OlMeetingStatus(olReceivedFlag) or
                            OlMeetingStatus(olCanceledFlag);

type
     OlCurrentCity = (olHomeCity          := 0,
                      olVisitingCity      := 1);

     OlRecurrenceType = (olRecursOnce        := -1,
                         olRecursDaily       := 0,
                         olRecursWeekly      := 1,
                         olRecursMonthly     := 2,
                         olRecursMonthNth    := 3,
                         olRecursYearly      := 5,
                         olRecursYearNth     := 6);

     OlDaysOfWeek = (olSunday            := 1,
                     olMonday            := 2,
                     olTuesday           := 4,
                     olWednesday         := 8,
                     olThursday          := 16,
                     olFriday            := 32,
                     olSaturday          := 64);

     OlSensitivity = (olNormal            := 0,
                      olPersonal          := 1,
                      olPrivate           := 2,
                      olConfidential      := 3);



// Menu Extensions - ItemRef types
const
      ITI_ContactItemRef:TIID = '{18103FDF-CAEC-42EB-90E7-1C76D6E60D24}';
      ITI_PimItemRef:TIID = '{18103FDF-CAEC-42EB-90E7-1C76D6E60D24}'; // back-compat w/ old clients // ITI_PimItemRef = ITI_ContactItemRef;

// Additional Prototypes for ALL interfaces
{
unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long __RPC_FAR *, unsigned long            , BSTR __RPC_FAR * );
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * );
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * );
void                      __RPC_USER  BSTR_UserFree(     unsigned long __RPC_FAR *, BSTR __RPC_FAR * );
}

// Flags for the Contact Chooser
const
      CCF_DEFAULT                         = $0000;
      CCF_HIDENEW                         = $0001;
      CCF_CHOOSECONTACTONLY               = $0002;
      CCF_CHOOSEPROPERTYONLY              = $0004;
      CCF_RETURNCONTACTNAME               = $0008;
      CCF_RETURNPROPERTYVALUE             = $0010;
      CCF_FILTERREQUIREDPROPERTIES        = $0020;
      CCF_NOUIONSINGLEORNOMATCH           = $0040;
      CCF_NOUI                            = $0080;
      CCF_ENABLEGAL                       = $0100;
      CCF_ALLOWNEWCONTACTSELECTION        = $0200;

type
     tagCHOOSECONTACT = record
       cbSize:UINT;
       hwndOwner:HWND;
       dwFlags:DWORD;
       lpstrTitle:LPCWSTR;
       lpstrChoosePropertyText:LPCWSTR;
       lpstrRestrictContacts:LPCWSTR;
       lpstrIncrementalFilter:LPCWSTR;
       cRequiredProperties:UINT;
       rgpropidRequiredProperties:PCEPROPID;
       oidContactID:CEOID;
       bstrContactName:BSTR;
       propidSelected:CEPROPID;
       bstrPropertyValueSelected:BSTR;
     end;
     _CHOOSECONTACT = tagCHOOSECONTACT;
     LPCHOOSECONTACT = ^tagCHOOSECONTACT;

const
      PIMStoreDLL = 'pimstore.dll';

function ChooseContact(lpcc:LPCHOOSECONTACT):HRESULT; cdecl; external PIMStoreDLL name 'ChooseContact';


// Returns a Contact object of the first closest match
function FindMatchingContact(pPOOM:IPOutlookApp; pszFind:LPCWSTR; dwFlags:DWORD; out ppContact:IItem; ppropid:PCEPROPID):HRESULT; cdecl; external PIMStoreDLL name 'FindMatchingContact';

// Flags for FindMatchingContact
const
      FMCF_FINDPHONE      = $00000001;
      FMCF_FINDEMAIL      = $00000002;
      FMCF_FINDFILEAS     = $00000004;


// Returns the index of the item in the collection for the given oidPIM
function GetItemIndexFromOid(pItems:IPOutlookItemCollection; oidPIM:CEOID; pdwIndex:LPDWORD):HRESULT; cdecl; external PIMStoreDLL name 'GetItemIndexFromOid';

// end of Additional Prototypes 

// Macro function.
function PIM_PROP_TAG(ulPropType:ULONG; ulPropID:ULONG):ULONG;
// Was declared as:
// #define PIM_PROP_TAG(ulPropType,ulPropID)   ((((ULONG)(ulPropID))<<16)|((ULONG)(ulPropType)))

const
      CEVT_PIM_STREAM                  = 100;
      CEVT_PIM_AUTO_I4                 = 102;

// Flags for GetIDsFromNames
      PIM_CREATE          = $010000;     // Creates if doesn't exist

// GetIDsFromNames Flags that are used only if PIM_CREATE flag is set:
      PIM_INDEXED         = $200000;   // whether the named property should be indexed for faster Find functionality
      PIM_DONTREPLICATE   = $400000;   // whether the named property shouldn't be replicated over to the new object when Copy is called

const
// If GetIDsFromNames fails to find an index for a proptag, the following proptag is returned
      PIMPR_INVALID_ID                      = (CEVT_I2 shl 16) or $FFFF;

// Properties which fast find is not implemented:  (Managed: Microsoft.WindowsMobile.PocketOutlook.ContactProperty )
      PIMPR_ALL_PHONE                       = (CEVT_LPWSTR shl 16) or $1800; // AllPhone: All telephone numbers.  This property is used with the contact chooser dialog.
      PIMPR_ALL_EMAIL                       = (CEVT_LPWSTR shl 16) or $1801; // AllEmail: All email addresses.  This property is used with the contact chooser dialog.
      PIMPR_ALL_TEXT_MESSAGING              = (CEVT_LPWSTR shl 16) or $1802; // AllTextMessaging: All text messaging addresses.  This property is used with the contact chooser dialog.
      PIMPR_ALL_INSTANT_MESSAGING           = (CEVT_LPWSTR shl 16) or $1803; // AllInstantMessaging: All instant messaging addresses.  This property is used with the contact chooser dialog.
      PIMPR_ALL_COMMUNICATION_METHODS       = (CEVT_LPWSTR shl 16) or $1804; // AllCommunicationMethods: All communication methods.  This property is used with the contact chooser dialog.
      PIMPR_ALL_VOICE                       = (CEVT_LPWSTR shl 16) or $1805; // AllVoice: All voice numbers.  This property is used with the contact chooser dialog.
      PIMPR_ALL_PHONE_AND_SIM               = (CEVT_LPWSTR shl 16) or $1806; // AllPhoneAndSIM: All telephone and SIM numbers.  This property is used with the contact chooser dialog.

// Shared Props (Managed: Microsoft.WindowsMobile.PocketOutlook.ContactProperty, Microsoft.WindowsMobile.PocketOutlook.AppointmentProperty, Microsoft.WindowsMobile.PocketOutlook.TaskProperty )
      PIMPR_OID                             = (CEVT_PIM_AUTO_I4 shl 16) or $1000;
      PIMPR_FOLDERNOTIFICATIONS             = (CEVT_UI4 shl 16) or $1001;
      PIMPR_FOLDER_CATEGORIES               = (CEVT_LPWSTR shl 16) or $101A; // FolderCategories: A comma-separated list of all the categories that are used in this folder
      PIMPR_SOURCE_ID                       = (CEVT_UI4 shl 16) or $001B; // SourceId: An integer indicating the source, or owning application, of the PIM item.  Zero is the default.
      PIMPR_RECENT                          = (CEVT_BOOL shl 16) or $101C; // Recent: A value indicating whether this PIM item is in the "recently viewed" list
      PIMPR_CATEGORIES                      = (CEVT_LPWSTR shl 16) or $101D; // Categories: A comma-separated list of the categories that apply to this item
      PIMPR_BODY_BINARY                     = (CEVT_PIM_STREAM shl 16) or $001E; // BodyInk: The ink Notes of the PIM item
      PIMPR_BODY_TEXT                       = (CEVT_LPWSTR shl 16) or $101F; // Body: The text Notes of the PIM item
      PIMPR_DO_NOT_SYNC                     = (CEVT_UI4 shl 16) or $1010; // DoNotSynchronize: Do not sync the item to Exchange/Outlook. This property can only be set during item creation.

// Calendar/Tasks Shared (Managed: Microsoft.WindowsMobile.PocketOutlook.AppointmentProperty , Microsoft.WindowsMobile.PocketOutlook.TaskProperty )
      PIMPR_SUBJECT                         = (CEVT_LPWSTR shl 16) or $0020; // Subject: The subject
      PIMPR_SENSITIVITY                     = (CEVT_UI4   shl 16) or $0021; // Sensitivity: The sensitivity (Normal = 0, Personal = 1, Private = 2,  Confidential = 3)
      PIMPR_IS_RECURRING                    = (CEVT_BOOL  shl 16) or $0022; // IsRecurring: A value indicating whether the appointment or task is recurring.

      PIMPR_REMINDER_SET                    = (CEVT_BOOL  shl 16) or $0028; // ReminderSet: A value indicating whether the user wants to be reminded of an appointment or task.
      PIMPR_REMINDER_SOUND_FILE             = (CEVT_LPWSTR shl 16) or $0029; // ReminderSoundFile: The path and file name of the sound file to play when the reminder occurs.
      PIMPR_REMINDER_OPTIONS                = (CEVT_UI4   shl 16) or $002A; // ReminderOptions: What actions to take when the reminder occurs. (LED=1, Vibrate=2, Dialog=4, Sound=8, Repeat=16)

// Recurring props
      PIMPR_RECURRING_TYPE                  = (CEVT_UI4 shl 16) or $1030; // RecurringType: The type (frequency) of recurrence:  NoRecurrence = -1, Daily = 0, Weekly = 1, Monthly = 2, MonthByNumber = 3, Yearly = 5, Every Nth Year = 6
      PIMPR_RECURRING_PATTERNSTARTDATE      = (CEVT_FILETIME shl 16) or $1031; // RecurringPatternStartDate: The starting date of the recurrence pattern.
      PIMPR_RECURRING_PATTERNENDDATE        = (CEVT_FILETIME shl 16) or $1032; // RecurringPatternEndDate: The ending date of the recurrence pattern.
      PIMPR_RECURRING_STARTTIME             = (CEVT_FILETIME shl 16) or $1033; // RecurringStartTime: The starting time of the recurrence pattern.
      PIMPR_RECURRING_ENDTIME               = (CEVT_FILETIME shl 16) or $1034; // RecurringEndTime: The ending date of the recurrence pattern.
      PIMPR_RECURRING_NOEND                 = (CEVT_BOOL shl 16) or $1035; // RecurringNoEnd: A value indicating whether the recurrence pattern has an end.
      PIMPR_RECURRING_OCCURRENCES           = (CEVT_UI4 shl 16) or $1036; // RecurringOccurrences: The number of occurences
      PIMPR_RECURRING_INTERVAL              = (CEVT_UI4 shl 16) or $1037; // RecurringInterval: The length of time between occurrences.
      PIMPR_RECURRING_DAYOFWEEKMASK         = (CEVT_UI4 shl 16) or $1038; // RecurringDayOfWeekMask: The days of the week of the recurrence. (Sunday=1, Monday=2, Tuesday=4, etc.)
      PIMPR_RECURRING_DAYOFMONTH            = (CEVT_UI4 shl 16) or $1039; // RecurringDayOfMonth: The day in a month on which an item occurs, from 1 to 31.
      PIMPR_RECURRING_INSTANCE              = (CEVT_UI4 shl 16) or $103A; // RecurringInstance: The week of the month in which an item occurs, from one to five.
      PIMPR_RECURRING_DURATION              = (CEVT_UI4 shl 16) or $103B; // RecurringDuration: The duration of the recurrence pattern.
      PIMPR_RECURRING_MONTHOFYEAR           = (CEVT_UI4 shl 16) or $103C; // RecurringMonthOfYear: The month of the year on which an item occurs, from one to twelve.

// Calendar props (Managed: Microsoft.WindowsMobile.PocketOutlook.AppointmentProperty )
      PIMPR_DURATION                        = (CEVT_UI4 shl 16) or $1040; // Duration: The length of an appointment in minutes.
      PIMPR_LOCATION                        = (CEVT_LPWSTR shl 16) or $0041; // Location: The location of an appointment.
      PIMPR_START                           = (CEVT_FILETIME shl 16) or $1042; // Start: The start time of an appointment.
      PIMPR_END                             = (CEVT_FILETIME shl 16) or $1043; // End: The end time of an appointment.
      PIMPR_ALL_DAY_EVENT                   = (CEVT_BOOL shl 16) or $0044; // AllDayEvent: A value indicating whether an appointment occurs as an all day event.
      PIMPR_BUSY_STATUS                     = (CEVT_UI4 shl 16) or $0045; // BusyStatus: A contact's availability in the time period spanned by an appointment (Free = 0, Tentative = 1, Busy = 2, OutOfOffice = 3)
      PIMPR_REMINDER_MINUTES_BEFORE_START   = (CEVT_UI4 shl 16) or $0046; // ReminderMinutesBeforeStart: The number of minutes a reminder alarm occurs before the start of an appointment.
      PIMPR_GLOBAL_OBJECT_ID                = (CEVT_BLOB shl 16) or $0047; // GlobalObjectId: A unique identifier for the appointment
      PIMPR_TIMEZONE                        = (CEVT_BLOB shl 16) or $0048; // RecurringTimeZone: The timezone of the appointment as a TIME_ZONE_INFORMATION structure

      PIMPR_MEETING_STATUS                  = (CEVT_UI4 shl 16) or $0050; // MeetingStatus: A value indicating whether an appointment is a meeting. (NotMeeting = 0, Meeting = 1, MeetingAccepted=3, MeetingCanceled=7)
      PIMPR_MEETING_ORGANIZER_NAME          = (CEVT_LPWSTR shl 16) or $0051; // MeetingOrganizerName: The name of the person who organized the meeting.
      PIMPR_MEETING_OWNER_CRITICAL_CHANGE   = (CEVT_FILETIME shl 16) or $0052; // MeetingOwnerCriticalChange: A timestamp indicating that the meeting owner wants to update the meeting request. Required when sending an updated meeting request to attendees. Call Appointment.Update on the appointment before setting either MeetingOwnerCriticalChange or AttendeesCriticalChange.
      PIMPR_ATTENDEES_CRITICAL_CHANGE       = (CEVT_FILETIME shl 16) or $0053; // AttendeesCriticalChange: A timestamp indicating that a meeting attendee wants to update the meeting request. Required when sending an updated meeting request back to the meeting owner.

// Tasks props (Managed: Microsoft.WindowsMobile.PocketOutlook.TaskProperty )
      PIMPR_IMPORTANCE                      = (CEVT_UI4 shl 16) or $0060; // Importance: The importance of the meeting. (Low=0, Normal=1, High=2)
      PIMPR_TEAM_TASK                       = (CEVT_BOOL shl 16) or $0061; // TeamTask: A value indicating whether this task is a team task.
      PIMPR_START_DATE                      = (CEVT_FILETIME shl 16) or $0062; // StartDate: The start date of the task.
      PIMPR_DUE_DATE                        = (CEVT_FILETIME shl 16) or $0063; // DueDate: The due date of the task.
      PIMPR_DATE_COMPLETED                  = (CEVT_FILETIME shl 16) or $0064; // DateCompleted: The date the task was completed.
      PIMPR_COMPLETE                        = (CEVT_BOOL shl 16) or $1065; // Complete: A value indicating whether the task has been completed.
      PIMPR_REMINDER_TIME                   = (CEVT_FILETIME shl 16) or $1066; // ReminderTime: When a reminder for the task will occur.
      PIMPR_RECURRING_REGENERATING          = (CEVT_BOOL shl 16) or $1067; // RecurringRegenerating: A value indicating whether the task recurs after it has been marked as completed.

// Contacts props (Managed: Microsoft.WindowsMobile.PocketOutlook.ContactProperty )
      PIMPR_FILEAS                          = (CEVT_LPWSTR shl 16) or $0080; // FileAs: The filing string for the contact.
      PIMPR_TITLE                           = (CEVT_LPWSTR shl 16) or $0081; // Title: The contact's title.
      PIMPR_FIRST_NAME                      = (CEVT_LPWSTR shl 16) or $0082; // FirstName: The contact's first name.
      PIMPR_MIDDLE_NAME                     = (CEVT_LPWSTR shl 16) or $0083; // MiddleName: The contact's middle name.
      PIMPR_LAST_NAME                       = (CEVT_LPWSTR shl 16) or $0084; // LastName: The contact's last name.
      PIMPR_SUFFIX                          = (CEVT_LPWSTR shl 16) or $0085; // Suffix: The contact's name suffix.
      PIMPR_NICKNAME                        = (CEVT_LPWSTR shl 16) or $0086; // Nickname: The contact's nickname.
      PIMPR_YOMI_FIRSTNAME                  = (CEVT_LPWSTR shl 16) or $0087; // YomiFirstName: The contact's first name rendered in the Japanese Yomigana phonetic system.
      PIMPR_YOMI_LASTNAME                   = (CEVT_LPWSTR shl 16) or $0088; // YomiLastName: The contact's last name rendered in the Japanese Yomigana phonetic system.
      PIMPR_YOMI_COMPANY                    = (CEVT_LPWSTR shl 16) or $0089; // YomiCompanyName: The contact's company name rendered in the Japanese Yomigana phonetic system.
      PIMPR_COMPANY_NAME                    = (CEVT_LPWSTR shl 16) or $008A; // CompanyName: The contact's company name.
      PIMPR_DEPARTMENT                      = (CEVT_LPWSTR shl 16) or $008B; // Department: The contact's department name.
      PIMPR_JOB_TITLE                       = (CEVT_LPWSTR shl 16) or $008C; // JobTitle: The contact's job title.
      PIMPR_MANAGER                         = (CEVT_LPWSTR shl 16) or $008D; // Manager: The contact's manager's name.
      PIMPR_OFFICE_LOCATION                 = (CEVT_LPWSTR shl 16) or $008E; // OfficeLocation: The contact's office location.
      PIMPR_ASSISTANT_NAME                  = (CEVT_LPWSTR shl 16) or $008F; // AssistantName: The name of the contact's assistant.
      PIMPR_EMAIL1_ADDRESS                  = (CEVT_LPWSTR shl 16) or $0090; // Email1Address: The contact's e-mail address.
      PIMPR_EMAIL2_ADDRESS                  = (CEVT_LPWSTR shl 16) or $0091; // Email2Address: The contact's second e-mail address.
      PIMPR_EMAIL3_ADDRESS                  = (CEVT_LPWSTR shl 16) or $0092; // Email3Address: The contact's third e-mail address.
      PIMPR_IM1_ADDRESS                     = (CEVT_LPWSTR shl 16) or $0093; // IM1Address: The contact's Instant Messaging address.
      PIMPR_IM2_ADDRESS                     = (CEVT_LPWSTR shl 16) or $0094; // IM2Address: The contact's second Instant Messaging address.
      PIMPR_IM3_ADDRESS                     = (CEVT_LPWSTR shl 16) or $0095; // IM3Address: The contact's third Instant Messaging address.
      PIMPR_MOBILE_TELEPHONE_NUMBER         = (CEVT_LPWSTR shl 16) or $0096; // MobileTelephoneNumber: The contact's mobile telephone number.
      PIMPR_BUSINESS_TELEPHONE_NUMBER       = (CEVT_LPWSTR shl 16) or $0097; // BusinessTelephoneNumber: The contact's business telephone number.
      PIMPR_BUSINESS2_TELEPHONE_NUMBER      = (CEVT_LPWSTR shl 16) or $0098; // Business2TelephoneNumber: The contact's second business telephone number.
      PIMPR_HOME_TELEPHONE_NUMBER           = (CEVT_LPWSTR shl 16) or $0099; // HomeTelephoneNumber: The contact's home telephone number.
      PIMPR_HOME2_TELEPHONE_NUMBER          = (CEVT_LPWSTR shl 16) or $009A; // Home2TelephoneNumber: The contact's second home telephone number.
      PIMPR_BUSINESS_FAX_NUMBER             = (CEVT_LPWSTR shl 16) or $009B; // BusinessFaxNumber: The contact's business fax number.
      PIMPR_HOME_FAX_NUMBER                 = (CEVT_LPWSTR shl 16) or $009C; // HomeFaxNumber: The contact's home fax number.
      PIMPR_PAGER_NUMBER                    = (CEVT_LPWSTR shl 16) or $009D; // PagerNumber: The contact's pager number.
      PIMPR_CAR_TELEPHONE_NUMBER            = (CEVT_LPWSTR shl 16) or $009E; // CarTelephoneNumber: The contact's car telephone number.
      PIMPR_RADIO_TELEPHONE_NUMBER          = (CEVT_LPWSTR shl 16) or $009F; // RadioTelephoneNumber: The contact's radio telephone number.
      PIMPR_COMPANY_TELEPHONE_NUMBER        = (CEVT_LPWSTR shl 16) or $00A0; // CompanyTelephoneNumber: The contact's company telephone number.
      PIMPR_ASSISTANT_TELEPHONE_NUMBER      = (CEVT_LPWSTR shl 16) or $00A1; // AssistantTelephoneNumber: The contact's assistant's telephone number.
      PIMPR_SMS                             = (CEVT_LPWSTR shl 16) or $10A2; // Sms: The contact's SMS address.
      PIMPR_MMS                             = (CEVT_LPWSTR shl 16) or $10A3; // Mms: The contact's MMS address.
      PIMPR_DISPLAY_NAME                    = (CEVT_LPWSTR shl 16) or $10A4; // DisplayName: The contact's name as it should appear in the user interface
      PIMPR_SPOUSE                          = (CEVT_LPWSTR shl 16) or $00A5; // Spouse: The contact's spouse's name.
      PIMPR_CHILDREN                        = (CEVT_LPWSTR shl 16) or $00A6; // Children: The names of the contact's children.
      PIMPR_WEB_PAGE                        = (CEVT_LPWSTR shl 16) or $00A7; // WebPage: The address of the contact's web page.
      PIMPR_RINGTONE                        = (CEVT_LPWSTR shl 16) or $00A8; // RingTone: The custom ring tone for the contact.
      PIMPR_CUSTOMERID                      = (CEVT_LPWSTR shl 16) or $00A9; // CustomerId: The contact's customer ID.
      PIMPR_GOVERNMENTID                    = (CEVT_LPWSTR shl 16) or $00AA; // GovernmentId: The contact's government ID.
      PIMPR_ACCOUNT_NAME                    = (CEVT_LPWSTR shl 16) or $00AB; // AccountName: The contact's account name.
      PIMPR_BUSINESS_ADDRESS                = (CEVT_LPWSTR shl 16) or $10C0; // BusinessAddress: The contact's business address.
      PIMPR_BUSINESS_ADDRESS_STREET         = (CEVT_LPWSTR shl 16) or $00C1; // BusinessAddressStreet: The street name in the contact's business address.
      PIMPR_BUSINESS_ADDRESS_CITY           = (CEVT_LPWSTR shl 16) or $00C2; // BusinessAddressCity: The city name in the contact's business address.
      PIMPR_BUSINESS_ADDRESS_STATE          = (CEVT_LPWSTR shl 16) or $00C3; // BusinessAddressState: The state name in the contact's business address.
      PIMPR_BUSINESS_ADDRESS_POSTAL_CODE    = (CEVT_LPWSTR shl 16) or $00C4; // BusinessAddressPostalCode: The postal code in the contact's business address.
      PIMPR_BUSINESS_ADDRESS_COUNTRY        = (CEVT_LPWSTR shl 16) or $00C5; // BusinessAddressCountry: The country name in the contact's business address.
      PIMPR_HOME_ADDRESS                    = (CEVT_LPWSTR shl 16) or $10D0; // HomeAddress: The contact's home address.
      PIMPR_HOME_ADDRESS_STREET             = (CEVT_LPWSTR shl 16) or $00D1; // HomeAddressStreet: The street name in the contact's home address.
      PIMPR_HOME_ADDRESS_CITY               = (CEVT_LPWSTR shl 16) or $00D2; // HomeAddressCity: The city name in the contact's home address.
      PIMPR_HOME_ADDRESS_STATE              = (CEVT_LPWSTR shl 16) or $00D3; // HomeAddressState: The state name in the contact's home address.
      PIMPR_HOME_ADDRESS_POSTAL_CODE        = (CEVT_LPWSTR shl 16) or $00D4; // HomeAddressPostalCode: The postal code in the contact's home address
      PIMPR_HOME_ADDRESS_COUNTRY            = (CEVT_LPWSTR shl 16) or $00D5; // HomeAddressCountry: The country name in the contact's home address.
      PIMPR_OTHER_ADDRESS                   = (CEVT_LPWSTR shl 16) or $10E0; // OtherAddress: The contact's other address.
      PIMPR_OTHER_ADDRESS_STREET            = (CEVT_LPWSTR shl 16) or $00E1; // OtherAddressStreet: The street name in the contact's other address.
      PIMPR_OTHER_ADDRESS_CITY              = (CEVT_LPWSTR shl 16) or $00E2; // OtherAddressCity: The city name in the contact's other address.
      PIMPR_OTHER_ADDRESS_STATE             = (CEVT_LPWSTR shl 16) or $00E3; // OtherAddressState: The state name in the contact's other address.
      PIMPR_OTHER_ADDRESS_POSTAL_CODE       = (CEVT_LPWSTR shl 16) or $00E4; // OtherAddressPostalCode: The postal code in the contact's other address
      PIMPR_OTHER_ADDRESS_COUNTRY           = (CEVT_LPWSTR shl 16) or $00E5; // OtherAddressCountry: The country name in the contact's other address.
      PIMPR_BIRTHDAY                        = (CEVT_FILETIME shl 16) or $00F0; // Birthday: The contact's birthdate.
      PIMPR_ANNIVERSARY                     = (CEVT_FILETIME shl 16) or $00F1; // Anniversary: The contact's wedding anniversary date.
      PIMPR_SMARTPROP                       = (CEVT_UI4 shl 16) or $00F8; // SmartProperty: The property id of the property that is used as the primary communication method for the contact.
      PIMPR_PICTURE                         = (CEVT_PIM_STREAM shl 16) or $00FF; // Picture: The contact's picture.
      PIMPR_YOMI_FILEAS                     = (CEVT_LPWSTR shl 16) or $0101; // YomiFileAs: The contact's filing string rendered in the Japanese Yomigana phonetic system.
      PIMPR_CONTACT_TYPE                    = (CEVT_UI4 shl 16) or $0102; // ContactType: The type of the contact (Device or SIM contact).
      PIMPR_SIM_PHONE                       = (CEVT_LPWSTR shl 16) or $0003; // SIMPhone: The contact's telephone number stored on the telephone's SIM card.

// Alowable values for PIMPR_CONTACT_TYPE
type
     _PIMPR_CONTACTTYPE = (PIMPR_CONTACTTYPE_DEVICE := 0,
                           PIMPR_CONTACTTYPE_SIM := 1);
     PIMPR_CONTACTTYPE = _PIMPR_CONTACTTYPE;

// Registry keys, paths and value names for PIM Source Providers
const
      PIMSRC_REGHKEY = HKEY_LOCAL_MACHINE;

      PIMSRC_REGPATH_COLORS           = 'Colors';
      PIMSRC_REGPATH_ROOT             = 'System\PIMSources';

      PIMSRC_REGVALUE_CLSID           = 'CLSID';
      PIMSRC_REGVALUE_CUSTOMIZATIONS  = 'Customizations';
      PIMSRC_REGVALUE_TYPE            = 'Type';
      PIMSRC_REGVALUE_DISPLAYNAME     = 'DisplayName';
      PIMSRC_REGVALUE_ICON            = 'Icon';


// Flags used to describe the types of PIM items that a source owns
const
      PIMSRC_TYPE_CONTACTS        = $00000001;
      PIMSRC_TYPE_APPOINTMENTS    = $00000002;
      PIMSRC_TYPE_VALIDMASK       = PIMSRC_TYPE_CONTACTS or PIMSRC_TYPE_APPOINTMENTS;

// Flags used to describe the types of customizations that a source supports
const
      PIMSRC_CUSTOM_CONTACTS_SUMMARY_CARD     = $00000001;
      PIMSRC_CUSTOM_CONTACTS_PAINT_LIST_ICON  = $00000004;
      PIMSRC_CUSTOM_CONTACTS_NEW              = $00000008;
      PIMSRC_CUSTOM_APPOINTMENTS_COLORS       = $00000800;
      PIMSRC_CUSTOM_VALIDMASK                 = PIMSRC_CUSTOM_CONTACTS_SUMMARY_CARD or
                                                PIMSRC_CUSTOM_CONTACTS_PAINT_LIST_ICON or
                                                PIMSRC_CUSTOM_CONTACTS_NEW or
                                                PIMSRC_CUSTOM_APPOINTMENTS_COLORS;

// Colors that a source can customize
type
     _PIMSRC_COLOR = (PIMSRC_COLOR_APPOINTMENTS_BACKGROUND := 1);
     PIMSRC_COLOR = _PIMSRC_COLOR;


// Flags to indicate the drawing state for an item
const
      PIMSRC_SCDS_SELECTED    = $00000001;
      PIMSRC_SCDS_FOCUS       = $00000010;

// Read-only struct provided to Paint()
type
     _SRCCUSTOMDRAW = record
       _hdc:HDC;                // Device context for painting
       rc:RECT;                // Bounding rect
       grfItemState:DWORD;     // State of the item being drawn (selected, focused...)
     end;
     SRCCUSTOMDRAW = _SRCCUSTOMDRAW;
     PSRCCUSTOMDRAW = ^_SRCCUSTOMDRAW;

////////////////////////////////////////////////////////////////////////////
// Interface IPimSrcContactSummaryCard
//
// Purpose:
//      Interface that a source can expose to customize the summary card for
//      a POOM contact.
//
type
     IPimSrcContactSummaryCard = interface(IUnknown)
      ['{95932f0a-e03a-412c-85ee-045365277e60}']
      ////////////////////////////////////////////////////////////////////////////
      // Display
      //
      // Purpose:
      //      Source-specific version of IItem::Display() for contacts.
      //
      // Parameters:
      //      IN      hwndParent      Parent HWND to use for UI
      //      IN      pitem           IItem instance to display
      //
      // Returns values:
      //      S_OK                    Success
      //      E_INVALIDARG            pitem was invalid
      //
       function Display(hwndParent:HWND; pitem:IItem):HRESULT; stdcall;
     end;

////////////////////////////////////////////////////////////////////////////
// Interface IPimSrcContactNew
//
// Purpose:
//      Interface that a source can expose to perform custom actions when the
//      user chooses to creates a new contact using this source provider.  The
//      source provider is responsible for displaying UI and ultimately saving
//      the new contact
//
     IPimSrcContactNew = interface(IUnknown)
      ['{FD76819E-4BDC-4e98-880C-15DED1FDA30D}']
      ////////////////////////////////////////////////////////////////////////////
      // Create
      //
      // Purpose:
      //      Allow a source provider to provide their own UI and behavior when
      //      creating a new contact
      //
      // Parameters:
      //      IN      hwndParent      Parent HWND to use for UI
      //      INOUT   pItem           In: If pItem is non-null then it contains
      //                              data to be saved to the new contact.
      //                              Out: If pItem is non-null the oid of the
      //                              item is considered an out parameter.
      //
      // Returns values:
      //      S_OK                    Success
      //      S_FALSE                 The provider does not require special behavior.
      //                              Display the default contact edit card and save the
      //                              new contact with our source provider ID and the don't
      //                              sync flag set.
      //      S_AUTO_CLOSED           Success, but the provider requests that the resulting
      //                              summary card not be displayed.  Providers should
      //                              return this if the creation dialog is being exited for
      //                              a reason other than the user dismissing it.  In these
      //                              situations it may feel unexpected for the summary card
      //                              to appear.
      //      E_INVALIDARG            One of the arguments was invalid
      //      E_ABORT                 The user cancelled item creation
      //
      // Remarks:
      //      When Create is called with pItem non-null this indicates that
      //      there is data that should be saved to the new created contact and
      //      this data is supplied in pItem.  In this case it is important to
      //      ensure that the oid of the newly created item is populated in
      //      pItem upon return so that the caller can link that oid to the
      //      data that was being saved.  A good example of this is saving
      //      call history information to a contact - the oid returned in
      //      pItem will be used to link the call history item to the new
      //      contact.  If the oid is not supplied then the call history item
      //      can not be linked so the display name will not be updated for
      //      that particular call history item in dialer or call history UI.
      //      Future calls to or from the contact would still be be matched in
      //      dialer or call history UI.
      //
       function Create(hwndParent:HWND; pItem:IItem):HRESULT; stdcall;
     end;

////////////////////////////////////////////////////////////////////////////
// Interface IPimSrcContactListIcon
//
// Purpose:
//      Interface that a source can expose to customize the painting of a
//      contact's icon in the contacts list view.  The source should draw
//      using a transparent background.
//
     IPimSrcContactListIcon = interface(IUnknown)
      ['{95932f0a-e03a-412c-85ee-045365277e62}']
      ////////////////////////////////////////////////////////////////////////////
      // Paint
      //
      // Purpose:
      //      This method enables a source to draw its own icon in the contacts
      //      list view.  The source should draw using a transparent background.
      //
      // Parameters:
      //      IN      pscd            Painting information that the source can use
      //      IN      oid             The OID for the current contact or zero if
      //                              the property could not be found.
      //
      // Returns values:
      //      S_OK                    The source drew the icon for this item
      //      S_FALSE                 The source did not draw the icon for this item.
      //                              The contacts app should do default painting
      //                              for this item
      //      E_INVALIDARG            One of the arguments was invalid
      //
       function Paint(pscd:PSRCCUSTOMDRAW; oid:CEOID):HRESULT; stdcall;
     end;

implementation

// Was declared as
// #define PIM_PROP_TAG(ulPropType,ulPropID)   ((((ULONG)(ulPropID))<<16)|((ULONG)(ulPropType)))
function PIM_PROP_TAG(ulPropType:ULONG; ulPropID:ULONG):ULONG; inline;
begin
  PIM_PROP_TAG:=(ulPropID shl 16) or ulPropType;
end;

end.