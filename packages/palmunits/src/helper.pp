(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Helper.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 * Public header file for the Helper API.
 *
 *****************************************************************************)

unit helper;

interface

uses palmos;

//------------------------------------------------------------------------
// HelperNotifyEventType structure
// This structure is passed as the notifyDetailsP field of SysNotifyParamType with
//  a sysNotifyHelperEvent notificatiom.
//------------------------------------------------------------------------

const
  kHelperNotifyCurrentVersion = 1;

// Action codes for the sysNotifyHelperEvent broadcast (used in HelperNotifyEventType)
type
  HelperNotifyActionCodeType = UInt16;

const
  kHelperNotifyActionCodeEnumerate = HelperNotifyActionCodeType(1);
  kHelperNotifyActionCodeValidate  = HelperNotifyActionCodeType(2);
  kHelperNotifyActionCodeExecute   = HelperNotifyActionCodeType(3);

type
  HelperNotifyEventTypeTag = record
    // Version - this definition  is version 1
    // Later versions should include all the same fields as version 1
    // of HelperNotifyEventType plus additional fields
    version: UInt16;

    // what to do: enumerate, validate, or do it.
    actionCode: HelperNotifyActionCodeType;

    // data specific to the action code - valid if version = 1 for now
    case Integer of
      1: (enumerateP: Pointer {^HelperNotifyEnumerateListTypeTag});
      2: (validateP: Pointer {^HelperNotifyValidateTypeTag});
      3: (executeP: Pointer {^HelperNotifyExecuteTypeTag});
  end;
  HelperNotifyEventType = HelperNotifyEventTypeTag;

//------------------------------------------------------------------------
// HelperNotifyEnumerateListType structure
// An element in a linked list of helpers (used below)...
//------------------------------------------------------------------------
//  In an helperNotifyActionCodeEnumerate response, this should be allocated
// by the helper in the heap with MemPtrNew() and changed to be owned by the
// system.  It will be freed by the broadcaster. If a helper supports multiple
// service classes, it should create multiple entries.
//

const
  kHelperAppMaxNameSize       = 48; // max. helper name length, including zero-
                                    //  terminator, expressed in # of bytes

  kHelperAppMaxActionNameSize = 32; // max. helper action name length, including
                                    //  zero-terminator, expressed in # of
                                    //  bytes

type
  HelperNotifyEnumerateListTypeTag = record
    // Pointer to the next element in the list, or NULL to signal end of list.
    nextP: ^HelperNotifyEnumerateListType;

    // The name of the helper to show to the user (for example, when choosing
    //  a default/preferred helper for a given class of service; zero-terminated
    //  string).
    helperAppName: array [0..kHelperAppMaxNameSize-1] of Char;

    // Custom text that represents the action to be taken, such as "Dial",
    //  "Send fax", etc. (for display in an action pop-up, or button, for example);
    //  zero-terminated string.
    actionName: array [0..kHelperAppMaxActionNameSize-1] of Char;

    // The registered, unique ID of the helper (typically the helper app's
    //  creator ID).
    helperAppID: UInt32;

    // Services class ID supported by the helper; for example:
    //  helperServiceClassIDEMail (see HelperServiceClass.h)
    serviceClassID: UInt32;
  end;
  HelperNotifyEnumerateListType = HelperNotifyEnumerateListTypeTag;

//------------------------------------------------------------------------
// HelperNotifyValidateType structure
// Data type for the helperNotifyActionCodeValidate event.
//------------------------------------------------------------------------
// The matching helper(s) must set the "handled" field of SysNotifyParamType
//  to true.

  HelperNotifyValidateTypeTag = record
    // IN:   Service Class ID of requested service (required); for example:
    //    helperServiceClassIDEMail (see HelperServiceClass.h)
    serviceClassID: UInt32;

    // IN:   The unique ID of the Helper; may be 0 (zero) to indicate any
    //    available helper of the specified service class;
    helperAppID: UInt32;
  end;
  HelperNotifyValidateType = HelperNotifyValidateTypeTag;

//------------------------------------------------------------------------
// Data type for the helperNotifyActionCodeExecute event
//------------------------------------------------------------------------
//
// The target helper that processes the request must set the "handled" field
//  of SysNotifyParamType to true, even if a failure occurred during processing.
//  The 'err' field of the HelperNotifyExecuteType structure is used to indicate
//  success or failure.
//
// Helpers must check if the "handled" field in HelperNotifyEventType structure is
//  already set, and *not* process the "execute" request if so.
//
// The helper is responsible for informing user of any errors.
//

  HelperNotifyExecuteTypeTag = record
    // IN:   Service Class ID of requested service (required); for example:
    //    helperServiceClassIDEMail (see HelperServiceClass.h)
    serviceClassID: UInt32;

    // IN:   The unique ID of the Helper; may be 0 (zero) to indicate any
    //    available helper of the specified service class;
    helperAppID: UInt32;

    // IN:  Service-dependent data string, such as a phone number or email
    //    address (see HelperServiceClass.h for data that is appropriate for
    //    each of the "common" service classes); zero-terminated; will be
    //    duplicated by helper if necessary (ex. "1-650-123-4567",
    //    "john@host.com", etc.). Multiple fields may be separated by
    //    semicolons (';').
    dataP: PChar;

    // IN:  Description of data; zero-terminated; will be duplicated by helper
    //    if necessary (ex. "John Doe"); this field is optional -- may be NULL.
    displayedName: PChar;

    // IN:   Pointer to a service-specific extended details data structure or NULL.
    //    This optional field is used for supplying additional arguments to the
    //    helper.  The type of each of these data structures is well-defined
    //    and associated with a specific helper service class; will be duplicated
    //    by helper if necessary; may be ignored by helper.
    detailsP: Pointer;

    // OUT:  service-specific error code; must be initialize to 0 (zero) by host
    //    before broadcasting this request. 0 is used to signal success (although
    //    this may only be an indication that a request was scheduled and may
    //    not be performed until an app switch takes place).
    err: Err;
  end;
  HelperNotifyExecuteType = HelperNotifyExecuteTypeTag;

implementation

end.
