{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1995-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: HelperServiceClass.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 * Public header file for the service class ID's and extended details
 * data structures used with the "address book helper" API.
 *
 * For each Service Class ID, this header file also defines the
 * corresponding extended details data structures that may
 * optionally be passed as the 'pDetails' element in the
 * HelperNotifyExecuteType structure. We strongly recommend that
 * every extra details data structure include a version number,
 * which will alow the structure to be extended by adding new structure
 * elements later.
 *
 * The Service Class ID is a 32-bit value that uniquely identifies the
 * class of service performed by the Helper -- for example, making a
 * voice telephone call, sending an internet e-mail, sending an SMS
 * message, sending a fax, etc.  Palm defines some common Service Class
 * ID's and the corresponding extra details structures in this header file.
 *
 * 3rd party developers:
 * If none of these service class ID's match the service performed by your
 * helper, you must register a unique service class ID using the Creator ID
 * registry on Palm's web site (or use a creator ID that you already own).
 * A group of developers may elect to support the same service class ID for
 * interoperability.
 *
 *****************************************************************************)

unit helperserviceclass;

interface

uses palmos;

//------------------------------------------------------------------------
// Current Helper Service Class ID's
//------------------------------------------------------------------------

//
// Helpers of this Service Class make a voice telephone call.
//
// The telephone number to dial is passed in the 'pData' element of the main
// structure (HelperNotifyExecuteType)
//
// The 'pDetails' struct member is NULL for this service class.
//

const
  kHelperServiceClassIDVoiceDial = Rsc('voic');

//
// Helpers of this Service Class send an Internet mail message.
//
// "To" address(es) are passed in the 'pData' element of the main structure
// (HelperNotifyExecuteType)
//
// The 'pDetails' struct member may optionally point to
// HelperServiceEMailDetailsType for this service class.
//

  kHelperServiceClassIDEMail = Rsc('mail');

type
  _HelperServiceEMailDetailsType = record
    version: UInt16; // this is version 1

    cc: PChar;       // IN: carbon copy address string or NULL -- will
                     //  be duplicated by helper if necessary;
                     //  multiple addresses are separated by
                     //  semicolon (ex. "john@host.com; jane@host.com")
    subject: PChar;  // IN: subject string or NULL -- will be duplicated
                     //  by helper if necessary (ex. "helper API")
    message: PChar;  // IN: initial message body string or NULL -- will be
                     //  duplicated by helper if necessary (ex.
                     //  "Lets discuss the helper API tomorrow.")
  end;
  HelperServiceEMailDetailsType = _HelperServiceEMailDetailsType;

//
// Helpers of this Service Class send an SMS message.
//
// SMS mailbox number is passed in the 'pData' element of the main structure
// (HelperNotifyExecuteType).
//
// The 'pDetails' struct member may optionally point to
//  HelperServiceSMSDetailsType for this service class.
//

const
  kHelperServiceClassIDSMS = Rsc('sms_');

type
  _HelperServiceSMSDetailsType = record
    version: UInt16; // this is version 1

    message: PChar;  // IN: initial message body string or NULL -- will be
                     //  duplicated by helper if necessary (ex.
                     //  "Lets discuss the helper API tomorrow.")
  end;
  HelperServiceSMSDetailsType = _HelperServiceSMSDetailsType;

//
// Helpers of this Service Class send a fax.
//
// The fax number is passed in the 'pData' element of the main structure
// (HelperNotifyExecuteType).
//
// The 'pDetails' struct member is NULL for this service class.
//

const
  kHelperServiceClassIDFax = Rsc('fax_');

implementation

end.
