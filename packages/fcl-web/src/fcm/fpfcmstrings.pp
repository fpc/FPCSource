{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    FCM (Firebase Cloud Messaging) - Strings used in protocol and messages

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpfcmstrings;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

// Constants used in the protocol. Do not localize

const
  SFCMBaseURL = 'https://fcm.googleapis.com/v1';
  SFCMSendURL = SFCMBaseURL + '/projects/%s/messages:send';

  SFCMHTTPv1SendResourceProjectID = 'projectid';
  SFCMHTTPv1SendResource = '/projects/{projectid}/messages:send';

  SFCMGrantType = 'urn:ietf:params:oauth:grant-type:jwt-bearer';
  SContentTypeApplicationJSON = 'application/json';
  SContentTypeApplicationFormUrlEncoded = 'application/x-www-form-urlencoded';

  SFCMAccessTokenQuery = 'grant_type=%s&assertion=%s';
  SFCMScopes = 'https://www.googleapis.com/auth/firebase.messaging';
  SFCMAudience = 'https://oauth2.googleapis.com/token';

// Constants uses in JSON reading/writing
Const
  keyClientID = 'client_id';
  keyClientEmail = 'client_email';
  keyPrivateKey = 'private_key';
  keyProjectID = 'project_id';
  keyAuthURI = 'auth_uri';
  keyTokenURI = 'token_uri';


resourcestring
  // Messages, these can be localized.
  SErrInvalidJSONServiceData = 'Invalid service account data in JSON';
  SErrNoServiceDataAt = 'JSON contains no service account data at "%s"';
  SErrInvalidServiceData = 'Service data is invalid';
  SErrInvalidResponseStatus = 'Invalid HTTP response status: %d (%s).'#10'Extra info: %s';
  SErrInvalidResponseContentType = 'Invalid HTTP content type: %s';
  SErrInvalidJSONResponse = 'Invalid JSON data in HTTP response';
  SErrNoWebclientSet = 'No webclient class was set, inclucde fphttpwebclient or another webclient implementation';
  SErrReceivedExpiredToken = 'Received expired bearer token';
  SErrInvalidPrivateKey = 'Invalid private key in service account';
  SErrorMessage = 'Error %s: %s';

implementation

end.

