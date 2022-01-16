(*
Copyright 1996, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from The Open Group.
*)

const
  SECURITY_EXTENSION_NAME = 'SECURITY';
  SECURITY_MAJOR_VERSION  = 1;
  SECURITY_MINOR_VERSION  = 0;

  XSecurityNumberEvents             = 1;
  XSecurityNumberErrors             = 2;
  XSecurityBadAuthorization         = 0;
  XSecurityBadAuthorizationProtocol = 1;

{ trust levels }
  XSecurityClientTrusted   = 0;
  XSecurityClientUntrusted = 1;

{ authorization attribute masks }
  XSecurityTimeout    = 1 shl 0;
  XSecurityTrustLevel = 1 shl 1;
  XSecurityGroup      = 1 shl 2;
  XSecurityEventMask  = 1 shl 3;
  XSecurityAllAuthorizationAttributes =
    XSecurityTimeout or XSecurityTrustLevel or XSecurityGroup or XSecurityEventMask;

{ event masks }
  XSecurityAuthorizationRevokedMask = 1 shl 0;
  XSecurityAllEventMasks = XSecurityAuthorizationRevokedMask;

{ event offsets }
  XSecurityAuthorizationRevoked = 0;

  XSecurityAuthorizationName    = 'XC-QUERY-SECURITY-1';
  XSecurityAuthorizationNameLen = 19;
