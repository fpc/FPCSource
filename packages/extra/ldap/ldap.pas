{
  Translation of the LDAP headers for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
}

unit ldap;

{$mode objfpc}

interface

uses
  ctypes,
  lber;

{$linklib ldap}

{$include ldap_featuresh.inc}
{$include ldap_schemah.inc}
{$include ldaph.inc}

implementation

function LDAP_OPT_ON: Pointer;
// #define LDAP_OPT_ON     ((void *) &ber_pvt_opt_on)
begin
  LDAP_OPT_ON := @ber_pvt_opt_on;
end;

function LDAP_RANGE(n, x, y: ber_int_t): Boolean;
// #define LDAP_RANGE(n,x,y)   (((x) <= (n)) && ((n) <= (y)))
begin
  LDAP_RANGE := (x <= n) and (n <= y);
end;

function LDAP_ATTR_ERROR(n: ber_int_t): Boolean;
// #define LDAP_ATTR_ERROR(n)  LDAP_RANGE((n),0x10,0x15) (* 16-21 *)
begin
  LDAP_ATTR_ERROR := LDAP_RANGE(n, $10, $15);
end;

function LDAP_NAME_ERROR(n: ber_int_t): Boolean;
// #define LDAP_NAME_ERROR(n)  LDAP_RANGE((n),0x20,0x24) (* 32-34,36 *)
begin
  LDAP_NAME_ERROR := LDAP_RANGE(n, $20, $24);
end;

function LDAP_SECURITY_ERROR(n: ber_int_t): Boolean;
// #define LDAP_SECURITY_ERROR(n)  LDAP_RANGE((n),0x2F,0x32) (* 47-50 *)
begin
  LDAP_SECURITY_ERROR := LDAP_RANGE(n, $2F, $32);
end;

function LDAP_SERVICE_ERROR(n: ber_int_t): Boolean;
// #define LDAP_SERVICE_ERROR(n)   LDAP_RANGE((n),0x33,0x36) (* 51-54 *)
begin
  LDAP_SERVICE_ERROR := LDAP_RANGE(n, $33, $36);
end;

function LDAP_UPDATE_ERROR(n: ber_int_t): Boolean;
// #define LDAP_UPDATE_ERROR(n)    LDAP_RANGE((n),0x40,0x47) (* 64-69,71 *)
begin
  LDAP_UPDATE_ERROR := LDAP_RANGE(n, $40, $47);
end;

function LDAP_API_ERROR(n: ber_int_t): Boolean;
// #define LDAP_API_ERROR(n)       LDAP_RANGE((n),0x51,0x61) (* 81-97 *)}
begin
  LDAP_API_ERROR := n < 0;
end;

function LDAP_API_RESULT(n: ber_int_t): Boolean;
// #define LDAP_API_RESULT(n)      (((n) == LDAP_SUCCESS) || LDAP_RANGE((n),0x51,0x61)) (* 0,81-97 *)
begin
  LDAP_API_RESULT := n <= 0;
end;

end.
