(* $OpenLDAP: pkg/ldap/include/ldap_schema.h,v 1.32.2.3 2005/01/20 17:00:59 kurt Exp $ *)
(* This work is part of OpenLDAP Software <http://www.openldap.org/>.
 *
 * Copyright 1998-2005 The OpenLDAP Foundation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted only as authorized by the OpenLDAP
 * Public License.
 *
 * A copy of this license is available in file LICENSE in the
 * top-level directory of the distribution or, alternatively, at
 * <http://www.OpenLDAP.org/license.html>.
 *)

(* ldap-schema.h - Header for basic schema handling functions that can be
 *      used by both clients and servers.
 * these routines should be renamed ldap_x_...
 *)


(* Codes for parsing errors *)

const
  LDAP_SCHERR_OUTOFMEM              = 1;
  LDAP_SCHERR_UNEXPTOKEN            = 2;
  LDAP_SCHERR_NOLEFTPAREN           = 3;
  LDAP_SCHERR_NORIGHTPAREN          = 4;
  LDAP_SCHERR_NODIGIT               = 5;
  LDAP_SCHERR_BADNAME               = 6;
  LDAP_SCHERR_BADDESC               = 7;
  LDAP_SCHERR_BADSUP                = 8;
  LDAP_SCHERR_DUPOPT                = 9;
  LDAP_SCHERR_EMPTY                 = 10;
  LDAP_SCHERR_MISSING               = 11;
  LDAP_SCHERR_OUT_OF_ORDER          = 12;

type
  pldap_schema_extension_item       = ^ldap_schema_extension_item;
  ldap_schema_extension_item        = record
    lsei_name   : PChar;
    lsei_values : PPChar;
  end;

  PPLDAPSchemaExtensionItem         = ^PLDAPSchemaExtensionItem;
  PLDAPSchemaExtensionItem          = ^LDAPSchemaExtensionItem;
  LDAPSchemaExtensionItem           = ldap_schema_extension_item;

  pldap_syntax                      = ^ldap_syntax;
  ldap_syntax                       = record
    syn_oid         : PChar;        (* REQUIRED *)
    syn_names       : PPChar;       (* OPTIONAL *)
    syn_desc        : PChar;        (* OPTIONAL *)
    syn_extensions  : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPSyntax                       = ^ldap_syntax;
  LDAPSyntax                        = ldap_syntax;

  pldap_matchingrule                = ^ldap_matchingrule;
  ldap_matchingrule                 = record
    mr_oid          : PChar;        (* REQUIRED *)
    mr_names        : PPChar;       (* OPTIONAL *)
    mr_desc         : PChar;        (* OPTIONAL *)
    mr_obsolete     : cint;         (* OPTIONAL *)
    mr_syntax_oid   : PChar;        (* REQUIRED *)
    mr_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPMatchingRule                 = ^LDAPMatchingRule;
  LDAPMatchingRule                  = ldap_matchingrule;

  pldap_matchingruleuse             = ^ldap_matchingruleuse;
  ldap_matchingruleuse              = record
    mru_oid         : PChar;        (* REQUIRED *)
    mru_names       : PPChar;       (* OPTIONAL *)
    mru_desc        : PChar;        (* OPTIONAL *)
    mru_obsolete    : cint;         (* OPTIONAL *)
    mru_applies_oids: PPChar;       (* REQUIRED *)
    mru_extensions  : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPMatchingRuleUse              = ^LDAPMatchingRuleUse;
  LDAPMatchingRuleUse               = ldap_matchingruleuse;

  pldap_attributetype               = ^ldap_attributetype;
  ldap_attributetype                = record
    at_oid          : PChar;        (* REQUIRED *)
    at_names        : PPChar;       (* OPTIONAL *)
    at_desc         : PChar;        (* OPTIONAL *)
    at_obsolete     : cint;         (* 0=no, 1=yes *)
    at_sup_oid      : PChar;        (* OPTIONAL *)
    at_equality_oid : PChar;        (* OPTIONAL *)
    at_ordering_oid : PChar;        (* OPTIONAL *)
    at_substr_oid   : PChar;        (* OPTIONAL *)
    at_syntax_oid   : PChar;        (* OPTIONAL *)
    at_syntax_len   : cint;         (* OPTIONAL *)
    at_single_value : cint;         (* 0=no, 1=yes *)
    at_collective   : cint;         (* 0=no, 1=yes *)
    at_no_user_mod  : cint;         (* 0=no, 1=yes *)
    at_usage        : cint;         (* 0=userApplications, 1=directoryOperation, 2=distributedOperation, 3=dSAOperation *)
    at_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPAttributeType                = ^LDAPAttributeType;
  LDAPAttributeType                 = ldap_attributetype;

  pldap_objectclass                 = ^ldap_objectclass;
  ldap_objectclass                  = record
    oc_oid          : PChar;        (* REQUIRED *)
    oc_names        : PPChar;       (* OPTIONAL *)
    oc_desc         : PChar;        (* OPTIONAL *)
    oc_obsolete     : cint;         (* 0=no, 1=yes *)
    oc_sup_oids     : PPChar;       (* OPTIONAL *)
    oc_kind         : cint;         (* 0=ABSTRACT, 1=STRUCTURAL, 2=AUXILIARY *)
    oc_at_oids_must : PPChar;       (* OPTIONAL *)
    oc_at_oids_may  : PPChar;       (* OPTIONAL *)
    oc_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPObjectClass                  = ^LDAPObjectClass;
  LDAPObjectClass                   = ldap_objectclass;

  pldap_contentrule                 = ^ldap_contentrule;
  ldap_contentrule                  = record
    cr_oid          : PChar;        (* REQUIRED *)
    cr_names        : PPChar;       (* OPTIONAL *)
    cr_desc         : PChar;        (* OPTIONAL *)
    cr_sup_oids     : PPChar;       (* OPTIONAL *)
    cr_obsolete     : cint;         (* 0=no, 1=yes *)
    cr_oc_oids_aux  : PPChar;       (* OPTIONAL *)
    cr_at_oids_must : PPChar;       (* OPTIONAL *)
    cr_at_oids_may  : PPChar;       (* OPTIONAL *)
    cr_at_oids_not  : PPChar;       (* OPTIONAL *)
    cr_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPContentRule                  = ^LDAPContentRule;
  LDAPContentRule                   = ldap_contentrule;

  pldap_nameform                    = ^ldap_nameform;
  ldap_nameform                     = record
    nf_oid          : PChar;        (* REQUIRED *)
    nf_names        : PPChar;       (* OPTIONAL *)
    nf_desc         : PChar;        (* OPTIONAL *)
    nf_obsolete     : cint;         (* 0=no, 1=yes *)
    nf_objectclass  : PChar;        (* REQUIRED *)
    nf_at_oids_must : PPChar;       (* REQUIRED *)
    nf_at_oids_may  : PPChar;       (* OPTIONAL *)
    nf_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPNameForm                     = ^LDAPNameForm;
  LDAPNameForm                      = ldap_nameform;

  pldap_structurerule               = ^ldap_structurerule;
  ldap_structurerule                = record
    sr_ruleid       : cint;          (* REQUIRED *)
    sr_names        : PPChar;        (* OPTIONAL *)
    sr_desc         : PChar;         (* OPTIONAL *)
    sr_obsolete     : cint;          (* 0=no, 1=yes *)
    sr_nameform     : PChar;         (* REQUIRED *)
    sr_nsup_ruleids : cint;          (* number of sr_sup_ruleids *)
    sr_sup_ruleids  : pcint;         (* OPTIONAL *)
    sr_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPStructureRule                = ^LDAPStructureRule;
  LDAPStructureRule                 = ldap_structurerule;


(*
 * Misc macros
 *)
const
  LDAP_SCHEMA_NO                    = 0;
  LDAP_SCHEMA_YES                   = 1;

  LDAP_SCHEMA_USER_APPLICATIONS     = 0;
  LDAP_SCHEMA_DIRECTORY_OPERATION   = 1;
  LDAP_SCHEMA_DISTRIBUTED_OPERATION = 2;
  LDAP_SCHEMA_DSA_OPERATION         = 3;

  LDAP_SCHEMA_ABSTRACT              = 0;
  LDAP_SCHEMA_STRUCTURAL            = 1;
  LDAP_SCHEMA_AUXILIARY             = 2;


(*
 * Flags that control how liberal the parsing routines are.
 *)
const
  LDAP_SCHEMA_ALLOW_NONE            = $00;      (* Strict parsing               *)
  LDAP_SCHEMA_ALLOW_NO_OID          = $01;      (* Allow missing oid            *)
  LDAP_SCHEMA_ALLOW_QUOTED          = $02;      (* Allow bogus extra quotes     *)
  LDAP_SCHEMA_ALLOW_DESCR           = $04;      (* Allow descr instead of OID   *)
  LDAP_SCHEMA_ALLOW_DESCR_PREFIX    = $08;      (* Allow descr as OID prefix    *)
  LDAP_SCHEMA_ALLOW_OID_MACRO       = $10;      (* Allow OID macros in slapd    *)
  LDAP_SCHEMA_ALLOW_OUT_OF_ORDER_FIELDS = $20;  (* Allow fields in most any order *)
  LDAP_SCHEMA_ALLOW_ALL             = $3f;      (* Be very liberal in parsing   *)
  LDAP_SCHEMA_SKIP                  = $80;      (* Don't malloc any result      *)


function ldap_syntax2name(syn: PLDAPSyntax): PChar; cdecl; external;
function ldap_matchingrule2name(mr: PLDAPMatchingRule): PChar; cdecl; external;
function ldap_matchingruleuse2name(mru: PLDAPMatchingRuleUse): PChar; cdecl; external;
function ldap_attributetype2name(_at: PLDAPAttributeType): PChar; cdecl; external;
function ldap_objectclass2name(oc: PLDAPObjectClass): PChar; cdecl; external;
function ldap_contentrule2name(cr: PLDAPContentRule): PChar; cdecl; external;
function ldap_nameform2name(nf: PLDAPNameForm): PChar; cdecl; external;
function ldap_structurerule2name(sr: PLDAPStructureRule): PChar; cdecl; external;

procedure ldap_syntax_free(syn: PLDAPSyntax); cdecl; external;
procedure ldap_matchingrule_free(mr: PLDAPMatchingRule); cdecl; external;
procedure ldap_matchingruleuse_free(mru: PLDAPMatchingRuleUse); cdecl; external;
procedure ldap_attributetype_free(_at: PLDAPAttributeType); cdecl; external;
procedure ldap_objectclass_free(oc: PLDAPObjectClass); cdecl; external;
procedure ldap_contentrule_free(cr: PLDAPContentRule); cdecl; external;
procedure ldap_nameform_free(nf: PLDAPNameForm); cdecl; external;
procedure ldap_structurerule_free(sr: PLDAPStructureRule); cdecl; external;

function ldap_str2structurerule(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPStructureRule; cdecl; external;
function ldap_str2nameform(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPNameForm; cdecl; external;
function ldap_str2contentrule(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPContentRule; cdecl; external;
function ldap_str2objectclass(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPObjectClass; cdecl; external;
function ldap_str2attributetype(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPAttributeType; cdecl; external;
function ldap_str2syntax(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPSyntax; cdecl; external;
function ldap_str2matchingrule(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPMatchingRule; cdecl; external;
function ldap_str2matchingruleuse(const s: PChar; code: pcint; const errp: PPChar; flags: cuint): PLDAPMatchingRuleUse; cdecl; external;

function ldap_structurerule2str(sr: PLDAPStructureRule): PChar; cdecl; external;
function ldap_structurerule2bv(sr: PLDAPStructureRule; bv: PBerval): PBerval; cdecl; external;

function ldap_nameform2str(nf: PLDAPNameForm): PChar; cdecl; external;
function ldap_nameform2bv(nf: PLDAPNameForm; bv: PBerval): PBerval; cdecl; external;

function ldap_contentrule2str(cr: PLDAPContentRule): PChar; cdecl; external;
function ldap_contentrule2bv(cr: PLDAPContentRule; bv: PBerval): PBerval; cdecl; external;

function ldap_objectclass2str(oc: PLDAPObjectClass): PChar; cdecl; external;
function ldap_objectclass2bv(oc: PLDAPObjectClass; bv: PBerval): PBerval; cdecl; external;

function ldap_attributetype2str(_at: PLDAPAttributeType): PChar; cdecl; external;
function ldap_attributetype2bv(_at: PLDAPAttributeType; bv: PBerval): PBerval; cdecl; external;

function ldap_syntax2str(syn: PLDAPSyntax): PChar; cdecl; external;
function ldap_syntax2bv(syn: PLDAPSyntax; bv: PBerval): PBerval; cdecl; external;

function ldap_matchingrule2str(mr: PLDAPMatchingRule): PChar; cdecl; external;
function ldap_matchingrule2bv(mr: PLDAPMatchingRule; bv: PBerval): PBerval; cdecl; external;

function ldap_matchingruleuse2str(mru: PLDAPMatchingRuleUse): PChar; cdecl; external;
function ldap_matchingruleuse2bv(mru: PLDAPMatchingRuleUse; bv: PBerval): PBerval; cdecl; external;

function ldap_scherr2str(code: cint): PChar; cdecl; external;
