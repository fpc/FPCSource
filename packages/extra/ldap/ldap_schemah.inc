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
    lsei_name   : pcchar;
    lsei_values : ppcchar;
  end;

  PPLDAPSchemaExtensionItem         = ^PLDAPSchemaExtensionItem;
  PLDAPSchemaExtensionItem          = ^LDAPSchemaExtensionItem;
  LDAPSchemaExtensionItem           = ldap_schema_extension_item;

  pldap_syntax                      = ^ldap_syntax;
  ldap_syntax                       = record
    syn_oid         : pcchar;        (* REQUIRED *)
    syn_names       : ppcchar;       (* OPTIONAL *)
    syn_desc        : pcchar;        (* OPTIONAL *)
    syn_extensions  : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPSyntax                       = ^ldap_syntax;
  LDAPSyntax                        = ldap_syntax;

  pldap_matchingrule                = ^ldap_matchingrule;
  ldap_matchingrule                 = record
    mr_oid          : pcchar;        (* REQUIRED *)
    mr_names        : ppcchar;       (* OPTIONAL *)
    mr_desc         : pcchar;        (* OPTIONAL *)
    mr_obsolete     : cint;         (* OPTIONAL *)
    mr_syntax_oid   : pcchar;        (* REQUIRED *)
    mr_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPMatchingRule                 = ^LDAPMatchingRule;
  LDAPMatchingRule                  = ldap_matchingrule;

  pldap_matchingruleuse             = ^ldap_matchingruleuse;
  ldap_matchingruleuse              = record
    mru_oid         : pcchar;        (* REQUIRED *)
    mru_names       : ppcchar;       (* OPTIONAL *)
    mru_desc        : pcchar;        (* OPTIONAL *)
    mru_obsolete    : cint;         (* OPTIONAL *)
    mru_applies_oids: ppcchar;       (* REQUIRED *)
    mru_extensions  : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPMatchingRuleUse              = ^LDAPMatchingRuleUse;
  LDAPMatchingRuleUse               = ldap_matchingruleuse;

  pldap_attributetype               = ^ldap_attributetype;
  ldap_attributetype                = record
    at_oid          : pcchar;        (* REQUIRED *)
    at_names        : ppcchar;       (* OPTIONAL *)
    at_desc         : pcchar;        (* OPTIONAL *)
    at_obsolete     : cint;         (* 0=no, 1=yes *)
    at_sup_oid      : pcchar;        (* OPTIONAL *)
    at_equality_oid : pcchar;        (* OPTIONAL *)
    at_ordering_oid : pcchar;        (* OPTIONAL *)
    at_substr_oid   : pcchar;        (* OPTIONAL *)
    at_syntax_oid   : pcchar;        (* OPTIONAL *)
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
    oc_oid          : pcchar;        (* REQUIRED *)
    oc_names        : ppcchar;       (* OPTIONAL *)
    oc_desc         : pcchar;        (* OPTIONAL *)
    oc_obsolete     : cint;         (* 0=no, 1=yes *)
    oc_sup_oids     : ppcchar;       (* OPTIONAL *)
    oc_kind         : cint;         (* 0=ABSTRACT, 1=STRUCTURAL, 2=AUXILIARY *)
    oc_at_oids_must : ppcchar;       (* OPTIONAL *)
    oc_at_oids_may  : ppcchar;       (* OPTIONAL *)
    oc_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPObjectClass                  = ^LDAPObjectClass;
  LDAPObjectClass                   = ldap_objectclass;

  pldap_contentrule                 = ^ldap_contentrule;
  ldap_contentrule                  = record
    cr_oid          : pcchar;        (* REQUIRED *)
    cr_names        : ppcchar;       (* OPTIONAL *)
    cr_desc         : pcchar;        (* OPTIONAL *)
    cr_sup_oids     : ppcchar;       (* OPTIONAL *)
    cr_obsolete     : cint;         (* 0=no, 1=yes *)
    cr_oc_oids_aux  : ppcchar;       (* OPTIONAL *)
    cr_at_oids_must : ppcchar;       (* OPTIONAL *)
    cr_at_oids_may  : ppcchar;       (* OPTIONAL *)
    cr_at_oids_not  : ppcchar;       (* OPTIONAL *)
    cr_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPContentRule                  = ^LDAPContentRule;
  LDAPContentRule                   = ldap_contentrule;

  pldap_nameform                    = ^ldap_nameform;
  ldap_nameform                     = record
    nf_oid          : pcchar;        (* REQUIRED *)
    nf_names        : ppcchar;       (* OPTIONAL *)
    nf_desc         : pcchar;        (* OPTIONAL *)
    nf_obsolete     : cint;         (* 0=no, 1=yes *)
    nf_objectclass  : pcchar;        (* REQUIRED *)
    nf_at_oids_must : ppcchar;       (* REQUIRED *)
    nf_at_oids_may  : ppcchar;       (* OPTIONAL *)
    nf_extensions   : PPLDAPSchemaExtensionItem; (* OPTIONAL *)
  end;

  PLDAPNameForm                     = ^LDAPNameForm;
  LDAPNameForm                      = ldap_nameform;

  pldap_structurerule               = ^ldap_structurerule;
  ldap_structurerule                = record
    sr_ruleid       : cint;          (* REQUIRED *)
    sr_names        : ppcchar;        (* OPTIONAL *)
    sr_desc         : pcchar;         (* OPTIONAL *)
    sr_obsolete     : cint;          (* 0=no, 1=yes *)
    sr_nameform     : pcchar;         (* REQUIRED *)
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


function ldap_syntax2name(syn: PLDAPSyntax): pcchar; cdecl; external;
function ldap_matchingrule2name(mr: PLDAPMatchingRule): pcchar; cdecl; external;
function ldap_matchingruleuse2name(mru: PLDAPMatchingRuleUse): pcchar; cdecl; external;
function ldap_attributetype2name(_at: PLDAPAttributeType): pcchar; cdecl; external;
function ldap_objectclass2name(oc: PLDAPObjectClass): pcchar; cdecl; external;
function ldap_contentrule2name(cr: PLDAPContentRule): pcchar; cdecl; external;
function ldap_nameform2name(nf: PLDAPNameForm): pcchar; cdecl; external;
function ldap_structurerule2name(sr: PLDAPStructureRule): pcchar; cdecl; external;

procedure ldap_syntax_free(syn: PLDAPSyntax); cdecl; external;
procedure ldap_matchingrule_free(mr: PLDAPMatchingRule); cdecl; external;
procedure ldap_matchingruleuse_free(mru: PLDAPMatchingRuleUse); cdecl; external;
procedure ldap_attributetype_free(_at: PLDAPAttributeType); cdecl; external;
procedure ldap_objectclass_free(oc: PLDAPObjectClass); cdecl; external;
procedure ldap_contentrule_free(cr: PLDAPContentRule); cdecl; external;
procedure ldap_nameform_free(nf: PLDAPNameForm); cdecl; external;
procedure ldap_structurerule_free(sr: PLDAPStructureRule); cdecl; external;

function ldap_str2structurerule(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPStructureRule; cdecl; external;
function ldap_str2nameform(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPNameForm; cdecl; external;
function ldap_str2contentrule(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPContentRule; cdecl; external;
function ldap_str2objectclass(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPObjectClass; cdecl; external;
function ldap_str2attributetype(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPAttributeType; cdecl; external;
function ldap_str2syntax(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPSyntax; cdecl; external;
function ldap_str2matchingrule(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPMatchingRule; cdecl; external;
function ldap_str2matchingruleuse(const s: pcchar; code: pcint; const errp: ppcchar; flags: cuint): PLDAPMatchingRuleUse; cdecl; external;

function ldap_structurerule2str(sr: PLDAPStructureRule): pcchar; cdecl; external;
function ldap_structurerule2bv(sr: PLDAPStructureRule; bv: PBerval): PBerval; cdecl; external;

function ldap_nameform2str(nf: PLDAPNameForm): pcchar; cdecl; external;
function ldap_nameform2bv(nf: PLDAPNameForm; bv: PBerval): PBerval; cdecl; external;

function ldap_contentrule2str(cr: PLDAPContentRule): pcchar; cdecl; external;
function ldap_contentrule2bv(cr: PLDAPContentRule; bv: PBerval): PBerval; cdecl; external;

function ldap_objectclass2str(oc: PLDAPObjectClass): pcchar; cdecl; external;
function ldap_objectclass2bv(oc: PLDAPObjectClass; bv: PBerval): PBerval; cdecl; external;

function ldap_attributetype2str(_at: PLDAPAttributeType): pcchar; cdecl; external;
function ldap_attributetype2bv(_at: PLDAPAttributeType; bv: PBerval): PBerval; cdecl; external;

function ldap_syntax2str(syn: PLDAPSyntax): pcchar; cdecl; external;
function ldap_syntax2bv(syn: PLDAPSyntax; bv: PBerval): PBerval; cdecl; external;

function ldap_matchingrule2str(mr: PLDAPMatchingRule): pcchar; cdecl; external;
function ldap_matchingrule2bv(mr: PLDAPMatchingRule; bv: PBerval): PBerval; cdecl; external;

function ldap_matchingruleuse2str(mru: PLDAPMatchingRuleUse): pcchar; cdecl; external;
function ldap_matchingruleuse2bv(mru: PLDAPMatchingRuleUse; bv: PBerval): PBerval; cdecl; external;

function ldap_scherr2str(code: cint): pcchar; cdecl; external;
