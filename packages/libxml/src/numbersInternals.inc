(*
 * Summary: Implementation of the XSLT number functions
 * Description: Implementation of the XSLT number functions
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Bjorn Reese <breese@users.sourceforge.net> and Daniel Veillard
 *)

{$IFDEF TYPE}

(**
 * xsltNumberData:
 *
 * This data structure is just a wrapper to pass xsl:number data in.
 *)
  xsltNumberDataPtr = ^xsltNumberData;
  xsltNumberData = record
    level: xmlCharPtr;
    count: xmlCharPtr;
    from: xmlCharPtr;
    value: xmlCharPtr;
    format: xmlCharPtr;
    has_format: cint;
    digitsPerGroup: cint;
    groupingCharacter: cint;
    groupingCharacterLen: cint;
    doc: xmlDocPtr;
    node: xmlNodePtr;
    countPat: xsltCompMatchPtr;
    fromPat: xsltCompMatchPtr;

    (*
     * accelerators
     *)
  end;

(**
 * xsltFormatNumberInfo,:
 *
 * This data structure lists the various parameters needed to format numbers.
 *)
  xsltFormatNumberInfoPtr = ^xsltFormatNumberInfo;

  xsltFormatNumberInfo = record
    integer_hash: cint;         (* Number of '#' in integer part *)
    integer_digits: cint;       (* Number of '0' in integer part *)
    frac_digits: cint;          (* Number of '0' in fractional part *)
    frac_hash: cint;            (* Number of '#' in fractional part *)
    group: cint;                (* Number of chars per display 'group' *)
    multiplier: cint;           (* Scaling for percent or permille *)
    add_decimal: Byte;          (* Flag for whether decimal point appears in pattern *)
    is_multiplier_set: Byte;    (* Flag to catch multiple occurences of percent/permille *)
    is_negative_pattern: Byte;  (* Flag for processing -ve prefix/suffix *)
  end;

{$ENDIF} {TYPE}
