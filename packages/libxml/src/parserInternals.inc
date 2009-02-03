(*
 * Summary: internals routines exported by the parser.
 * Description: this module exports a number of internal parsing routines
 *              they are not really all intended for applications but
 *              can prove useful doing low level processing.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF FUNCTION}
(**
 * xmlParserMaxDepth:
 *
 * arbitrary depth limit for the XML documents that we allow to 
 * process. This is not a limitation of the parser but a safety 
 * boundary feature.
 *)
{$IFNDEF NO_EXTERNAL_VARS}
var
  xmlParserMaxDepth: cuint; cvar; external;
{$ENDIF}
{$ENDIF}

{$IFDEF CONST}
 (**
  * XML_MAX_NAMELEN:
  *
  * Identifiers can be longer, but this will be more costly
  * at runtime.
  *)
  XML_MAX_NAMELEN = 100;

(**
 * INPUT_CHUNK:
 *
 * The parser tries to always have that amount of input ready.
 * One of the point is providing context when reporting errors.
 *)
  INPUT_CHUNK	= 250;
{$ENDIF}

{$IFDEF FUNCTION_}
(************************************************************************
 *									*
 * UNICODE version of the macros.      					*
 *									*
 ************************************************************************)
(**
 * IS_BYTE_CHAR:
 * @c:  an byte value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [2] Char ::= #x9 | #xA | #xD | [#x20...]
 * any byte character in the accepted range
 *)
#define IS_BYTE_CHAR(c)	 xmlIsChar_ch(c)

(**
 * IS_CHAR:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD]
 *                  | [#x10000-#x10FFFF]
 * any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
 *)
#define IS_CHAR(c)   xmlIsCharQ(c)

(**
 * IS_CHAR_CH:
 * @c: an xmlChar (usually an unsigned char)
 *
 * Behaves like IS_CHAR on single-byte value
 *)
#define IS_CHAR_CH(c)  xmlIsChar_ch(c)

(**
 * IS_BLANK:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [3] S ::= (#x20 | #x9 | #xD | #xA)+
 *)
#define IS_BLANK(c)  xmlIsBlankQ(c)

(**
 * IS_BLANK_CH:
 * @c:  an xmlChar value (normally unsigned char)
 *
 * Behaviour same as IS_BLANK
 *)
#define IS_BLANK_CH(c)  xmlIsBlank_ch(c)

(**
 * IS_BASECHAR:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [85] BaseChar ::= ... long list see REC ...
 *)
#define IS_BASECHAR(c) xmlIsBaseCharQ(c)

(**
 * IS_DIGIT:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [88] Digit ::= ... long list see REC ...
 *)
#define IS_DIGIT(c) xmlIsDigitQ(c)

(**
 * IS_DIGIT_CH:
 * @c:  an xmlChar value (usually an unsigned char)
 *
 * Behaves like IS_DIGIT but with a single byte argument
 *)
#define IS_DIGIT_CH(c)  xmlIsDigit_ch(c)

(**
 * IS_COMBINING:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [87] CombiningChar ::= ... long list see REC ...
 *)
#define IS_COMBINING(c) xmlIsCombiningQ(c)

(**
 * IS_COMBINING_CH:
 * @c:  an xmlChar (usually an unsigned char)
 *
 * Always false (all combining chars > 0xff)
 *)
#define IS_COMBINING_CH(c) 0 

(**
 * IS_EXTENDER:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [89] Extender ::= #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 |
 *                   #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] |
 *                   [#x309D-#x309E] | [#x30FC-#x30FE]
 *)
#define IS_EXTENDER(c) xmlIsExtenderQ(c)

(**
 * IS_EXTENDER_CH:
 * @c:  an xmlChar value (usually an unsigned char)
 *
 * Behaves like IS_EXTENDER but with a single-byte argument
 *)
#define IS_EXTENDER_CH(c)  xmlIsExtender_ch(c)

(**
 * IS_IDEOGRAPHIC:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [86] Ideographic ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
 *)
#define IS_IDEOGRAPHIC(c) xmlIsIdeographicQ(c)

(**
 * IS_LETTER:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [84] Letter ::= BaseChar | Ideographic 
 *)
#define IS_LETTER(c) (IS_BASECHAR(c) || IS_IDEOGRAPHIC(c))

(**
 * IS_LETTER_CH:
 * @c:  an xmlChar value (normally unsigned char)
 *
 * Macro behaves like IS_LETTER, but only check base chars
 *
 *)
#define IS_LETTER_CH(c) xmlIsBaseChar_ch(c)

(**
 * IS_ASCII_LETTER:
 * @c: an xmlChar value
 *
 * Macro to check [a-zA-Z]
 *
 *)
#define IS_ASCII_LETTER(c)	(((0x41 <= (c)) && ((c) <= 0x5a)) || \
				 ((0x61 <= (c)) && ((c) <= 0x7a)))

(**
 * IS_ASCII_DIGIT:
 * @c: an xmlChar value
 *
 * Macro to check [0-9]
 *
 *)
#define IS_ASCII_DIGIT(c)	((0x30 <= (c)) && ((c) <= 0x39))

(**
 * IS_PUBIDCHAR:
 * @c:  an UNICODE value (int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
 *)
#define IS_PUBIDCHAR(c)	xmlIsPubidCharQ(c)

(**
 * IS_PUBIDCHAR_CH:
 * @c:  an xmlChar value (normally unsigned char)
 *
 * Same as IS_PUBIDCHAR but for single-byte value
 *)
#define IS_PUBIDCHAR_CH(c) xmlIsPubidChar_ch(c)

(**
 * SKIP_EOL:
 * @p:  and UTF8 string pointer
 *
 * Skips the end of line chars.
 *)
#define SKIP_EOL(p) 							\
    if ( *(p) == 0x13) { p++ ; if ( *(p) == 0x10) p++; }			\
    if ( *(p) == 0x10) { p++ ; if ( *(p) == 0x13) p++; }

(**
 * MOVETO_ENDTAG:
 * @p:  and UTF8 string pointer
 *
 * Skips to the next '>' char.
 *)
#define MOVETO_ENDTAG(p)						\
    while (( *p) && ( *(p) != '>')) (p)++

(**
 * MOVETO_STARTTAG:
 * @p:  and UTF8 string pointer
 *
 * Skips to the next '<' char.
 *)
#define MOVETO_STARTTAG(p)						\
    while (( *p) && ( *(p) != '<')) (p)++

(**
 * Global variables used for predefined strings.
 *)
cvar; external xmlChar xmlStringText[];
cvar; external xmlChar xmlStringTextNoenc[];
cvar; external xmlChar xmlStringComment[];

(*
 * Function to finish the work of the macros where needed.
 *)
external xml2lib int EXTDECL                   xmlIsLetter     (int c);

(**
 * Parser context.
 *)
external xml2lib xmlParserCtxtPtr EXTDECL	
			xmlCreateFileParserCtxt	(char *filename);
external xml2lib xmlParserCtxtPtr EXTDECL	
			xmlCreateURLParserCtxt	(char *filename,
						 int options);
external xml2lib xmlParserCtxtPtr EXTDECL	
			xmlCreateMemoryParserCtxt(char *buffer,
						 int size);
external xml2lib xmlParserCtxtPtr EXTDECL	
			xmlCreateEntityParserCtxt(xmlChar *URL,
						 xmlChar *ID,
						 xmlChar *base);
external xml2lib int EXTDECL			
			xmlSwitchEncoding	(xmlParserCtxtPtr ctxt,
						 xmlCharEncoding enc);
external xml2lib int EXTDECL			
			xmlSwitchToEncoding	(xmlParserCtxtPtr ctxt,
					 xmlCharEncodingHandlerPtr handler);
external xml2lib int EXTDECL			
			xmlSwitchInputEncoding	(xmlParserCtxtPtr ctxt,
						 xmlParserInputPtr input,
					 xmlCharEncodingHandlerPtr handler);

{$IFDEF IN_LIBXML}
(* internal error reporting *)
external xml2lib void EXTDECL
			__xmlErrEncoding	(xmlParserCtxtPtr ctxt,
						 xmlParserErrors xmlerr,
						 char *msg,
						 xmlChar * str1,
						 xmlChar * str2);
{$ENDIF}

(**
 * Input Streams.
 *)
external xml2lib xmlParserInputPtr EXTDECL	
			xmlNewStringInputStream	(xmlParserCtxtPtr ctxt,
						 xmlChar *buffer);
external xml2lib xmlParserInputPtr EXTDECL	
			xmlNewEntityInputStream	(xmlParserCtxtPtr ctxt,
						 xmlEntityPtr entity);
external xml2lib void EXTDECL			
			xmlPushInput		(xmlParserCtxtPtr ctxt,
						 xmlParserInputPtr input);
external xml2lib xmlChar EXTDECL			
			xmlPopInput		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlFreeInputStream	(xmlParserInputPtr input);
external xml2lib xmlParserInputPtr EXTDECL	
			xmlNewInputFromFile	(xmlParserCtxtPtr ctxt,
						 char *filename);
external xml2lib xmlParserInputPtr EXTDECL	
			xmlNewInputStream	(xmlParserCtxtPtr ctxt);

(**
 * Namespaces.
 *)
external xml2lib xmlChar * EXTDECL		
			xmlSplitQName		(xmlParserCtxtPtr ctxt,
						 xmlChar *name,
						 xmlChar **prefix);

(**
 * Generic production rules.
 *)
external xml2lib xmlChar * EXTDECL		
			xmlParseName		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseNmtoken		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseEntityValue	(xmlParserCtxtPtr ctxt,
						 xmlChar **orig);
external xml2lib xmlChar * EXTDECL		
			xmlParseAttValue	(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseSystemLiteral	(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParsePubidLiteral	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseCharData	(xmlParserCtxtPtr ctxt,
						 int cdata);
external xml2lib xmlChar * EXTDECL		
			xmlParseExternalID	(xmlParserCtxtPtr ctxt,
						 xmlChar **publicID,
						 int strict);
external xml2lib void EXTDECL			
			xmlParseComment		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParsePITarget	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParsePI		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseNotationDecl	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseEntityDecl	(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			
			xmlParseDefaultDecl	(xmlParserCtxtPtr ctxt,
						 xmlChar **value);
external xml2lib xmlEnumerationPtr EXTDECL	
			xmlParseNotationType	(xmlParserCtxtPtr ctxt);
external xml2lib xmlEnumerationPtr EXTDECL	
			xmlParseEnumerationType	(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			
			xmlParseEnumeratedType	(xmlParserCtxtPtr ctxt,
						 xmlEnumerationPtr *tree);
external xml2lib int EXTDECL			
			xmlParseAttributeType	(xmlParserCtxtPtr ctxt,
						 xmlEnumerationPtr *tree);
external xml2lib void EXTDECL			
			xmlParseAttributeListDecl(xmlParserCtxtPtr ctxt);
external xml2lib xmlElementContentPtr EXTDECL	
			xmlParseElementMixedContentDecl
						(xmlParserCtxtPtr ctxt,
						 int inputchk);
external xml2lib xmlElementContentPtr EXTDECL	
			xmlParseElementChildrenContentDecl
						(xmlParserCtxtPtr ctxt,
						 int inputchk);
external xml2lib int EXTDECL			
			xmlParseElementContentDecl(xmlParserCtxtPtr ctxt,
						 xmlChar *name,
						 xmlElementContentPtr *result);
external xml2lib int EXTDECL			
			xmlParseElementDecl	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseMarkupDecl	(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			
			xmlParseCharRef		(xmlParserCtxtPtr ctxt);
external xml2lib xmlEntityPtr EXTDECL		
			xmlParseEntityRef	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseReference	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParsePEReference	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseDocTypeDecl	(xmlParserCtxtPtr ctxt);
{$IFDEF LIBXML_SAX1_ENABLED}
external xml2lib xmlChar * EXTDECL		
			xmlParseAttribute	(xmlParserCtxtPtr ctxt,
						 xmlChar **value);
external xml2lib xmlChar * EXTDECL		
			xmlParseStartTag	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseEndTag		(xmlParserCtxtPtr ctxt);
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
external xml2lib void EXTDECL			
			xmlParseCDSect		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseContent		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseElement		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseVersionNum	(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseVersionInfo	(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseEncName		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlParseEncodingDecl	(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			
			xmlParseSDDecl		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseXMLDecl		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseTextDecl	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseMisc		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			
			xmlParseExternalSubset	(xmlParserCtxtPtr ctxt,
						 xmlChar *ExternalID,
						 xmlChar *SystemID);
{$ENDIF}

{$IFDEF CONST}
(**
 * XML_SUBSTITUTE_NONE:
 *
 * If no entities need to be substituted.
 *)
  XML_SUBSTITUTE_NONE	= 0;

(**
 * XML_SUBSTITUTE_REF:
 *
 * Whether general entities need to be substituted.
 *)
  XML_SUBSTITUTE_REF	= 1;

(**
 * XML_SUBSTITUTE_PEREF:
 *
 * Whether parameter entities need to be substituted.
 *)
  XML_SUBSTITUTE_PEREF	= 2;

(**
 * XML_SUBSTITUTE_BOTH:
 *
 * Both general and parameter entities need to be substituted.
 *)
  XML_SUBSTITUTE_BOTH 	= 3;
{$ENDIF}

{$IFDEF FUNCTION_}
external xml2lib xmlChar * EXTDECL
		xmlStringDecodeEntities		(xmlParserCtxtPtr ctxt,
						 xmlChar *str,
						 int what,
						 xmlChar end,
						 xmlChar  end2,
						 xmlChar end3);
external xml2lib xmlChar * EXTDECL
		xmlStringLenDecodeEntities	(xmlParserCtxtPtr ctxt,
						 xmlChar *str,
						 int len,
						 int what,
						 xmlChar end,
						 xmlChar  end2,
						 xmlChar end3);

(*
 * Generated by MACROS on top of parser.c c.f. PUSH_AND_POP.
 *)
external xml2lib int EXTDECL			nodePush		(xmlParserCtxtPtr ctxt,
						 xmlNodePtr value);
external xml2lib xmlNodePtr EXTDECL		nodePop			(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			inputPush		(xmlParserCtxtPtr ctxt,
						 xmlParserInputPtr value);
external xml2lib xmlParserInputPtr EXTDECL	inputPop		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL	namePop			(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			namePush		(xmlParserCtxtPtr ctxt,
						 xmlChar *value);

(*
 * other commodities shared between parser.c and parserInternals.
 *)
external xml2lib int EXTDECL			xmlSkipBlankChars	(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			xmlStringCurrentChar	(xmlParserCtxtPtr ctxt,
						 xmlChar *cur,
						 int *len);
external xml2lib void EXTDECL			xmlParserHandlePEReference(xmlParserCtxtPtr ctxt);
external xml2lib int EXTDECL			xmlCheckLanguageID	(xmlChar *lang);

(*
 * Really core function shared with HTML parser.
 *)
external xml2lib int EXTDECL			xmlCurrentChar		(xmlParserCtxtPtr ctxt,
						 int *len);
external xml2lib int EXTDECL		xmlCopyCharMultiByte	(xmlChar *out,
						 int val);
external xml2lib int EXTDECL			xmlCopyChar		(int len,
						 xmlChar *out,
						 int val);
external xml2lib void EXTDECL			xmlNextChar		(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL			xmlParserInputShrink	(xmlParserInputPtr in);

{$IFDEF LIBXML_HTML_ENABLED}
(*
 * Actually comes from the HTML parser but launched from the init stuff.
 *)
external xml2lib void EXTDECL			htmlInitAutoClose	(void);
external xml2lib htmlParserCtxtPtr EXTDECL	htmlCreateFileParserCtxt(char *filename,
	                                         char *encoding);
{$ENDIF}

(*
 * Specific function to keep track of entities references
 * and used by the XSLT debugger.
 *)
{$IFDEF LIBXML_LEGACY_ENABLED}
(**
 * xmlEntityReferenceFunc:
 * @ent: the entity
 * @firstNode:  the fist node in the chunk
 * @lastNode:  the last nod in the chunk
 *
 * Callback function used when one needs to be able to track back the
 * provenance of a chunk of nodes inherited from an entity replacement.
 *)
typedef	void	(xmlEntityReferenceFunc)	(xmlEntityPtr ent,
						 xmlNodePtr firstNode,
						 xmlNodePtr lastNode);
  
external xml2lib void EXTDECL		xmlSetEntityReferenceFunc	(xmlEntityReferenceFunc func);

external xml2lib xmlChar * EXTDECL		
			xmlParseQuotedString	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL
                        xmlParseNamespace       (xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlNamespaceParseNSDef	(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlScanName		(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlNamespaceParseNCName	(xmlParserCtxtPtr ctxt);
external xml2lib void EXTDECL	xmlParserHandleReference(xmlParserCtxtPtr ctxt);
external xml2lib xmlChar * EXTDECL		
			xmlNamespaceParseQName	(xmlParserCtxtPtr ctxt,
						 xmlChar **prefix);

(**
 * Entities
 *)
external xml2lib xmlChar * EXTDECL
		xmlDecodeEntities		(xmlParserCtxtPtr ctxt,
						 int len,
						 int what,
						 xmlChar end,
						 xmlChar  end2,
						 xmlChar end3);
external xml2lib void EXTDECL			
			xmlHandleEntity		(xmlParserCtxtPtr ctxt,
						 xmlEntityPtr entity);

{$ENDIF} (* LIBXML_LEGACY_ENABLED *)

{$IFDEF IN_LIBXML}
(*
 * internal only
 *)
external xml2lib void EXTDECL
	xmlErrMemory		(xmlParserCtxtPtr ctxt,
				 char *extra);
{$ENDIF}
{$ENDIF}
