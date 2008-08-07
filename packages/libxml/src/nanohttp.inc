(*
 * Summary: minimal HTTP implementation
 * Description: minimal HTTP implementation allowing to fetch resources
 *              like external subset.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)
 
#ifndef __NANO_HTTP_H__
#define __NANO_HTTP_H__

#include <libxml/xmlversion.h>

{ LIBXML_HTTP_ENABLED

{ __cplusplus
extern "C" {
#endif
XMLPUBFUN void XMLCALL
	xmlNanoHTTPInit		(void);
XMLPUBFUN void XMLCALL	
	xmlNanoHTTPCleanup	(void);
XMLPUBFUN void XMLCALL	
	xmlNanoHTTPScanProxy	(char *URL);
XMLPUBFUN int XMLCALL	
	xmlNanoHTTPFetch	(char *URL,
				 char *filename,
				 char **contentType);
XMLPUBFUN void * XMLCALL	
	xmlNanoHTTPMethod	(char *URL,
				 char *method,
				 char *input,
				 char **contentType,
				 char *headers,
				 int   ilen);
XMLPUBFUN void * XMLCALL	
	xmlNanoHTTPMethodRedir	(char *URL,
				 char *method,
				 char *input,
				 char **contentType,
				 char **redir,
				 char *headers,
				 int   ilen);
XMLPUBFUN void * XMLCALL	
	xmlNanoHTTPOpen		(char *URL,
				 char **contentType);
XMLPUBFUN void * XMLCALL	
	xmlNanoHTTPOpenRedir	(char *URL,
				 char **contentType,
				 char **redir);
XMLPUBFUN int XMLCALL	
	xmlNanoHTTPReturnCode	(void *ctx);
XMLPUBFUN char * XMLCALL 
	xmlNanoHTTPAuthHeader	(void *ctx);
XMLPUBFUN char * XMLCALL
	xmlNanoHTTPRedir	(void *ctx);
XMLPUBFUN int XMLCALL
	xmlNanoHTTPContentLength( void * ctx );
XMLPUBFUN char * XMLCALL
	xmlNanoHTTPEncoding	(void *ctx);
XMLPUBFUN char * XMLCALL
	xmlNanoHTTPMimeType	(void *ctx);
XMLPUBFUN int XMLCALL	
	xmlNanoHTTPRead		(void *ctx,
				 void *dest,
				 int len);
{ LIBXML_OUTPUT_ENABLED
XMLPUBFUN int XMLCALL	
	xmlNanoHTTPSave		(void *ctxt,
				 char *filename);
#endif (* LIBXML_OUTPUT_ENABLED *)
XMLPUBFUN void XMLCALL	
	xmlNanoHTTPClose	(void *ctx);
{ __cplusplus
}
#endif

#endif (* LIBXML_HTTP_ENABLED *)
#endif (* __NANO_HTTP_H__ *)
