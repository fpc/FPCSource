This directory contains some example how to create a searchable website.

The docindexer program can be used to create the index.
The httpsearch program is a HTTP server program that can search in the index.

Both programs use an ini file that tells them what database must be used to
store/consult the index.

The docindexer essentially works as
docindexer -c sample.ini -d /the/directory/to/index

The httpsearch program can be compiled and started as a standalone HTTP server
(no command-line args needed) 
or can be compiled as a .cgi program. See the project source, the usecgi
define can be enabled/disabled to switch between the behaviour.

The programs support 4 database types:

PostGres
Firebird
SQLite
File

Which ones are compiled-in depend on some defines in the httpsearcher.pp file
 
The HTTP server supports 2 kinds of queries:
[baseURL]/search  : search pages matching the search term.
[baseURL]/list    : search words matching the search term. Can be used for typeahead funcionnality
[baseURL] is the base URL where the HTTP service is listening...

Responses are in JSON dataset format (see extjsdataset unit)

The following HTTP Request query parameters are understood for "search":
q - search term. Required.
m - include metadata in response
r - minimum rank for response (integer>0)
c - Use "contains" to search. the default is  exact match (boolean: 0,1)

The following HTTP Request query parameters are understood for "list":
q - search term. Required unless type=all.
m - include metadata in response
t - Query type. One of
    all
    exact
    contains
    startswith
    Determines how to search words. in case t=all, q must be empty.
s - Return a simple array list.

The sample.ini file can contain some default configuration settings.

Defaults are shown
[search]
; Format the returned JSON (boolean)
formatjson=0
; Default for minimum rank in search results (integer)
minrank=1 
; Default for returning metadata (boolean)
metadata=1
; Set a CORS header on the response or not (boolean)
allowcors=1
