#!/bin/sh
echo Setting environment variables:
REQUEST_METHOD=GET
QUERY_STRING="name=michael&address=home&city=heaven"
HTTP_USER_AGENT="Shell script"
HTTP_REFERER="The shell prompt"
export REQUEST_METHOD QUERY_STRING HTTP_USER_AGENT HTTP_REFERER
echo Running program..
testcgi
#end of script


