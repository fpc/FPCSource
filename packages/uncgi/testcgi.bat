@echo off
echo Setting environment variables:
set REQUEST_METHOD=GET
set QUERY_STRING=name=michael&address=home&city=heaven
set HTTP_USER_AGENT=Shell script
set HTTP_REFERER=The shell prompt
echo Running program
testcgi.exe

