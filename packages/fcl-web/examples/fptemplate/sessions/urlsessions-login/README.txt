Sessions stored in the URLs with Login (no cookies needed), example
==========================================================

Every visitor needs to log in (using userdb.txt for the user names and 
passwords in the example) to get a session ID that is always passed in the 
URLs of the response pages as a query parameter and stored on the web server 
until it expires.
This way fully functional web sites can be built without mixing the different 
visitor's screens and without using cookies to keep the session separated.

Note: Because the session ID is stored dynamically in the generated pages, all
web browser tabs will have separate sessions after logging in with them.
The sample user logins are stored in userdb.txt for the example.

=====================
1. Compiling
1.a; with FPC
1.b; with Lazarus
2. Setup
2.a; as CGI
2.b; as Apache module
2.c; as FCGI
=====================

1. Compiling:
-------------
We can either use FPC directly, or Lazarus to compile the CGI/FCGI/Apache 
module application. The main project .lpr file, as well as the Lazarus .lpi is 
in the cgi/fcgi/apache directories.

1.a; with FPC
-------------
Enter to the directory (cgi/fcgi/apache) that has the .lpr file you wish to 
compile, and then execute the command 

fpc -Fu../webmodule urlsession.lpr

The -Fu parameter shows FPC where to find the web module source code. All 
three web applications share the same web module code.

1.b; with Lazarus
-----------------
It needs the WebLaz Package installed. Open the .lpi file from the chosen 
application directory (cgi/fcgi/apache), and then 

Run -> Build from the menu.


2. Setup:
---------
The application needs read access to the template (testurl*.html) files.
It is best to use full paths with the file names in the web module 
(webmodule.pas), because Apache will probably look relative to the / (main 
root) directory or main Apache directory and not relative to the application 
file location.
ex: ModuleTemplate.FileName := '/full/path/to/templates/testurllogin.html'; 
and so on for all the other templates.

It needs read/write access to the file where the session informations will be 
stored with the login names (session-db.txt).
ex: sessiondbfile := '/full/path/to/session-db.txt';
The session-db.txt can be created empty, and needs read/write access.

It also needs read access to the user database (userdb.txt)
ex: userdbfile := '/full/path/to/userdb.txt';


2.a; as CGI
-----------
Usually it works if you put the templates (testurl*.html) next to the CGI 
executable file in the Apache cgi-bin directory. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheScriptAliasName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/urlsession/login
if in the Apache configuration file (ex: httpd.conf) it was set up as 
ScriptAlias /urlsession "<path_to_app>/<app_name>"
ex:
ScriptAlias /urlsession "C:/Program Files/Apache Software Foundation/Apache2.2/cgi-bin/urlsession.exe"

Note: You need to change the URLs if "cgi-bin" or "urlsession.exe" changes 
(for example on Linux it is not urlsession.exe).
Also, if your server is listening on port 80 instead of 8080, you can leave 
the :8080 part from the calling URL.


2.b; as Apache module
---------------------
Usually it works if you put the templates (testurl*.html) into the Apache 
main directory (not the DocumentRoot, but the main Apache directory), under 
sub-directory "templates" or something similar. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheLocationName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/urlsession/login
if in httpd.conf it was set up as:
LoadModule mod_urlsession "<path_to_mod>/urlsession.dll"
<Location /urlsession>
    SetHandler mod_urlsession
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "urlsession" changes. 
Also, for example on Linux the module can be liburlsession.so and not 
urlsession.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation might fail because the file is in use 
(Apache modules stay in the memory). So first, you always need to stop the 
server before you recompile or before you copy over the new version of the 
newly created module.
On Linux, it is enough to simply reload Apache after recompile.


2.c; as FCGI
------------
Usually it works if you put the templates (testurl*.html) next to the FCGI 
executable file. Adjust the file path in the web module (webmodule.pas) 
accordingly.

http://<WebServer>/<ApacheScriptAliasName>/login should start the example 
if everything is set up properly.
ex: http://127.0.0.1:8080/urlsession/login
or  http://127.0.0.1/urlsession/login
if in the Apache configuration file (ex: httpd.conf) it was set up as:

LoadModule fastcgi_module "<path_to_mod>/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "<path_to_fcgi_app>">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiExternalServer "<path_to_fcgi_app>/urlsession.exe" -host 127.0.0.1:2015 -idle-timeout 30 -flush
  ScriptAlias /urlsession "<path_to_fcgi_app>/urlsession.exe"
</IfModule>

Note: You need to change the module name if needed. For example on Linux, 
the module is not mod_fastcgi-2.4.6-AP22.dll but mod_fastcgi.so (need to be 
compiled from sources found at http://www.fastcgi.com/dist/ ).
The port (2015 in this example) must match the one set in the project main 
file (urlsession.lpr).
The FCGI application must be running in order for this demo to work (external 
FCGI server setup). Do not forget to restart it after changes and 
recompilation.
Also, mod_fastcgi is not the same as mod_fcgid that the Apache project is 
developing. The latter does not support external FCGI server apps.
There are other ways than external FCGI server apps to handle the FCGI 
protocol and both mod_fastcgi and mod_fcgid supports that. However, external 
FCGI servers are the best for debugging and development, that is why the 
examples do it that way.
