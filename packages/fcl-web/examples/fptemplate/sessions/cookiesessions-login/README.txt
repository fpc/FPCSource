Sessions stored in cookies with Login, example
======================================

Note: Cookies must be enabled for the website in the visitor's browser for it 
to work.

Every visitor needs to log in (using userdb.txt for the user names and 
passwords in the example) to get a session ID that is passed in a cookie to 
the web visitor's browser and stored on the web server until it expires.
This way fully functional web sites can be built without mixing the different 
visitor's screens.

Note: This example can not distinguish between sessions on the same computer, same 
browser but different tabs. If for example someone opens three tabs and logs in 
with all of them, all three browser tabs will be the same session.
In order to handle sessions differently by browser tabs the best way to go is 
to store session IDs in the URLs, Links, etc. for all pages the web application
 generates (this way cookies are not needed to maintain the session).
The sample user logins are stored in userdb.txt .

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

fpc -Fu../webmodule cookiesession.lpr

The -Fu parameter shows FPC where to find the web module source code. All 
three web applications share the same web module code.

1.b; with Lazarus
-----------------
It needs the WebLaz Package installed. Open the .lpi file from the chosen 
application directory (cgi/fcgi/apache), and then 

Run -> Build from the menu.


2. Setup:
---------
The application needs read access to the template (test*.html) files.
It is best to use full paths with the file names in the web module 
(webmodule.pas), because Apache will probably look relative to the / (main 
root) directory or main Apache directory and not relative to the application 
file location.
ex: ModuleTemplate.FileName := '/full/path/to/templates/testlogin.html';
and so on for all the other templates.

The web server application needs read/write access to the file where the session
 information will be stored with the login names (sessiondb.txt by default).
ex: sessiondbfile := '/full/path/to/sessiondb.txt';
The sessiondb.txt can be created empty, and needs read/write access.

It also needs read access to the user database (userdb.txt)
ex: userdbfile := '/full/path/to/userdb.txt';

The fpweb generated cookie session files usually need to be deleted after 
expiration manually or by a periodic cleaning program, because they are piling 
up in the default temporary directory if the sessions are not terminated (ie. 
if people are not logging out).


2.a; as CGI
-----------
Usually it works if you put the templates (test*.html) next to the CGI 
executable file in the Apache cgi-bin directory. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheScriptAliasName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cookiesession/login
if in the Apache configuration file (ex: httpd.conf) it was set up as 
ScriptAlias /cookiesession "<path_to_app>/<app_name>"
ex:
ScriptAlias /cookiesession "C:/Program Files/Apache Software Foundation/Apache2.2/cgi-bin/cookiesession.exe"

Note: You need to change the URLs if "cgi-bin" or "cookiesession.exe" changes 
(for example on Linux it is not cookiesession.exe).
Also, if your server is listening on port 80 instead of 8080, you can leave 
the :8080 part from the calling URL.


2.b; as Apache module
---------------------
Usually it works if you put the templates (test*.html) into the Apache 
main directory (not the DocumentRoot, but the main Apache directory), under 
sub-directory "templates" or something similar. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheLocationName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cookiesession/login
if in httpd.conf it was set up as:
LoadModule mod_cookiesession "<path_to_mod>/cookiesession.dll"
<Location /cookiesession>
    SetHandler mod_cookiesession
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "/cookiesession" changes. 
Also, for example on Linux the module can be libcookiesession.so and not 
cookiesession.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation might fail because the file is in use 
(Apache modules stay in the memory). So first, you always need to stop the 
server before you recompile or before you copy over the new version of the 
newly created module.
On Linux, it is enough to simply reload Apache after recompile.


2.c; as FCGI
------------
Usually it works if you put the templates (test*.html) next to the FCGI 
executable file. Adjust the file path in the web module (webmodule.pas) 
accordingly.

http://<WebServer>/<ApacheScriptAliasName>/gotonextpage should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cookiesession/login
or  http://127.0.0.1/cookiesession/login
if in the Apache configuration file (ex: httpd.conf) it was set up as:

LoadModule fastcgi_module "<path_to_mod>/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "<path_to_fcgi_app>">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiExternalServer "<path_to_fcgi_app>/cookiesession.exe" -host 127.0.0.1:2015 -idle-timeout 30 -flush
  ScriptAlias /cookiesession "<path_to_fcgi_app>/cookiesession.exe"
</IfModule>

Note: You need to change the module name if needed. For example on Linux, 
the module is not mod_fastcgi-2.4.6-AP22.dll but mod_fastcgi.so (need to be 
compiled from sources found at http://www.fastcgi.com/dist/ ).
The port (2015 in this example) must match the one set in the project main 
file (cookiesession.lpr).
The FCGI application must be running in order for this demo to work (external 
FCGI server setup). Do not forget to restart it after changes and 
recompilation.
Also, mod_fastcgi is not the same as mod_fcgid that the Apache project is 
developing. The latter does not support external FCGI server apps.
There are other ways than external FCGI server apps to handle the FCGI 
protocol and both mod_fastcgi and mod_fcgid supports that. However, external 
FCGI servers are the best for debugging and development, that is why the 
examples do it that way.