Simple template, example
================
The simplest template with one template tag in it to be replaced by the 
CGI/FCGI/Apache module application when generating the response page 
-> ex: {TagName1}

Note, that the only difference between CGI/FCGI and Apache module is in the 
main project .lpr file.

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

fpc -Fu../webmodule simpletemplate.lpr

The -Fu parameter shows FPC where to find the web module source code. All 
three web applications share the same web module code.

1.b; with Lazarus
-----------------
It needs the WebLaz Package installed. Open the .lpi file from the chosen 
application directory (cgi/fcgi/apache), and then 

Run -> Build from the menu.


2. Setup:
---------
The application needs read access to the template file(s).
It is best to use full paths with the file names in the web module 
(webmodule.pas), because Apache will probably look relative to the / (main 
root) directory or main Apache directory and not relative to the application 
file location.
ex: ModuleTemplate.FileName := '/full/path/to/template/mytemplate1.html';


2.a; as CGI
-----------
Usually it works if you put the template (mytemplate1.html) next to the CGI 
executable file in the Apache cgi-bin directory. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/cgi-bin/<CGIExecutableName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/simpletemplate.exe/func1call

If the calling URL looks too long, or you want to hide it a little bit more, 
you can use a ScriptAlias in the Apache configuration file to make it shorter.
ex: ScriptAlias /mycgi "<path_to_cgi_app>/simpletemplate.exe" in your conf 
file will make http://127.0.0.1:8080/mycgi/func1call work the same way.

Note: You need to change the URLs if "cgi-bin" or "simpletemplate.exe" 
changes (for example on Linux it is not simpletemplate.exe).
Also, if your server is listening on port 80 instead of 8080, you can leave 
the :8080 part from the calling URL.


2.b; as Apache module
---------------------
Usually it works if you put the template (mytemplate1.html) into the Apache 
main directory (not the DocumentRoot, but the main Apache directory), under 
sub-directory "templates" or something similar. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheLocationName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/myapache/func1call

if in httpd.conf it was set up as:
LoadModule mod_simpletemplate "<path_to_mod>/simpletemplate.dll"
<Location /myapache>
    SetHandler mod_simpletemplate
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs "myapache" or "simpletemplate.dll" 
changes. For example on Linux the module can be libsimpletemplate.so and not 
simpletemplate.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation might fail because the file is in use 
(Apache modules stay in the memory). So first, you always need to stop the 
server before you recompile or before you copy over the new version of the 
newly created module.
On Linux, it is enough to simply reload Apache after recompile.


2.c; as FCGI
------------
Usually it works if you put the template (mytemplate1.html) next to the FCGI 
executable file. Adjust the file path in the web module (webmodule.pas) 
accordingly.

http://<WebServer>/<ApacheScriptAliasName>/func1call should start the example 
if everything is set up properly.
ex: http://127.0.0.1:8080/myfcgi/func1call
if in the Apache configuration file (ex: httpd.conf) it was set up as:

LoadModule fastcgi_module "<path_to_mod>/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "<path_to_fcgi_app>">
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#External FCGI app, has to start manually, Apache will not do it
  FastCgiExternalServer "<path_to_fcgi_app>/simpletemplate.exe" -host 127.0.0.1:2015 -idle-timeout 30 -flush
#optionally, to shorten the calling URL and to not display the executable file name (if used, no +ExecCGI is needed above)
  ScriptAlias /myfcgi "<path_to_fcgi_app>/simpletemplate.exe"
</IfModule>

Note: You need to change the module name if needed. For example on Linux, 
the module is not mod_fastcgi-2.4.6-AP22.dll but mod_fastcgi.so (need to be 
compiled from sources found at http://www.fastcgi.com/dist/ ).
The port (2015 in this example) must match the one set in the project main 
file (simpletemplate.lpr).
The FCGI application must be running in order for this demo to work (external 
FCGI server setup). Do not forget to restart it after changes and 
recompilation.
Also, mod_fastcgi is not the same as mod_fcgid that the Apache project is 
developing. The latter does not support external FCGI server apps.
There are other ways than external FCGI server apps to handle the FCGI 
protocol and both mod_fastcgi and mod_fcgid supports that. However, external 
FCGI servers are the best for debugging and development, that is why the 
examples do it that way.
