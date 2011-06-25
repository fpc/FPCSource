File upload from html form, example
==========================
Demonstrates how to handle the file upload (multipart) html forms with 
templates.

Note, that the only difference between CGI/FCGI and Apache module is in the 
main project .lpr file and the web server (Apache) configuration.

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

fpc -Fu../webmodule fileupload.lpr

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
ex: ModuleTemplate.FileName := '/full/path/to/templates/uploadform.html';

It needs read/write access to create the file database filelist.txt
ex: FileDB := '/full/path/to/templates/filelist.txt';

Also, it needs an "/upfiles" directory with read/write access where the files 
will be uploaded to.
ex: UploadDir := '/full/path/to/upfiles/';


2.a; as CGI
-----------
Usually it works if you put the template (uploadform.html) next to the CGI 
executable file in the Apache cgi-bin directory. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheScriptAliasName>/listfiles should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/fileuploader/listfiles
if in the Apache configuration file (ex: httpd.conf) it was set up as 
ScriptAlias /fileuploader "<path_to_app>/<app_name>"
ex:
ScriptAlias /fileuploader "C:/Program Files/Apache Software Foundation/Apache2.2/cgi-bin/fileupload.exe"
(uploadform.html is copied next to fileupload.exe into cgi-bin, and cgi-bin 
has a subdirectory called "upfiles")

Note: You need to change the URLs or the Apache configuration if "fileuploader"
 or "fileupload.exe" changes (for example on Linux it is not fileupload.exe).
Also, if your server is listening on port 80 instead of 8080, you can leave 
the :8080 part from the calling URL:
http://127.0.0.1/fileuploader/listfiles


2.b; as Apache module
---------------------
Usually it works if you put the template (uploadform.html) into the Apache 
main directory (not the DocumentRoot, but the main Apache directory), under 
sub-directory "templates" or something similar. Adjust the file path in the 
web module (webmodule.pas) accordingly.

http://<WebServer>/<ApacheLocationName>/listfiles should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/fileuploader/listfiles
or http://127.0.0.1/fileuploader/listfiles
if in httpd.conf it was set up as:
LoadModule mod_fileupload "<path_to_mod>/fileupload.dll"
<Location /fileuploader>
    SetHandler mod_fileupload
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "fileuploader" changes. 
Also, for example on Linux the module can be libfileupload.so and not 
fileupload.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation might fail because the file is in use 
(Apache modules stay in the memory). So first, you always need to stop the 
server before you recompile or before you copy over the new version of the 
newly created module.
On Linux, it is enough to simply reload Apache after recompile.


2.c; as FCGI
------------
Usually it works if you put the template (uploadform.html) next to the FCGI 
executable file. Adjust the file path in the web module (webmodule.pas) 
accordingly.

http://<WebServer>/<ApacheScriptAliasName>/listfiles should start the example 
if everything is set up properly.
ex: http://127.0.0.1:8080/fileuploader/func1call
or  http://127.0.0.1/fileuploader/func1call
if in the Apache configuration file (ex: httpd.conf) it was set up as:

LoadModule fastcgi_module "<path_to_mod>/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "<path_to_fcgi_app>">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiExternalServer "<path_to_fcgi_app>/fileupload.exe" -host 127.0.0.1:2015 -idle-timeout 30 -flush
  ScriptAlias /fileuploader "<path_to_fcgi_app>/fileupload.exe"
</IfModule>

Note: You need to change the module name if needed. For example on Linux, 
the module is not mod_fastcgi-2.4.6-AP22.dll but mod_fastcgi.so (need to be 
compiled from sources found at http://www.fastcgi.com/dist/ ).
The port (2015 in this example) must match the one set in the project main 
file (fileupload.lpr).
The FCGI application must be running in order for this demo to work (external 
FCGI server setup). Do not forget to restart it after changes and 
recompilation.
Also, mod_fastcgi is not the same as mod_fcgid that the Apache project is 
developing. The latter does not support external FCGI server apps.
There are other ways than external FCGI server apps to handle the FCGI 
protocol and both mod_fastcgi and mod_fcgid supports that. However, external 
FCGI servers are the best for debugging and development, that is why the 
examples do it that way.