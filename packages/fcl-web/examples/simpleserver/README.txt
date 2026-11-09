
Small demo for simple file module. The server will listen on a specified
port (default 3000) and will serve files starting from the current working
directory.

Just start the server, no options, and point your browser at
http://localhost:3000/

running simpleserver -h will provide the following help:

-d --directory=dir

  Base directory from which to serve files.
  Default is current working directory: /home/michael/FPC/trunk/packages/fcl-web/examples/simpleserver

-i --indexpage=name

  Directory index page to use (default: index.html)

-n --noindexpage

  Do not allow index page.

-p --port=NNNN

  TCP/IP port to listen on (default is 3000)

-m --mimetypes=file

  path of mime.types file. Loaded in addition to OS known types.

  A sample mime.types file is provided.

-q --quiet

  Do not write diagnostic messages

-s --ssl

  Use SSL.
  If you set this, the -H or --hostname option must also be used.

-H --hostname=NAME
  set hostname for self-signed SSL certificate

-C --certificate=FILE

  PEM file with the SSL certificate. Implies --ssl.
  Without this option, --ssl generates a self-signed certificate.

-k --key=FILE

  PEM file with the private key of the certificate.

-K --key-password=PWD

  Password of the private key file, if it is encrypted.

-R --ca=FILE

  PEM file with the CA certificate chain.

The same settings can be put in the [Server] section of the configuration
file (simpleserver.ini):

  [Server]
  Certificate=/path/to/cert.pem
  PrivateKey=/path/to/key.pem
  KeyPassword=secret
  CACertificate=/path/to/ca.pem

A certificate and key for testing can be created with openssl:

  openssl req -x509 -newkey rsa:2048 -nodes -days 365 \
    -keyout key.pem -out cert.pem -subj "/CN=localhost"

and used with:

  simpleserver --certificate=cert.pem --key=key.pem

after which the server is reachable on https://localhost:3000/
(the browser will warn that the certificate is not trusted).

