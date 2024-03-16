# Firebase Cloud Messaging demo

This demo show how to use the fpfcmsender unit to send push notification messages to
all kinds of devices using Google Firebase Cloud Messaging services.

## Setup

### You need a Firebase project. 

### Web Client setup:

   - For this project, under Messaging - WebPush, you need to create a VAPID key. 
     The value of this key must be entered in the webclient, in the project file:
     webclient/webclient.lpr, in the constant "TheVAPIDKey".

   - The firebase application config must be saved in a config.js file, with
     the following content (the keys must obviously be filled with the right
     content): 
```
  var firebaseConfig = {
    apiKey: "",
    authDomain: "",
    projectId: "",
    storageBucket: "",
    messagingSenderId: "",
    appId: ""
   } 

### Server setup

For the server project, you need to create a service account for your
firebase application, and download the configuration file for this account. 
This is a JSON file which contains the credentials for the service
account.

The JSON should be saved in a file called
```
messagingserver-serviceaccount.json
```
It will be loaded by the server when communicaton with FCM servers is
needed.

### HTTPS 

Normally, the files and JSON-RPC calls should be using the HTTPS protocol.
So either 

- You configure the server to use SSL and provide a certificate.
- You set up a webserver with HTTP snd forward the requests to the
  application server
- For testing purposes, you can configure the browser to accept HTTP for
  localhost requests and allow a service worker on http.

### Runnng the client
The client application needs to be compiled with pas2js. 
When executed in the browser, the 'Register' button must be used to register
the application with Firebase. The browser will ask you if notifications must be
allowed, and you must allow this or the application will not function.

When done, you can enter a message and press the 'Send' button to send a
notification message to yourself.
