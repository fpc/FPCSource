This is the Free Pascal debug server tool.

It's design goals and usage principle are the same as the ones for the
gdebug tools found in the gexperts collection for Delphi. 
(see http://www.gexperts.org/)
However, it is a totally new implementation, designed to be cross-platform
and with more options such as a remote debug server.

The interface of the dbugintf unit is designed to be more or less compatible 
with the dbugintf unit og gexperts.

To use this:

- Start the debug server (gtk or console version)
  (the unix socket version of dbugintf will attempt to start it if it
   is not yet started. The inet socket version will not)

- Include 'dbugintf' unit in your program/unit's 'uses' clause.
  
- Include 'SendDebug()' statements wherever needed in possible.
  (see other possible statements in the dbugintf unit file)

- Enjoy output of debugserver program !

Michael.
