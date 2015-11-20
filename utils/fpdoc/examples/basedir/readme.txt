This directory demonstrates the use of a fpdoc project file.
It uses the files in the examples/simple directory.

The project file contains the names of the files without paths.
That means that fpdoc must be executed from this directory, 
supplying the paths to the input and description files

fpdoc --project=sample-project.xml --base-input-dir=../simple --base-descr-dir=../simple

The docs will be written to a subdirectory doc. 
This directory can be deleted if it is no longer necessary.
