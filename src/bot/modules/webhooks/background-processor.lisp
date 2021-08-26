(in-package #:mm-module.webhook)
#||
This file contains the code to manage the execution of a hook in the background.
This means that hooks have to be uniquely identifiable and the unique id has 
to be returned to the caller on the other side so they can check in and 
see the state of their hook at the time.
||#
