# LunaMech (Luna for short)
LunaMech is a featureful and modular Bot to be used for general administration with
Matrix server. LunaMech started as a way to create large private communities that are
invite only as Matrix in its current form doesn't have anything similar to 'guilds'
on Discord, so LunaMech has been built around the idea of communities. Because of the
idea of creating "communities" using LunaMech, commands are prefixed with community
names and commands that start with community names only impact that community.

for more information visit: [lunamech.com](https://lunamech.com/)


## Communities

The current community based commands executable by an administrator in that
community are:
* populate-community
* help
* add-admin
* admins
* rooms
* members
* invite-to-community
* ban-from-community
* kick-from-community
* displayname
* add-rooms
* add-room
* remove-room
* message-community
* add-listen-in
* rate-limit
* echo
* create-room
* remove-admin
* member-count
* join-room
* find-room
* remove-listen-in


The prefix for these commands is the name of the community ie .<name> ie .deep-lore invite-to-community <user-id>.

## Modules

LunaMech is also modular, a large amount of its functionality is implemented as
external modules that can be loaded/unloaded as required. The current modules are:

1. Admin.lisp

This module adds commands that effect LunaMech directly.
Its prefix is .admin
The current commands are:
* unload-module
* hotload-module
* room-count
* create-local-community
* copy-admins
* list-communities
* print-ubermensch
* remove-ubermensch
* add-ubermensch
* member-count

2. Compass

This module is provides a way for LunaMech to take a political compass score and
return an image with the score plotted.
Its prefix is .compass
The current commands are:
* All
* id
* self
* add
* plot-xy
* help

3. Huginn (this is a massive work in progress)

This module is supposed to be an interface between the bot and Huginn.
Its prefix is .huginn 
The current commands are:

* send
* echo
* help

Do not use this module yet.

4. Luna

This module is for commands that any user can use.
Its prefix it .luna
The current commands are:
* help
* apologize 
* hi

5. Direct-message 

The module consists of two parts
* Contexts - These are the environments that the bot creates
for each DM; each context has a set of functions the user can use, an initial message
a helper function, a set of values that can be set by the user. These can be defined
quite easily.

* Collectors - These are used to do something with the collected results, each collector is associated with a type of context, the only useful one that currently exists is 
`add-to-compass` which will take the results grabbed by the compass context and add them 
to the the compass results.

Its prefix it .direct-message
Its not very hard to add new contexts and collectors due to the way its designed.

The current commands are: 
* message-open-dms
* collect-results
* community-compass-collect
* clear-completed-contexts
* clear-failed-contexts
* get-contexts
* name-failed
* collectors
* contexts
* current-dms
* leave-dms
* send-dm
* help
