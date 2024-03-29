;;;; package.lisp
(defpackage #:matrix-moonbot
  (:use #:cl
        #:lmav2)
  (:nicknames #:moonbot #:lunamech)
  (:export #:*commands*
           #:*api*
           #:module-moonmat-message
           #:is-me-p
           #:request-open-id-token
           #:*url*
           #:*luna*
           #:setup-and-go
           #:moonmat-message
           #:lunamat-message
           #:luna-message
           #:timer
           #:timers
           #:timestamp
           #:execute-module-communications
           #:register-module
           #:module-information
           #:new-module
           #:defmodule
           #:pass-to-module
           #:find-module
           #:find-timer
           #:make-timer
           #:make-timers
           #:make-ubermensch
           #:remove-ubermensch
           #:make-module-admin
           #:retract-module-admin
           #:update-permission
           #:with-per-module-permissions
           #:determine-permissions
           #:find-permissions
           #:accept-invites-from
           #:stop-accepting-invites-from
           #:find-and-reset-timer
           #:find-types-in-rooms-timeline
           #:homeserver
           #:same-homeserver-p
           #:moonhelp
           #:add-admin
           #:add-command
           #:add-correct-con-to-community
           #:add-listen-in
           #:start-moonbot
           #:stop-moonbot
           #:add-room
           #:%emergency-format
           #:report-condition-to-matrix
           #:execute-stamp-n-after-luna
           #:parallel-p
           #:add-rooms
           #:add-string->condition
           #:pkv
           #:safe-execution
           #:admin
           #:admin-change-a-users-device-data
           #:admin-delete-room
           #:admin-shadow-ban-user-id
           #:admin-change-user-to/from-administrator
           #:admin-make-user-id-room-admin
           #:module-command
           #:admin-community-command
           #:admin-create-or-modify-account
           #:admin-deactivate-account
           #:admin-delete-a-device
           #:admin-delete-a-users-devices
           #:admin-delete-request
           #:admin-get-request
           #:admin-global-command
           #:admin-list-all-of-a-user-devices
           #:admin-list-user-accounts
           #:admin-post-request
           #:admin-privilege
           #:admin-put-request
           #:admin-query-current-sessions-for-user
           #:admin-query-user-account
           #:admin-reset-password
           #:admin-server-administrator
           #:admin-show-a-users-device
           #:admin-delete-local-media-before-time
           #:admin-delete-local-media
           #:admin-delete-remote-media-beforetime
           #:admin-force-user-to-join-room
           #:admin-whois
           #:admins
           #:aliases
           #:add-new-alias
           #:already-processed
           #:api
           #:api-error-args
           #:api-error-code
           #:api-error-description
           #:api-error-error
           #:auth
           #:auth-req
           #:avatar-url
           #:ban-from-community
           #:ban-user-from-room
           #:banning-from
           #:bytes
           #:catch-limit-exceeded
           #:command
           #:command-type
           #:commands
           #:commands-admin
           #:communities
           #:community
           #:find-community
           #:community->connection
           #:community-command
           #:community-prefix-p
           #:condition-sym
           #:config
           #:config->moonbot
           #:config-missing
           #:config-missing-message
           #:connection
           #:connections
           #:content
           #:content-bytes
           #:content-type
           #:control-string
           #:create-room
           #:create-private-room
           #:creation-content
           #:curl
           #:cycle-history
           #:data
           #:deactivated
           #:determine-privilege
           #:device-id
           #:display-name
           #:displayname
           #:doc-string
           #:docs
           #:docs-string
           #:downcase-symbols
           #:echo
           #:entry
           #:err
           #:err-count
           #:error-val
           #:event
           #:event-id
           #:events-in-room
           #:execute
           #:execute-command
           #:extra
           #:extract-command-and-args
           #:extract-message
           #:extract-messages
           #:failed-to-parse-msg
           #:files
           #:filter
           #:force-stop
           #:found-modules
           #:from
           #:from-end
           #:gen-headers
           #:gen-url
           #:get-params
           #:get-request
           #:get-string->condition
           #:global-config
           #:global-config-to-list
           #:grab-config
           #:grab-latest-config
           #:grab-messages-and-process
           #:guests
           #:headers
           #:inform-command-is-missing
           #:inform-user-of-error
           #:initiate-command-execution
           #:initiate-message-receive
           #:invalid-arguments
           #:invalid-arguments-arguments
           #:invalid-command
           #:invalid-command-command
           #:invalid-command-message
           #:invalid-prefix
           #:invite
           #:invite-member-to-room
           #:invite-to-community
           #:inviting-to
           #:invoker
           #:item
           #:join-room
           #:leave-room
           #:joined-rooms
           #:kick-from-community
           #:kick-user-from-room
           #:kicking-from
           #:latest-sync
           #:limit
           #:list-community->community-object
           #:list-of-devices
           #:list-of-lists
           #:listen-and-process
           #:listen-in
           #:locate-command
           #:locate-module
           #:logout-devices
           #:m-bad-json
           #:m-image
           #:m-invalid-param
           #:m-limit-exceeded
           #:m-message
           #:m-missing-token
           #:m-not-found
           #:m-not-json
           #:m-room-in-use
           #:m-text
           #:m-unauthorized
           #:m-unknown
           #:m-unknown-token
           #:m-unrecognized
           #:make-auth
           #:make-command
           #:make-connection
           #:make-event
           #:media
           #:members
           #:members-in-room
           #:members-in-room%ids
           #:members-in-room-ids
           #:message
           #:message-community
           #:message-event
           #:message-from?
           #:message-process-failure
           #:message-process-failure-culprit
           #:message-process-failure-message
           #:messages
           #:messages-in-room
           #:missing-command
           #:missing-expected-key
           #:missing-module
           #:module
           #:background-module
           #:module-error
           #:module-error-message
           #:module-error-module
           #:modules
           #:moon-mapc
           #:moon-message
           #:moonbot
           #:moonbot->config
           #:moonbot-still-running
           #:moonbot-still-running-message
           #:moonmat
           #:msgtype
           #:msoc
           #:name
           #:new-admin-community-command
           #:new-normie-community-command
           #:new-normie-global-command
           #:new-pass
           #:new-password
           #:new-r-t
           #:new-request-type
           #:new-val
           #:normie-privilege
           #:object
           #:object-to-list
           #:origin-server-ts
           #:populate-community
           #:preset
           #:print-command-information
           #:priv
           #:private
           #:privilege
           #:privilege-required
           #:process-message
           #:process-messages
           #:public-rooms
           #:put-request
           #:put-request-object
           #:rate-limit
           #:reason
           #:reason-why
           #:remove-room
           #:req
           #:requires-args-p
           #:resignal
           #:resp
           #:response
           #:response-var
           #:return-nil
           #:room-alias
           #:room-id
           #:rooms
           #:mimetype
           #:bot-member-id-p 
           #:non-bot-members
           #:send-image-bytes-to-room
           #:send-file-bytes-to-room
           #:send-image-file-to-room
           #:send-message-event-to-room
           #:send-message-to-room
           #:sender
           #:sigcon-when-nil-p
           #:signal-condition-from-response
           #:get-user-presence
           #:user-online-p
           #:start
           #:stop
           
           #:strings
           #:symbol/function
           #:fun
           #:m-forbidden
           #:create-new-validator
           #:args
           #:format-command
           #:clean-user-id
           #:event-content
           #:m-room-name
           #:send-event-to-room
           #:send-state-event-to-room
           #:find-room-either
           #:find-room-by-id
           #:find-room-by-name
           #:plist-key-val
           #:sync
           #:syntax
           #:tell-user
           #:text
           #:timeout
           #:to
           #:to-add
           #:to-remove
           #:to-send
           #:topic
           #:total-failure
           #:total-failure-caught-condition
           #:type-find
           #:type-remove
           
           #:download-content
           #:user-profile-url
           #:on-sync
           #:on-save
           #:on-cycle
           #:moonbot-condition
           #:m-file
           #:file-info
           #:on-load-up
           #:hotload-module
           #:unload-module
           #:api-timeout
           #:get-validator
           #:on-module-hotload
           #:on-shutdown
           #:on-module-unload
           #:find-messages-from-rooms
           #:validation-failed
           #:m-direct
           #:direct-content
           #:add-to-account-data
           #:get-account-data
           #:membership-events
           #:conn
           #:room-leaves
           #:room-joins
           #:arg-count
           #:list-of-lists->validators
           #:arg-list
           #:args-from-validation-lists
           #:module-already-loaded
           #:ubermensch
           #:ubermensch-privilege
           #:me-privilege
           #:permissions
           #:can-remove-ubermensch-p
           #:all-ubermensch
           #:retract-module-admin
           #:make-module-admin
           #:unban-user-from-room
           #:upload-content
           #:use-media-instead
           #:user
           #:user-display-name
           #:user-id
           #:username
           #:val
           #:valid-user-p
           #:validate-community
           #:validate-room
           #:validate-user
           #:validation-list
           #:generate-and-upload-listen-in-filter
           #:filters
           #:key
           #:id
           #:last-sync-string
           #:command-defining-macro-no-moonbot
           #:command-defining-macro-moonbot
           #:rooms-id
           #:rooms-name))

