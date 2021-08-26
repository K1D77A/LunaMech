(in-package #:mm-module.direct-message)

(defmethod jojo:%to-json ((obj context))
  (jojo:with-object
    (jojo:write-key-value "completed" (slot-value obj 'completep))
    (jojo:write-key-value "start-time" (slot-value obj 'start-time))
    (when (slot-boundp obj 'completed-time)
      (jojo:write-key-value "completed-time" (slot-value obj 'completed-time)))
    (jojo:write-key-value "results" (slot-value obj 'results))))

(defmethod jojo:%to-json ((obj result))
  (jojo:with-object
    (jojo:write-key-value "id" (slot-value obj 'id))
    (jojo:write-key-value "value" (if (slot-boundp obj 'value)
                                      (slot-value obj 'value)
                                      nil))
    (jojo:write-key-value "validator" (slot-value obj 'validator))))

(defmethod jojo:%to-json ((obj direct-message-module))
  (declare (special context-wanted))
  (jojo:with-object
    (jojo:write-key-value "completed-contexts" (slot-value obj 'completed-contexts))
    (jojo:write-key-value "failed-contexts" (slot-value obj 'failed-contexts))))

(defmethod module-to-bytes ((mod direct-message-module))
  (babel:string-to-octets (jojo:to-json mod)))

(defmethod upload-module (connection room-id (mod direct-message-module))
  (let* ((bytes (module-to-bytes mod))
         (info (make-instance 'file-info :size (length bytes) :mimetype "application/json")))
    (send-file-bytes-to-room connection room-id "contexts.json" bytes info)))
