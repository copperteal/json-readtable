;;; Copyright 2025 J. David Taylor
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright notice,
;;;      this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the copyright holder nor the names of its
;;;      contributors may be used to endorse or promote products derived from
;;;      this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package #:json-readtable)

;; TODO: How does inlining affect performance?
(declaim (inline next-char next-char-skip pop-char next-value
                 json-reader-report
                 read-pair
                 read-codepoint read-surrogate-pair read-escape
                 read-integer)
         ;; TODO: optimize for speed after testing
         (optimize (compilation-speed 0)
                   (debug 3)
                   (safety 3)
                   (space 0)
                   (speed 0)))

(defun next-char (stream)
  (declare (type stream stream))
  "Peek at the next char in `STREAM'.  At end of file return `NIL'."
  (peek-char nil stream nil nil t))

(defun next-char-skip (stream)
  (declare (type stream stream))
  "Skip to and peek at the next non-whitespace char in `STREAM'.  At end of file
return `NIL'."
  (peek-char t stream nil nil t))

(defun pop-char (stream)
  (declare (type stream stream))
  "Read the next char in `STREAM'.  At end of file return `NIL'."
  (read-char stream nil nil t))

(defun next-value (stream)
  (declare (type stream stream))
  "Read the next value in `STREAM'.  At end of file signal an error."
  (read stream t nil t))

(defun json-reader-report (stream context datum expected)
  (declare (type stream stream)
           (type string context)
           (type (or null character string) datum)
           (type (or null string) expected))
  (format stream (concatenate 'string
                              "While reading ~A, found "
                              (if (characterp datum) "~@C" "~A")
                              (if expected " instead of ~A." "."))
          context (or datum "EOF") expected))

(define-condition json-reader-error (reader-error)
  ((context :reader json-reader-error-context
            :initarg :context
            :initform "JSON"
            :type string)
   (datum :reader json-reader-error-datum
          :initarg :datum
          ;; TODO: Should we use a special `EOF' object/symbol?
          :initform nil ; `NIL' means "EOF"
          :type (or null character string))
   (expected :reader json-reader-error-expected
             :initarg :expected
             :initform nil
             :type (or null string)))
  (:documentation
   "A `JSON-READER-ERROR' is the error condition that arises from reading invalid
JSON.")
  (:report
   (lambda (condition stream)
     (declare (type json-reader-error condition)
              (type stream stream))
     (json-reader-report stream
                         (json-reader-error-context condition)
                         (json-reader-error-datum condition)
                         (json-reader-error-expected condition)))))

(defun read-pair (stream)
  (declare (type stream stream))
  "Read a colon-separated name-value pair from `STREAM'."
  (cons
   (next-value stream) ; get name
   (let ((char (next-char-skip stream)))
     (declare (type (or null character) char)
              (dynamic-extent char))
     (assert (and char ; handle EOF
                  (char= char #\:))
             (char)
             'json-reader-error
             :context "pair"
             :datum char
             :expected "a colon separator"
             :stream stream)
     (pop-char stream) ; consume colon
     (next-value stream)))) ; get value

(declaim (type (member :hash-table :alist :plist) *json-object*))

(defvar *json-object* :hash-table
  "`*JSON-OBJECT*' is the target type for JSON objects.

Default value: `:HASH-TABLE'

Valid values: `:HASH-TABLE' | `:ALIST' | `:PLIST'")

(defun read-object (stream char)
  (declare (type stream stream)
           (type character char))
  (assert (char= char #\{)
          (char)
          'json-reader-error
          :context "object"
          :datum char
          :expected "an opening brace"
          :stream stream)
  (assert (and (setq char (next-char-skip stream)) ; handle EOF
               (or (char= char #\")
                   (char= char #\})))
          (char)
          'json-reader-error
          :context "object"
          :datum char
          :expected "a name or closing brace"
          :stream stream)
  (do (object)
      ((char= char #\})
       (pop-char stream) ; consume right brace
       ;; ensure unique names
       (loop for (head . tail) on object
             for name = (car head)
             when (member name tail :key #'car :test #'string=)
             do (error 'json-reader-error
                       :context "object"
                       :datum (format nil "duplicate name ~S" name)
                       :stream stream))
       (ecase *json-object*
         (:hash-table
          (loop with hash-table = (make-hash-table :test #'equal)
                for (name . value) in (nreverse object)
                do (setf (gethash name hash-table) value)
                finally (return hash-table)))
         (:alist
          (nreverse object))
         (:plist
          (loop with plist
                for (name . value) in (nreverse object)
                do (push value plist)
                   (push name plist)
                finally (return plist)))))
    (declare (type list object))
    (push (read-pair stream) object) ; get name-value pair
    (assert (and (setq char (next-char-skip stream)) ; handle EOF
                 (or (char= char #\,)
                     (char= char #\})))
            (char)
            'json-reader-error
            :context "object"
            :datum char
            :expected "a comma separator or closing brace"
            :stream stream)
    (when (char= char #\,)
      (pop-char stream) ; consume comma
      (assert (and (setq char (next-char-skip stream)) ; handle EOF
                   (char= char #\"))
              (char)
              'json-reader-error
              :context "object"
              :datum char
              :expected "a name"
              :stream stream))))

(declaim (type (member :vector :list) *json-array*))

(defvar *json-array* :vector
  "`JSON-ARRAY*' is the target type for JSON arrays.

Default value: `:VECTOR'

Valid values: `:VECTOR' | `:LIST'")

(defun read-array (stream char)
  (declare (type stream stream)
           (type character char))
  (assert (char= char #\[)
          (char)
          'json-reader-error
          :context "array"
          :datum char
          :expected "an opening bracket"
          :stream stream)
  (assert (setq char (next-char-skip stream)) ; handle EOF
          nil
          'json-reader-error
          :context "array"
          :expected "a JSON value or closing bracket"
          :stream stream)
  (do (array)
      ((char= char #\])
       (pop-char stream) ; consume right bracket
       (ecase *json-array*
         (:vector
          (coerce (nreverse array) 'vector))
         (:list
          (nreverse array))))
    (declare (type list array))
    (push (next-value stream) array) ; get array item
    (assert (and (setq char (next-char-skip stream)) ; handle EOF
                 (or (char= char #\,)
                     (char= char #\])))
            (char)
            'json-reader-error
            :context "array"
            :datum char
            :expected "a comma separator or closing bracket"
            :stream stream)
    (when (char= char #\,)
      (pop-char stream) ; consume comma
      (assert (and (setq char (next-char-skip stream)) ; handle EOF
                   (char/= char #\]))
              (char)
              'json-reader-error
              :context "array"
              :datum char
              :expected "a JSON value"
              :stream stream))))

(defun read-codepoint (stream)
  (declare (type stream stream))
  (let ((hexcode (make-string 4
                              :initial-element
                              #+sbcl #\Nul
                              #-sbcl (code-char 0))))
    (declare (type (string 4) hexcode))
    (read-sequence hexcode stream)
    (assert (char/= (char hexcode 3) ; handle EOF
                    #+sbcl #\Nul
                    #-sbcl (code-char 0))
            ()
            'json-reader-error
            :context "Unicode codepoint"
            :expected "four hex digits"
            :stream stream)
    (let ((code (parse-integer hexcode :radix 16)))
      (declare (type unsigned-byte code))
      (values code
              (cond
                ((<= #xD800 code #xDBFF)
                 'leading-surrogate)
                ((<= #xDC00 code #xDFFF)
                 'trailing-surrogate))
              hexcode))))

(defun read-surrogate-pair (stream leading-surrogate)
  (declare (type stream stream)
           (type unsigned-byte leading-surrogate))
  (multiple-value-bind (trailing-surrogate surrogate-type hexcode)
      (read-codepoint stream)
    (declare (type unsigned-byte trailing-surrogate)
             (type (member leading-surrogate trailing-surrogate nil) surrogate-type)
             (type (string 4) hexcode)
             (dynamic-extent trailing-surrogate surrogate-type hexcode))
    (assert (eq surrogate-type 'trailing-surrogate)
            nil
            'json-reader-error
            :context "UTF-16 surrogate pair"
            :datum (format nil "\\u~A" hexcode)
            :expected "a trailing UTF-16 surrogate"
            :stream stream)
    (code-char (+ (ash (- leading-surrogate #xD800) 10)
                  (- trailing-surrogate #xDC00)
                  #x10000))))

(defun read-escape (stream)
  (declare (type stream stream))
  (let ((char (pop-char stream)))
    (declare (type (or null character) char))
    (assert char ; handle EOF
            nil
            'json-reader-error
            :context "escape sequence"
            :expected "a character"
            :stream stream)
    (case char
      (#\b
       #\Backspace)
      (#\f
       #\Formfeed)
      (#\n
       #\Linefeed)
      (#\r
       #\Return)
      (#\t
       #\Tab)
      (#\u
       (multiple-value-bind (code surrogate-type hexcode)
           (read-codepoint stream)
         (declare (type unsigned-byte code)
                  (type (member leading-surrogate trailing-surrogate nil) surrogate-type)
                  (type (string 4) hexcode)
                  (dynamic-extent code surrogate-type hexcode))
         (assert (or (null surrogate-type)
                     (eq surrogate-type 'leading-surrogate))
                 nil
                 'json-reader-error
                 :context "Unicode escape"
                 :datum (format nil "\\u~A" hexcode)
                 :expected "a character in the Basic Multilingual Plane or a leading UTF-16 surrogate"
                 :stream stream)
         (unless surrogate-type ; `CHAR' is in the Basic Multilingual Plane
           (return-from read-escape (code-char code)))
         (assert (and (setq char (pop-char stream))
                      (char= char #\\)
                      (setq char (pop-char stream))
                      (char= char #\u))
                 (char)
                 'json-reader-error
                 :context "UTF-16 surrogate pair"
                 :datum char
                 :expected "a Unicode escape sequence"
                 :stream stream)
         (read-surrogate-pair stream code)))
      (t
       char))))

(defun read-string (stream char)
  (declare (type stream stream)
           (type character char))
  (assert (char= char #\")
          (char)
          'json-reader-error
          :context "string"
          :datum char
          :expected "an opening quote"
          :stream stream)
  (assert (setq char (pop-char stream)) ; handle EOF
          nil
          'json-reader-error
          :context "string"
          :expected "a character or closing quote"
          :stream stream)
  (with-output-to-string (string)
    (declare (type stream string))
    (do ()
        ((char= char #\"))
      (assert (not (or (char= char #\/)
                       (<= 0 (char-code char) #x1F)))
              (char)
              'json-reader-error
              :context "string"
              :datum (format nil "unescaped ~@C" char)
              :stream stream)
      (write-char (if (char= char #\\)
                      (read-escape stream)
                      char)
                  string)
      (assert (setq char (pop-char stream)) ; handle EOF
              nil
              'json-reader-error
              :context "string"
              :expected "a character or closing quote"
              :stream stream))))

;;; This macro was taken from the SBCL manual
(macrolet ((define-constant (name value &optional doc)
             `(defconstant ,name (if (boundp ',name)
                                     (symbol-value ',name)
                                     ,value)
                ,@(when doc (list doc)))))
  (define-constant +digits+ "0123456789"))

(defun read-integer (stream)
  ;; It is the caller's responsibility to ensure that the lead char of `STREAM'
  ;; is a digit.
  (declare (type stream stream))
  (let ((digits 0))
    (declare (type unsigned-byte digits))
    (values (parse-integer
             (with-output-to-string (string)
               (declare (type stream string))
               (do ((char (next-char stream)
                          (next-char stream)))
                   ((not (and char ; handle EOF
                              (find char +digits+ :test #'char=))))
                 (declare (type (or null character) char)
                          (dynamic-extent char))
                 (write-char (pop-char stream) string)
                 (incf digits))))
            digits)))

(defun read-number (stream char)
  (declare (type stream stream)
           (type character char))
  (assert (find char +digits+ :test #'char=)
          (char)
          'json-reader-error
          :context "number"
          :datum char
          :expected "a digit"
          :stream stream)
  (let ((integer 0) (fraction 0) (digits 0) (exponent 0) (floatp nil))
    (declare (type unsigned-byte integer fraction digits exponent)
             (type boolean floatp)
             (dynamic-extent integer fraction digits exponent floatp))
    ;; integer part
    (unless (char= char #\0)
      (unread-char char stream)
      (setq integer (read-integer stream)))
    ;; fraction part
    (when (and (setq char (next-char stream)) ; handle EOF
               (char= char #\.))
      (setq floatp t)
      (pop-char stream) ; consume decimal point
      (assert (and (setq char (next-char stream)) ; handle EOF
                   (find char +digits+ :test #'char=))
              (char)
              'json-reader-error
              :context "number"
              :datum char
              :expected "a digit"
              :stream stream)
      (multiple-value-setq (fraction digits) (read-integer stream)))
    ;; exponent
    (when (and (setq char (next-char stream)) ; handle EOF
               (or (char= char #\e)
                   (char= char #\E)))
      (pop-char stream) ; consume exponent marker
      (assert (and (setq char (next-char stream)) ; handle EOF
                   (or (char= char #\-)
                       (char= char #\+)
                       (find char +digits+ :test #'char=)))
              (char)
              'json-reader-error
              :context "number"
              :datum char
              :expected "a sign or digit"
              :stream stream)
      (setq exponent (case char
                       ((#\-)
                        (pop-char stream) ; consume sign
                        (assert (and (setq char (next-char stream))
                                     (find char +digits+ :test #'char=))
                                (char)
                                'json-reader-error
                                :context "number"
                                :datum char
                                :expected "a digit"
                                :stream stream)
                        (- (read-integer stream)))
                       ((#\+)
                        (pop-char stream) ; consume sign
                        (assert (and (setq char (next-char stream))
                                     (find char +digits+ :test #'char=))
                                (char)
                                'json-reader-error
                                :context "number"
                                :datum char
                                :expected "a digit"
                                :stream stream)
                        (read-integer stream))
                       (t
                        (read-integer stream)))))
    (let ((number (* (+ integer (/ fraction (expt 10 digits)))
                     (expt 10 exponent))))
      (declare (type rational number))
      (if floatp
          (float number)
          number))))

(defun read-negative-number (stream char)
  (declare (type stream stream)
           (type character char))
  (assert (char= char #\-)
          (char)
          'json-reader-error
          :context "negative number"
          :datum char
          :expected "a minus sign"
          :stream stream)
  (assert (setq char (pop-char stream)) ; handle EOF
          nil
          'json-reader-error
          :context "negative number"
          :expected "a digit"
          :stream stream)
  (- (read-number stream char)))

(defmacro define-json-literal (symbol default)
  (declare (type symbol symbol)
           (type t default))
  (let* ((symbol-name/u (symbol-name symbol))
         (symbol-name/d (string-downcase symbol-name/u))
         (default-sym (intern (concatenate 'string "*JSON-" symbol-name/u "*")))
         (function-name (intern (concatenate 'string "READ-" symbol-name/u)))
         (head (char symbol-name/d 0))
         (tail (subseq symbol-name/d 1))
         (length (length tail))
         (default-fill (char (make-string 1) 0))
         (use-alternate-fill-p (char= default-fill (char tail (1- length))))
         (alternate-fill (if (char= default-fill (code-char 0))
                             (code-char 1) (code-char 0))))
    (declare (type string symbol-name/u symbol-name/d)
             (type symbol default-sym function-name)
             (type character head)
             (type string tail)
             (type unsigned-byte length))
    `(progn
       (defvar ,default-sym ,default
         ,(format nil
                  "`~A' is the target for JSON ~A.

Default value: `~A'"
                  default-sym
                  symbol-name/d
                  default))
       (defun ,function-name (stream char)
         (declare (type stream stream)
                  (type character char))
         (assert (char= char ,head)
                 (char)
                 'json-reader-error
                 :expected ,symbol-name/d
                 :datum char
                 :expected ,head
                 :stream stream)
         (let ((tail (make-string ,length
                                  ;; Handle the possibility that the final
                                  ;; character matches the implementation's
                                  ;; default fill.
                                  ,.(when use-alternate-fill-p
                                      `(:initial-element ,alternate-fill)))))
           (declare (type (string ,length) tail)
                    (dynamic-extent tail))
           (read-sequence tail stream)
           (assert (char/= (char tail ,(1- length)) ; handle EOF
                           ,(if use-alternate-fill-p
                                alternate-fill
                                default-fill))
                   ()
                   'json-reader-error
                   :context ,symbol-name/d
                   :stream stream)
           (let ((mismatch (mismatch tail ,tail :test #'char=)))
             (declare (type (or null unsigned-byte) mismatch)
                      (dynamic-extent mismatch))
             (assert (not mismatch)
                     nil
                     'json-reader-error
                     :context ,symbol-name/d
                     :datum (char tail mismatch)
                     :stream stream)
             ,default-sym))))))

(define-json-literal true t)
(define-json-literal false nil)
(define-json-literal null :null)

(defun read-invalid (stream char)
  (declare (type stream stream)
           (type character char))
  (error 'json-reader-error
         :datum (format nil "invalid character ~@C" char)
         :stream stream))

(defparameter *json-readtable*
  (let ((readtable (copy-readtable nil)))
    (declare (type readtable readtable))
    (setf (readtable-case readtable) :preserve)
    (loop for code from 0 below 128
          for char = (code-char code)
          unless (member char '(#\Space #\Tab #\Linefeed #\Return))
          do (set-macro-character (code-char code)
                                  #'read-invalid nil readtable))
    (loop for (c f) in '((#\{ read-object)
                         (#\[ read-array)
                         (#\" read-string)
                         (#\t read-true)
                         (#\f read-false)
                         (#\n read-null))
          do (set-macro-character c f nil readtable))
    (loop for d across +digits+
          do (set-macro-character d #'read-number nil readtable))
    (set-macro-character #\- #'read-negative-number nil readtable)
    readtable)
  "A readtable for JSON.")

(defmacro with-json-readtable ((&key
                                (object nil object-supplied-p)
                                (array nil array-supplied-p)
                                (true nil true-supplied-p)
                                (false nil false-supplied-p)
                                (null nil null-supplied-p))
                               &body body)
  "Execute `BODY' as an implicit `PROGN' with `*READTABLE*' bound to
`*JSON-READTABLE*'.

Optionally, change the targets of JSON values via keyword arguments."
  `(let ((*readtable* *json-readtable*)
         ,@(when object-supplied-p
             `((*json-object* ,object)))
         ,@(when array-supplied-p
             `((*json-array* ,array)))
         ,@(when true-supplied-p
             `((*json-true* ,true)))
         ,@(when false-supplied-p
             `((*json-false* ,false)))
         ,@(when null-supplied-p
             `((*json-null* ,null))))
     ,@body))
