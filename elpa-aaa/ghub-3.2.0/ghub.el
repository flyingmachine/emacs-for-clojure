;;; ghub.el --- minuscule client libraries for Git forge APIs  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GPL see https://www.gnu.org/licenses/gpl.txt.

;;; Commentary:

;; Ghub provides basic support for using the APIs of various Git forges
;; from Emacs packages.  Originally it only supported the Github REST
;; API, but now it also supports the Github GraphQL API as well as the
;; REST APIs of Gitlab, Gitea, Gogs and Bitbucket.

;; Ghub abstracts access to API resources using only a handful of basic
;; functions such as `ghub-get'.  These are convenience wrappers around
;; `ghub-request'.  Additional forge-specific wrappers like `glab-put',
;; `gtea-put', `gogs-post' and `buck-delete' are also available.  Ghub
;; does not provide any resource-specific functions, with the exception
;; of `FORGE-repository-id'.

;; When accessing Github, then Ghub handles the creation and storage of
;; access tokens using a setup wizard to make it easier for users to get
;; started.  The tokens for other forges have to be created manually.

;; Ghub is intentionally limited to only provide these two essential
;; features — basic request functions and guided setup — to avoid being
;; too opinionated, which would hinder wide adoption.  It is assumed that
;; wide adoption would make life easier for users and maintainers alike,
;; because then all packages that talk to forge APIs could be configured
;; the same way.

;; Please consult the manual (info "ghub") for more information.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'let-alist)
(require 'url)
(require 'url-auth)
(require 'url-http)

(eval-when-compile
  (require 'subr-x))

(defvar url-callback-arguments)
(defvar url-http-end-of-headers)
(defvar url-http-extra-headers)
(defvar url-http-response-status)

;;; Settings

(defconst ghub-default-host "api.github.com"
  "The default host that is used if `ghub.host' is not set.")

(defvar ghub-github-token-scopes '(repo)
  "The Github API scopes that your private tools need.

The token that is created based on the value of this variable
is used when `ghub-request' (or one of its wrappers) is called
without providing a value for AUTH.  Packages should always
identify themselves using that argument, but when you use Ghub
directly in private tools, then that is not necessary and the
request is made on behalf of the `ghub' package itself, aka on
behalf of some private tool.

By default the only requested scope is `repo' because that is
sufficient as well as required for most common uses.  This and
other scopes are documented at URL `https://magit.vc/goto/2e586d36'.

If your private tools need other scopes, then you have to add
them here *before* creating the token.  Alternatively you can
edit the scopes of an existing token using the web interface
at URL `https://github.com/settings/tokens'.")

(defvar ghub-override-system-name nil
  "If non-nil, the string used to identify the local machine.
If this is nil, then the value returned by `system-name' is
used instead.")

;;; Request
;;;; Object

(cl-defstruct (ghub--req
               (:constructor ghub--make-req)
               (:copier nil))
  (url        nil :read-only nil)
  (forge      nil :read-only t)
  (silent     nil :read-only t)
  (method     nil :read-only t)
  (headers    nil :read-only t)
  (handler    nil :read-only t)
  (unpaginate nil :read-only nil)
  (noerror    nil :read-only t)
  (reader     nil :read-only t)
  (callback   nil :read-only t)
  (errorback  nil :read-only t)
  (value      nil :read-only nil)
  (extra      nil :read-only nil))

(defalias 'ghub-req-extra 'ghub--req-extra)

;;;; API

(define-error 'ghub-error "Ghub/Url Error" 'error)
(define-error 'ghub-http-error "HTTP Error" 'ghub-error)

(defvar ghub-response-headers nil
  "The headers returned in response to the last request.
`ghub-request' returns the response body and stores the
response headers in this variable.")

(cl-defun ghub-head (resource &optional params
                              &key query payload headers
                              silent unpaginate noerror reader
                              username auth host
                              callback errorback extra)
  "Make a `HEAD' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"HEAD\" as METHOD."
  (ghub-request "HEAD" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-get (resource &optional params
                             &key query payload headers
                             silent unpaginate noerror reader
                             username auth host
                             callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.
Like calling `ghub-request' (which see) with \"GET\" as METHOD."
  (ghub-request "GET" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-put (resource &optional params
                             &key query payload headers
                             silent unpaginate noerror reader
                             username auth host
                             callback errorback extra)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PUT\" as METHOD."
  (ghub-request "PUT" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-post (resource &optional params
                              &key query payload headers
                              silent unpaginate noerror reader
                              username auth host
                              callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"POST\" as METHOD."
  (ghub-request "POST" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-patch (resource &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               username auth host
                               callback errorback extra)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"PATCH\" as METHOD."
  (ghub-request "PATCH" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-delete (resource &optional params
                                &key query payload headers
                                silent unpaginate noerror reader
                                username auth host
                                callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.
Like calling `ghub-request' (which see) with \"DELETE\" as METHOD."
  (ghub-request "DELETE" resource params
                :query query :payload payload :headers headers
                :silent silent :unpaginate unpaginate
                :noerror noerror :reader reader
                :username username :auth auth :host host
                :callback callback :errorback errorback :extra extra))

(cl-defun ghub-request (method resource &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               username auth host forge
                               callback errorback value extra)
  "Make a request for RESOURCE and return the response body.

Also place the response headers in `ghub-response-headers'.

METHOD is the HTTP method, given as a string.
RESOURCE is the resource to access, given as a string beginning
  with a slash.

PARAMS, QUERY, PAYLOAD and HEADERS are alists used to specify
  data.  The Github API documentation is vague on how data has
  to be transmitted and for a particular resource usually just
  talks about \"parameters\".  Generally speaking when the METHOD
  is \"HEAD\" or \"GET\", then they have to be transmitted as a
  query, otherwise as a payload.
Use PARAMS to automatically transmit like QUERY or PAYLOAD would
  depending on METHOD.
Use QUERY to explicitly transmit data as a query.
Use PAYLOAD to explicitly transmit data as a payload.
  Instead of an alist, PAYLOAD may also be a string, in which
  case it gets encoded as UTF-8 but is otherwise transmitted as-is.
Use HEADERS for those rare resources that require that the data
  is transmitted as headers instead of as a query or payload.
  When that is the case, then the API documentation usually
  mentions it explicitly.

If SILENT is non-nil, then don't message progress reports and
  the like.

If UNPAGINATE is t, then make as many requests as necessary to
  get all values.  If UNPAGINATE is a natural number, then get
  at most that many pages.  For any other non-nil value raise
  an error.
If NOERROR is non-nil, then do not raise an error if the request
  fails and return nil instead.  If NOERROR is `return', then
  return the error payload instead of nil.
If READER is non-nil, then it is used to read and return from the
  response buffer.  The default is `ghub--read-json-payload'.
  For the very few resources that do not return JSON, you might
  want to use `ghub--decode-payload'.

If USERNAME is non-nil, then make a request on behalf of that
  user.  It is better to specify the user using the Git variable
  `github.user' for \"api.github.com\", or `github.HOST.user' if
  connecting to a Github Enterprise instance.

Each package that uses `ghub' should use its own token. If AUTH
  is nil, then the generic `ghub' token is used instead.  This
  is only acceptable for personal utilities.  A packages that
  is distributed to other users should always use this argument
  to identify itself, using a symbol matching its name.

  Package authors who find this inconvenient should write a
  wrapper around this function and possibly for the
  method-specific functions as well.

  Some symbols have a special meaning.  `none' means to make an
  unauthorized request.  `basic' means to make a password based
  request.  If the value is a string, then it is assumed to be
  a valid token.  `basic' and an explicit token string are only
  intended for internal and debugging uses.

  If AUTH is a package symbol, then the scopes are specified
  using the variable `AUTH-github-token-scopes'.  It is an error
  if that is not specified.  See `ghub-github-token-scopes' for
  an example.

If HOST is non-nil, then connect to that Github instance.  This
  defaults to \"api.github.com\".  When a repository is connected
  to a Github Enterprise instance, then it is better to specify
  that using the Git variable `github.host' instead of using this
  argument.

If FORGE is `gitlab', then connect to Gitlab.com or, depending
  on HOST, to another Gitlab instance.  This is only intended for
  internal use.  Instead of using this argument you should use
  function `glab-request' and other `glab-*' functions.

If CALLBACK and/or ERRORBACK is non-nil, then make one or more
  asynchronous requests and call CALLBACK or ERRORBACK when
  finished.  If no error occurred, then call CALLBACK, unless
  that is nil.

  If an error occurred, then call ERRORBACK, or if that is nil,
  then CALLBACK.  ERRORBACK can also be t, in which case an error
  is signaled instead.  NOERROR is ignored for all asynchronous
  requests.

Both callbacks are called with four arguments.
  1. For CALLBACK, the combined value of the retrieved pages.
     For ERRORBACK, the error that occured when retrieving the
     last page.
  2. The headers of the last page as an alist.
  3. Status information provided by `url-retrieve'. Its `:error'
     property holds the same information as ERRORBACK's first
     argument.
  4. A `ghub--req' struct, which can be passed to `ghub-continue'
     (which see) to retrieve the next page, if any."
  (cl-assert (or (booleanp unpaginate) (natnump unpaginate)))
  (unless (string-prefix-p "/" resource)
    (setq resource (concat "/" resource)))
  (unless host
    (setq host (ghub--host forge)))
  (unless (or username (stringp auth) (eq auth 'none))
    (setq username (ghub--username host forge)))
  (cond ((not params))
        ((member method '("GET" "HEAD"))
         (when query
           (error "PARAMS and QUERY are mutually exclusive for METHOD %S"
                  method))
         (setq query params))
        (t
         (when payload
           (error "PARAMS and PAYLOAD are mutually exclusive for METHOD %S"
                  method))
         (setq payload params)))
  (when (or callback errorback)
    (setq noerror t))
  (ghub--retrieve
   (ghub--encode-payload payload)
   (ghub--make-req
    :url (url-generic-parse-url
          (concat "https://" host resource
                  (and query (concat "?" (ghub--url-encode-params query)))))
    :forge forge
    :silent silent
    ;; Encode in case caller used (symbol-name 'GET). #35
    :method     (encode-coding-string method 'utf-8)
    :headers    (ghub--headers headers host auth username forge)
    :handler    'ghub--handle-response
    :unpaginate unpaginate
    :noerror    noerror
    :reader     reader
    :callback   callback
    :errorback  errorback
    :value      value
    :extra      extra)))

(defun ghub-continue (req)
  "If there is a next page, then retrieve that.

This function is only intended to be called from callbacks.  If
there is a next page, then retrieve that and return the buffer
that the result will be loaded into, or t if the process has
already completed.  If there is no next page, then return nil.

Callbacks are called with four arguments (see `ghub-request').
The forth argument is a `ghub--req' struct, intended to be passed
to this function.  A callback may use the struct's `extra' slot
to pass additional information to the callback that will be
called after the next request has finished.  Use the function
`ghub-req-extra' to get and set the value of this slot."
  (and (assq 'next (ghub-response-link-relations req))
       (or (ghub--retrieve nil req) t)))

(cl-defun ghub-wait (resource &optional duration &key username auth host)
  "Busy-wait up to DURATION seconds for RESOURCE to become available.

DURATION specifies how many seconds to wait at most.  It defaults
to 64 seconds.  The first attempt is made immediately, the second
after two seconds, and each subsequent attempt is made after
waiting as long again as we already waited between all preceding
attempts combined.

See `ghub-request' for information about the other arguments."
  (unless duration
    (setq duration 64))
  (with-local-quit
    (let ((total 0))
      (while (not (ghub-get resource nil
                            :noerror t
                            :username username
                            :auth auth
                            :host host))
        (message "Waited (%3ss of %ss) for %s..." total duration resource)
        (if (= total duration)
            (error "Github is taking too long to create %s" resource)
          (if (> total 0)
              (let ((wait (min total (- duration total))))
                (sit-for wait)
                (cl-incf total wait))
            (sit-for (setq total 2))))))))

(defun ghub-response-link-relations (req &optional headers payload)
  "Return an alist of link relations in HEADERS.
If optional HEADERS is nil, then return those that were
previously stored in the variable `ghub-response-headers'.

When accessing a Bitbucket instance then the link relations
are in PAYLOAD instead of HEADERS, making their API merely
RESTish and forcing this function to append those relations
to the value of `ghub-response-headers', for later use when
this function is called with nil for PAYLOAD."
  (if (eq (ghub--req-forge req) 'bitbucket)
      (if payload
          (let* ((page (cl-mapcan (lambda (key)
                                    (when-let ((elt (assq key payload)))
                                      (list elt)))
                                  '(size page pagelen next previous)))
                 (headers (cons (cons 'link-alist page) headers)))
            (if (and req (or (ghub--req-callback req)
                             (ghub--req-errorback req)))
                (setq-local ghub-response-headers headers)
              (setq-default ghub-response-headers headers))
            page)
        (cdr (assq 'link-alist ghub-response-headers)))
  (when-let ((rels (cdr (assoc "Link" (or headers ghub-response-headers)))))
    (mapcar (lambda (elt)
              (pcase-let ((`(,url ,rel) (split-string elt "; ")))
                (cons (intern (substring rel 5 -1))
                      (substring url 1 -1))))
            (split-string rels ", ")))))

(cl-defun ghub-repository-id (owner name &key username auth host forge noerror)
  "Return the id of the specified repository.
Signal an error if the id cannot be determined."
  (let ((fn (intern (format "%s-repository-id" (or forge 'ghub)))))
    (or (funcall (if (eq fn 'ghub-repository-id) 'ghub--repository-id fn)
                 owner name :username username :auth auth :host host)
        (and (not noerror)
             (error "Repository %S does not exist on %S.\n%s%S?"
                    (concat owner "/" name)
                    (or host (ghub--host host))
                    "Maybe it was renamed and you have to update "
                    "remote.<remote>.url")))))

;;;; Internal

(cl-defun ghub--retrieve (payload req)
  (let ((url-request-extra-headers
         (let ((headers (ghub--req-headers req)))
           (if (functionp headers) (funcall headers) headers)))
        (url-request-method (ghub--req-method req))
        (url-request-data payload)
        (url-show-status nil)
        (url     (ghub--req-url req))
        (handler (ghub--req-handler req))
        (silent  (ghub--req-silent req)))
    (if (or (ghub--req-callback  req)
            (ghub--req-errorback req))
        (url-retrieve url handler (list req) silent)
      ;; When this function has already been called, then it is a
      ;; no-op.  Otherwise it sets `url-registered-auth-schemes' among
      ;; other things.  If we didn't ensure that it has been run, then
      ;; `url-retrieve-synchronously' would do it, which would cause
      ;; the value that we let-bind below to be overwritten, and the
      ;; "default" value to be lost outside the let-binding.
      (url-do-setup)
      (with-current-buffer
          (let ((url-registered-auth-schemes
                 '(("basic" ghub--basic-auth-errorback . 10))))
            (url-retrieve-synchronously url silent))
        (funcall handler (car url-callback-arguments) req)))))

(defun ghub--handle-response (status req)
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((unpaginate (ghub--req-unpaginate req))
                 (headers    (ghub--handle-response-headers status req))
                 (payload    (ghub--handle-response-payload req))
                 (payload    (ghub--handle-response-error status payload req))
                 (value      (ghub--handle-response-value payload req))
                 (prev       (ghub--req-url req))
                 (next       (cdr (assq 'next (ghub-response-link-relations
                                               req headers payload)))))
            (when (numberp unpaginate)
              (cl-decf unpaginate))
            (setf (ghub--req-url req)
                  (url-generic-parse-url next))
            (setf (ghub--req-unpaginate req) unpaginate)
            (or (and next
                     unpaginate
                     (or (eq unpaginate t)
                         (>  unpaginate 0))
                     (ghub-continue req))
                (let ((callback  (ghub--req-callback req))
                      (errorback (ghub--req-errorback req))
                      (err       (plist-get status :error)))
                  (cond ((and err errorback)
                         (setf (ghub--req-url req) prev)
                         (funcall (if (eq errorback t)
                                      'ghub--errorback
                                    errorback)
                                  err headers status req))
                        (callback
                         (funcall callback value headers status req))
                        (t value))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ghub--handle-response-headers (status req)
  (goto-char (point-min))
  (forward-line 1)
  (let (headers)
    (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                              url-http-end-of-headers t)
      (push (cons (match-string 1)
                  (match-string 2))
            headers))
    (setq headers (nreverse headers))
    (unless url-http-end-of-headers
      (error "BUG: missing headers %s" (plist-get status :error)))
    (goto-char (1+ url-http-end-of-headers))
    (if (and req (or (ghub--req-callback req)
                     (ghub--req-errorback req)))
        (setq-local ghub-response-headers headers)
      (setq-default ghub-response-headers headers))
    headers))

(defun ghub--handle-response-error (status payload req)
  (let ((noerror (ghub--req-noerror req))
        (err (plist-get status :error)))
    (if err
        (if noerror
            (if (eq noerror 'return)
                payload
              (setcdr (last err) (list payload))
              nil)
          (ghub--signal-error err payload req))
      payload)))

(defun ghub--signal-error (err &optional payload req)
  (pcase-let ((`(,symb . ,data) err))
    (if (eq symb 'error)
        (if (eq (car-safe data) 'http)
            (signal 'ghub-http-error
                    (let ((code (car (cdr-safe data))))
                      (list code
                            (nth 2 (assq code url-http-codes))
                            (and req (url-filename (ghub--req-url req)))
                            payload)))
          (signal 'ghub-error data))
      (signal symb data))))

(defun ghub--errorback (err _headers _status req)
  (ghub--signal-error err (nth 3 err) req))

(defun ghub--handle-response-value (payload req)
  (setf (ghub--req-value req)
        (nconc (ghub--req-value req)
               (if-let ((nested (and (eq (ghub--req-forge req) 'bitbucket)
                                     (assq 'values payload))))
                   (cdr nested)
                 payload))))

(defun ghub--handle-response-payload (req)
  (funcall (or (ghub--req-reader req)
               'ghub--read-json-payload)
           url-http-response-status))

(defun ghub--read-json-payload (_status)
  (let ((raw (ghub--decode-payload)))
    (and raw
         (condition-case nil
             (let ((json-object-type 'alist)
                   (json-array-type  'list)
                   (json-key-type    'symbol)
                   (json-false       nil)
                   (json-null        nil))
               (json-read-from-string raw))
           (json-readtable-error
            `((message
               . ,(if (looking-at "<!DOCTYPE html>")
                      (if (re-search-forward
                           "<p>\\(?:<strong>\\)?\\([^<]+\\)" nil t)
                          (match-string 1)
                        "error description missing")
                    (string-trim (buffer-substring (point) (point-max)))))
              (documentation_url
               . "https://github.com/magit/ghub/wiki/Github-Errors")))))))

(defun ghub--decode-payload (&optional _status)
  (and (not (eobp))
       (decode-coding-string
        (buffer-substring-no-properties (point) (point-max))
        'utf-8)))

(defun ghub--encode-payload (payload)
  (and payload
       (progn
         (unless (stringp payload)
           (setq payload (json-encode-list payload)))
         (encode-coding-string payload 'utf-8))))

(defun ghub--url-encode-params (params)
  (mapconcat (lambda (param)
               (pcase-let ((`(,key . ,val) param))
                 (concat (url-hexify-string (symbol-name key)) "="
                         (if (integerp val)
                             (number-to-string val)
                           (url-hexify-string val)))))
             params "&"))

;;; Authentication
;;;; API

;;;###autoload
(defun ghub-create-token (host username package scopes)
  "Create, store and return a new token.

HOST is the Github instance, usually \"api.github.com\".
USERNAME is the name of a user on that instance.
PACKAGE is the package that will use the token.
SCOPES are the scopes the token is given access to."
  (interactive
   (pcase-let ((`(,host ,username ,package)
                (ghub--read-triplet)))
     (list host username package
           (split-string
            (read-string
             "Scopes (separated by commas): "
             (mapconcat #'symbol-name
                        (symbol-value
                         (intern (format "%s-github-token-scopes" package)))
                        ","))
            "," t "[\s\t]+"))))
  (let ((user (ghub--ident username package)))
    (cl-destructuring-bind (save token)
        (ghub--auth-source-get (list :save-function :secret)
          :create t :host host :user user
          :secret
          (cdr (assq 'token
                     (ghub-post
                      "/authorizations"
                      `((scopes . ,scopes)
                        (note   . ,(ghub--ident-github package)))
                      :username username :auth 'basic :host host))))
      ;; Build-in back-ends return a function that does the actual
      ;; saving, while for some third-party back-ends ":create t"
      ;; is enough.
      (when (functionp save)
        (funcall save))
      ;; If the Auth-Source cache contains the information that there
      ;; is no value, then setting the value does not invalidate that
      ;; now incorrect information.
      ;; The (:max 1) is needed and has to be placed at the
      ;; end for Emacs releases before 26.1.  #24 #64 #72
      (auth-source-forget (list :host host :user user :max 1))
      token)))

;;;###autoload
(defun ghub-token-scopes (host username package)
  "Return and echo the scopes of the specified token.
This is intended for debugging purposes only.  The user
has to provide several values including their password."
  (interactive (ghub--read-triplet))
  (let ((scopes
         (cdr (assq 'scopes (ghub--get-token-plist host username package)))))
    (when (called-interactively-p 'any)
      ;; Also show the input values to make it easy for package
      ;; authors to verify that the user has done it correctly.
      (message "Scopes for %s@%s: %S"
               (ghub--ident username package)
               host scopes))
    scopes))

;;;###autoload
(defun ghub-clear-caches ()
  "Clear all caches that might negatively affect Ghub.

If a library that is used by Ghub caches incorrect information
such as a mistyped password, then that can prevent Ghub from
asking the user for the correct information again.

Set `url-http-real-basic-auth-storage' to nil
and call `auth-source-forget+'."
  (interactive)
  (setq url-http-real-basic-auth-storage nil)
  (auth-source-forget+))

;;;; Internal

(defun ghub--headers (headers host auth username forge)
  (push (cons "Content-Type" "application/json") headers)
  (if (eq auth 'none)
      headers
    (unless (or username (stringp auth))
      (setq username (ghub--username host forge)))
    (lambda ()
      (if (eq auth 'basic)
          (cons (cons "Authorization" (ghub--basic-auth host username))
                headers)
        (cons (ghub--auth host auth username forge) headers)))))

(defun ghub--auth (host auth &optional username forge)
  (unless username
    (setq username (ghub--username host)))
  (if (eq auth 'basic)
      (cl-ecase forge
        ((nil github gitea gogs bitbucket)
         (cons "Authorization" (ghub--basic-auth host username)))
        (gitlab
         (error "Gitlab does not support basic authentication")))
    (cons (cl-ecase forge
            ((nil github gitea gogs bitbucket)
             "Authorization")
            (gitlab
             "Private-Token"))
          (concat
           (and (not (eq forge 'gitlab)) "token ")
           (encode-coding-string
            (cl-typecase auth
              (string auth)
              (null   (ghub--token host username 'ghub nil forge))
              (symbol (ghub--token host username auth  nil forge))
              (t (signal 'wrong-type-argument
                         `((or stringp symbolp) ,auth))))
            'utf-8)))))

(defun ghub--basic-auth (host username)
  (let ((url (url-generic-parse-url (concat "https://" host))))
    (setf (url-user url) username)
    (url-basic-auth url t)))

(defun ghub--basic-auth-errorback (url &optional prompt _overwrite _realm _args)
  ;; This gets called twice.  Do nothing the first time,
  ;; when PROMPT is nil.  See `url-get-authentication'.
  (when prompt
    (if (assoc "X-GitHub-OTP" (ghub--handle-response-headers nil nil))
        (progn
          (setq url-http-extra-headers
                `(("Content-Type" . "application/json")
                  ("X-GitHub-OTP" . ,(ghub--read-2fa-code))
                  ;; Without "Content-Type" and "Authorization".
                  ;; The latter gets re-added from the return value.
                  ,@(cddr url-http-extra-headers)))
          ;; Return the cached values, they are correct.
          (url-basic-auth url nil nil nil))
      ;; Remove the invalid cached values and fail, which
      ;; is better than the invalid values sticking around.
      (setq url-http-real-basic-auth-storage
            (cl-delete (format "%s:%d" (url-host url) (url-port url))
                       url-http-real-basic-auth-storage
                       :test #'equal :key #'car))
      nil)))

(defun ghub--token (host username package &optional nocreate forge)
  (let* ((user (ghub--ident username package))
         (token
          (or (car (ghub--auth-source-get (list :secret)
                     :host host :user user))
              (progn
                ;; Auth-Source caches the information that there is no
                ;; value, but in our case that is a situation that needs
                ;; fixing so we want to keep trying by invalidating that
                ;; information.
                ;; The (:max 1) is needed and has to be placed at the
                ;; end for Emacs releases before 26.1.  #24 #64 #72
                (auth-source-forget (list :host host :user user :max 1))
                (and (not nocreate)
                     (cl-ecase forge
                       ((nil github)
                        (ghub--confirm-create-token host username package))
                       ((gitlab gitea gogs bitbucket)
                        (error "Required %s token (%S for %S) does not exist.
See https://magit.vc/manual/ghub/Support-for-Other-Forges.html for instructions."
                               (capitalize (symbol-name forge))
                               user host))))))))
    (if (functionp token) (funcall token) token)))

(defun ghub--host (&optional forge)
  (cl-ecase forge
    ((nil github)
     (or (ignore-errors (car (process-lines "git" "config" "github.host")))
         ghub-default-host))
    (gitlab
     (or (ignore-errors (car (process-lines "git" "config" "gitlab.host")))
         (bound-and-true-p glab-default-host)))
    (gitea
     (or (ignore-errors (car (process-lines "git" "config" "gitea.host")))
         (bound-and-true-p gtea-default-host)))
    (gogs
     (or (ignore-errors (car (process-lines "git" "config" "gogs.host")))
         (bound-and-true-p gogs-default-host)))
    (bitbucket
     (or (ignore-errors (car (process-lines "git" "config" "bitbucket.host")))
         (bound-and-true-p buck-default-host)))))

(defun ghub--username (host &optional forge)
  (let ((var
         (cl-ecase forge
           ((nil github)
            (if (equal host ghub-default-host)
                "github.user"
              (format "github.%s.user" host)))
           (gitlab
            (if (equal host "gitlab.com/api/v4")
                "gitlab.user"
              (format "gitlab.%s.user" host)))
           (bitbucket
            (if (equal host "api.bitbucket.org/2.0")
                "bitbucket.user"
              (format "bitbucket.%s.user" host)))
           (gitea
            (when (zerop (call-process "git" nil nil nil "config" "gitea.host"))
              (error "gitea.host is set but always ignored"))
            (format "gitea.%s.user" host))
           (gogs
            (when (zerop (call-process "git" nil nil nil "config" "gogs.host"))
              (error "gogs.host is set but always ignored"))
            (format "gogs.%s.user"  host)))))
    (condition-case nil
        (car (process-lines "git" "config" var))
      (error
       (let ((user (read-string
                    (format "Git variable `%s' is unset.  Set to: " var))))
         (or (and user (progn (call-process "git" nil nil nil
                                            "config" "--global" var user)
                              user))
             (user-error "Abort")))))))

(defun ghub--ident (username package)
  (format "%s^%s" username package))

(defun ghub--ident-github (package)
  (format "Emacs package %s @ %s"
          package
          (or ghub-override-system-name (system-name))))

(defun ghub--package-scopes (package)
  (let ((var (intern (format "%s-github-token-scopes" package))))
    (if (boundp var)
        (symbol-value var)
      (error "%s fails to define %s" package var))))

(defun ghub--confirm-create-token (host username package)
  (let* ((ident (ghub--ident-github package))
         (scopes (ghub--package-scopes package))
         (max-mini-window-height 40))
    (if (let ((message-log-max nil))
          (yes-or-no-p
           (format
            "Such a Github API token is not available:

  Host:    %s
  User:    %s
  Package: %s

  Scopes requested in `%s-github-token-scopes':\n%s
  Store on Github as:\n    %S
  Store locally according to option `auth-sources':\n    %S
%s
If in doubt, then abort and first view the section of
the Ghub documentation called \"Interactively Creating
and Storing a Token\".

Otherwise confirm and then provide your Github username and
password at the next two prompts.  Depending on the backend
you might have to provide a passphrase and confirm that you
really want to save the token.

Create and store such a token? "
            host username package package
            (mapconcat (lambda (scope) (format "    %s" scope)) scopes "\n")
            ident auth-sources
            (if (and (stringp (car auth-sources))
                     (not (string-suffix-p ".gpg" (car auth-sources))))
                (format "
WARNING: The token will be stored unencrypted in %S.
         If you don't want that, you have to abort and customize
         the `auth-sources' option.\n" (car auth-sources))
              ""))))
        (condition-case ghub--create-token-error
            ;; Naively attempt to create the token since the user told us to
            (ghub-create-token host username package scopes)
          ;; The API _may_ respond with the fact that a token of the name
          ;; we wanted already exists. At this point we're out of luck. We
          ;; don't have a token (otherwise why would we be here?) and, if
          ;; the user is using SMS 2FA, we have no way of telling GitHub
          ;; to send a new 2FA code to the user other than sending a POST
          ;; to /authorizations which is ugly.
          ;;
          ;; If they are not using SMS 2FA then we could try to delete the
          ;; existing token (which will require them to hand us another
          ;; OTP for the delete request) and then call create again,
          ;; possibly requiring _another_ OTP if they don't do things fast
          ;; enough, but this is only because non-SMS 2FA doesn't require
          ;; any action on GitHub's part.
          ;;
          ;; GitHub does hand us a header that indicates what type of 2FA
          ;; is in use, but it's not currently available in this location
          ;; and would make the following code which is already quite
          ;; complicated even more complicated. So in the interest of
          ;; simplicity it's better to error out here and ask the user to
          ;; take action. This situation should almost never arise anyway.
          (ghub-http-error
           (if (string-equal (let-alist (nth 3 ghub--create-token-error)
                               (car .errors.code))
                             "already_exists")
               (error "\
A token named %S already exists on Github. \
Please visit https://github.com/settings/tokens and delete it." ident))))
      (user-error "Abort"))))

(defun ghub--get-token-id (host username package)
  (let ((ident (ghub--ident-github package)))
    (cl-some (lambda (x)
               (let-alist x
                 (and (equal .app.name ident) .id)))
             (ghub-get "/authorizations"
                       '((per_page . 100))
                       :unpaginate t
                       :username username :auth 'basic :host host))))

(defun ghub--get-token-plist (host username package)
  (ghub-get (format "/authorizations/%s"
                    (ghub--get-token-id host username package))
            nil :username username :auth 'basic :host host))

(defun ghub--delete-token (host username package)
  (ghub-delete (format "/authorizations/%s"
                       (ghub--get-token-id host username package))
               nil :username username :auth 'basic :host host))

(defun ghub--read-triplet ()
  (let ((host (read-string "Host: " (ghub--host))))
    (list host
          (read-string "Username: " (ghub--username host))
          (intern (read-string "Package: " "ghub")))))

(defvar ghub--2fa-cache nil)

(defun ghub--read-2fa-code ()
  (let ((code (read-number "Two-factor authentication code: "
                           (and ghub--2fa-cache
                                (< (float-time (time-subtract
                                                (current-time)
                                                (cdr ghub--2fa-cache)))
                                   25)
                                (car ghub--2fa-cache)))))
    (setq ghub--2fa-cache (cons code (current-time)))
    (number-to-string code)))

(defun ghub--auth-source-get (keys &rest spec)
  (declare (indent 1))
  (let ((plist (car (apply #'auth-source-search
                           (append spec (list :max 1))))))
    (mapcar (lambda (k)
              (plist-get plist k))
            keys)))

(advice-add 'auth-source-netrc-parse-next-interesting :around
            'auth-source-netrc-parse-next-interesting@save-match-data)
(defun auth-source-netrc-parse-next-interesting@save-match-data (fn)
  "Save match-data for the benefit of caller `auth-source-netrc-parse-one'.
Without wrapping this function in `save-match-data' the caller
won't see the secret from a line that is followed by a commented
line."
  (save-match-data (funcall fn)))

;;; _
(provide 'ghub)
(require 'ghub-graphql)
;;; ghub.el ends here
