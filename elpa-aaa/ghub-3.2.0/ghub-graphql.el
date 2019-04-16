;;; ghub-graphql.el --- access Github API using GrapthQL  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/ghub

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

;;; Code:

(require 'dash)
(require 'ghub)
(require 'graphql)
(require 'treepy)

(eval-when-compile
  (require 'subr-x))

;;; Api

(cl-defun ghub-graphql (graphql &optional variables
                                &key username auth host
                                silent
                                callback errorback value extra)
  "Make a GraphQL request using GRAPHQL and VARIABLES.
Return the response as a JSON-like alist.  Even if the response
contains `errors', do not raise an error.  GRAPHQL is a GraphQL
string.  VARIABLES is a JSON-like alist.  The other arguments
behave as for `ghub-request' (which see)."
  (cl-assert (stringp graphql))
  (cl-assert (not (stringp variables)))
  (ghub-request "POST" "/graphql" nil :payload
                (json-encode `(("query" . ,graphql)
                               ,@(and variables `(("variables" ,@variables)))))
                :silent silent
                :username username :auth auth :host host
                :callback callback :errorback errorback
                :extra extra :value value))

(cl-defun ghub-graphql-rate-limit (&key username auth host)
  "Return rate limit information."
  (let-alist (ghub-graphql
              "query { rateLimit { limit cost remaining resetAt }}"
              nil :username username :auth auth :host host)
    .data.rateLimit))

(cl-defun ghub--repository-id (owner name &key username auth host)
  "Return the id of the repository specified by OWNER, NAME and HOST."
  (let-alist (ghub-graphql
              "query ($owner:String!, $name:String!) {
                 repository(owner:$owner, name:$name) { id }
               }"
              `((owner . ,owner)
                (name  . ,name))
              :username username :auth auth :host host)
    .data.repository.id))

;;; Api (drafts)

(defconst ghub-fetch-repository
  '(query
    (repository
     [(owner $owner String!)
      (name  $name  String!)]
     name
     id
     createdAt
     updatedAt
     nameWithOwner
     (parent nameWithOwner)
     description
     homepageUrl
     (defaultBranchRef name)
     isArchived
     isFork
     isLocked
     isMirror
     isPrivate
     hasIssuesEnabled
     hasWikiEnabled
     (licenseInfo name)
     (stargazers totalCount)
     (watchers totalCount)
     (assignableUsers [(:edges t)]
                      id
                      login
                      name)
     (issues         [(:edges t)
                      (:singular issue number)
                      (orderBy ((field . UPDATED_AT) (direction . DESC)))]
                     number
                     state
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     locked
                     (milestone id)
                     body
                     (assignees [(:edges t)]
                                id)
                     (comments  [(:edges t)]
                                databaseId
                                (author login)
	                        createdAt
	                        updatedAt
                                body)
                     (labels    [(:edges t)]
                                id))
     (labels         [(:edges t)
                      (:singular label id)]
                     id
                     name
                     color
                     description)
     (pullRequests   [(:edges t)
                      (:singular pullRequest number)
                      (orderBy ((field . UPDATED_AT) (direction . DESC)))]
                     number
                     state
                     (author login)
                     title
                     createdAt
                     updatedAt
                     closedAt
                     mergedAt
                     locked
                     maintainerCanModify
                     isCrossRepository
                     (milestone id)
                     body
                     (baseRef name
                              (repository nameWithOwner))
                     (headRef name
                              (repository (owner login)
                                          nameWithOwner))
                     (assignees [(:edges t)]
                                id)
                     (comments  [(:edges t)]
                                databaseId
                                (author login)
                                createdAt
	                        updatedAt
	                        body)
                     (labels    [(:edges t)]
                                id)))))

(cl-defun ghub-fetch-repository (owner name callback
                                       &optional until
                                       &key username auth host forge)
  "Asynchronously fetch forge data about the specified repository.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum ghub-fetch-repository
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge))

(cl-defun ghub-fetch-issue (owner name number callback
                                  &optional until
                                  &key username auth host forge)
  "Asynchronously fetch forge data about the specified issue.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum (ghub--graphql-prepare-query
                         ghub-fetch-repository
                         `(repository issues (issue . ,number)))
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository issue)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge))

(cl-defun ghub-fetch-pullreq (owner name number callback
                                    &optional until
                                    &key username auth host forge)
  "Asynchronously fetch forge data about the specified pull-request.
Once all data has been collected, CALLBACK is called with the
data as the only argument."
  (ghub--graphql-vacuum (ghub--graphql-prepare-query
                         ghub-fetch-repository
                         `(repository pullRequests (pullRequest . ,number)))
                        `((owner . ,owner)
                          (name  . ,name))
                        callback until
                        :narrow   '(repository pullRequest)
                        :username username
                        :auth     auth
                        :host     host
                        :forge    forge))

;;; Internal

(cl-defstruct (ghub--graphql-req
               (:include ghub--req)
               (:constructor ghub--make-graphql-req)
               (:copier nil))
  (query     nil :read-only t)
  (variables nil :read-only t)
  (until     nil :read-only t)
  (buffer    nil :read-only t)
  (pages     0   :read-only nil))

(cl-defun ghub--graphql-vacuum (query variables callback
                                      &optional until
                                      &key narrow username auth host forge)
  "Make a GraphQL request using QUERY and VARIABLES.
See Info node `(ghub)GraphQL Support'."
  (unless host
    (setq host (ghub--host forge)))
  (unless (or username (stringp auth) (eq auth 'none))
    (setq username (ghub--username host forge)))
  (ghub--graphql-retrieve
   (ghub--make-graphql-req
    :url       (url-generic-parse-url (concat "https://" host "/graphql"))
    :method    "POST"
    :headers   (ghub--headers nil host auth username forge)
    :handler   'ghub--graphql-handle-response
    :query     query
    :variables variables
    :until     until
    :buffer    (current-buffer)
    :callback  (let ((buf (current-buffer)))
                 (if narrow
                     (lambda (data)
                       (let ((path narrow) key)
                         (while (setq key (pop path))
                           (setq data (cdr (assq key data)))))
                       (ghub--graphql-set-mode-line buf nil)
                       (funcall callback data))
                   (lambda (data)
                     (ghub--graphql-set-mode-line buf nil)
                     (funcall callback data)))))))

(cl-defun ghub--graphql-retrieve (req &optional lineage cursor)
  (let ((p (cl-incf (ghub--graphql-req-pages req))))
    (when (> p 1)
      (ghub--graphql-set-mode-line req "Fetching page %s" p)))
  (ghub--retrieve
   (let ((json-false nil))
     (ghub--encode-payload
      `((query     . ,(ghub--graphql-encode
                       (ghub--graphql-prepare-query
                        (ghub--graphql-req-query req)
                        lineage cursor)))
        (variables . ,(ghub--graphql-req-variables req)))))
   req))

(defun ghub--graphql-prepare-query (query &optional lineage cursor)
  (when lineage
    (setq query (ghub--graphql-narrow-query query lineage cursor)))
  (let ((loc (ghub--alist-zip query))
        variables)
    (cl-block nil
      (while t
        (let ((node (treepy-node loc)))
          (when (vectorp node)
            (let ((alist (cl-coerce node 'list))
                  vars)
              (when (assq :edges alist)
                (push (list 'first 100) vars)
                (setq loc  (treepy-up loc))
                (setq node (treepy-node loc))
                (setq loc  (treepy-replace
                            loc `(,(car  node)
                                  ,(cadr node)
                                  (pageInfo endCursor hasNextPage)
                                  (edges (node ,@(cddr node))))))
                (setq loc  (treepy-down loc))
                (setq loc  (treepy-next loc)))
              (dolist (elt alist)
                (cond ((eq (car elt) :alias)
                       (push elt vars))
                      ((keywordp (car elt)))
                      ((= (length elt) 3)
                       (push (list (nth 0 elt)
                                   (nth 1 elt)) vars)
                       (push (list (nth 1 elt)
                                   (nth 2 elt)) variables))
                      ((= (length elt) 2)
                       (push elt vars))))
              (setq loc (treepy-replace loc (cl-coerce vars 'vector))))))
        (if (treepy-end-p loc)
            (let ((node (copy-sequence (treepy-node loc))))
              (when variables
                (push (cl-coerce variables 'vector)
                      (cdr node)))
              (cl-return node))
          (setq loc (treepy-next loc)))))))

(defun ghub--graphql-handle-response (status req)
  (let ((buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer-multibyte t)
          (let* ((headers (ghub--handle-response-headers status req))
                 (payload (ghub--handle-response-payload req))
                 (payload (ghub--handle-response-error status payload req))
                 (err     (plist-get status :error))
                 (errors  (cdr (assq 'errors payload)))
                 (errors  (and errors
                               (cons 'ghub-graphql-error errors)))
                 (data    (assq 'data payload))
                 (value   (ghub--req-value req)))
            (setf (ghub--req-value req) value)
            (if (or err errors)
                (if-let ((errorback (ghub--req-errorback req)))
                    (funcall errorback (or err errors) headers status req)
                  (ghub--signal-error (or err errors)))
              (ghub--graphql-walk-response value data req))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun ghub--graphql-walk-response (loc data req)
  (if (not loc)
      (setf (ghub--req-value req)
            (setq loc (ghub--alist-zip data)))
    (setq data (ghub--graphql-narrow-data data (ghub--graphql-lineage loc)))
    (setf (alist-get 'edges data)
          (append (alist-get 'edges (treepy-node loc))
                  (or (alist-get 'edges data)
                      (error "BUG: Expected new nodes"))))
    (setq loc (treepy-replace loc data)))
  (cl-block nil
    (while t
      (when (eq (car-safe (treepy-node loc)) 'edges)
        (setq loc (treepy-up loc))
        (pcase-let ((`(,key . ,val) (treepy-node loc)))
          (let-alist val
            (let* ((cursor (and .pageInfo.hasNextPage
                                .pageInfo.endCursor))
                   (until (cdr (assq (intern (format "%s-until" key))
                                     (ghub--graphql-req-until req))))
                   (nodes (mapcar #'cdar .edges))
                   (nodes (if until
                              (--take-while
                               (or (string> (cdr (assq 'updatedAt it)) until)
                                   (setq cursor nil))
                               nodes)
                            nodes)))
              (if cursor
                  (progn
                    (setf (ghub--req-value req) loc)
                    (ghub--graphql-retrieve req
                                            (ghub--graphql-lineage loc)
                                            cursor)
                    (cl-return))
                (setq loc (treepy-replace loc (cons key nodes))))))))
      (if (not (treepy-end-p loc))
          (setq loc (treepy-next loc))
        (funcall (ghub--req-callback req)
                 (treepy-root loc))
        (cl-return)))))

(defun ghub--graphql-lineage (loc)
  (let (lineage)
    (while (treepy-up loc)
      (push (car (treepy-node loc)) lineage)
      (setq loc (treepy-up loc)))
    lineage))

(defun ghub--graphql-narrow-data (data lineage)
  (let (key)
    (while (setq key (pop lineage))
      (if (consp (car lineage))
          (progn (pop lineage)
                 (setf data (cadr data)))
        (setq data (assq key (cdr data))))))
  data)

(defun ghub--graphql-narrow-query (query lineage cursor)
  (if (consp (car lineage))
      (let* ((child  (cddr query))
             (alist  (cl-coerce (cadr query) 'list))
             (single (cdr (assq :singular alist))))
        `(,(car single)
          ,(vector (list (cadr single) (cdr (car lineage))))
          ,@(if (cdr lineage)
               (ghub--graphql-narrow-query child (cdr lineage) cursor)
             child)))
    (let* ((child  (or (assq (car lineage) (cdr query))
                       (cl-find-if (lambda (c)
                                     (and (listp c)
                                          (vectorp (cadr c))
                                          (eq (cadr (assq :singular
                                                          (cl-coerce (cadr c)
                                                                     'list)))
                                              (car lineage))))
                                   (cdr query))))
           (object (car query))
           (args   (and (vectorp (cadr query))
                        (cadr query))))
      `(,object
        ,@(and args (list args))
        ,(cond ((cdr lineage)
                (ghub--graphql-narrow-query child (cdr lineage) cursor))
               (cursor
                `(,(car child)
                  ,(vconcat `((after ,cursor))
                            (cadr child))
                  ,@(cddr child)))
               (t
                child))))))

(defun ghub--graphql-encode (g)
  (if (symbolp g)
      (symbol-name g)
    (let* ((object (graphql--encode-object (car g)))
           (args   (and (vectorp (cadr g))
                        (cl-coerce (cadr g) 'list)))
           (aliasp (cadr (assq :alias args)))
           (fields (if args (cddr g) (cdr g)))
           (fields (and fields
                        (mapconcat #'ghub--graphql-encode fields "\n")))
           (args   (and args
                        (mapconcat (pcase-lambda (`(,key ,val))
                                     (graphql--encode-argument key val))
                                   args ",\n"))))
      (if aliasp
          (concat object ": " fields)
        (concat object
                (and args   (format " (\n%s)" args))
                (and fields (format " {\n%s\n}" fields)))))))

(defun ghub--alist-zip (root)
  (let ((branchp (lambda (elt) (and (listp elt) (listp (cdr elt)))))
        (make-node (lambda (_ children) children)))
    (treepy-zipper branchp #'identity make-node root)))

(defun ghub--graphql-set-mode-line (buf string &rest args)
  (when (ghub--graphql-req-p buf)
    (setq buf (ghub--graphql-req-buffer buf)))
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq mode-line-process
            (and string (concat " " (apply #'format string args))))
      (force-mode-line-update t))))

;;; _
(provide 'ghub-graphql)
;;; ghub-graphql.el ends here
