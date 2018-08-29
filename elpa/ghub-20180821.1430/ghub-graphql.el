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

(require 'ghub)

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

(cl-defun ghub-repository-id (owner name &key username auth host)
  "Return the id of the repository specified by OWNER, NAME and HOST."
  (let-alist (ghub-graphql
              "query ($owner:String!, $name:String!) {
                 repository(owner:$owner, name:$name) { id }
               }"
              `((owner . ,owner)
                (name  . ,name))
              :username username :auth auth :host host)
    .data.repository.id))

;;; _
(provide 'ghub-graphql)
;;; ghub-graphql.el ends here
