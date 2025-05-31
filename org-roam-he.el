;;; org-roam-he.el --- Org-roam Hyper-Edge extension -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2025 Tomke Pfoch <pfoch.tomke@gmail.com>

;; Author: Tomke Pfoch <pfoch.tomke@gmail.com>
;;;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, hyperedge
;; Version: 0.1
;; Package-Requires: ((org-roam "2.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This module extends org-roam with Hyper-Edges with orientation
;; for named Relations
;;
;;; Code:

(require 'org-roam-db)
(require 'ol)


;; Insert Hyper-Edge Schema
(add-to-list 'org-roam-db--table-schemata
	     '(hyper-edges
	       ([(pos :not-null)
		 (source-idx :not-null)
		 (node-a-id :not-null)
		 (node-via-id :not-null)
		 (node-b-id :not-null)
		 (properties)]
		(:foreign-key [node-a-id ] :references nodes [id] :on-delete :cascade)
		(:foreign-key [node-b-id ] :references nodes [id] :on-delete :cascade)
		(:foreign-key [node-via-id] :references nodes [id] :on-delete :cascade))))




(defun org-roam-he-insert-link (link)
  "Insert rel-link data for LINK at current point into the Org-roam cache."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (when (equal "roamh" (org-element-property :type link))
      (let* ((path (org-element-property :path link))
	     (option (and (string-match "::\\(.*\\)\\'" path)
			  (match-string 1 path)))
	     (properties (when option '(:search-option option))))
	(-when-let* ((path (if (not option) path
			     (substring path 0 (match-beginning 0))))
		     ((a via b) (split-string path ":"))
		     (source (org-roam-id-at-point))
		     (source-idx (--find-index (equal source it)
					       (list a via b))))
	  ;; For Org-ref links, we need to split the path into the cite keys
   	  (org-roam-db-query
           [:insert :into hyper-edges
   		    :values $v1]
   	   (vector (point) source-idx a via b properties))
	  t)))))

(defun org-roam-he-sort-promote (nodes)
  "Return sort function which displays NODES at top when using org-roam-node-read."
  (let ((ids (--map (org-roam-node-id it) nodes)))
    (lambda (completion-a _)
      (-contains-p ids (org-roam-node-id (cdr completion-a))))))

(defvar org-roam-he-nodes
  '((node-a . "Node A")
    (node-via . "Relation")
    (node-b . "Node B")))

(defun org-roam-he--prompt-id (he-node &optional must-match)
  "Prompt user for HE-NODE Node while displaying node at point at the top.
If MUST-MATCH is non-nil the selected Node must exist.
Return Node-id to of the selected node"
  (org-roam-node-id
   (org-roam-node-read nil nil
		       (org-roam-he-sort-promote (list (org-roam-node-at-point)))
		       must-match
		       (concat (cdr (assoc he-node org-roam-he-nodes)) ": "))))

(defun org-roam-he-query ()
  "Find all Hyper-Edges"
  (let ((selection 
	 (-fix (lambda (l)
		 (let ((pending (-filter (lambda (e)
					   (not (--any-p (eq (car it) e) l)))
					 '(node-a node-via node-b))))
		   (if (< (length l) 2)
		       (cons (--any (when-let (id (org-roam-he--prompt-id it))
				      (cons it id))
				    pending)
			     l)
		     l)))
	       nil)))
    selection))


(advice-add 'org-roam-db-insert-link :before-until 'org-roam-db-insert-he-link)

;; TODO:
(defun org-roam-he-follow (rel-link)
  "Follow a roamh: Hyper-Edge Link REL-LINK.

The User will be promted for which node to visit,
the node at point will be ignored.

rel-link should have the format [[roamh:node-a-id:node-via-id:node-b-id]].

Consider using org-roam-he-insert to create a link using the wizard."
  
  (when-let* ((current (org-roam-id-at-point))
	      (targets (--filter (not (equal current it)) (string-split rel-link ":")))
	      (filter (lambda (node) (-contains-p targets (org-roam-node-id node))))
	      (choice (org-roam-node-read nil filter nil t "visit: ")))
    (org-roam-node-visit choice)))

(defun org-roam-he-insert ()
  "Insert a roamh: Hyper_edge Link at point.

The user will be promted to select NODE-A, RELATION and NODE_B
as well as a description.  The node at point is displayed at top
using org-roam-he-sort-promote."
  (interactive)
  (-when-let* ((all-nodes (org-roam-node-list))
	       ((node-a node-via node-b)
		(--map (org-roam-he--prompt-id it t)
		 '("Node A" "Relation" "Node B")))
	       (description (read-string "Description: ")))
    (org-insert-link nil (concat "roamh:" node-a ":" node-via ":" node-b) description)))
(org-link-set-parameters "roamh" :follow #'org-roam-he-follow)

(provide 'org-roam-he)
;;; org-roam-he.el ends here
