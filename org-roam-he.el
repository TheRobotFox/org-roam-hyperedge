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

(require 'org-element-ast)
(require 'dash)
(require 'ol)

(require 'org-roam-db)


(defun org-roam-db-insert-he-link (link)
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
		     ;; (source (org-roam-id-at-point))
		     (source-idx (or 1 (--find-index (equal source it);; TODO source needed?
					       (list a via b)))))
	  ;; For Org-ref links, we need to split the path into the cite keys
   	  (org-roam-db-query
           [:insert :into hyper-edges
   		    :values $v1]
   	   (vector (point) a via b properties))
	  t)))))


(defvar org-roam-he-nodes
  '((node-a-id . "Node A")
    (node-via-id . "Relation")
    (node-b-id . "Node B")))

;; Insert Hyper-Edge Schema
(add-to-list 'org-roam-db--table-schemata
	     '(hyper-edges
	       ([(pos :not-null)
		 ;;(source-idx :not-null) TODO useless?
		 (node-a-id :not-null)
		 (node-via-id :not-null)
		 (node-b-id :not-null)
		 (properties)]
		(:foreign-key [node-a-id ] :references nodes [id] :on-delete :cascade)
		(:foreign-key [node-b-id ] :references nodes [id] :on-delete :cascade)
		(:foreign-key [node-via-id] :references nodes [id] :on-delete :cascade))))

(advice-add 'org-roam-db-insert-link :before-until 'org-roam-db-insert-he-link)

(require 'org-roam)

(defun org-roam-he-sort-promote (nodes)
  "Return sort function which displays NODES at top for `org-roam-node-read'."
  (let ((ids (--map (org-roam-node-id it) nodes)))
    (lambda (completion-a _)
      (-contains-p ids (org-roam-node-id (cdr completion-a))))))


(defun org-roam-he--prompt-node (he-node &optional must-match)
  "Prompt user for HE-NODE Node while showing node at point at the top.
If MUST-MATCH is non-nil the selected Node must exist.
Return org-roam-node to of the selected node.
If no node was selected return nil."
  (let* ((promote-nodes (when-let (node (and (derived-mode-p 'org-mode)
					     (org-roam-node-at-point)))
			  (list node)))
	 (node (org-roam-node-read
		nil nil (org-roam-he-sort-promote promote-nodes) must-match
		(concat (cdr (assoc he-node org-roam-he-nodes)) ": "))))
    (when (org-roam-node-id node) node)))


(defun org-roam-he--max-title-length (he-node selected collection)
  "Get maximal length of HE-NODE title for alignment.
Called by `org-roam-he--prompt-query' wth SELECTED and COLLECTION."
  (if-let (assoc-node (assoc he-node selected))
      (length (org-roam-node-title (cdr assoc-node)))
  (-max (--map (length (org-roam-node-title (cdr (assoc he-node it)))) collection))))


;; TODO allow custom render function to get alignment (max length)
;; TODO optimize render function
(defun org-roam-he--prompt-query (selected collection &optional renderfn)
  "Prompt user to select Hyper-Edge from COLLECTION narrowed to SELECTED.

Optionally a custom display function can be used with RENDERFN."
  (-let* ((render
	   (or renderfn
	       (let* ((node-a-max-length
		       (org-roam-he--max-title-length 'node-a-id selected collection))
		      (node-via-max-length
		       (org-roam-he--max-title-length 'node-via-id selected collection)))
		 (-lambda ((&alist 'node-a-id a 'node-via-id v 'node-b-id b))
		   (let ((node-a-text
			  (string-pad (org-roam-node-title a) node-a-max-length))
			 (node-via-text
			  (string-pad (org-roam-node-title v) node-via-max-length))
			 (node-b-text (org-roam-node-title b)))
		     (concat (if (assoc 'node-a-id selected)
				 (propertize node-a-text 'face 'shadow)
			       node-a-text)
			     " ⟵ "
			     (if (assoc 'node-via-id selected)
				 (propertize node-via-text 'face 'shadow)
			       node-via-text)
			     " ⟶ "
			     (if (assoc 'node-b-id selected)
				 (propertize node-b-text 'face 'shadow)
			       node-b-text) ))))))
	  (nodes (--map (cons (funcall render (-concat selected it)) it) collection))
	  (node (completing-read "Node: " nodes nil t)))
    (cdr (assoc node nodes))))


(defun org-roam-he--query (selected variable)
  "Query org-roam DB for Hyper-Edges using SELECTED fields to narrow edges.

When selecting from table only use fields in VARIABLE.
The user is also prompted for queried edges using a `completing-read'."
  (let* ((selected-ids (--map (list (car it) (org-roam-node-id (cdr it)))
			      selected))
	 (ids
	  (cond
	   ((= 0 (length selected))
	    (apply 'org-roam-db-query [:select [$i1 $i2 $i3] :from hyper-edges] variable))
	   ((= 1 (length selected))
	    (apply 'org-roam-db-query [:select [$i1 $i2] :from hyper-edges
					       :where (= $i3 $s4)]
		   (-concat variable (car selected-ids))))
	   ((= 2 (length selected))
	    (apply 'org-roam-db-query [:select [$i1] :from hyper-edges
					       :where (AND (= $i2 $s3) (= $i4 $s5))]
		   (-concat variable (-flatten selected-ids))))))
	 (collection (-map (lambda (e)
			     (-zip-pair variable (--map (org-roam-node-from-id it) e)))
			   ids)))
    (when ids (org-roam-he--prompt-query selected collection))))


(defun org-roam-he-find-oriented ()
  "Query Hyper-Edges in roam Database.

User will be prompted for NODE-A, RELATION and NODE-B any one of which
can be left empty to widen selection.  A Database Query will be issued
using the selected nodes for narrowing.  User will be prompted with
ramaining nodes."
  (interactive)
  (let ((selection nil)
	(variable (--map (car it) org-roam-he-nodes)))
    (--each-while org-roam-he-nodes
	(> 2 (length selection))
      (when-let (node (org-roam-he--prompt-node (car it)))
	(push (cons (car it) node) selection)
	(setq variable (delq (car it) variable))))
    
    (if-let* ((query (org-roam-he--query selection variable))
	      (nodes (--map (cdr it) query))
	      (filter (lambda (node) (-contains-p nodes node)))
	      (choice (org-roam-node-read nil filter nil t "visit: ")))
	(org-roam-node-visit choice)
      (prin1 "Found 0 Hyperedges!"))))
	  

;; TODO:
(defun org-roam-he-follow (rel-link)
  "Follow a roamh: Hyper-Edge Link REL-LINK.

The User will be promted for which node to visit, the node at point will
be ignored.

rel-link should have the format [[roamh:node-a-id:node-via-id:node-b-id]].

Consider using `org-roam-he-insert' to create a link using the wizard."
  
  (when-let* ((current (org-roam-id-at-point))
	      (targets (--filter (not (equal current it)) (string-split rel-link ":")))
	      (filter (lambda (node) (-contains-p targets (org-roam-node-id node))))
	      (choice (org-roam-node-read nil filter nil t "visit: ")))
    (org-roam-node-visit choice)))

(defun org-roam-he-insert ()
  "Insert a roamh: Hyper_edge Link at point.

The user will be promted to select NODE-A, RELATION and NODE_B
as well as a description.  The node at point is displayed at top
using `org-roam-he-sort-promote'."
  (interactive)
  (-when-let* ((all-nodes (org-roam-node-list))
	       ((node-a node-via node-b)
		(--map (org-roam-node-id (org-roam-he--prompt-node (car it) t))
		 org-roam-he-nodes))
	       (description (read-string "Description: ")))
    (org-insert-link nil (concat "roamh:" node-a ":" node-via ":" node-b) description)))
(org-link-set-parameters "roamh" :follow #'org-roam-he-follow)

(provide 'org-roam-he)
;;; org-roam-he.el ends here
