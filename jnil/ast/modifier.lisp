;;; -*- mode: lisp; package: jnil.ast -*-
;;; Copyright (c) 2004-2005 Tiago Maduro-Dias. All Rights Reserved.
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by 
;;; the Free Software Foundation, as clarified by the preamble
;;; found in license-llgpl.txt.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose. See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file 
;;; license-lgpl.txt that was distributed with this file.
;;; If it is not present, you can access it from
;;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;;; Suite 330, Boston, MA  02111-1307  USA
;;;
;;; $Id: modifier.lisp,v 1.1.1.1 2006/04/23 17:11:41 tdias Exp $
;;;
;;; Description: Definition of a mapping for the Modifier class,
;;; defined in Eclipse's JDT.
;;;
;;; -- start of modifier.lisp --

(in-package :jnil.ast)

; See org.eclipse.jdt.core.dom.Modifier for the bitfields
;;;;;;;;;;;;;;;;;;;;;;;;;
;;; symbol definition ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant +abstract+ 1024)
(defconstant +default+ 65536)
(defconstant +final+ 16)
(defconstant +module+ 32768)
(defconstant +native+ 256)
(defconstant +non_sealed+ 4096)
(defconstant +none+ 0)
(defconstant +private+ 2)
(defconstant +protected+ 4)
(defconstant +public+ 1)
(defconstant +sealed+ 512)
(defconstant +static+ 8)
(defconstant +strictfp+ 2048)
(defconstant +synchronized+ 32)
(defconstant +transient+ 128)
(defconstant +volatile+ 64)

;;; Java methods
(defmethod abstract-p ((modifier integer))
  (logtest +abstract+ modifier))

(defmethod static-p ((modifier integer))
  ;(break "static-p modifier")
  (logtest +static+ modifier))

(defmethod public-p ((modifier integer))
  (logtest +public+ modifier))

(defmethod final-p ((modifier integer))
  (logtest +final+ modifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; symbols are exported here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(abstract-p static-p public-p final-p) 'jnil.ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; other miscellaneous operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- end of modifier.lisp --
