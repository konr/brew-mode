;;; brew-mode.el --- Brewing assistant

;; Author: Konrad Scorciapino <scorciapino@gmail.com>
;; Keywords: homebrewing, beer, mead, cider, sake
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An emacs-based brewing assistant.

;;; Code:

(defun brew/brix->gravity (brix)
  (1+ (/ brix (- 258.6 (* (/ brix 258.2) 227.1)))))

(defun brew/gravity->brix (gravity)
  (- (* (+ (* (- (* 182.4601 gravity) 775.6821) gravity) 1262.7794) gravity) 669.5622))

(defun brew/gravity->potential-abv (gravity)
  (* 76.08
     (- gravity 1)
     (/ 1.0 (- 1.775 gravity))
     (/ 1.0 0.794)))

;;; brew-mode.el ends here
