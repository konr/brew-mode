;;; brew-mode-test.el --- unit tests for brew-mode

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

;; Unit tests for brew-mode

;;; Code:

(defun brew/brix<->gravity-test ()
  (let ((tests '((15.0 1.0611)
                 (20.0 1.0830)
                 (25.0 1.1057))))
    (loop for test in tests
          doing (assert (= (round-float (brew/brix->gravity (car test)) 4) (cadr test)))
          doing (assert (= (round-float (brew/gravity->brix (cadr test)) 1) (car test))))))

(defun brew/gravity->potential-abv-test ()
  (let ((tests '((1.02 2.5)
                 (1.10 14.2)
                 (1.05 6.6)
                 (1.15 23.0))))
    (loop for test in tests
          doing (assert (= (round-float (brew/gravity->potential-abv (car test)) 1) (cadr test))))))

;;; brew-mode-test.el ends here
