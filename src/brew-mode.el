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

;; Utils

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))


;; Formulae
;; https://www.brewtoad.com/tools/alcohol-calculator

(defun brew/brix->gravity (brix)
  (->>
   (/ brix 258.2)
   (* 227.1)
   (- 258.6 )
   (/ brix )
   (1+)))

(defun brew/gravity->brix (gravity)
  (->
   (* 182.4601 gravity)
   (- 775.6821)
   (* gravity)
   (+ 1262.7794)
   (* gravity)
   (- 669.5622)))

(defun brew/gravity->potential-abv (gravity)
  (* 76.08
     (- gravity 1)
     (/ 1.0 (- 1.775 gravity))
     (/ 1.0 0.794)))


;; Carbonation
;; http://brainlubeonline.com/GasLawsBeer.html

(defun brew/psi->kgf (psi)
  (* psi 0.07030695782964))

(defun brew/celsius->fahrenheit (celsius)
  (-> celsius (* 9.0) (/ 5.0) (+ 32)))

(setq brew/carbonation-style-guide
      '(("British Style Ales"	1.5 2.0)
        ("Belgian Ales"	1.9 2.4)
        ("American Ales and Lager"	2.2 2.7)
        ("Fruit Lambic" 3.0 4.5)
        ("Porter, Stout" 1.7 2.3)
        ("European Lagers"	2.2 2.7)
        ("Lambic" 2.4 2.8)
        ("German Wheat Beer" 3.3 4.5)))

(defun brew/needed-pressure-for (volumes temperature-c)
  (let ((temperature-f (brew/celsius->fahrenheit temperature-c)))
  (brew/psi->kgf
  (+
   -16.6999
   (- (* temperature-f 0.0101059))
   (* temperature-f temperature-f 0.00116512)
   (* temperature-f volumes 0.173354)
   (* volumes 4.24267)
   (- (* volumes volumes 0.0684226))))))

;;; brew-mode.el ends here
