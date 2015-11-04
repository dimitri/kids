;;;
;;; School exercise about sorting words in alphabet order
;;;


(defpackage #:alpharange (:use :cl))

(in-package #:alpharange)

(defstruct exercice debut fin liste)

(defvar *exercice-list*
  (list (make-exercice :debut "vanne"
                       :fin "veau"
                       :liste (list "variable"
                                    "vapeur"
                                    "vent"
                                    "végétal"
                                    "vase"
                                    "vanille"))

        (make-exercice :debut "blond"
                       :fin "bol"
                       :liste (list "bobine"
                                    "bord"
                                    "bolet"
                                    "boa"
                                    "blouse"
                                    "bloc"))

        (make-exercice :debut "leçon"
                       :fin "léser"
                       :liste (list "légende"
                                    "lévrier"
                                    "léopard"
                                    "légion"
                                    "léger"
                                    "laverie"))

        (make-exercice :debut "ravi"
                       :fin "rebelle"
                       :liste (list "rayer"
                                    "réaliser"
                                    "recette"
                                    "ravage"
                                    "rayon"
                                    "réaction")))
  "Liste des exercices à résoudre. Chaque exercice contient deux mots
  limites, et une liste de mots pour lesquels il s'agit de décider s'ils se
  trouve entre les mots limites." )

(defun in-range-p (candidat debut fin)
  "Retourne non-nil quand le mot candidat se trouve entre debut et fin."
  (and (string< debut candidat) (string< candidat fin)))

(defun resoud-exercice (exercice)
  "Retourne la liste des mots qui sont entre debut et fin pour EXERCICE."
  (loop :for mot :in (exercice-liste exercice)
     :when (in-range-p mot (exercice-debut exercice) (exercice-fin exercice))
     :collect mot))

(defun affiche-resultat (exercice)
  "Affiche la liste des mots qui sont dans la solution de l'exercice."
  (format t "Trouver les mots qui sont entre ~s et ~s.~%"
          (exercice-debut exercice) (exercice-fin exercice))
  (format t "      parmis les mots suivants: ~{~s~^, ~}~%" (exercice-liste exercice))
  (format t "Les mots ~{~s~^, ~} sont entre les mots ~s et ~s.~%"
          (resoud-exercice exercice)
          (exercice-debut exercice)
          (exercice-fin exercice)))

(defun affiche-resultats (&optional (liste-des-exercices *exercice-list*))
  "Affiche les résultats de tous les exercices."
  (loop :for numero :from 1 :to (length liste-des-exercices)
     :for exercice :in liste-des-exercices
     :do (progn
           (format t "Exercice numéro ~a:~%" numero)
           (affiche-resultat exercice)
           (format t "  -=-=-=-=~%~%"))))
