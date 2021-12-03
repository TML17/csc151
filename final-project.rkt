#lang racket

(require csc151)
(require csc151/rex)
(require csc151/verbose-bindings)
(require rackunit)
(require csc151www)

;; CSC 151.02 (Fall 2021)
;; Final Project
;; Authors: Tommy Liu
;; Date: 12/02/2021
;; Acknowledgements:
;; 创立本地monster可以用来代入其他monster的值

(struct equipment (name armor attack-bonus agility critical-strike))
(struct player (health damage equipment) #:mutable)
(struct monster (name health damage level) #:mutable)

(define shit (equipment 1 1 1 1 1))
(define player1 (player 1 1 shit))
(define winnie (monster "Winnie the Pooh" 1 1 1))

(define pike (equipment "pike" 0 2 -1 0.1))
(define ax (equipment "ax" 0 3 -2 0.2))
(define knief(equipment "knief" 0 1 0 0.1))
(define bow (equipment "bow" 0 4 1 0.2))
(define hammer (equipment "hammer" 0 5 -3 0.3))
(define fast (equipment "fast" 0 0 3 0))
(define sound-speed (equipment "sound speed" 0 0 5 0))
(define protection (equipment "protection" 0 3 1 0))
(define god-speed (equipment "god speed" 0 0 7 0))
(define boost (equipment "boost" 0 0 2 0))
(define wood-s (equipment "wooden shield" 1 0 0 0))
(define bronze-s (equipment "bronze shield" 2 0 -1 0))
(define iron-s (equipment "iron shield" 5 0 -4 0))
(define future-s(equipment "future sheild" 4 0 0 0))
(define EA-s (equipment "Edman alloy sheild" 3 0 1 0))
(define basic (equipment "basic" 0 0 1 0.1))
(define epic (equipment "epic" 0 0 2 0.2))
(define speeding (equipment "speeding" 0 0 6 -0.2))
(define critical (equipment "critical" 0 0 -4 0.5))
(define legend (equipment "legend" 0 0 4 0.3))

(define equipment-list
  (list
   ;; weapon
   pike ax knief bow hammer
   ;; boot
   fast sound-speed protection god-speed boost
   ;; sheild
   wood-s bronze-s iron-s future-s EA-s
   ;; accessary 
   basic epic speeding critical legend))

;;; (monster-attack player monster) -> null?
;;; player : player?
;;; monster : monster?
;;; player attacks monster with equipemnt and prints out situation.
(define monster-attack
  (let ([ph-remain (- (player-health player1) (monster-damage winnie))])
    (set-player-health! player1 ph-remain))
  )

;;; (player-attack player monster) -> null?
;;; player : player?
;;; monster : monster?
;;; monster attacks player with damage corresponding to its level and prints out situation.
(define player-attack
  (let ([ph-remain (- (monster-health winnie) (player-damage player1))])
    (set-monster-health! winnie ph-remain)))

(define attack
  (λ (obj1 obj2)
    (if (and (player? obj1) (monster? obj2))
        (when #t
          player-attack
          monster-attack
          (if (player-death? obj1)
              (println "GOOD GAME")
              (if (monster-death? obj2)
                  (println "SUCCESS")
                  (attack obj1 obj2))))
        (println "Invalid Input"))))

(define player-death?
  (λ (obj)
    (<= (player-health obj) 0)))

(define monster-death?
  (λ (obj)
    (<= (monster-health obj) 0)))