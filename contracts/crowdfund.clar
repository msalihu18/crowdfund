;; ------------------------------------------------------
;; Crowdfund Contract
;; Backers contribute STX to fund a project.
;; If target is reached before deadline -> creator can claim.
;; If not -> backers can withdraw their refunds.
;; ------------------------------------------------------

(define-constant project-creator 'ST000000000000000000002AMW42H) ;; replace with real address
(define-constant target-amount u10000) ;; funding goal in microSTX (1 STX = 1_000_000 microSTX)
(define-constant deadline u5000) ;; block height deadline

(define-data-var total-raised uint u0)
(define-map contributions {backer: principal} {amount: uint})
(define-data-var claimed bool false)

;; ------------------------------------------------------
;; Public functions
;; ------------------------------------------------------

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-NO-CONTRIBUTION (err u2))
(define-constant ERR-REFUND-NOT-AVAILABLE (err u3))
(define-constant ERR-INVALID-AMOUNT (err u4))

;; Backer contributes STX
(define-public (contribute (amount uint))
    (begin
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (var-set total-raised (+ (var-get total-raised) amount))
      (let ((existing (default-to {amount: u0} (map-get? contributions {backer: tx-sender}))))
        (map-set contributions {backer: tx-sender} {amount: (+ (get amount existing) amount)}))
      (ok true)))

;; Project creator claims funds if goal met before deadline
(define-public (claim-funds)
  (if (and (is-eq tx-sender project-creator)
           (>= (var-get total-raised) target-amount)
           (<= burn-block-height deadline)
           (not (var-get claimed)))
      (begin
        (var-set claimed true)
        (ok (try! (as-contract (stx-transfer? (var-get total-raised) tx-sender project-creator)))))
      ERR-NOT-AUTHORIZED))

;; Backer requests refund if goal not met by deadline
(define-public (refund)
  (let ((contribution (unwrap! (map-get? contributions {backer: tx-sender}) ERR-NO-CONTRIBUTION)))
    (if (and (> burn-block-height deadline)
             (< (var-get total-raised) target-amount))
        (begin
          (map-delete contributions {backer: tx-sender})
          (ok (try! (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender)))))
        ERR-REFUND-NOT-AVAILABLE)))

;; ------------------------------------------------------
;; Read-only helpers
;; ------------------------------------------------------

(define-read-only (get-total-raised)
  (ok (var-get total-raised)))

(define-read-only (get-contribution (backer principal))
  (ok (default-to {amount: u0} (map-get? contributions {backer: backer}))))
