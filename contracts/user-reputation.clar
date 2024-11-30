;; user-reputation contract

;; Constants
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_INVALID_SCORE (err u2))

;; Data maps
(define-map user-scores
  { user: principal }
  { score: uint, markets-participated: uint }
)

;; Public functions
(define-public (update-user-score (user principal) (market-id uint) (outcome bool))
  (let
    (
      (current-score (default-to { score: u500, markets-participated: u0 } (map-get? user-scores { user: user })))
      (score-change (if outcome u10 (- u0 u5)))
    )
    (asserts! (is-eq tx-sender .prediction-market) ERR_UNAUTHORIZED)
    (map-set user-scores
      { user: user }
      {
        score: (+ (get score current-score) score-change),
        markets-participated: (+ (get markets-participated current-score) u1)
      }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-user-score (user principal))
  (map-get? user-scores { user: user })
)

(define-read-only (get-user-reputation-tier (user principal))
  (let
    (
      (score (default-to u500 (get score (map-get? user-scores { user: user }))))
    )
    (if (< score u200)
      "Novice"
      (if (< score u500)
        "Intermediate"
        (if (< score u800)
          "Expert"
          "Master"
        )
      )
    )
  )
)

