;; prediction-market contract

;; Constants
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_MARKET_CLOSED (err u2))
(define-constant ERR_INSUFFICIENT_BALANCE (err u3))
(define-constant ERR_MARKET_NOT_RESOLVED (err u4))
(define-constant ERR_INVALID_AMOUNT (err u5))
(define-constant ERR_NO_BET (err u6))
(define-constant MINIMUM_BET_AMOUNT u1000000) ;; 1 STX

;; Data variables
(define-data-var market-nonce uint u0)

;; Data maps
(define-map markets
  { market-id: uint }
  {
    creator: principal,
    description: (string-ascii 256),
    options: (list 2 (string-ascii 64)),
    total-stake: uint,
    is-resolved: bool,
    winning-option: (optional uint)
  }
)

(define-map bets
  { market-id: uint, better: principal }
  { amount: uint, option: uint }
)

(define-map market-stats
  { market-id: uint }
  { total-bets: uint, unique-betters: uint }
)

;; Public functions
(define-public (create-market (description (string-ascii 256)) (option-a (string-ascii 64)) (option-b (string-ascii 64)))
  (let
    (
      (market-id (var-get market-nonce))
    )
    (map-set markets
      { market-id: market-id }
      {
        creator: tx-sender,
        description: description,
        options: (list option-a option-b),
        total-stake: u0,
        is-resolved: false,
        winning-option: none
      }
    )
    (var-set market-nonce (+ market-id u1))
    (ok market-id)
  )
)

(define-public (place-bet (market-id uint) (option uint) (amount uint))
  (let
    (
      (market (unwrap! (map-get? markets { market-id: market-id }) ERR_MARKET_CLOSED))
      (stats (default-to { total-bets: u0, unique-betters: u0 } (map-get? market-stats { market-id: market-id })))
    )
    (asserts! (not (get is-resolved market)) ERR_MARKET_CLOSED)
    (asserts! (or (is-eq option u0) (is-eq option u1)) ERR_UNAUTHORIZED)
    (asserts! (>= amount MINIMUM_BET_AMOUNT) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set bets
      { market-id: market-id, better: tx-sender }
      { amount: amount, option: option }
    )
    (map-set markets
      { market-id: market-id }
      (merge market { total-stake: (+ (get total-stake market) amount) })
    )
    (map-set market-stats
      { market-id: market-id }
      {
        total-bets: (+ (get total-bets stats) u1),
        unique-betters: (if (is-some (map-get? bets { market-id: market-id, better: tx-sender }))
                            (get unique-betters stats)
                            (+ (get unique-betters stats) u1))
      }
    )
    (ok true)
  )
)

(define-public (resolve-market (market-id uint) (winning-option uint))
  (let
    (
      (market (unwrap! (map-get? markets { market-id: market-id }) ERR_MARKET_CLOSED))
    )
    (asserts! (is-eq (get creator market) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get is-resolved market)) ERR_MARKET_CLOSED)
    (asserts! (or (is-eq winning-option u0) (is-eq winning-option u1)) ERR_UNAUTHORIZED)
    (map-set markets
      { market-id: market-id }
      (merge market { is-resolved: true, winning-option: (some winning-option) })
    )
    (ok true)
  )
)

(define-public (claim-winnings (market-id uint))
  (let
    (
      (market (unwrap! (map-get? markets { market-id: market-id }) ERR_MARKET_CLOSED))
      (bet (unwrap! (map-get? bets { market-id: market-id, better: tx-sender }) ERR_UNAUTHORIZED))
      (winning-option (unwrap! (get winning-option market) ERR_MARKET_NOT_RESOLVED))
    )
    (asserts! (get is-resolved market) ERR_MARKET_NOT_RESOLVED)
    (asserts! (is-eq (get option bet) winning-option) ERR_UNAUTHORIZED)
    (let
      (
        (total-stake (get total-stake market))
        (bet-amount (get amount bet))
        (winnings (/ (* bet-amount total-stake) bet-amount))
      )
      (try! (as-contract (stx-transfer? winnings tx-sender tx-sender)))
      (ok winnings)
    )
  )
)

(define-public (cancel-bet (market-id uint))
  (let
    (
      (market (unwrap! (map-get? markets { market-id: market-id }) ERR_MARKET_CLOSED))
      (bet (unwrap! (map-get? bets { market-id: market-id, better: tx-sender }) ERR_NO_BET))
    )
    (asserts! (not (get is-resolved market)) ERR_MARKET_CLOSED)
    (try! (as-contract (stx-transfer? (get amount bet) tx-sender tx-sender)))
    (map-delete bets { market-id: market-id, better: tx-sender })
    (map-set markets
      { market-id: market-id }
      (merge market { total-stake: (- (get total-stake market) (get amount bet)) })
    )
    (ok true)
  )
)

(define-public (increase-bet (market-id uint) (additional-amount uint))
  (let
    (
      (market (unwrap! (map-get? markets { market-id: market-id }) ERR_MARKET_CLOSED))
      (bet (unwrap! (map-get? bets { market-id: market-id, better: tx-sender }) ERR_NO_BET))
    )
    (asserts! (not (get is-resolved market)) ERR_MARKET_CLOSED)
    (asserts! (>= additional-amount MINIMUM_BET_AMOUNT) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? additional-amount tx-sender (as-contract tx-sender)))
    (map-set bets
      { market-id: market-id, better: tx-sender }
      { amount: (+ (get amount bet) additional-amount), option: (get option bet) }
    )
    (map-set markets
      { market-id: market-id }
      (merge market { total-stake: (+ (get total-stake market) additional-amount) })
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-market (market-id uint))
  (map-get? markets { market-id: market-id })
)

(define-read-only (get-bet (market-id uint) (better principal))
  (map-get? bets { market-id: market-id, better: better })
)

(define-read-only (get-market-stats (market-id uint))
  (map-get? market-stats { market-id: market-id })
)

(define-read-only (get-total-markets)
  (var-get market-nonce)
)

(define-read-only (get-user-markets (user principal))
  (let
    ((total-markets (var-get market-nonce)))
    (filter get-user-market (list total-markets))
  )
)

(define-private (get-user-market (market-id uint))
  (let
    ((market (unwrap! (map-get? markets { market-id: market-id }) false)))
    (is-eq (get creator market) tx-sender)
  )
)

