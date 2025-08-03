;; Automated Bill Splitting Smart Contract
;; Handles shared expenses and subscription management with automatic splitting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-participants (err u106))
(define-constant err-already-paid (err u107))
(define-constant err-subscription-inactive (err u108))

;; Data Variables
(define-data-var next-expense-id uint u1)
(define-data-var next-subscription-id uint u1)
(define-data-var platform-fee-rate uint u25) ;; 0.25% fee (25 basis points)

;; Data Maps
(define-map expenses 
  { expense-id: uint }
  {
    creator: principal,
    title: (string-ascii 50),
    total-amount: uint,
    participants: (list 10 principal),
    amount-per-person: uint,
    payments: (list 10 { participant: principal, paid: bool }),
    created-at: uint,
    settled: bool
  }
)

(define-map subscriptions
  { subscription-id: uint }
  {
    creator: principal,
    title: (string-ascii 50),
    monthly-amount: uint,
    participants: (list 10 principal),
    amount-per-person: uint,
    active: bool,
    created-at: uint,
    next-billing: uint
  }
)

(define-map subscription-payments
  { subscription-id: uint, participant: principal, billing-cycle: uint }
  { paid: bool, paid-at: uint }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

;; Private Functions
(define-private (calculate-split (total-amount uint) (participant-count uint))
  (/ total-amount participant-count)
)

(define-private (get-participant-count (participants (list 10 principal)))
  (len participants)
)

(define-private (is-participant (user principal) (participants (list 10 principal)))
  (is-some (index-of participants user))
)

(define-private (calculate-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-private (update-user-balance (user principal) (amount uint) (add bool))
  (let ((current-balance (default-to u0 (get balance (map-get? user-balances { user: user })))))
    (if add
      (map-set user-balances { user: user } { balance: (+ current-balance amount) })
      (if (>= current-balance amount)
        (map-set user-balances { user: user } { balance: (- current-balance amount) })
        false
      )
    )
  )
)

;; Public Functions

;; Create a new shared expense
(define-public (create-expense (expense-name (string-ascii 50)) (total-amount uint) (participants (list 10 principal)))
  (let (
    (expense-id (var-get next-expense-id))
    (participant-count (get-participant-count participants))
    (amount-per-person (calculate-split total-amount participant-count))
  )
    (asserts! (> total-amount u0) err-invalid-amount)
    (asserts! (> participant-count u0) err-invalid-participants)
    (asserts! (<= participant-count u10) err-invalid-participants)
    (asserts! (is-participant tx-sender participants) err-unauthorized)
    
    (map-set expenses
      { expense-id: expense-id }
      {
        creator: tx-sender,
        title: expense-name,
        total-amount: total-amount,
        participants: participants,
        amount-per-person: amount-per-person,
        payments: (map create-payment-record participants),
        created-at: block-height,
        settled: false
      }
    )
    
    (var-set next-expense-id (+ expense-id u1))
    (ok expense-id)
  )
)

;; Helper function to create payment records
(define-private (create-payment-record (participant principal))
  { participant: participant, paid: false }
)

;; Pay for an expense
(define-public (pay-expense (expense-id uint))
  (let ((expense-data (unwrap! (map-get? expenses { expense-id: expense-id }) err-not-found)))
    (asserts! (not (get settled expense-data)) err-already-paid)
    (asserts! (is-participant tx-sender (get participants expense-data)) err-unauthorized)
    
    (let (
      (amount-to-pay (get amount-per-person expense-data))
      (fee (calculate-fee amount-to-pay))
      (net-amount (- amount-to-pay fee))
    )
      (asserts! (update-user-balance tx-sender amount-to-pay false) err-insufficient-balance)
      (asserts! (update-user-balance (get creator expense-data) net-amount true) (err u999))
      (asserts! (update-user-balance contract-owner fee true) (err u999))
      
      ;; Update payment status (simplified - in production would need more complex logic)
      (ok true)
    )
  )
)

;; Create a subscription
(define-public (create-subscription (expense-name (string-ascii 50)) (monthly-amount uint) (participants (list 10 principal)))
  (let (
    (subscription-id (var-get next-subscription-id))
    (participant-count (get-participant-count participants))
    (amount-per-person (calculate-split monthly-amount participant-count))
  )
    (asserts! (> monthly-amount u0) err-invalid-amount)
    (asserts! (> participant-count u0) err-invalid-participants)
    (asserts! (<= participant-count u10) err-invalid-participants)
    (asserts! (is-participant tx-sender participants) err-unauthorized)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      {
        creator: tx-sender,
        title: expense-name,
        monthly-amount: monthly-amount,
        participants: participants,
        amount-per-person: amount-per-person,
        active: true,
        created-at: block-height,
        next-billing: (+ block-height u4320) ;; ~30 days in blocks
      }
    )
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

;; Pay subscription for current billing cycle
(define-public (pay-subscription (subscription-id uint) (billing-cycle uint))
  (let ((subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found)))
    (asserts! (get active subscription-data) err-subscription-inactive)
    (asserts! (is-participant tx-sender (get participants subscription-data)) err-unauthorized)
    
    (let ((payment-key { subscription-id: subscription-id, participant: tx-sender, billing-cycle: billing-cycle }))
      (asserts! (is-none (map-get? subscription-payments payment-key)) err-already-paid)
      
      (let (
        (amount-to-pay (get amount-per-person subscription-data))
        (fee (calculate-fee amount-to-pay))
        (net-amount (- amount-to-pay fee))
      )
        (asserts! (update-user-balance tx-sender amount-to-pay false) err-insufficient-balance)
        (asserts! (update-user-balance (get creator subscription-data) net-amount true) (err u999))
        (asserts! (update-user-balance contract-owner fee true) (err u999))
        
        (map-set subscription-payments payment-key { paid: true, paid-at: block-height })
        (ok true)
      )
    )
  )
)

;; Deposit funds to user balance
(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (update-user-balance tx-sender amount true)
    (ok amount)
  )
)

;; Withdraw funds from user balance
(define-public (withdraw (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (update-user-balance tx-sender amount false) err-insufficient-balance)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok amount)
  )
)

;; Cancel/deactivate subscription (creator only)
(define-public (cancel-subscription (subscription-id uint))
  (let ((subscription-data (unwrap! (map-get? subscriptions { subscription-id: subscription-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get creator subscription-data)) err-unauthorized)
    
    (map-set subscriptions
      { subscription-id: subscription-id }
      (merge subscription-data { active: false })
    )
    (ok true)
  )
)

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-rate new-fee-rate)
    (ok new-fee-rate)
  )
)

;; Read-only functions

(define-read-only (get-expense (expense-id uint))
  (map-get? expenses { expense-id: expense-id })
)

(define-read-only (get-subscription (subscription-id uint))
  (map-get? subscriptions { subscription-id: subscription-id })
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-read-only (get-subscription-payment (subscription-id uint) (participant principal) (billing-cycle uint))
  (map-get? subscription-payments { subscription-id: subscription-id, participant: participant, billing-cycle: billing-cycle })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-next-expense-id)
  (var-get next-expense-id)
)

(define-read-only (get-next-subscription-id)
  (var-get next-subscription-id)
)