;; Decentralized Recycling Incentives Smart Contract
;; Rewards proper recycling behavior with tradeable EcoTokens

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1001))
(define-constant ERR_INVALID_AMOUNT (err u1002))
(define-constant ERR_INVALID_MATERIAL (err u1003))
(define-constant ERR_ALREADY_VERIFIED (err u1004))
(define-constant ERR_NOT_FOUND (err u1005))
(define-constant ERR_VALIDATOR_EXISTS (err u1006))
(define-constant ERR_NOT_VALIDATOR (err u1007))
(define-constant ERR_INSUFFICIENT_TOKENS (err u1008))

;; Token name and symbol
(define-fungible-token eco-token)
(define-constant TOKEN_NAME "EcoToken")
(define-constant TOKEN_SYMBOL "ECO")

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-recycled-weight uint u0)
(define-data-var total-rewards-distributed uint u0)

;; Data Maps
(define-map user-stats principal 
  {
    total-recycled: uint,
    total-earned: uint,
    recycling-streak: uint,
    last-activity: uint
  }
)

(define-map recycling-submissions uint
  {
    user: principal,
    material-type: (string-ascii 20),
    weight: uint,
    location: (string-ascii 50),
    timestamp: uint,
    verified: bool,
    validator: (optional principal),
    reward-amount: uint
  }
)

(define-map material-rates (string-ascii 20) uint)

(define-map authorized-validators principal bool)

(define-map validator-stats principal
  {
    verifications-count: uint,
    reputation-score: uint,
    earnings: uint
  }
)

;; Submission counter
(define-data-var submission-counter uint u0)

;; Initialize material recycling rates (tokens per kg)
(map-set material-rates "plastic" u10)
(map-set material-rates "glass" u15)
(map-set material-rates "metal" u20)
(map-set material-rates "paper" u8)
(map-set material-rates "electronics" u50)
(map-set material-rates "organic" u5)

;; Read-only functions
(define-read-only (get-name)
  (ok TOKEN_NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN_SYMBOL)
)

(define-read-only (get-decimals)
  (ok u6)
)

(define-read-only (get-balance (user principal))
  (ok (ft-get-balance eco-token user))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply eco-token))
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

(define-read-only (get-submission (submission-id uint))
  (map-get? recycling-submissions submission-id)
)

(define-read-only (get-material-rate (material (string-ascii 20)))
  (map-get? material-rates material)
)

(define-read-only (is-validator (user principal))
  (default-to false (map-get? authorized-validators user))
)

(define-read-only (get-validator-stats (validator principal))
  (map-get? validator-stats validator)
)

(define-read-only (get-contract-stats)
  {
    total-recycled-weight: (var-get total-recycled-weight),
    total-rewards-distributed: (var-get total-rewards-distributed),
    total-supply: (ft-get-supply eco-token)
  }
)

;; Private functions
(define-private (calculate-reward (material-type (string-ascii 20)) (weight uint))
  (let ((rate (default-to u0 (map-get? material-rates material-type))))
    (if (> rate u0)
        (* rate weight)
        u0
    )
  )
)

(define-private (update-user-stats (user principal) (weight uint) (reward uint))
  (let ((current-stats (default-to 
          {total-recycled: u0, total-earned: u0, recycling-streak: u0, last-activity: u0}
          (map-get? user-stats user))))
    (map-set user-stats user
      {
        total-recycled: (+ (get total-recycled current-stats) weight),
        total-earned: (+ (get total-earned current-stats) reward),
        recycling-streak: (+ (get recycling-streak current-stats) u1),
        last-activity: block-height
      }
    )
  )
)

(define-private (update-validator-stats (validator principal) (reward uint))
  (let ((current-stats (default-to 
          {verifications-count: u0, reputation-score: u0, earnings: u0}
          (map-get? validator-stats validator))))
    (map-set validator-stats validator
      {
        verifications-count: (+ (get verifications-count current-stats) u1),
        reputation-score: (+ (get reputation-score current-stats) u10),
        earnings: (+ (get earnings current-stats) reward)
      }
    )
  )
)

;; Public functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender sender) (is-eq tx-sender (var-get contract-owner))) ERR_NOT_AUTHORIZED)
    (ft-transfer? eco-token amount sender recipient)
  )
)

(define-public (submit-recycling (material-type (string-ascii 20)) (weight uint) (location (string-ascii 50)))
  (let (
    (submission-id (+ (var-get submission-counter) u1))
    (reward (calculate-reward material-type weight))
  )
    (asserts! (> weight u0) ERR_INVALID_AMOUNT)
    (asserts! (> reward u0) ERR_INVALID_MATERIAL)
    
    ;; Create submission record
    (map-set recycling-submissions submission-id
      {
        user: tx-sender,
        material-type: material-type,
        weight: weight,
        location: location,
        timestamp: block-height,
        verified: false,
        validator: none,
        reward-amount: reward
      }
    )
    
    ;; Update submission counter
    (var-set submission-counter submission-id)
    
    (ok submission-id)
  )
)

(define-public (verify-submission (submission-id uint))
  (let (
    (submission (unwrap! (map-get? recycling-submissions submission-id) ERR_NOT_FOUND))
    (user (get user submission))
    (weight (get weight submission))
    (reward (get reward-amount submission))
    (validator-reward (/ reward u10)) ;; 10% to validator
  )
    (asserts! (is-validator tx-sender) ERR_NOT_VALIDATOR)
    (asserts! (not (get verified submission)) ERR_ALREADY_VERIFIED)
    
    ;; Update submission as verified
    (map-set recycling-submissions submission-id
      (merge submission {verified: true, validator: (some tx-sender)})
    )
    
    ;; Mint tokens for user
    (try! (ft-mint? eco-token reward user))
    
    ;; Mint validator reward
    (try! (ft-mint? eco-token validator-reward tx-sender))
    
    ;; Update statistics
    (update-user-stats user weight reward)
    (update-validator-stats tx-sender validator-reward)
    (var-set total-recycled-weight (+ (var-get total-recycled-weight) weight))
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward))
    
    (ok {user-reward: reward, validator-reward: validator-reward})
  )
)

(define-public (add-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-validator validator)) ERR_VALIDATOR_EXISTS)
    
    (map-set authorized-validators validator true)
    (map-set validator-stats validator
      {verifications-count: u0, reputation-score: u100, earnings: u0}
    )
    
    (ok true)
  )
)

(define-public (remove-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (asserts! (is-validator validator) ERR_NOT_VALIDATOR)
    
    (map-delete authorized-validators validator)
    (ok true)
  )
)

(define-public (update-material-rate (material (string-ascii 20)) (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (map-set material-rates material new-rate)
    (ok true)
  )
)

(define-public (burn-tokens (amount uint))
  (begin
    (asserts! (>= (ft-get-balance eco-token tx-sender) amount) ERR_INSUFFICIENT_BALANCE)
    (ft-burn? eco-token amount tx-sender)
  )
)

(define-public (trade-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (ft-get-balance eco-token tx-sender) amount) ERR_INSUFFICIENT_TOKENS)
    (ft-transfer? eco-token amount tx-sender recipient)
  )
)

;; Administrative functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (emergency-mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (ft-mint? eco-token amount recipient)
  )
)