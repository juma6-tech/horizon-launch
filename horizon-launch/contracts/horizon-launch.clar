;; HorizonLaunch - Supply Chain Transparency Contract
;; A simple smart contract for tracking products and compliance in supply chains

;; Contract owner
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-data (err u104))

;; Data structures
(define-map products
  { product-id: (string-ascii 64) }
  {
    manufacturer: principal,
    product-name: (string-ascii 100),
    dna-fingerprint: (string-ascii 128),
    creation-timestamp: uint,
    compliance-status: (string-ascii 20),
    current-location: (string-ascii 100),
    temperature-range: { min: int, max: int },
    is-active: bool
  }
)

(define-map supply-chain-events
  { event-id: uint }
  {
    product-id: (string-ascii 64),
    event-type: (string-ascii 50),
    timestamp: uint,
    location: (string-ascii 100),
    temperature: (optional int),
    handler: principal,
    compliance-check: bool,
    notes: (string-ascii 200)
  }
)

(define-map trust-scores
  { participant: principal }
  {
    reputation-tokens: uint,
    compliance-violations: uint,
    successful-verifications: uint,
    trust-level: (string-ascii 20)
  }
)

(define-map compliance-rules
  { rule-id: (string-ascii 50) }
  {
    description: (string-ascii 200),
    temperature-threshold: (optional int),
    time-limit: (optional uint),
    required-certifications: (list 5 (string-ascii 50)),
    penalty-tokens: uint,
    is-active: bool
  }
)

;; Data variables
(define-data-var next-event-id uint u1)
(define-data-var platform-fee uint u10) ;; Platform fee in tokens
(define-data-var total-products-tracked uint u0)

;; Read-only functions

;; Get product information
(define-read-only (get-product (product-id (string-ascii 64)))
  (map-get? products { product-id: product-id })
)

;; Get supply chain event
(define-read-only (get-supply-chain-event (event-id uint))
  (map-get? supply-chain-events { event-id: event-id })
)

;; Get trust score for participant
(define-read-only (get-trust-score (participant principal))
  (map-get? trust-scores { participant: participant })
)

;; Get compliance rule
(define-read-only (get-compliance-rule (rule-id (string-ascii 50)))
  (map-get? compliance-rules { rule-id: rule-id })
)

;; Get total products tracked
(define-read-only (get-total-products)
  (var-get total-products-tracked)
)

;; Check if product exists
(define-read-only (product-exists (product-id (string-ascii 64)))
  (is-some (map-get? products { product-id: product-id }))
)

;; Public functions

;; Register a new product in the supply chain
(define-public (register-product 
  (product-id (string-ascii 64))
  (product-name (string-ascii 100))
  (dna-fingerprint (string-ascii 128))
  (temp-min int)
  (temp-max int)
  (initial-location (string-ascii 100))
)
  (let
    (
      (existing-product (map-get? products { product-id: product-id }))
    )
    (asserts! (is-none existing-product) err-already-exists)
    (asserts! (> (len product-id) u0) err-invalid-data)
    (asserts! (> (len product-name) u0) err-invalid-data)
    
    ;; Create product record
    (map-set products
      { product-id: product-id }
      {
        manufacturer: tx-sender,
        product-name: product-name,
        dna-fingerprint: dna-fingerprint,
        creation-timestamp: block-height,
        compliance-status: "compliant",
        current-location: initial-location,
        temperature-range: { min: temp-min, max: temp-max },
        is-active: true
      }
    )
    
    ;; Initialize trust score for manufacturer if not exists
    (if (is-none (map-get? trust-scores { participant: tx-sender }))
      (map-set trust-scores
        { participant: tx-sender }
        {
          reputation-tokens: u100,
          compliance-violations: u0,
          successful-verifications: u1,
          trust-level: "verified"
        }
      )
      true
    )
    
    ;; Update total products counter
    (var-set total-products-tracked (+ (var-get total-products-tracked) u1))
    
    (ok product-id)
  )
)

;; Add supply chain event
(define-public (add-supply-chain-event
  (product-id (string-ascii 64))
  (event-type (string-ascii 50))
  (location (string-ascii 100))
  (temperature (optional int))
  (notes (string-ascii 200))
)
  (let
    (
      (event-id (var-get next-event-id))
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
      (compliance-check (check-temperature-compliance product-id temperature))
    )
    
    ;; Create supply chain event
    (map-set supply-chain-events
      { event-id: event-id }
      {
        product-id: product-id,
        event-type: event-type,
        timestamp: block-height,
        location: location,
        temperature: temperature,
        handler: tx-sender,
        compliance-check: compliance-check,
        notes: notes
      }
    )
    
    ;; Update product location
    (map-set products
      { product-id: product-id }
      (merge product { current-location: location })
    )
    
    ;; Update next event ID
    (var-set next-event-id (+ event-id u1))
    
    ;; Update trust score based on compliance
    (if compliance-check
      (reward-compliance tx-sender)
      (penalize-violation tx-sender)
    )
    
    (ok event-id)
  )
)

;; Set compliance rule (owner only)
(define-public (set-compliance-rule
  (rule-id (string-ascii 50))
  (description (string-ascii 200))
  (temperature-threshold (optional int))
  (time-limit (optional uint))
  (penalty-tokens uint)
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set compliance-rules
      { rule-id: rule-id }
      {
        description: description,
        temperature-threshold: temperature-threshold,
        time-limit: time-limit,
        required-certifications: (list),
        penalty-tokens: penalty-tokens,
        is-active: true
      }
    )
    
    (ok rule-id)
  )
)

;; Update product compliance status
(define-public (update-compliance-status
  (product-id (string-ascii 64))
  (new-status (string-ascii 20))
)
  (let
    (
      (product (unwrap! (map-get? products { product-id: product-id }) err-not-found))
    )
    ;; Only manufacturer or contract owner can update compliance status
    (asserts! 
      (or 
        (is-eq tx-sender (get manufacturer product))
        (is-eq tx-sender contract-owner)
      ) 
      err-unauthorized
    )
    
    (map-set products
      { product-id: product-id }
      (merge product { compliance-status: new-status })
    )
    
    (ok true)
  )
)

;; Private functions

;; Check temperature compliance
(define-private (check-temperature-compliance 
  (product-id (string-ascii 64)) 
  (temperature (optional int))
)
  (match temperature
    temp-value
      (match (map-get? products { product-id: product-id })
        product
          (let
            (
              (temp-range (get temperature-range product))
              (min-temp (get min temp-range))
              (max-temp (get max temp-range))
            )
            (and (>= temp-value min-temp) (<= temp-value max-temp))
          )
        false
      )
    true ;; No temperature recorded, assume compliant
  )
)

;; Reward compliance with reputation tokens
(define-private (reward-compliance (participant principal))
  (match (map-get? trust-scores { participant: participant })
    existing-score
      (map-set trust-scores
        { participant: participant }
        (merge existing-score 
          { 
            reputation-tokens: (+ (get reputation-tokens existing-score) u10),
            successful-verifications: (+ (get successful-verifications existing-score) u1)
          }
        )
      )
    ;; Create new trust score if doesn't exist
    (map-set trust-scores
      { participant: participant }
      {
        reputation-tokens: u110,
        compliance-violations: u0,
        successful-verifications: u1,
        trust-level: "verified"
      }
    )
  )
)

;; Penalize compliance violations
(define-private (penalize-violation (participant principal))
  (match (map-get? trust-scores { participant: participant })
    existing-score
      (map-set trust-scores
        { participant: participant }
        (merge existing-score 
          { 
            reputation-tokens: (if (>= (get reputation-tokens existing-score) u20)
                                 (- (get reputation-tokens existing-score) u20)
                                 u0),
            compliance-violations: (+ (get compliance-violations existing-score) u1),
            trust-level: (if (>= (get compliance-violations existing-score) u3) 
                           "warning" 
                           (get trust-level existing-score))
          }
        )
      )
    ;; Create new trust score with violation if doesn't exist
    (map-set trust-scores
      { participant: participant }
      {
        reputation-tokens: u80,
        compliance-violations: u1,
        successful-verifications: u0,
        trust-level: "warning"
      }
    )
  )
)

;; Contract initialization
(begin
  ;; Set initial compliance rule for temperature monitoring
  (map-set compliance-rules
    { rule-id: "temp-monitor" }
    {
      description: "Temperature monitoring for cold chain compliance",
      temperature-threshold: (some 8), ;; 8C threshold
      time-limit: (some u144), ;; 24 hours in blocks (assuming 10 min blocks)
      required-certifications: (list),
      penalty-tokens: u50,
      is-active: true
    }
  )
)