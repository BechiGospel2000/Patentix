;; Intellectual Property Rights Management
;; Enables creators to register, manage, and monetize intellectual property rights
;; with transparent licensing, royalty distribution, and usage tracking

;; Define NFT trait locally instead of importing from an external contract
(define-trait nft-trait
  (
    ;; Last token ID, limited to uint range
    (get-last-token-id () (response uint uint))
    ;; URI for metadata associated with the token
    (get-token-uri (uint) (response (optional (string-utf8 256)) uint))
    ;; Owner of a specific token
    (get-owner (uint) (response (optional principal) uint))
    ;; Transfer token to a new principal
    (transfer (uint principal principal) (response bool uint))
  )
)

;; Intellectual property registrations
(define-map ip-registrations
  { registration-id: uint }
  {
    title: (string-utf8 256),
    description: (string-utf8 1024),
    creator: principal,
    created-at: uint,
    ip-type: (string-ascii 32),     ;; "image", "music", "text", "code", "video", "design", etc.
    content-hash: (buff 64),        ;; Hash of the IP content
    status: (string-ascii 16),      ;; "registered", "disputed", "revoked"
    nft-contract: (optional principal),  ;; Optional NFT contract for this IP
    nft-id: (optional uint),        ;; Optional NFT ID within the contract
    public-domain: bool,            ;; Whether the work is in the public domain
    registration-expiry: (optional uint)  ;; Optional block height when registration expires
  }
)

;; IP ownership shares (can be fractional)
(define-map ip-ownership
  { registration-id: uint, owner: principal }
  {
    share-percentage: uint,         ;; Out of 10000 (e.g., 5000 = 50%)
    acquired-at: uint,
    acquired-from: (optional principal)
  }
)

;; License templates
(define-map license-templates
  { template-id: uint }
  {
    name: (string-utf8 64),
    description: (string-utf8 1024),
    creator: principal,
    created-at: uint,
    usage-rights: (list 10 (string-ascii 32)),  ;; e.g., "reproduce", "distribute", "derivative", "commercial"
    license-fee-type: (string-ascii 16),        ;; "one-time", "recurring", "usage-based", "free"
    default-fee: uint,                          ;; Default fee amount
    default-duration: (optional uint),          ;; Default duration in blocks
    transferable: bool,                         ;; Whether license can be transferred
    exclusivity-available: bool,                ;; Whether exclusive licenses are available
    territory-restricted: bool,                 ;; Whether license can be territory-restricted
    template-uri: (string-utf8 256)             ;; URI to the full legal template
  }
)

;; Granted licenses
(define-map granted-licenses
  { license-id: uint }
  {
    registration-id: uint,          ;; The IP being licensed
    template-id: uint,              ;; The license template used
    licensor: principal,            ;; Entity granting the license
    licensee: principal,            ;; Entity receiving the license
    granted-at: uint,
    expires-at: (optional uint),
    fee-paid: uint,
    territory: (optional (string-ascii 64)),
    exclusive: bool,
    active: bool,
    usage-counter: uint,            ;; Counter for usage-based licensing
    max-usage: (optional uint),     ;; Max allowed usage
    custom-terms: (optional (string-utf8 1024)),
    revoked: bool,
    revoked-reason: (optional (string-utf8 256))
  }
)

;; Usage logs for IP
(define-map ip-usage-logs
  { registration-id: uint, usage-id: uint }
  {
    licensee: principal,
    license-id: (optional uint),
    usage-type: (string-ascii 32),
    platform: (string-ascii 64),
    usage-hash: (buff 32),          ;; Hash of usage evidence
    timestamp: uint,
    revenue-generated: (optional uint),
    verified: bool,
    verifier: (optional principal)
  }
)

;; Royalty recipients
(define-map royalty-recipients
  { registration-id: uint, recipient: principal }
  {
    share-percentage: uint,         ;; Out of 10000
    recipient-type: (string-ascii 16),  ;; "creator", "collaborator", "label", "publisher", etc.
    active: bool
  }
)

;; Royalty payments
(define-map royalty-payments
  { payment-id: uint }
  {
    registration-id: uint,
    license-id: (optional uint),
    payer: principal,
    amount: uint,
    timestamp: uint,
    usage-id: (optional uint),
    payment-type: (string-ascii 16),  ;; "license-fee", "royalty", "settlement"
    distributed: bool
  }
)

;; Dispute records
(define-map ip-disputes
  { dispute-id: uint }
  {
    registration-id: uint,
    claimant: principal,
    filed-at: uint,
    claim-basis: (string-utf8 256),
    evidence-hash: (buff 32),
    status: (string-ascii 16),      ;; "pending", "resolved", "rejected", "withdrawn"
    resolution: (optional (string-utf8 256)),
    resolver: (optional principal),
    resolved-at: (optional uint)
  }
)

;; Derivative works
(define-map derivative-works
  { original-id: uint, derivative-id: uint }
  {
    relationship-type: (string-ascii 32),  ;; "adaptation", "translation", "remix", etc.
    approved: bool,
    approval-date: (optional uint),
    royalty-percentage: uint        ;; How much goes back to original work
  }
)

;; Next available IDs
(define-data-var next-registration-id uint u0)
(define-data-var next-template-id uint u0)
(define-data-var next-license-id uint u0)
(define-data-var next-dispute-id uint u0)
(define-data-var next-payment-id uint u0)
(define-map next-usage-id { registration-id: uint } { id: uint })

;; Protocol configuration
(define-data-var mediation-address principal tx-sender)
(define-data-var protocol-fee-percentage uint u250)  ;; 2.5% of transactions
(define-data-var dispute-filing-fee uint u1000000)   ;; 1 STX

;; Validation functions
(define-private (validate-registration-id (registration-id uint))
  (if (< registration-id (var-get next-registration-id))
      (ok registration-id)
      (err u"Invalid registration ID"))
)

(define-private (validate-utf8-256 (text (string-utf8 256)))
  (if (> (len text) u0)
      (ok text)
      (err u"Text cannot be empty"))
)

(define-private (validate-utf8-64 (text (string-utf8 64)))
  (if (> (len text) u0)
      (ok text)
      (err u"Text cannot be empty"))
)

(define-private (validate-utf8-1024 (text (string-utf8 1024)))
  (if (> (len text) u0)
      (ok text)
      (err u"Text cannot be empty"))
)

(define-private (validate-content-hash (content-hash (buff 64)))
  (if (> (len content-hash) u0)
      (ok content-hash)
      (err u"Content hash cannot be empty"))
)

(define-private (validate-template-id (template-id uint))
  (if (< template-id (var-get next-template-id))
      (ok template-id)
      (err u"Invalid template ID"))
)

(define-private (validate-license-id (license-id uint))
  (if (< license-id (var-get next-license-id))
      (ok license-id)
      (err u"Invalid license ID"))
)

(define-private (validate-dispute-id (dispute-id uint))
  (if (< dispute-id (var-get next-dispute-id))
      (ok dispute-id)
      (err u"Invalid dispute ID"))
)

(define-private (validate-usage-id (registration-id uint) (usage-id uint))
  (match (map-get? next-usage-id { registration-id: registration-id })
    counter (if (< usage-id (get id counter))
               (ok usage-id)
               (err u"Invalid usage ID"))
    (err u"Registration ID not found"))
)

(define-private (validate-relationship-type (relationship-type (string-ascii 32)))
  (if (or (is-eq relationship-type "adaptation")
          (or (is-eq relationship-type "translation")
              (or (is-eq relationship-type "remix")
                  (is-eq relationship-type "derivative"))))
      (ok relationship-type)
      (err u"Invalid relationship type"))
)

(define-private (validate-usage-type (usage-type (string-ascii 32)))
  (if (or (is-eq usage-type "online-display")
          (or (is-eq usage-type "broadcast")
              (or (is-eq usage-type "print")
                  (or (is-eq usage-type "merchandise")
                      (is-eq usage-type "performance")))))
      (ok usage-type)
      (err u"Invalid usage type"))
)

(define-private (validate-recipient-type (recipient-type (string-ascii 16)))
  (if (or (is-eq recipient-type "creator")
          (or (is-eq recipient-type "collaborator")
              (or (is-eq recipient-type "label")
                  (or (is-eq recipient-type "publisher")
                      (is-eq recipient-type "distributor")))))
      (ok recipient-type)
      (err u"Invalid recipient type"))
)

(define-private (validate-payment-type (payment-type (string-ascii 16)))
  (if (or (is-eq payment-type "license-fee")
          (or (is-eq payment-type "royalty")
              (is-eq payment-type "settlement")))
      (ok payment-type)
      (err u"Invalid payment type"))
)

;; Register new intellectual property
(define-public (register-ip
                (title (string-utf8 256))
                (description (string-utf8 1024))
                (ip-type (string-ascii 32))
                (content-hash (buff 64))
                (public-domain bool)
                (registration-expiry (optional uint)))
  (let
    ((validated-title-resp (validate-utf8-256 title))
     (validated-description-resp (validate-utf8-1024 description))
     (validated-content-hash-resp (validate-content-hash content-hash))
     (registration-id (var-get next-registration-id)))
    
    ;; Validate parameters
    (asserts! (is-valid-ip-type ip-type) (err u"Invalid IP type"))
    (asserts! (is-ok validated-title-resp) (err (unwrap-err! validated-title-resp (err u"Title validation failed"))))
    (asserts! (is-ok validated-description-resp) (err (unwrap-err! validated-description-resp (err u"Description validation failed"))))
    (asserts! (is-ok validated-content-hash-resp) (err (unwrap-err! validated-content-hash-resp (err u"Content hash validation failed"))))
    
    ;; Create the registration
    (map-set ip-registrations
      { registration-id: registration-id }
      {
        title: (unwrap-panic validated-title-resp),
        description: (unwrap-panic validated-description-resp),
        creator: tx-sender,
        created-at: block-height,
        ip-type: ip-type,
        content-hash: (unwrap-panic validated-content-hash-resp),
        status: "registered",
        nft-contract: none,
        nft-id: none,
        public-domain: public-domain,
        registration-expiry: registration-expiry
      }
    )
    
    ;; Set initial ownership
    (map-set ip-ownership
      { registration-id: registration-id, owner: tx-sender }
      {
        share-percentage: u10000,     ;; 100%
        acquired-at: block-height,
        acquired-from: none
      }
    )
    
    ;; Initialize usage counter
    (map-set next-usage-id
      { registration-id: registration-id }
      { id: u0 }
    )
    
    ;; Increment registration ID counter
    (var-set next-registration-id (+ registration-id u1))
    
    (ok registration-id)
  )
)

;; Check if IP type is valid
(define-private (is-valid-ip-type (ip-type (string-ascii 32)))
  (or (is-eq ip-type "image")
      (or (is-eq ip-type "music")
          (or (is-eq ip-type "text")
              (or (is-eq ip-type "code")
                  (or (is-eq ip-type "video")
                      (is-eq ip-type "design"))))))
)

;; Link an NFT to an IP registration
(define-public (link-nft-to-ip
                (registration-id uint)
                (nft-contract principal)
                (nft-id uint))
  (let
    ((validated-id-resp (validate-registration-id registration-id)))
    
    ;; Validate registration ID is valid
    (asserts! (is-ok validated-id-resp) 
              (err (unwrap-err! validated-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-id (unwrap-panic validated-id-resp)))
      ;; Get the registration
      (let ((registration (unwrap! (map-get? ip-registrations { registration-id: validated-id }) 
                                  (err u"Registration not found"))))
        ;; Validate
        (asserts! (is-eq tx-sender (get creator registration)) 
                  (err u"Only creator can link NFT"))
        (asserts! (is-eq (get status registration) "registered") 
                  (err u"Registration not in valid state"))
        
        ;; TODO: In a real implementation, verify NFT ownership
        
        ;; Update registration with NFT info
        (map-set ip-registrations
          { registration-id: validated-id }
          (merge registration 
            { 
              nft-contract: (some nft-contract),
              nft-id: (some nft-id)
            }
          )
        )
        
        (ok true)
      )
    )
  )
)

;; Create a license template
(define-public (create-license-template
                (name (string-utf8 64))
                (description (string-utf8 1024))
                (usage-rights (list 10 (string-ascii 32)))
                (license-fee-type (string-ascii 16))
                (default-fee uint)
                (default-duration (optional uint))
                (transferable bool)
                (exclusivity-available bool)
                (territory-restricted bool)
                (template-uri (string-utf8 256)))
  (let
    ((validated-name-resp (validate-utf8-64 name))
     (validated-description-resp (validate-utf8-1024 description))
     (template-id (var-get next-template-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-name-resp) 
              (err (unwrap-err! validated-name-resp (err u"Name validation failed"))))
    (asserts! (is-ok validated-description-resp) 
              (err (unwrap-err! validated-description-resp (err u"Description validation failed"))))
    (asserts! (is-valid-fee-type license-fee-type) (err u"Invalid fee type"))
    (asserts! (> (len usage-rights) u0) (err u"Must provide at least one usage right"))
    
    (let
      ((validated-name (unwrap-panic validated-name-resp))
       (validated-description (unwrap-panic validated-description-resp)))
      
      ;; Create the template
      (map-set license-templates
        { template-id: template-id }
        {
          name: validated-name,
          description: validated-description,
          creator: tx-sender,
          created-at: block-height,
          usage-rights: usage-rights,
          license-fee-type: license-fee-type,
          default-fee: default-fee,
          default-duration: default-duration,
          transferable: transferable,
          exclusivity-available: exclusivity-available,
          territory-restricted: territory-restricted,
          template-uri: template-uri
        }
      )
      
      ;; Increment template ID counter
      (var-set next-template-id (+ template-id u1))
      
      (ok template-id)
    )
  )
)

;; Check if fee type is valid
(define-private (is-valid-fee-type (fee-type (string-ascii 16)))
  (or (is-eq fee-type "one-time")
      (or (is-eq fee-type "recurring")
          (or (is-eq fee-type "usage-based")
              (is-eq fee-type "free"))))
)

;; Grant a license to use IP - split into free and paid versions
;; This version is for free licenses (fee = 0)
(define-public (grant-free-license
                (registration-id uint)
                (template-id uint)
                (licensee principal)
                (duration (optional uint))
                (territory (optional (string-ascii 64)))
                (exclusive bool)
                (max-usage (optional uint))
                (custom-terms (optional (string-utf8 1024))))
  (let
    ((validated-registration-id-resp (validate-registration-id registration-id))
     (validated-template-id-resp (validate-template-id template-id)))
    
    ;; Check validation results
    (asserts! (is-ok validated-registration-id-resp) 
              (err (unwrap-err! validated-registration-id-resp (err u"Invalid registration ID"))))
    (asserts! (is-ok validated-template-id-resp) 
              (err (unwrap-err! validated-template-id-resp (err u"Invalid template ID"))))
    
    (let ((validated-registration-id (unwrap-panic validated-registration-id-resp))
          (validated-template-id (unwrap-panic validated-template-id-resp)))
      
      ;; Get registration and template records
      (let ((registration (unwrap! (map-get? ip-registrations { registration-id: validated-registration-id }) 
                                 (err u"Registration not found")))
            (template (unwrap! (map-get? license-templates { template-id: validated-template-id }) 
                             (err u"Template not found")))
            (ownership (unwrap! (map-get? ip-ownership 
                               { registration-id: validated-registration-id, owner: tx-sender })
                              (err u"Not an owner of this IP")))
            (license-id (var-get next-license-id)))
        
        ;; Validate
        (asserts! (is-eq (get status registration) "registered") 
                  (err u"Registration not in valid state"))
        (asserts! (not (get public-domain registration)) 
                  (err u"Public domain works don't require licenses"))
        (asserts! (or (not exclusive) (get exclusivity-available template)) 
                  (err u"Exclusive license not available for this template"))
        (asserts! (or (is-none territory) (get territory-restricted template)) 
                  (err u"Territory restrictions not available for this template"))
        
        ;; Calculate expiration if duration provided
        (let ((expiry (if (is-some duration)
                          (some (+ block-height (unwrap-panic duration)))
                          (get default-duration template))))
          
          ;; Create the license grant
          (map-set granted-licenses
            { license-id: license-id }
            {
              registration-id: validated-registration-id,
              template-id: validated-template-id,
              licensor: tx-sender,
              licensee: licensee,
              granted-at: block-height,
              expires-at: expiry,
              fee-paid: u0,  ;; Free license
              territory: territory,
              exclusive: exclusive,
              active: true,
              usage-counter: u0,
              max-usage: max-usage,
              custom-terms: custom-terms,
              revoked: false,
              revoked-reason: none
            }
          )
          
          ;; Increment license ID counter
          (var-set next-license-id (+ license-id u1))
          
          (ok license-id)
        )
      )
    )
  )
)

;; Grant a license with payment
(define-public (grant-paid-license
                (registration-id uint)
                (template-id uint)
                (licensee principal)
                (fee uint)  ;; Must be > 0
                (duration (optional uint))
                (territory (optional (string-ascii 64)))
                (exclusive bool)
                (max-usage (optional uint))
                (custom-terms (optional (string-utf8 1024))))
  (let
    ((validated-registration-id-resp (validate-registration-id registration-id))
     (validated-template-id-resp (validate-template-id template-id)))
    
    ;; Check validation results
    (asserts! (is-ok validated-registration-id-resp) 
              (err (unwrap-err! validated-registration-id-resp (err u"Invalid registration ID"))))
    (asserts! (is-ok validated-template-id-resp) 
              (err (unwrap-err! validated-template-id-resp (err u"Invalid template ID"))))
    
    (let ((validated-registration-id (unwrap-panic validated-registration-id-resp))
          (validated-template-id (unwrap-panic validated-template-id-resp)))
      
      ;; Get registration and template records
      (let ((registration (unwrap! (map-get? ip-registrations { registration-id: validated-registration-id }) 
                                 (err u"Registration not found")))
            (template (unwrap! (map-get? license-templates { template-id: validated-template-id }) 
                             (err u"Template not found")))
            (ownership (unwrap! (map-get? ip-ownership 
                               { registration-id: validated-registration-id, owner: tx-sender })
                              (err u"Not an owner of this IP")))
            (license-id (var-get next-license-id))
            (protocol-fee (/ (* fee (var-get protocol-fee-percentage)) u10000)))
        
        ;; Validate
        (asserts! (is-eq (get status registration) "registered") 
                  (err u"Registration not in valid state"))
        (asserts! (not (get public-domain registration)) 
                  (err u"Public domain works don't require licenses"))
        (asserts! (or (not exclusive) (get exclusivity-available template)) 
                  (err u"Exclusive license not available for this template"))
        (asserts! (or (is-none territory) (get territory-restricted template)) 
                  (err u"Territory restrictions not available for this template"))
        (asserts! (> fee u0) (err u"Fee must be greater than 0"))
        
        ;; Transfer fee from licensee
        (asserts! (is-ok (stx-transfer? fee licensee (as-contract tx-sender))) 
                  (err u"License fee transfer failed"))
        
        ;; Transfer protocol fee
        (asserts! (is-ok (as-contract (stx-transfer? protocol-fee tx-sender (var-get mediation-address))))
                  (err u"Protocol fee transfer failed"))
        
        ;; Calculate expiration if duration provided
        (let ((expiry (if (is-some duration)
                          (some (+ block-height (unwrap-panic duration)))
                          (get default-duration template))))
          
          ;; Create the license grant
          (map-set granted-licenses
            { license-id: license-id }
            {
              registration-id: validated-registration-id,
              template-id: validated-template-id,
              licensor: tx-sender,
              licensee: licensee,
              granted-at: block-height,
              expires-at: expiry,
              fee-paid: fee,
              territory: territory,
              exclusive: exclusive,
              active: true,
              usage-counter: u0,
              max-usage: max-usage,
              custom-terms: custom-terms,
              revoked: false,
              revoked-reason: none
            }
          )
          
          ;; Record payment
          (let ((payment-id (var-get next-payment-id)))
            ;; Create payment record
            (map-set royalty-payments
              { payment-id: payment-id }
              {
                registration-id: validated-registration-id,
                license-id: (some license-id),
                payer: licensee,
                amount: fee,
                timestamp: block-height,
                usage-id: none,
                payment-type: "license-fee",
                distributed: true  ;; Simplified for this example
              }
            )
            
            ;; Increment payment ID counter
            (var-set next-payment-id (+ payment-id u1))
          )
          
          ;; Increment license ID counter
          (var-set next-license-id (+ license-id u1))
          
          (ok license-id)
        )
      )
    )
  )
)

;; Record IP usage
(define-public (record-ip-usage
                (registration-id uint)
                (license-id (optional uint))
                (usage-type (string-ascii 32))
                (platform (string-ascii 64))
                (usage-hash (buff 32))
                (revenue-generated (optional uint)))
  (let
    ((validated-registration-id-resp (validate-registration-id registration-id))
     (validated-usage-type-resp (validate-usage-type usage-type)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-registration-id-resp) 
              (err (unwrap-err! validated-registration-id-resp (err u"Invalid registration ID"))))
    (asserts! (is-ok validated-usage-type-resp) 
              (err (unwrap-err! validated-usage-type-resp (err u"Invalid usage type"))))
    
    (let ((validated-registration-id (unwrap-panic validated-registration-id-resp))
          (validated-usage-type (unwrap-panic validated-usage-type-resp)))
      
      ;; Get registration and usage counter
      (let ((registration (unwrap! (map-get? ip-registrations 
                                 { registration-id: validated-registration-id }) 
                                (err u"Registration not found")))
            (usage-counter (unwrap! (map-get? next-usage-id 
                                  { registration-id: validated-registration-id }) 
                                   (err u"Counter not found")))
            (usage-id (get id usage-counter)))
        
        ;; Validate license if provided
        (if (is-some license-id)
            (let ((license-id-value (unwrap-panic license-id))
                  (validated-license-id-resp (validate-license-id (unwrap-panic license-id))))
              
              (asserts! (is-ok validated-license-id-resp)
                        (err (unwrap-err! validated-license-id-resp (err u"Invalid license ID"))))
              
              (let ((validated-license-id (unwrap-panic validated-license-id-resp))
                    (license (unwrap! (map-get? granted-licenses 
                                     { license-id: validated-license-id })
                                    (err u"License not found"))))
                ;; Check license validity
                (asserts! (and (is-eq (get registration-id license) validated-registration-id)
                              (is-eq (get licensee license) tx-sender))
                          (err u"Invalid license for this usage"))
                (asserts! (get active license) (err u"License not active"))
                (asserts! (not (get revoked license)) (err u"License revoked"))
                
                ;; Check license expiration
                (if (is-some (get expires-at license))
                    (asserts! (< block-height (unwrap-panic (get expires-at license))) 
                              (err u"License expired"))
                    true)
                
                ;; Check usage limits
                (if (is-some (get max-usage license))
                    (asserts! (< (get usage-counter license) (unwrap-panic (get max-usage license)))
                              (err u"Usage limit exceeded"))
                    true)
                
                ;; Update usage counter for license
                (map-set granted-licenses
                  { license-id: validated-license-id }
                  (merge license { usage-counter: (+ (get usage-counter license) u1) })
                )
              )
            )
            ;; If no license provided, ensure the work is public domain
            (asserts! (get public-domain registration) (err u"Non-public domain works require a license"))
        )
        
        ;; Create the usage record
        (map-set ip-usage-logs
          { registration-id: validated-registration-id, usage-id: usage-id }
          {
            licensee: tx-sender,
            license-id: license-id,
            usage-type: validated-usage-type,
            platform: platform,
            usage-hash: usage-hash,
            timestamp: block-height,
            revenue-generated: revenue-generated,
            verified: false,
            verifier: none
          }
        )
        
        ;; Increment usage counter
        (map-set next-usage-id
          { registration-id: validated-registration-id }
          { id: (+ usage-id u1) }
        )
        
        ;; If revenue was generated, process royalty payment
        (if (and (is-some revenue-generated) (> (unwrap-panic revenue-generated) u0))
            (record-usage-royalty validated-registration-id usage-id (unwrap-panic revenue-generated))
            (ok usage-id))
      )
    )
  )
)

;; Record royalty from usage revenue
(define-public (record-usage-royalty (registration-id uint) (usage-id uint) (revenue uint))
  (let
    ((validated-registration-id-resp (validate-registration-id registration-id)))
    
    ;; Validate registration ID
    (asserts! (is-ok validated-registration-id-resp) 
              (err (unwrap-err! validated-registration-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-registration-id (unwrap-panic validated-registration-id-resp)))
      ;; Validate usage ID with the unwrapped registration ID
      (let ((validated-usage-id-resp (validate-usage-id validated-registration-id usage-id)))
        
        ;; Check if usage ID is valid
        (asserts! (is-ok validated-usage-id-resp)
                  (err (unwrap-err! validated-usage-id-resp (err u"Invalid usage ID"))))
        
        (let ((validated-usage-id (unwrap-panic validated-usage-id-resp))
              (standard-royalty-rate u1000)  ;; 10% standard rate
              (royalty-amount (/ (* revenue standard-royalty-rate) u10000))
              (payment-id (var-get next-payment-id)))
          
          ;; Create payment record
          (map-set royalty-payments
            { payment-id: payment-id }
            {
              registration-id: validated-registration-id,
              license-id: none,
              payer: tx-sender,
              amount: royalty-amount,
              timestamp: block-height,
              usage-id: (some validated-usage-id),
              payment-type: "royalty",
              distributed: false
            }
          )
          
          ;; Increment payment ID counter
          (var-set next-payment-id (+ payment-id u1))
          
          ;; Transfer royalty payment
          (asserts! (is-ok (stx-transfer? royalty-amount tx-sender (as-contract tx-sender)))
                    (err u"Royalty payment transfer failed"))
          
          ;; Mark as distributed
          (map-set royalty-payments
            { payment-id: payment-id }
            (merge (unwrap-panic (map-get? royalty-payments { payment-id: payment-id }))
              { distributed: true })
          )
          
          (ok payment-id)
        )
      )
    )
  )
)

;; Verify IP usage
(define-public (verify-ip-usage (registration-id uint) (usage-id uint))
  (let
    ((validated-registration-id-resp (validate-registration-id registration-id)))
    
    ;; Validate registration ID
    (asserts! (is-ok validated-registration-id-resp) 
              (err (unwrap-err! validated-registration-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-registration-id (unwrap-panic validated-registration-id-resp)))
      ;; Validate usage ID with the unwrapped registration ID
      (let ((validated-usage-id-resp (validate-usage-id validated-registration-id usage-id)))
        
        ;; Check if usage ID is valid
        (asserts! (is-ok validated-usage-id-resp)
                  (err (unwrap-err! validated-usage-id-resp (err u"Invalid usage ID"))))
        
        (let ((validated-usage-id (unwrap-panic validated-usage-id-resp))
              (registration (unwrap! (map-get? ip-registrations 
                                     { registration-id: validated-registration-id }) 
                                    (err u"Registration not found")))
              (usage (unwrap! (map-get? ip-usage-logs 
                             { registration-id: validated-registration-id, usage-id: validated-usage-id })
                            (err u"Usage not found"))))
          
          ;; Validate
          (asserts! (or (is-eq tx-sender (get creator registration))
                       (is-ip-owner validated-registration-id tx-sender))
                    (err u"Not authorized to verify usage"))
          
          ;; Update usage verification
          (map-set ip-usage-logs
            { registration-id: validated-registration-id, usage-id: validated-usage-id }
            (merge usage { 
              verified: true,
              verifier: (some tx-sender)
            })
          )
          
          (ok true)
        )
      )
    )
  )
)

;; Check if principal is an IP owner
(define-private (is-ip-owner (registration-id uint) (user principal))
  (is-some (map-get? ip-ownership { registration-id: registration-id, owner: user }))
)

;; Transfer IP ownership shares
(define-public (transfer-ip-shares
                (registration-id uint)
                (recipient principal)
                (share-percentage uint))
  (let
    ((validated-registration-id-resp (validate-registration-id registration-id)))
    
    ;; Validate registration ID
    (asserts! (is-ok validated-registration-id-resp) 
              (err (unwrap-err! validated-registration-id-resp (err u"Invalid registration ID"))))
    
    (let ((validated-registration-id (unwrap-panic validated-registration-id-resp))
          (registration (unwrap! (map-get? ip-registrations 
                               { registration-id: (unwrap-panic validated-registration-id-resp) }) 
                              (err u"Registration not found")))
          (sender-ownership (unwrap! (map-get? ip-ownership 
                                   { registration-id: (unwrap-panic validated-registration-id-resp), owner: tx-sender })
                                  (err u"No ownership found")))
          (recipient-ownership (map-get? ip-ownership 
                              { registration-id: (unwrap-panic validated-registration-id-resp), owner: recipient })))
      
      ;; Validate
      (asserts! (is-eq (get status registration) "registered") 
                (err u"Registration not in valid state"))
      (asserts! (<= share-percentage (get share-percentage sender-ownership)) 
                (err u"Insufficient ownership shares"))
      (asserts! (> share-percentage u0) 
                (err u"Share percentage must be greater than zero"))
      
      ;; Update sender's ownership
      (map-set ip-ownership
        { registration-id: validated-registration-id, owner: tx-sender }
        (merge sender-ownership 
          { share-percentage: (- (get share-percentage sender-ownership) share-percentage) }
        )
      )
      
      ;; Update or create recipient's ownership
      (if (is-some recipient-ownership)
          (map-set ip-ownership
            { registration-id: validated-registration-id, owner: recipient }
            (merge (unwrap-panic recipient-ownership)
              { 
                share-percentage: (+ (get share-percentage (unwrap-panic recipient-ownership)) 
                                   share-percentage),
                acquired-at: block-height,
                acquired-from: (some tx-sender)
              }
            )
          )
          (map-set ip-ownership
            { registration-id: validated-registration-id, owner: recipient }
            {
              share-percentage: share-percentage,
              acquired-at: block-height,
              acquired-from: (some tx-sender)
            }
          )
      )
      
      (ok true)
    )
  )
)