;; Temporal Distortion Generator
;; Experimental device that manipulates local spacetime to create practice bubbles 
;; where hours of practice occur in minutes of real time.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PORTAL_NOT_FOUND (err u101))
(define-constant ERR_PORTAL_ACTIVE (err u102))
(define-constant ERR_INVALID_PARAMETERS (err u103))
(define-constant ERR_INSUFFICIENT_ENERGY (err u104))
(define-constant ERR_TEMPORAL_OVERLOAD (err u105))
(define-constant ERR_PARADOX_DETECTED (err u106))
(define-constant ERR_PORTAL_FULL (err u107))

(define-constant MAX_PORTALS u100)
(define-constant MAX_TIME_RATIO u10)
(define-constant MIN_TIME_RATIO u1)
(define-constant BASE_ENERGY_COST u1000)
(define-constant MAX_PARTICIPANTS_PER_PORTAL u50)

;; Data Variables
(define-data-var next-portal-id uint u1)
(define-data-var total-active-portals uint u0)
(define-data-var total-energy-consumed uint u0)
(define-data-var system-status bool true)
(define-data-var emergency-shutdown bool false)

;; Data Maps
(define-map temporal-portals uint {
    creator: principal,
    time-ratio: uint,
    energy-cost: uint,
    start-time: uint,
    duration: uint,
    participants: uint,
    max-participants: uint,
    active: bool,
    stability-rating: uint
})

(define-map authorized-operators principal bool)

(define-map user-portal-sessions principal {
    total-sessions: uint,
    active-portal-id: (optional uint),
    total-practice-time: uint,
    energy-debt: uint
})

(define-map portal-participants uint (list 50 principal))

(define-map temporal-field-readings uint {
    dimensional-flux: uint,
    chrono-stability: uint,
    energy-efficiency: uint,
    paradox-risk: uint,
    last-updated: uint
})

;; Private Functions
(define-private (calculate-energy-cost (time-ratio uint) (duration uint) (participants uint))
    (+ BASE_ENERGY_COST 
       (* time-ratio duration)
       (* participants u100)))

(define-private (is-authorized (user principal))
    (or (is-eq user CONTRACT_OWNER)
        (default-to false (map-get? authorized-operators user))))

(define-private (validate-temporal-parameters (time-ratio uint) (duration uint))
    (and (>= time-ratio MIN_TIME_RATIO)
         (<= time-ratio MAX_TIME_RATIO)
         (> duration u0)
         (<= duration u86400))) ;; Max 24 hours

(define-private (check-paradox-risk (portal-id uint) (new-time-ratio uint))
    (let ((current-portals (var-get total-active-portals))
          (temporal-stress (* current-portals new-time-ratio)))
        (< temporal-stress u500))) ;; Prevent temporal overload

(define-private (update-stability-rating (portal-id uint))
    (let ((portal-data (unwrap! (map-get? temporal-portals portal-id) u0))
          (field-data (default-to 
              { dimensional-flux: u100, chrono-stability: u100, 
                energy-efficiency: u100, paradox-risk: u0, last-updated: u0 }
              (map-get? temporal-field-readings portal-id))))
        (let ((stability (- u1000 
                            (+ (get paradox-risk field-data)
                               (/ (get dimensional-flux field-data) u10)))))
            (begin
                (map-set temporal-portals portal-id 
                    (merge portal-data { stability-rating: stability }))
                stability))))

;; Public Functions
(define-public (initialize-system)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set system-status true)
        (var-set emergency-shutdown false)
        (ok true)))

(define-public (authorize-operator (operator principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set authorized-operators operator true)
        (ok true)))

(define-public (create-temporal-portal (time-ratio uint) (duration uint) (max-participants uint))
    (let ((portal-id (var-get next-portal-id))
          (energy-cost (calculate-energy-cost time-ratio duration max-participants)))
        (asserts! (var-get system-status) ERR_UNAUTHORIZED)
        (asserts! (not (var-get emergency-shutdown)) ERR_UNAUTHORIZED)
        (asserts! (< (var-get total-active-portals) MAX_PORTALS) ERR_PORTAL_FULL)
        (asserts! (validate-temporal-parameters time-ratio duration) ERR_INVALID_PARAMETERS)
        (asserts! (<= max-participants MAX_PARTICIPANTS_PER_PORTAL) ERR_INVALID_PARAMETERS)
        (asserts! (check-paradox-risk portal-id time-ratio) ERR_PARADOX_DETECTED)
        
        ;; Create the portal
        (map-set temporal-portals portal-id {
            creator: tx-sender,
            time-ratio: time-ratio,
            energy-cost: energy-cost,
            start-time: burn-block-height,
            duration: duration,
            participants: u0,
            max-participants: max-participants,
            active: true,
            stability-rating: u1000
        })
        
        ;; Initialize field readings
        (map-set temporal-field-readings portal-id {
            dimensional-flux: u50,
            chrono-stability: u950,
            energy-efficiency: u850,
            paradox-risk: u25,
            last-updated: burn-block-height
        })
        
        ;; Initialize empty participant list
        (map-set portal-participants portal-id (list))
        
        ;; Update system state
        (var-set next-portal-id (+ portal-id u1))
        (var-set total-active-portals (+ (var-get total-active-portals) u1))
        (var-set total-energy-consumed (+ (var-get total-energy-consumed) energy-cost))
        
        (ok portal-id)))

(define-public (join-temporal-portal (portal-id uint))
    (let ((portal-data (unwrap! (map-get? temporal-portals portal-id) ERR_PORTAL_NOT_FOUND))
          (current-participants (default-to (list) (map-get? portal-participants portal-id)))
          (user-data (default-to 
              { total-sessions: u0, active-portal-id: none, 
                total-practice-time: u0, energy-debt: u0 }
              (map-get? user-portal-sessions tx-sender))))
        
        (asserts! (get active portal-data) ERR_PORTAL_NOT_FOUND)
        (asserts! (< (get participants portal-data) (get max-participants portal-data)) ERR_PORTAL_FULL)
        (asserts! (is-none (get active-portal-id user-data)) ERR_PORTAL_ACTIVE)
        
        ;; Add user to portal
        (let ((updated-participants (unwrap! (as-max-len? (append current-participants tx-sender) u50) ERR_PORTAL_FULL)))
            (map-set portal-participants portal-id updated-participants)
            (map-set temporal-portals portal-id 
                (merge portal-data { participants: (+ (get participants portal-data) u1) }))
            
            ;; Update user session
            (map-set user-portal-sessions tx-sender
                (merge user-data { 
                    active-portal-id: (some portal-id),
                    total-sessions: (+ (get total-sessions user-data) u1)
                }))
            
            (ok true))))

(define-public (exit-temporal-portal (portal-id uint))
    (let ((portal-data (unwrap! (map-get? temporal-portals portal-id) ERR_PORTAL_NOT_FOUND))
          (user-data (unwrap! (map-get? user-portal-sessions tx-sender) ERR_UNAUTHORIZED))
          (practice-time (* (get time-ratio portal-data) 
                           (- burn-block-height (get start-time portal-data)))))
        
        (asserts! (is-some (get active-portal-id user-data)) ERR_PORTAL_NOT_FOUND)
        (asserts! (is-eq (unwrap-panic (get active-portal-id user-data)) portal-id) ERR_UNAUTHORIZED)
        
        ;; Update user practice time and clear active portal
        (map-set user-portal-sessions tx-sender
            (merge user-data {
                active-portal-id: none,
                total-practice-time: (+ (get total-practice-time user-data) practice-time)
            }))
        
        ;; Update portal participant count
        (map-set temporal-portals portal-id
            (merge portal-data { participants: (- (get participants portal-data) u1) }))
        
        (ok practice-time)))

(define-public (emergency-shutdown-system)
    (begin
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (var-set emergency-shutdown true)
        (var-set system-status false)
        (ok true)))

(define-public (close-temporal-portal (portal-id uint))
    (let ((portal-data (unwrap! (map-get? temporal-portals portal-id) ERR_PORTAL_NOT_FOUND)))
        (asserts! (or (is-eq tx-sender (get creator portal-data))
                     (is-authorized tx-sender)) ERR_UNAUTHORIZED)
        
        ;; Deactivate portal
        (map-set temporal-portals portal-id
            (merge portal-data { active: false }))
        
        ;; Update system counters
        (var-set total-active-portals (- (var-get total-active-portals) u1))
        
        (ok true)))

;; Read-only Functions
(define-read-only (get-portal-info (portal-id uint))
    (map-get? temporal-portals portal-id))

(define-read-only (get-system-status)
    { 
        system-active: (var-get system-status),
        emergency-mode: (var-get emergency-shutdown),
        active-portals: (var-get total-active-portals),
        total-energy-used: (var-get total-energy-consumed),
        next-portal-id: (var-get next-portal-id)
    })

(define-read-only (get-user-session (user principal))
    (map-get? user-portal-sessions user))

(define-read-only (get-temporal-readings (portal-id uint))
    (map-get? temporal-field-readings portal-id))

(define-read-only (calculate-practice-time (portal-id uint))
    (match (map-get? temporal-portals portal-id)
        portal-data (let ((elapsed-time (- burn-block-height (get start-time portal-data))))
                        (some (* (get time-ratio portal-data) elapsed-time)))
        none))
