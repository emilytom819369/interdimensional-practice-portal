;; Dimensional Stability Monitor
;; Safety system that ensures practice portals remain stable and prevents 
;; temporal paradoxes or dimensional bleeding effects.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_PORTAL_NOT_FOUND (err u201))
(define-constant ERR_CRITICAL_INSTABILITY (err u202))
(define-constant ERR_SYSTEM_OFFLINE (err u203))
(define-constant ERR_DIMENSIONAL_BREACH (err u204))
(define-constant ERR_PARADOX_IMMINENT (err u205))
(define-constant ERR_INVALID_SENSOR_DATA (err u206))

(define-constant STABILITY_THRESHOLD u800)
(define-constant CRITICAL_THRESHOLD u500)
(define-constant PARADOX_THRESHOLD u300)
(define-constant MAX_DIMENSIONAL_VARIANCE u100)
(define-constant SENSOR_TIMEOUT_BLOCKS u144) ;; ~24 hours
(define-constant EMERGENCY_COOLDOWN_BLOCKS u72) ;; ~12 hours

;; Data Variables
(define-data-var system-active bool true)
(define-data-var total-monitored-portals uint u0)
(define-data-var emergency-protocol-active bool false)
(define-data-var last-emergency-trigger uint u0)
(define-data-var global-stability-index uint u1000)
(define-data-var dimensional-breach-count uint u0)

;; Data Maps
(define-map dimensional-readings uint {
    portal-id: uint,
    stability-level: uint,
    dimensional-variance: uint,
    temporal-coherence: uint,
    paradox-probability: uint,
    energy-fluctuation: uint,
    last-reading: uint,
    critical-events: uint
})

(define-map authorized-monitors principal bool)

(define-map portal-alert-status uint {
    alert-level: uint, ;; 0=normal, 1=warning, 2=critical, 3=emergency
    alert-message: (string-ascii 256),
    triggered-at: uint,
    auto-shutdown-pending: bool
})

(define-map stability-history uint (list 20 {
    timestamp: uint,
    stability: uint,
    variance: uint
}))

(define-map sensor-network uint {
    sensor-id: uint,
    location-id: uint,
    operational: bool,
    calibration-drift: uint,
    last-maintenance: uint,
    fault-count: uint
})

(define-map emergency-protocols uint {
    protocol-type: (string-ascii 64),
    trigger-condition: uint,
    auto-execute: bool,
    execution-count: uint,
    last-executed: uint
})

;; Private Functions
(define-private (is-authorized (user principal))
    (or (is-eq user CONTRACT_OWNER)
        (default-to false (map-get? authorized-monitors user))))

(define-private (calculate-stability-score (variance uint) (coherence uint) (paradox uint))
    (let ((base-stability (- u1000 (/ (* variance u2) u1)))
          (coherence-bonus (/ coherence u10))
          (paradox-penalty (* paradox u3)))
        (if (> (+ base-stability coherence-bonus) paradox-penalty)
            (- (+ base-stability coherence-bonus) paradox-penalty)
            u0)))

(define-private (assess-critical-risk (portal-id uint))
    (match (map-get? dimensional-readings portal-id)
        reading-data (let ((stability (get stability-level reading-data))
                          (variance (get dimensional-variance reading-data))
                          (paradox (get paradox-probability reading-data)))
                        (or (< stability CRITICAL_THRESHOLD)
                            (> variance MAX_DIMENSIONAL_VARIANCE)
                            (> paradox PARADOX_THRESHOLD)))
        false))

(define-private (update-stability-history (portal-id uint) (new-stability uint) (new-variance uint))
    (let ((new-entry { timestamp: burn-block-height, stability: new-stability, variance: new-variance }))
        (map-set stability-history portal-id (list new-entry))))

(define-private (calculate-global-stability)
    (let ((total-portals (var-get total-monitored-portals)))
        (if (is-eq total-portals u0)
            u1000
            (let ((breach-impact (* (var-get dimensional-breach-count) u50))
                  (emergency-impact (if (var-get emergency-protocol-active) u200 u0)))
                (if (> (+ breach-impact emergency-impact) u1000)
                    u0
                    (- u1000 (+ breach-impact emergency-impact)))))))

(define-private (trigger-emergency-protocol (portal-id uint) (protocol-type (string-ascii 64)))
    (begin
        (var-set emergency-protocol-active true)
        (var-set last-emergency-trigger burn-block-height)
        (map-set portal-alert-status portal-id {
            alert-level: u3,
            alert-message: "EMERGENCY: Dimensional breach imminent - auto-shutdown initiated",
            triggered-at: burn-block-height,
            auto-shutdown-pending: true
        })
        (var-set dimensional-breach-count (+ (var-get dimensional-breach-count) u1))
        true))

;; Public Functions
(define-public (initialize-monitoring-system)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set system-active true)
        (var-set emergency-protocol-active false)
        (var-set global-stability-index u1000)
        (ok true)))

(define-public (authorize-monitor (monitor principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set authorized-monitors monitor true)
        (ok true)))

(define-public (register-portal-monitoring (portal-id uint))
    (begin
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (var-get system-active) ERR_SYSTEM_OFFLINE)
        
        ;; Initialize monitoring data
        (map-set dimensional-readings portal-id {
            portal-id: portal-id,
            stability-level: u1000,
            dimensional-variance: u10,
            temporal-coherence: u950,
            paradox-probability: u5,
            energy-fluctuation: u25,
            last-reading: burn-block-height,
            critical-events: u0
        })
        
        ;; Set normal alert status
        (map-set portal-alert-status portal-id {
            alert-level: u0,
            alert-message: "Portal monitoring active - all systems nominal",
            triggered-at: burn-block-height,
            auto-shutdown-pending: false
        })
        
        ;; Update system counters
        (var-set total-monitored-portals (+ (var-get total-monitored-portals) u1))
        (var-set global-stability-index (calculate-global-stability))
        
        (ok portal-id)))

(define-public (update-dimensional-readings (portal-id uint) (stability uint) (variance uint) (coherence uint) (paradox uint) (energy-flux uint))
    (begin
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (var-get system-active) ERR_SYSTEM_OFFLINE)
        (asserts! (is-some (map-get? dimensional-readings portal-id)) ERR_PORTAL_NOT_FOUND)
        (asserts! (<= stability u1000) ERR_INVALID_SENSOR_DATA)
        (asserts! (<= variance u1000) ERR_INVALID_SENSOR_DATA)
        (asserts! (<= coherence u1000) ERR_INVALID_SENSOR_DATA)
        (asserts! (<= paradox u1000) ERR_INVALID_SENSOR_DATA)
        
        (let ((current-reading (unwrap! (map-get? dimensional-readings portal-id) ERR_PORTAL_NOT_FOUND))
              (calculated-stability (calculate-stability-score variance coherence paradox)))
            
            ;; Update readings
            (map-set dimensional-readings portal-id {
                portal-id: portal-id,
                stability-level: calculated-stability,
                dimensional-variance: variance,
                temporal-coherence: coherence,
                paradox-probability: paradox,
                energy-fluctuation: energy-flux,
                last-reading: burn-block-height,
                critical-events: (if (assess-critical-risk portal-id) 
                                    (+ (get critical-events current-reading) u1)
                                    (get critical-events current-reading))
            })
            
            ;; Update stability history
            (update-stability-history portal-id calculated-stability variance)
            
            ;; Check for alerts and emergency conditions
            (if (< calculated-stability CRITICAL_THRESHOLD)
                (if (< calculated-stability PARADOX_THRESHOLD)
                    ;; Critical emergency
                    (trigger-emergency-protocol portal-id "PARADOX_PREVENTION")
                    ;; Warning alert
                    (map-set portal-alert-status portal-id {
                        alert-level: u2,
                        alert-message: "CRITICAL: Portal stability compromised - immediate attention required",
                        triggered-at: burn-block-height,
                        auto-shutdown-pending: true
                    }))
                ;; Normal or warning levels
                (if (< calculated-stability STABILITY_THRESHOLD)
                    (map-set portal-alert-status portal-id {
                        alert-level: u1,
                        alert-message: "WARNING: Portal stability below optimal threshold",
                        triggered-at: burn-block-height,
                        auto-shutdown-pending: false
                    })
                    (map-set portal-alert-status portal-id {
                        alert-level: u0,
                        alert-message: "Portal operating within normal parameters",
                        triggered-at: burn-block-height,
                        auto-shutdown-pending: false
                    })))
            
            ;; Update global stability
            (var-set global-stability-index (calculate-global-stability))
            
            (ok calculated-stability))))

(define-public (execute-emergency-shutdown (portal-id uint))
    (begin
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (assess-critical-risk portal-id) ERR_CRITICAL_INSTABILITY)
        
        ;; Force emergency shutdown
        (trigger-emergency-protocol portal-id "EMERGENCY_SHUTDOWN")
        
        ;; Mark portal for immediate closure
        (map-set portal-alert-status portal-id {
            alert-level: u3,
            alert-message: "EMERGENCY SHUTDOWN: Portal forcibly closed due to critical instability",
            triggered-at: burn-block-height,
            auto-shutdown-pending: false
        })
        
        (ok true)))

(define-public (clear-emergency-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> (- burn-block-height (var-get last-emergency-trigger)) EMERGENCY_COOLDOWN_BLOCKS) ERR_CRITICAL_INSTABILITY)
        
        (var-set emergency-protocol-active false)
        (var-set global-stability-index (calculate-global-stability))
        
        (ok true)))

(define-public (deregister-portal (portal-id uint))
    (begin
        (asserts! (is-authorized tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? dimensional-readings portal-id)) ERR_PORTAL_NOT_FOUND)
        
        ;; Remove portal from monitoring
        (map-delete dimensional-readings portal-id)
        (map-delete portal-alert-status portal-id)
        (map-delete stability-history portal-id)
        
        ;; Update counters
        (var-set total-monitored-portals (- (var-get total-monitored-portals) u1))
        (var-set global-stability-index (calculate-global-stability))
        
        (ok true)))

;; Read-only Functions
(define-read-only (get-portal-readings (portal-id uint))
    (map-get? dimensional-readings portal-id))

(define-read-only (get-alert-status (portal-id uint))
    (map-get? portal-alert-status portal-id))

(define-read-only (get-monitoring-status)
    {
        system-active: (var-get system-active),
        emergency-active: (var-get emergency-protocol-active),
        monitored-portals: (var-get total-monitored-portals),
        global-stability: (var-get global-stability-index),
        breach-count: (var-get dimensional-breach-count),
        last-emergency: (var-get last-emergency-trigger)
    })

(define-read-only (get-stability-history (portal-id uint))
    (map-get? stability-history portal-id))

(define-read-only (check-portal-safety (portal-id uint))
    (match (map-get? dimensional-readings portal-id)
        reading-data {
            safe: (>= (get stability-level reading-data) STABILITY_THRESHOLD),
            stability-score: (get stability-level reading-data),
            risk-level: (if (< (get stability-level reading-data) PARADOX_THRESHOLD) u3
                           (if (< (get stability-level reading-data) CRITICAL_THRESHOLD) u2
                              (if (< (get stability-level reading-data) STABILITY_THRESHOLD) u1 u0))),
            last-updated: (get last-reading reading-data)
        }
        { safe: false, stability-score: u0, risk-level: u3, last-updated: u0 }))

(define-read-only (is-emergency-shutdown-required (portal-id uint))
    (assess-critical-risk portal-id))
