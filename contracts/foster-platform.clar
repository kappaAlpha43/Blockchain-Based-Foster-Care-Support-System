;; Foster Care Support System - Main Contract
;; Comprehensive platform for foster family coordination with child placement matching,
;; support service delivery, and outcome tracking

;; Error constants
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-placement-denied (err u104))
(define-constant err-insufficient-capacity (err u105))
(define-constant err-invalid-status (err u106))
(define-constant err-training-required (err u107))
(define-constant err-assessment-pending (err u108))

;; Contract owner and authorized personnel
(define-constant contract-owner tx-sender)
(define-data-var authorized-caseworkers (list 50 principal) (list))

;; Child welfare priority levels
(define-constant priority-critical u1)
(define-constant priority-high u2)
(define-constant priority-medium u3)
(define-constant priority-low u4)

;; Child status constants
(define-constant status-available u1)
(define-constant status-placed u2)
(define-constant status-reunified u3)
(define-constant status-adopted u4)

;; Foster family status constants
(define-constant family-active u1)
(define-constant family-inactive u2)
(define-constant family-suspended u3)
(define-constant family-training u4)

;; Data structures
(define-map children
  { child-id: uint }
  {
    name: (string-ascii 100),
    age: uint,
    special-needs: (list 10 (string-ascii 50)),
    priority-level: uint,
    status: uint,
    placement-history: (list 20 principal),
    current-placement: (optional principal),
    case-worker: principal,
    created-at: uint,
    last-updated: uint
  }
)

(define-map foster-families
  { family-id: principal }
  {
    family-name: (string-ascii 100),
    capacity: uint,
    current-placements: uint,
    specializations: (list 10 (string-ascii 50)),
    location: (string-ascii 100),
    status: uint,
    certification-level: uint,
    training-completed: (list 20 (string-ascii 50)),
    stability-score: uint,
    success-rate: uint,
    created-at: uint,
    last-assessment: uint
  }
)

(define-map placements
  { placement-id: uint }
  {
    child-id: uint,
    family-id: principal,
    start-date: uint,
    end-date: (optional uint),
    status: uint,
    support-services: (list 10 (string-ascii 50)),
    outcome-metrics: {
      stability-rating: uint,
      child-wellbeing: uint,
      family-satisfaction: uint,
      case-progress: uint
    },
    notes: (string-ascii 500),
    created-by: principal
  }
)

(define-map support-services
  { service-id: uint }
  {
    service-type: (string-ascii 50),
    provider: principal,
    child-id: uint,
    family-id: principal,
    scheduled-date: uint,
    completion-date: (optional uint),
    status: uint,
    effectiveness-rating: uint,
    notes: (string-ascii 300)
  }
)

(define-map training-programs
  { program-id: uint }
  {
    program-name: (string-ascii 100),
    description: (string-ascii 300),
    duration-hours: uint,
    certification-level: uint,
    prerequisites: (list 5 (string-ascii 50)),
    created-by: principal
  }
)

(define-map family-training-records
  { family-id: principal, program-id: uint }
  {
    completion-date: uint,
    score: uint,
    certified-by: principal,
    expiry-date: uint,
    notes: (string-ascii 200)
  }
)

;; Counter variables
(define-data-var next-child-id uint u1)
(define-data-var next-placement-id uint u1)
(define-data-var next-service-id uint u1)
(define-data-var next-program-id uint u1)

;; Statistics tracking
(define-data-var total-children uint u0)
(define-data-var total-families uint u0)
(define-data-var successful-placements uint u0)
(define-data-var failed-placements uint u0)

;; Authorization functions
(define-private (is-authorized (user principal))
  (or (is-eq user contract-owner)
      (is-some (index-of (var-get authorized-caseworkers) user)))
)

(define-public (add-caseworker (caseworker principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set authorized-caseworkers
      (unwrap! (as-max-len? (append (var-get authorized-caseworkers) caseworker) u50)
               err-insufficient-capacity))
    (ok true)
  )
)

;; Child management functions
(define-public (register-child
  (name (string-ascii 100))
  (age uint)
  (special-needs (list 10 (string-ascii 50)))
  (priority-level uint)
  (case-worker principal))
  (let ((child-id (var-get next-child-id)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (and (>= priority-level priority-critical) (<= priority-level priority-low)) err-invalid-input)
    (asserts! (> age u0) err-invalid-input)

    (map-set children
      { child-id: child-id }
      {
        name: name,
        age: age,
        special-needs: special-needs,
        priority-level: priority-level,
        status: status-available,
        placement-history: (list),
        current-placement: none,
        case-worker: case-worker,
        created-at: stacks-block-height,
        last-updated: stacks-block-height
      }
    )

    (var-set next-child-id (+ child-id u1))
    (var-set total-children (+ (var-get total-children) u1))
    (ok child-id)
  )
)

(define-public (update-child-status (child-id uint) (new-status uint))
  (let ((child-data (unwrap! (map-get? children { child-id: child-id }) err-not-found)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (and (>= new-status status-available) (<= new-status status-adopted)) err-invalid-status)

    (map-set children
      { child-id: child-id }
      (merge child-data {
        status: new-status,
        last-updated: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Foster family management functions
(define-public (register-family
  (family-name (string-ascii 100))
  (capacity uint)
  (specializations (list 10 (string-ascii 50)))
  (location (string-ascii 100)))
  (begin
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (> capacity u0) err-invalid-input)
    (asserts! (is-none (map-get? foster-families { family-id: tx-sender })) err-already-exists)

    (map-set foster-families
      { family-id: tx-sender }
      {
        family-name: family-name,
        capacity: capacity,
        current-placements: u0,
        specializations: specializations,
        location: location,
        status: family-active,
        certification-level: u1,
        training-completed: (list),
        stability-score: u50,
        success-rate: u0,
        created-at: stacks-block-height,
        last-assessment: stacks-block-height
      }
    )

    (var-set total-families (+ (var-get total-families) u1))
    (ok true)
  )
)

(define-public (update-family-status (family-id principal) (new-status uint))
  (let ((family-data (unwrap! (map-get? foster-families { family-id: family-id }) err-not-found)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (and (>= new-status family-active) (<= new-status family-training)) err-invalid-status)

    (map-set foster-families
      { family-id: family-id }
      (merge family-data {
        status: new-status,
        last-assessment: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Advanced placement matching algorithm
(define-private (calculate-match-score (child-id uint) (family-id principal))
  (let (
    (child-data (unwrap! (map-get? children { child-id: child-id }) u0))
    (family-data (unwrap! (map-get? foster-families { family-id: family-id }) u0))
  )
    (let (
      (capacity-score (if (> (get capacity family-data) (get current-placements family-data)) u25 u0))
      (priority-score (if (is-eq (get priority-level child-data) priority-critical) u30 u15))
      (specialization-score (if (> (len (get specializations family-data)) u0) u20 u10))
      (stability-score (/ (get stability-score family-data) u2))
      (success-rate-score (/ (get success-rate family-data) u5))
    )
      (+ capacity-score priority-score specialization-score stability-score success-rate-score)
    )
  )
)

(define-public (create-placement (child-id uint) (family-id principal) (placement-services (list 10 (string-ascii 50))))
  (let (
    (placement-id (var-get next-placement-id))
    (child-data (unwrap! (map-get? children { child-id: child-id }) err-not-found))
    (family-data (unwrap! (map-get? foster-families { family-id: family-id }) err-not-found))
    (match-score (calculate-match-score child-id family-id))
  )
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (is-eq (get status child-data) status-available) err-invalid-status)
    (asserts! (is-eq (get status family-data) family-active) err-invalid-status)
    (asserts! (< (get current-placements family-data) (get capacity family-data)) err-insufficient-capacity)
    (asserts! (> match-score u50) err-placement-denied)

    ;; Create placement record
    (map-set placements
      { placement-id: placement-id }
      {
        child-id: child-id,
        family-id: family-id,
        start-date: stacks-block-height,
        end-date: none,
        status: status-placed,
        support-services: placement-services,
        outcome-metrics: {
          stability-rating: u50,
          child-wellbeing: u50,
          family-satisfaction: u50,
          case-progress: u50
        },
        notes: "",
        created-by: tx-sender
      }
    )

    ;; Update child status
    (map-set children
      { child-id: child-id }
      (merge child-data {
        status: status-placed,
        current-placement: (some family-id),
        placement-history: (unwrap! (as-max-len? (append (get placement-history child-data) family-id) u20) err-insufficient-capacity),
        last-updated: stacks-block-height
      })
    )

    ;; Update family placement count
    (map-set foster-families
      { family-id: family-id }
      (merge family-data {
        current-placements: (+ (get current-placements family-data) u1),
        last-assessment: stacks-block-height
      })
    )

    (var-set next-placement-id (+ placement-id u1))
    (ok placement-id)
  )
)

;; Support service management
(define-public (schedule-support-service
  (service-type (string-ascii 50))
  (provider principal)
  (child-id uint)
  (family-id principal)
  (scheduled-date uint))
  (let ((service-id (var-get next-service-id)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? children { child-id: child-id })) err-not-found)
    (asserts! (is-some (map-get? foster-families { family-id: family-id })) err-not-found)

    (map-set support-services
      { service-id: service-id }
      {
        service-type: service-type,
        provider: provider,
        child-id: child-id,
        family-id: family-id,
        scheduled-date: scheduled-date,
        completion-date: none,
        status: u1,
        effectiveness-rating: u0,
        notes: ""
      }
    )

    (var-set next-service-id (+ service-id u1))
    (ok service-id)
  )
)

(define-public (complete-support-service
  (service-id uint)
  (effectiveness-rating uint)
  (notes (string-ascii 300)))
  (let ((service-data (unwrap! (map-get? support-services { service-id: service-id }) err-not-found)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (and (>= effectiveness-rating u1) (<= effectiveness-rating u100)) err-invalid-input)

    (map-set support-services
      { service-id: service-id }
      (merge service-data {
        completion-date: (some stacks-block-height),
        status: u2,
        effectiveness-rating: effectiveness-rating,
        notes: notes
      })
    )
    (ok true)
  )
)

;; Training program management
(define-public (create-training-program
  (program-name (string-ascii 100))
  (description (string-ascii 300))
  (duration-hours uint)
  (certification-level uint)
  (prerequisites (list 5 (string-ascii 50))))
  (let ((program-id (var-get next-program-id)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (> duration-hours u0) err-invalid-input)

    (map-set training-programs
      { program-id: program-id }
      {
        program-name: program-name,
        description: description,
        duration-hours: duration-hours,
        certification-level: certification-level,
        prerequisites: prerequisites,
        created-by: tx-sender
      }
    )

    (var-set next-program-id (+ program-id u1))
    (ok program-id)
  )
)

(define-public (complete-training
  (family-id principal)
  (program-id uint)
  (score uint)
  (expiry-date uint))
  (begin
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? foster-families { family-id: family-id })) err-not-found)
    (asserts! (is-some (map-get? training-programs { program-id: program-id })) err-not-found)
    (asserts! (and (>= score u1) (<= score u100)) err-invalid-input)

    (map-set family-training-records
      { family-id: family-id, program-id: program-id }
      {
        completion-date: stacks-block-height,
        score: score,
        certified-by: tx-sender,
        expiry-date: expiry-date,
        notes: ""
      }
    )
    (ok true)
  )
)

;; Outcome tracking and metrics
(define-public (update-placement-metrics
  (placement-id uint)
  (stability-rating uint)
  (child-wellbeing uint)
  (family-satisfaction uint)
  (case-progress uint))
  (let ((placement-data (unwrap! (map-get? placements { placement-id: placement-id }) err-not-found)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (and (<= stability-rating u100) (<= child-wellbeing u100)
                   (<= family-satisfaction u100) (<= case-progress u100)) err-invalid-input)

    (map-set placements
      { placement-id: placement-id }
      (merge placement-data {
        outcome-metrics: {
          stability-rating: stability-rating,
          child-wellbeing: child-wellbeing,
          family-satisfaction: family-satisfaction,
          case-progress: case-progress
        }
      })
    )
    (ok true)
  )
)

(define-public (finalize-placement (placement-id uint) (outcome uint) (notes (string-ascii 500)))
  (let ((placement-data (unwrap! (map-get? placements { placement-id: placement-id }) err-not-found)))
    (asserts! (is-authorized tx-sender) err-unauthorized)
    (asserts! (and (>= outcome u1) (<= outcome u3)) err-invalid-input)

    (map-set placements
      { placement-id: placement-id }
      (merge placement-data {
        end-date: (some stacks-block-height),
        status: outcome,
        notes: notes
      })
    )

    ;; Update statistics
    (if (is-eq outcome u3)
      (var-set successful-placements (+ (var-get successful-placements) u1))
      (var-set failed-placements (+ (var-get failed-placements) u1))
    )

    ;; Update family placement count
    (let ((family-data (unwrap! (map-get? foster-families { family-id: (get family-id placement-data) }) err-not-found)))
      (map-set foster-families
        { family-id: (get family-id placement-data) }
        (merge family-data {
          current-placements: (- (get current-placements family-data) u1),
          success-rate: (if (is-eq outcome u3)
                         (+ (get success-rate family-data) u5)
                         (if (> (get success-rate family-data) u5)
                           (- (get success-rate family-data) u5)
                           u0))
        })
      )
    )

    (ok true)
  )
)

;; Read-only functions for data retrieval
(define-read-only (get-child-info (child-id uint))
  (map-get? children { child-id: child-id })
)

(define-read-only (get-family-info (family-id principal))
  (map-get? foster-families { family-id: family-id })
)

(define-read-only (get-placement-info (placement-id uint))
  (map-get? placements { placement-id: placement-id })
)

(define-read-only (get-service-info (service-id uint))
  (map-get? support-services { service-id: service-id })
)

(define-read-only (get-system-statistics)
  {
    total-children: (var-get total-children),
    total-families: (var-get total-families),
    successful-placements: (var-get successful-placements),
    failed-placements: (var-get failed-placements),
    success-rate: (if (> (+ (var-get successful-placements) (var-get failed-placements)) u0)
                    (/ (* (var-get successful-placements) u100)
                       (+ (var-get successful-placements) (var-get failed-placements)))
                    u0)
  }
)

(define-read-only (get-family-training-status (family-id principal) (program-id uint))
  (map-get? family-training-records { family-id: family-id, program-id: program-id })
)

(define-read-only (is-family-qualified (family-id principal) (required-certifications (list 5 uint)))
  (let ((family-data (unwrap! (map-get? foster-families { family-id: family-id }) (err u404))))
    (ok (fold check-certification required-certifications true))
  )
)

(define-private (check-certification (cert-id uint) (acc bool))
  (and acc (is-some (map-get? family-training-records { family-id: tx-sender, program-id: cert-id })))
)
