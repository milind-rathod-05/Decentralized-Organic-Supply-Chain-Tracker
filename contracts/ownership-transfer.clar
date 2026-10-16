;; ownership-transfer.clar
(define-constant ERR-UNAUTHORIZED (err u3000))
(define-constant ERR-BATCH-NOT-FOUND (err u3001))
(define-constant ERR-INVALID-BATCH-ID (err u3002))
(define-constant ERR-INVALID-NEW-OWNER (err u3003))
(define-constant ERR-TRANSFER-IN-PROGRESS (err u3004))
(define-constant ERR-TRANSFER-ALREADY-COMPLETED (err u3005))
(define-constant ERR-INSUFFICIENT-FUNDS (err u3006))
(define-constant ERR-INVALID-ESCROW-AMOUNT (err u3007))
(define-constant ERR-ESCROW-NOT-FOUND (err u3008))
(define-constant ERR-INVALID-TIMESTAMP (err u3009))
(define-constant ERR-MAX-TRANSFERS-EXCEEDED (err u3010))
(define-constant ERR-INVALID-FEE (err u3011))
(define-constant ERR-AUTHORITY-NOT-SET (err u3012))
(define-constant ERR-TRANSFER-FAILED (err u3013))
(define-data-var last-transfer-id uint u0)
(define-data-var total-transfers uint u0)
(define-data-var max-transfers uint u5000)
(define-data-var transfer-fee uint u300)
(define-data-var authority-contract (optional principal) none)
(define-map transfers 
    { transfer-id: uint }
    {
        batch-id: uint,
        from-owner: principal,
        to-owner: principal,
        timestamp: uint,
        escrow-amount: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)
(define-map transfer-history 
    { batch-id: uint, transfer-index: uint }
    {
        transfer-id: uint,
        from: principal,
        to: principal,
        timestamp: uint
    }
)
(define-map batch-transfer-count
    { batch-id: uint }
    uint
)
(define-map escrows 
    { transfer-id: uint }
    {
        amount: uint,
        locked-by: principal,
        release-to: principal
    }
)
(define-map transfer-updates 
    { transfer-id: uint }
    {
        update-status: (string-ascii 20),
        update-timestamp: uint,
        updater: principal
    }
)
(define-read-only (get-transfer (transfer-id uint))
    (map-get? transfers { transfer-id: transfer-id })
)
(define-read-only (get-total-transfers)
    (var-get total-transfers)
)
(define-read-only (get-transfer-history (batch-id uint))
    (map-get? transfer-history { batch-id: batch-id, transfer-index: u0 })
)
(define-read-only (get-escrow (transfer-id uint))
    (map-get? escrows { transfer-id: transfer-id })
)
(define-read-only (is-transfer-active (transfer-id uint))
    (let ((transfer (unwrap-panic (get-transfer transfer-id))))
        (is-eq (get status transfer) "pending")
    )
)
(define-read-only (get-history-count (batch-id uint))
    (default-to u0 (map-get? batch-transfer-count { batch-id: batch-id }))
)
(define-private (validate-new-owner (no principal))
    (if true (ok true) ERR-INVALID-NEW-OWNER)
)
(define-private (validate-timestamp (ts uint))
    (if (>= ts block-height) (ok true) ERR-INVALID-TIMESTAMP)
)
(define-private (validate-status (s (string-ascii 20)))
    (if (or (is-eq s "pending") (is-eq s "accepted") (is-eq s "rejected") (is-eq s "completed")) (ok true) ERR-INVALID-BATCH-ID)
)
(define-private (validate-escrow-amount (amt uint))
    (if (> amt u0) (ok true) ERR-INVALID-ESCROW-AMOUNT)
)
(define-public (set-authority-contract (contract-principal principal))
    (begin
        (asserts! (is-none (var-get authority-contract)) ERR-AUTHORITY-NOT-SET)
        (var-set authority-contract (some contract-principal))
        (ok true)
    )
)
(define-public (set-max-transfers (new-max uint))
    (begin
        (asserts! (is-some (var-get authority-contract)) ERR-AUTHORITY-NOT-SET)
        (asserts! (> new-max u0) ERR-MAX-TRANSFERS-EXCEEDED)
        (var-set max-transfers new-max)
        (ok true)
    )
)
(define-public (set-transfer-fee (new-fee uint))
    (begin
        (asserts! (is-some (var-get authority-contract)) ERR-AUTHORITY-NOT-SET)
        (asserts! (>= new-fee u0) ERR-INVALID-FEE)
        (var-set transfer-fee new-fee)
        (ok true)
    )
)
(define-public (initiate-transfer 
    (batch-id uint)
    (new-owner principal)
    (timestamp uint)
    (escrow-amount uint)
)
    (let
        (
            (caller tx-sender)
            (new-id (var-get last-transfer-id))
            (next-id (+ new-id u1))
            (current-max (var-get max-transfers))
            (authority (unwrap! (var-get authority-contract) ERR-AUTHORITY-NOT-SET))
            (history-count (get-history-count batch-id))
        )
        (asserts! (< next-id current-max) ERR-MAX-TRANSFERS-EXCEEDED)
        (try! (validate-new-owner new-owner))
        (try! (validate-timestamp timestamp))
        (try! (validate-escrow-amount escrow-amount))
        (asserts! (not (is-eq caller new-owner)) ERR-INVALID-NEW-OWNER)
        (let ((batch-data (unwrap! (contract-call? .product-registry get-batch batch-id) ERR-BATCH-NOT-FOUND)))
            (asserts! (is-eq caller (get farmer batch-data)) ERR-UNAUTHORIZED)
        )
        (try! (stx-transfer? (var-get transfer-fee) tx-sender authority))
        (try! (contract-call? .product-registry transfer-ownership batch-id new-owner))  ;; Provisional, but lock escrow
        (map-set transfers 
            { transfer-id: next-id }
            {
                batch-id: batch-id,
                from-owner: caller,
                to-owner: new-owner,
                timestamp: timestamp,
                escrow-amount: escrow-amount,
                status: "pending",
                created-at: block-height
            }
        )
        (map-set transfer-history 
            { batch-id: batch-id, transfer-index: history-count }
            {
                transfer-id: next-id,
                from: caller,
                to: new-owner,
                timestamp: block-height
            }
        )
        (map-set batch-transfer-count { batch-id: batch-id } (+ history-count u1))
        (map-set escrows 
            { transfer-id: next-id }
            {
                amount: escrow-amount,
                locked-by: caller,
                release-to: new-owner
            }
        )
        (var-set last-transfer-id next-id)
        (var-set total-transfers (+ (var-get total-transfers) u1))
        (print { event: "transfer-initiated", id: next-id })
        (ok next-id)
    )
)
(define-public (accept-transfer (transfer-id uint))
    (let
        (
            (transfer (unwrap! (get-transfer transfer-id) ERR-TRANSFER-FAILED))
            (escrow (unwrap! (get-escrow transfer-id) ERR-ESCROW-NOT-FOUND))
            (caller tx-sender)
            (to-owner (get to-owner transfer))
        )
        (asserts! (is-eq caller to-owner) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status transfer) "pending") ERR-TRANSFER-IN-PROGRESS)
        (map-set transfers 
            { transfer-id: transfer-id }
            (merge transfer { status: "accepted" })
        )
        (map-set transfer-updates 
            { transfer-id: transfer-id }
            {
                update-status: "accepted",
                update-timestamp: block-height,
                updater: caller
            }
        )
        (try! (as-contract (contract-call? .product-registry transfer-ownership (get batch-id transfer) caller)))
        (print { event: "transfer-accepted", id: transfer-id })
        (ok true)
    )
)
(define-public (reject-transfer (transfer-id uint) (reason (string-ascii 200)))
    (let
        (
            (transfer (unwrap! (get-transfer transfer-id) ERR-TRANSFER-FAILED))
            (escrow (unwrap! (get-escrow transfer-id) ERR-ESCROW-NOT-FOUND))
            (caller tx-sender)
            (from-owner (get from-owner transfer))
        )
        (asserts! (is-eq caller from-owner) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status transfer) "pending") ERR-TRANSFER-IN-PROGRESS)
        (map-set transfers 
            { transfer-id: transfer-id }
            (merge transfer { status: "rejected" })
        )
        (map-set transfer-updates 
            { transfer-id: transfer-id }
            {
                update-status: "rejected",
                update-timestamp: block-height,
                updater: caller
            }
        )
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender from-owner)))
        (map-delete escrows { transfer-id: transfer-id })
        (print { event: "transfer-rejected", id: transfer-id, reason: reason })
        (ok true)
    )
)
(define-public (complete-transfer (transfer-id uint))
    (let
        (
            (transfer (unwrap! (get-transfer transfer-id) ERR-TRANSFER-FAILED))
            (escrow (unwrap! (get-escrow transfer-id) ERR-ESCROW-NOT-FOUND))
            (caller tx-sender)
            (to-owner (get to-owner transfer))
        )
        (asserts! (is-eq caller to-owner) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status transfer) "accepted") ERR-TRANSFER-IN-PROGRESS)
        (map-set transfers 
            { transfer-id: transfer-id }
            (merge transfer { status: "completed" })
        )
        (map-set transfer-updates 
            { transfer-id: transfer-id }
            {
                update-status: "completed",
                update-timestamp: block-height,
                updater: caller
            }
        )
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender to-owner)))
        (map-delete escrows { transfer-id: transfer-id })
        (print { event: "transfer-completed", id: transfer-id })
        (ok true)
    )
)
(define-public (cancel-transfer (transfer-id uint))
    (let
        (
            (transfer (unwrap! (get-transfer transfer-id) ERR-TRANSFER-FAILED))
            (escrow (unwrap! (get-escrow transfer-id) ERR-ESCROW-NOT-FOUND))
            (caller tx-sender)
            (from-owner (get from-owner transfer))
        )
        (asserts! (is-eq caller from-owner) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status transfer) "pending") ERR-TRANSFER-IN-PROGRESS)
        (map-set transfers 
            { transfer-id: transfer-id }
            (merge transfer { status: "cancelled" })
        )
        (map-set transfer-updates 
            { transfer-id: transfer-id }
            {
                update-status: "cancelled",
                update-timestamp: block-height,
                updater: caller
            }
        )
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender from-owner)))
        (map-delete escrows { transfer-id: transfer-id })
        (print { event: "transfer-cancelled", id: transfer-id })
        (ok true)
    )
)
(define-read-only (get-transfer-updates (transfer-id uint))
    (map-get? transfer-updates { transfer-id: transfer-id })
)
(define-public (get-transfer-count)
    (ok (var-get last-transfer-id))
)
(define-public (check-transfer-existence (transfer-id uint))
    (ok (is-some (get-transfer transfer-id)))
)
(define-public (check-transfer-status (transfer-id uint))
    (let ((transfer (unwrap! (get-transfer transfer-id) ERR-TRANSFER-FAILED)))
        (ok (get status transfer))
    )
)