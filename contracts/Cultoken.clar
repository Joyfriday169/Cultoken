
(define-non-fungible-token heritage-site uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-not-found (err u102))
(define-constant err-metadata-frozen (err u103))
(define-constant err-listing-not-found (err u104))
(define-constant err-insufficient-payment (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-cannot-purchase-own-token (err u107))
(define-constant err-site-already-registered (err u108))
(define-constant err-invalid-coordinates (err u109))
(define-constant err-fundraising-not-active (err u110))
(define-constant err-goal-already-reached (err u111))
(define-constant err-lease-not-found (err u112))
(define-constant err-lease-expired (err u113))
(define-constant err-lease-still-active (err u114))
(define-constant err-invalid-lease-duration (err u115))
(define-constant err-site-already-leased (err u116))
(define-constant err-not-lessee (err u117))
(define-constant err-lease-payment-failed (err u118))
(define-constant err-invalid-visitor-verification (err u119))
(define-constant err-duplicate-visit (err u120))
(define-constant err-invalid-rating (err u121))
(define-constant err-visit-not-found (err u122))
(define-constant err-insufficient-visits (err u123))
(define-constant err-reward-already-claimed (err u124))
(define-constant err-invalid-reward-tier (err u125))

(define-data-var last-token-id uint u0)
(define-data-var base-token-uri (string-ascii 210) "https://cultoken.heritage/metadata/")
(define-data-var metadata-frozen bool false)
(define-data-var total-fundraised uint u0)

(define-map token-count principal uint)
(define-map heritage-sites uint {
    name: (string-ascii 64),
    description: (string-ascii 256),
    location: (string-ascii 128),
    latitude: int,
    longitude: int,
    heritage-type: (string-ascii 32),
    year-established: uint,
    owner: principal,
    fundraising-goal: uint,
    current-funds: uint,
    fundraising-active: bool,
    created-at: uint
})

(define-map site-coordinates (tuple (lat int) (lng int)) uint)
(define-map marketplace-listings uint {
    seller: principal,
    price: uint,
    active: bool
})

(define-map token-donations uint (list 50 {donor: principal, amount: uint, timestamp: uint}))
(define-map user-donations principal uint)

(define-map heritage-leases uint {
    lessor: principal,
    lessee: principal,
    lease-price: uint,
    lease-duration: uint,
    lease-start: uint,
    lease-end: uint,
    active: bool,
    revenue-sharing: uint,
    auto-renew: bool
})

(define-map lease-applications uint (list 20 {
    applicant: principal,
    offered-price: uint,
    requested-duration: uint,
    application-time: uint,
    status: (string-ascii 16)
}))

(define-map user-lease-history principal (list 100 {
    token-id: uint,
    role: (string-ascii 8),
    start-block: uint,
    end-block: uint,
    lease-price: uint
}))

(define-map site-visitors uint (list 100 {
    visitor: principal,
    visit-timestamp: uint,
    verification-method: (string-ascii 32),
    visit-duration: uint,
    coordinates-verified: bool
}))

(define-map visitor-profile principal {
    total-visits: uint,
    total-sites-visited: uint,
    loyalty-points: uint,
    preferred-heritage-type: (string-ascii 32),
    last-visit: uint,
    tourism-level: (string-ascii 16)
})

(define-map site-reviews uint (list 50 {
    reviewer: principal,
    rating: uint,
    review-text: (string-ascii 256),
    visit-verified: bool,
    helpful-votes: uint,
    review-timestamp: uint
}))

(define-map site-tourism-stats uint {
    total-visitors: uint,
    total-reviews: uint,
    average-rating: uint,
    peak-visit-season: (string-ascii 16),
    visitor-satisfaction: uint,
    last-updated: uint
})

(define-map loyalty-rewards principal (list 20 {
    reward-tier: (string-ascii 16),
    reward-description: (string-ascii 128),
    points-cost: uint,
    claimed: bool,
    claim-timestamp: uint
}))

(define-map tourism-achievements principal (list 30 {
    achievement-type: (string-ascii 32),
    achievement-name: (string-ascii 64),
    description: (string-ascii 128),
    unlocked-at: uint,
    sites-involved: (list 10 uint)
}))

(define-public (get-last-token-id)
    (ok (var-get last-token-id))
)


(define-public (get-owner (token-id uint))
    (ok (nft-get-owner? heritage-site token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (asserts! (is-some (nft-get-owner? heritage-site token-id)) err-not-found)
        (nft-transfer? heritage-site token-id sender recipient)
    )
)

(define-public (mint-heritage-site 
    (name (string-ascii 64))
    (description (string-ascii 256))
    (location (string-ascii 128))
    (latitude int)
    (longitude int)
    (heritage-type (string-ascii 32))
    (year-established uint)
    (fundraising-goal uint)
    (recipient principal)
)
    (let 
        (
            (token-id (+ (var-get last-token-id) u1))
            (coord-key {lat: latitude, lng: longitude})
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (>= latitude -90000000) (<= latitude 90000000)) err-invalid-coordinates)
        (asserts! (and (>= longitude -180000000) (<= longitude 180000000)) err-invalid-coordinates)
        (asserts! (is-none (map-get? site-coordinates coord-key)) err-site-already-registered)
        
        (try! (nft-mint? heritage-site token-id recipient))
        (map-set heritage-sites token-id {
            name: name,
            description: description,
            location: location,
            latitude: latitude,
            longitude: longitude,
            heritage-type: heritage-type,
            year-established: year-established,
            owner: recipient,
            fundraising-goal: fundraising-goal,
            current-funds: u0,
            fundraising-active: true,
            created-at: stacks-block-height
        })
        (map-set site-coordinates coord-key token-id)
        (map-set token-count recipient 
            (+ (default-to u0 (map-get? token-count recipient)) u1))
        (var-set last-token-id token-id)
        (ok token-id)
    )
)

(define-public (donate-to-site (token-id uint) (amount uint))
    (let 
        (
            (site-data (unwrap! (map-get? heritage-sites token-id) err-not-found))
            (current-donations (default-to (list) (map-get? token-donations token-id)))
            (new-donation {donor: tx-sender, amount: amount, timestamp: stacks-block-height})
        )
        (asserts! (get fundraising-active site-data) err-fundraising-not-active)
        (asserts! (< (get current-funds site-data) (get fundraising-goal site-data)) err-goal-already-reached)
        
        (try! (stx-transfer? amount tx-sender (get owner site-data)))
        
        (map-set heritage-sites token-id 
            (merge site-data {current-funds: (+ (get current-funds site-data) amount)}))
        
        (map-set token-donations token-id 
            (unwrap! (as-max-len? (append current-donations new-donation) u50) (err u999)))
        
        (map-set user-donations tx-sender 
            (+ (default-to u0 (map-get? user-donations tx-sender)) amount))
        
        (var-set total-fundraised (+ (var-get total-fundraised) amount))
        (ok true)
    )
)

(define-public (toggle-fundraising (token-id uint))
    (let 
        (
            (site-data (unwrap! (map-get? heritage-sites token-id) err-not-found))
            (token-owner (unwrap! (nft-get-owner? heritage-site token-id) err-not-found))
        )
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (map-set heritage-sites token-id 
            (merge site-data {fundraising-active: (not (get fundraising-active site-data))}))
        (ok true)
    )
)

(define-public (list-for-sale (token-id uint) (price uint))
    (let 
        (
            (token-owner (unwrap! (nft-get-owner? heritage-site token-id) err-not-found))
        )
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (> price u0) err-invalid-price)
        (map-set marketplace-listings token-id {
            seller: tx-sender,
            price: price,
            active: true
        })
        (ok true)
    )
)

(define-public (unlist-from-sale (token-id uint))
    (let 
        (
            (listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
        )
        (asserts! (is-eq tx-sender (get seller listing)) err-not-token-owner)
        (map-delete marketplace-listings token-id)
        (ok true)
    )
)

(define-public (purchase-token (token-id uint))
    (let 
        (
            (listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
            (token-owner (unwrap! (nft-get-owner? heritage-site token-id) err-not-found))
        )
        (asserts! (get active listing) err-listing-not-found)
        (asserts! (not (is-eq tx-sender token-owner)) err-cannot-purchase-own-token)
        
        (try! (stx-transfer? (get price listing) tx-sender (get seller listing)))
        (try! (nft-transfer? heritage-site token-id token-owner tx-sender))
        
        (map-delete marketplace-listings token-id)
        (map-set token-count token-owner 
            (- (default-to u1 (map-get? token-count token-owner)) u1))
        (map-set token-count tx-sender 
            (+ (default-to u0 (map-get? token-count tx-sender)) u1))
        (ok true)
    )
)

(define-public (set-base-uri (new-base-uri (string-ascii 210)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (var-get metadata-frozen)) err-metadata-frozen)
        (ok (var-set base-token-uri new-base-uri))
    )
)

(define-public (freeze-metadata)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set metadata-frozen true)
        (ok true)
    )
)

(define-read-only (get-heritage-site (token-id uint))
    (map-get? heritage-sites token-id)
)

(define-read-only (get-site-by-coordinates (latitude int) (longitude int))
    (map-get? site-coordinates {lat: latitude, lng: longitude})
)

(define-read-only (get-marketplace-listing (token-id uint))
    (map-get? marketplace-listings token-id)
)

(define-read-only (get-token-donations (token-id uint))
    (map-get? token-donations token-id)
)

(define-read-only (get-user-donations (user principal))
    (map-get? user-donations user)
)

(define-read-only (get-user-token-count (user principal))
    (default-to u0 (map-get? token-count user))
)

(define-read-only (get-total-fundraised)
    (var-get total-fundraised)
)

(define-read-only (get-fundraising-progress (token-id uint))
    (match (map-get? heritage-sites token-id)
        site-data 
        (let 
            (
                (current (get current-funds site-data))
                (goal (get fundraising-goal site-data))
            )
            (some {
                current-funds: current,
                fundraising-goal: goal,
                percentage: (if (> goal u0) (/ (* current u100) goal) u0),
                is-complete: (>= current goal)
            })
        )
        none
    )
)

(define-public (offer-lease (token-id uint) (lease-price uint) (lease-duration uint) (revenue-sharing uint))
    (let 
        (
            (token-owner (unwrap! (nft-get-owner? heritage-site token-id) err-not-found))
            (current-lease (map-get? heritage-leases token-id))
        )
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (> lease-duration u0) err-invalid-lease-duration)
        (asserts! (<= lease-duration u52560) err-invalid-lease-duration)
        (asserts! (<= revenue-sharing u100) err-invalid-lease-duration)
        (asserts! (is-none current-lease) err-site-already-leased)
        
        (map-set heritage-leases token-id {
            lessor: tx-sender,
            lessee: tx-sender,
            lease-price: lease-price,
            lease-duration: lease-duration,
            lease-start: u0,
            lease-end: u0,
            active: false,
            revenue-sharing: revenue-sharing,
            auto-renew: false
        })
        (ok true)
    )
)

(define-public (apply-for-lease (token-id uint) (offered-price uint) (requested-duration uint))
    (let 
        (
            (lease-offer (unwrap! (map-get? heritage-leases token-id) err-lease-not-found))
            (current-applications (default-to (list) (map-get? lease-applications token-id)))
            (new-application {
                applicant: tx-sender,
                offered-price: offered-price,
                requested-duration: requested-duration,
                application-time: stacks-block-height,
                status: "pending"
            })
        )
        (asserts! (not (get active lease-offer)) err-site-already-leased)
        (asserts! (> offered-price u0) err-invalid-price)
        (asserts! (> requested-duration u0) err-invalid-lease-duration)
        
        (map-set lease-applications token-id
            (unwrap! (as-max-len? (append current-applications new-application) u20) (err u999)))
        (ok true)
    )
)

(define-public (accept-lease-application (token-id uint) (applicant principal))
    (let 
        (
            (lease-offer (unwrap! (map-get? heritage-leases token-id) err-lease-not-found))
            (applications (unwrap! (map-get? lease-applications token-id) err-lease-not-found))
            (lease-start-block stacks-block-height)
        )
        (asserts! (is-eq tx-sender (get lessor lease-offer)) err-not-token-owner)
        (asserts! (not (get active lease-offer)) err-site-already-leased)
        
        (let 
            (
                (accepted-app (unwrap! 
                    (element-at? 
                        (filter check-applicant applications) 
                        u0
                    ) 
                    err-lease-not-found
                ))
                (lease-end-block (+ lease-start-block (get requested-duration accepted-app)))
                (updated-history (default-to (list) (map-get? user-lease-history applicant)))
                (lessor-history (default-to (list) (map-get? user-lease-history tx-sender)))
            )
            
            (try! (stx-transfer? (get offered-price accepted-app) applicant tx-sender))
            
            (map-set heritage-leases token-id {
                lessor: tx-sender,
                lessee: applicant,
                lease-price: (get offered-price accepted-app),
                lease-duration: (get requested-duration accepted-app),
                lease-start: lease-start-block,
                lease-end: lease-end-block,
                active: true,
                revenue-sharing: (get revenue-sharing lease-offer),
                auto-renew: false
            })
            
            (map-set user-lease-history applicant
                (unwrap! (as-max-len? (append updated-history {
                    token-id: token-id,
                    role: "lessee",
                    start-block: lease-start-block,
                    end-block: lease-end-block,
                    lease-price: (get offered-price accepted-app)
                }) u100) (err u999)))
            
            (map-set user-lease-history tx-sender
                (unwrap! (as-max-len? (append lessor-history {
                    token-id: token-id,
                    role: "lessor",
                    start-block: lease-start-block,
                    end-block: lease-end-block,
                    lease-price: (get offered-price accepted-app)
                }) u100) (err u999)))
            
            (map-delete lease-applications token-id)
            (ok true)
        )
    )
)

(define-public (end-lease (token-id uint))
    (let 
        (
            (lease-data (unwrap! (map-get? heritage-leases token-id) err-lease-not-found))
        )
        (asserts! (get active lease-data) err-lease-not-found)
        (asserts! 
            (or 
                (is-eq tx-sender (get lessor lease-data))
                (is-eq tx-sender (get lessee lease-data))
                (>= stacks-block-height (get lease-end lease-data))
            ) 
            err-not-token-owner
        )
        
        (map-set heritage-leases token-id 
            (merge lease-data {active: false}))
        (ok true)
    )
)

(define-public (renew-lease (token-id uint) (new-duration uint) (new-price uint))
    (let 
        (
            (lease-data (unwrap! (map-get? heritage-leases token-id) err-lease-not-found))
            (new-end-block (+ stacks-block-height new-duration))
        )
        (asserts! (get active lease-data) err-lease-not-found)
        (asserts! (is-eq tx-sender (get lessee lease-data)) err-not-lessee)
        (asserts! (> new-duration u0) err-invalid-lease-duration)
        
        (try! (stx-transfer? new-price tx-sender (get lessor lease-data)))
        
        (map-set heritage-leases token-id
            (merge lease-data {
                lease-price: new-price,
                lease-duration: new-duration,
                lease-start: stacks-block-height,
                lease-end: new-end-block
            }))
        (ok true)
    )
)

(define-public (collect-lease-revenue (token-id uint) (amount uint))
    (let 
        (
            (lease-data (unwrap! (map-get? heritage-leases token-id) err-lease-not-found))
            (revenue-share (/ (* amount (get revenue-sharing lease-data)) u100))
            (lessee-share (- amount revenue-share))
        )
        (asserts! (get active lease-data) err-lease-not-found)
        (asserts! (< stacks-block-height (get lease-end lease-data)) err-lease-expired)
        (asserts! (is-eq tx-sender (get lessee lease-data)) err-not-lessee)
        
        (try! (stx-transfer? revenue-share tx-sender (get lessor lease-data)))
        (ok {lessor-revenue: revenue-share, lessee-revenue: lessee-share})
    )
)

(define-private (check-applicant (application {applicant: principal, offered-price: uint, requested-duration: uint, application-time: uint, status: (string-ascii 16)}))
    (is-eq (get applicant application) tx-sender)
)

(define-read-only (get-heritage-lease (token-id uint))
    (map-get? heritage-leases token-id)
)

(define-read-only (get-lease-applications (token-id uint))
    (map-get? lease-applications token-id)
)

(define-read-only (get-user-lease-history (user principal))
    (map-get? user-lease-history user)
)

(define-read-only (is-lease-active (token-id uint))
    (match (map-get? heritage-leases token-id)
        lease-data 
        (and 
            (get active lease-data)
            (< stacks-block-height (get lease-end lease-data))
        )
        false
    )
)

(define-read-only (get-lease-status (token-id uint))
    (match (map-get? heritage-leases token-id)
        lease-data 
        (some {
            active: (get active lease-data),
            lessor: (get lessor lease-data),
            lessee: (get lessee lease-data),
            lease-price: (get lease-price lease-data),
            blocks-remaining: (if (> (get lease-end lease-data) stacks-block-height)
                                (- (get lease-end lease-data) stacks-block-height)
                                u0),
            expired: (>= stacks-block-height (get lease-end lease-data))
        })
        none
    )
)

(define-public (check-in-to-site (token-id uint) (verification-method (string-ascii 32)) (visit-duration uint) (coordinates-verified bool))
(let 
(
    (site-data (unwrap! (map-get? heritage-sites token-id) err-not-found))
    (current-visitors (default-to (list) (map-get? site-visitors token-id)))
    (visitor-data (default-to {
            total-visits: u0,
                total-sites-visited: u0,
                loyalty-points: u0,
                preferred-heritage-type: "",
                last-visit: u0,
                tourism-level: "novice"
            } (map-get? visitor-profile tx-sender)))
            (new-visit {
                visitor: tx-sender,
                visit-timestamp: stacks-block-height,
                verification-method: verification-method,
                visit-duration: visit-duration,
                coordinates-verified: coordinates-verified
            })
        )
        (asserts! (> visit-duration u0) err-invalid-visitor-verification)
        (asserts! (is-none (element-at? (filter check-duplicate-visit current-visitors) u0)) err-duplicate-visit)
        
        (map-set site-visitors token-id
            (unwrap! (as-max-len? (append current-visitors new-visit) u100) (err u999)))
        
        (let 
            (
                (points-earned (calculate-visit-points visit-duration coordinates-verified))
                (new-total-visits (+ (get total-visits visitor-data) u1))
                (updated-sites-visited (if (is-none (map-get? site-visitors token-id)) 
                                         (+ (get total-sites-visited visitor-data) u1)
                                         (get total-sites-visited visitor-data)))
            )
            (map-set visitor-profile tx-sender {
                total-visits: new-total-visits,
                total-sites-visited: updated-sites-visited,
                loyalty-points: (+ (get loyalty-points visitor-data) points-earned),
                preferred-heritage-type: (get heritage-type site-data),
                last-visit: stacks-block-height,
                tourism-level: (calculate-tourism-level new-total-visits)
            })
            
            (try! (update-site-tourism-stats token-id))
            (ok {points-earned: points-earned, total-points: (+ (get loyalty-points visitor-data) points-earned)})
        )
    )
)

(define-public (submit-site-review (token-id uint) (rating uint) (review-text (string-ascii 256)))
    (let 
        (
            (site-data (unwrap! (map-get? heritage-sites token-id) err-not-found))
            (current-reviews (default-to (list) (map-get? site-reviews token-id)))
            (visit-verified (has-visited-site token-id tx-sender))
            (new-review {
                reviewer: tx-sender,
                rating: rating,
                review-text: review-text,
                visit-verified: visit-verified,
                helpful-votes: u0,
                review-timestamp: stacks-block-height
            })
        )
        (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
        
        (map-set site-reviews token-id
            (unwrap! (as-max-len? (append current-reviews new-review) u50) (err u999)))
        
        (if visit-verified
            (let ((visitor-data (unwrap! (map-get? visitor-profile tx-sender) err-visit-not-found)))
                (map-set visitor-profile tx-sender
                    (merge visitor-data {
                        loyalty-points: (+ (get loyalty-points visitor-data) u10)
                    }))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (claim-loyalty-reward (reward-tier (string-ascii 16)) (points-cost uint))
    (let 
        (
            (visitor-data (unwrap! (map-get? visitor-profile tx-sender) err-visit-not-found))
            (current-rewards (default-to (list) (map-get? loyalty-rewards tx-sender)))
            (reward-description (get-reward-description reward-tier))
        )
        (asserts! (>= (get loyalty-points visitor-data) points-cost) err-insufficient-visits)
        (asserts! (is-some reward-description) err-invalid-reward-tier)
        
        (let 
            (
                (new-reward {
                    reward-tier: reward-tier,
                    reward-description: (unwrap-panic reward-description),
                    points-cost: points-cost,
                    claimed: true,
                    claim-timestamp: stacks-block-height
                })
            )
            (map-set loyalty-rewards tx-sender
                (unwrap! (as-max-len? (append current-rewards new-reward) u20) (err u999)))
            
            (map-set visitor-profile tx-sender
                (merge visitor-data {
                    loyalty-points: (- (get loyalty-points visitor-data) points-cost)
                }))
            (ok true)
        )
    )
)

(define-public (update-site-tourism-stats (token-id uint))
    (let 
        (
            (site-data (unwrap! (map-get? heritage-sites token-id) err-not-found))
            (visitors (default-to (list) (map-get? site-visitors token-id)))
            (reviews (default-to (list) (map-get? site-reviews token-id)))
            (total-visitors (len visitors))
            (total-reviews (len reviews))
            (average-rating (calculate-average-rating reviews))
        )
        (map-set site-tourism-stats token-id {
            total-visitors: total-visitors,
            total-reviews: total-reviews,
            average-rating: average-rating,
            peak-visit-season: "summer",
            visitor-satisfaction: (calculate-satisfaction-score average-rating total-reviews),
            last-updated: stacks-block-height
        })
        (ok true)
    )
)

(define-private (check-duplicate-visit (visit {visitor: principal, visit-timestamp: uint, verification-method: (string-ascii 32), visit-duration: uint, coordinates-verified: bool}))
    (and 
        (is-eq (get visitor visit) tx-sender)
        (> (- stacks-block-height (get visit-timestamp visit)) u144)
    )
)

(define-private (calculate-visit-points (duration uint) (coordinates-verified bool))
    (let 
        (
            (base-points (if (> duration u60) u20 u10))
            (verification-bonus (if coordinates-verified u5 u0))
        )
        (+ base-points verification-bonus)
    )
)

(define-private (calculate-tourism-level (total-visits uint))
    (if (>= total-visits u50)
        "expert"
        (if (>= total-visits u20)
            "advanced"
            (if (>= total-visits u5)
                "intermediate"
                "novice"
            )
        )
    )
)

(define-private (has-visited-site (token-id uint) (visitor principal))
    (let ((visitors (default-to (list) (map-get? site-visitors token-id))))
        (is-some (element-at? (filter check-visitor-match visitors) u0))
    )
)

(define-private (check-visitor-match (visit {visitor: principal, visit-timestamp: uint, verification-method: (string-ascii 32), visit-duration: uint, coordinates-verified: bool}))
    (is-eq (get visitor visit) tx-sender)
)

(define-private (calculate-average-rating (reviews (list 50 {reviewer: principal, rating: uint, review-text: (string-ascii 256), visit-verified: bool, helpful-votes: uint, review-timestamp: uint})))
    (let 
        (
            (total-reviews (len reviews))
            (total-rating (fold + (map get-rating reviews) u0))
        )
        (if (> total-reviews u0)
            (/ total-rating total-reviews)
            u0
        )
    )
)

(define-private (get-rating (review {reviewer: principal, rating: uint, review-text: (string-ascii 256), visit-verified: bool, helpful-votes: uint, review-timestamp: uint}))
    (get rating review)
)

(define-private (calculate-satisfaction-score (average-rating uint) (total-reviews uint))
    (let 
        (
            (rating-weight (* average-rating u20))
            (review-weight (if (> total-reviews u10) u20 (* total-reviews u2)))
        )
        (/ (+ rating-weight review-weight) u2)
    )
)

(define-private (get-reward-description (tier (string-ascii 16)))
    (if (is-eq tier "bronze")
        (some "Heritage Explorer Badge - 5% discount on future visits")
        (if (is-eq tier "silver")
            (some "Cultural Ambassador Status - 10% discount and priority access")
            (if (is-eq tier "gold")
                (some "Heritage Guardian Title - 20% discount and exclusive events")
                none
            )
        )
    )
)

(define-read-only (get-site-visitors (token-id uint))
    (map-get? site-visitors token-id)
)

(define-read-only (get-visitor-profile (visitor principal))
    (map-get? visitor-profile visitor)
)

(define-read-only (get-site-reviews (token-id uint))
    (map-get? site-reviews token-id)
)

(define-read-only (get-site-tourism-stats (token-id uint))
    (map-get? site-tourism-stats token-id)
)

(define-read-only (get-loyalty-rewards (visitor principal))
    (map-get? loyalty-rewards visitor)
)

(define-read-only (get-tourism-achievements (visitor principal))
    (map-get? tourism-achievements visitor)
)

(define-read-only (get-contract-info)
    {
        total-tokens: (var-get last-token-id),
        total-fundraised: (var-get total-fundraised),
        metadata-frozen: (var-get metadata-frozen),
        base-uri: (var-get base-token-uri)
    }
)


