
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

(define-read-only (get-contract-info)
    {
        total-tokens: (var-get last-token-id),
        total-fundraised: (var-get total-fundraised),
        metadata-frozen: (var-get metadata-frozen),
        base-uri: (var-get base-token-uri)
    }
)
