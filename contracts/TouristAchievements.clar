;; Tourist Achievement System
;; Rewards visitors with collectible NFT badges based on heritage site milestones
;; Enhances gamification and encourages exploration of cultural heritage

;; NFT Definition
(define-non-fungible-token achievement-badge uint)

;; Constants  
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-ALREADY-EARNED (err u501))
(define-constant ERR-REQUIREMENTS-NOT-MET (err u502))
(define-constant ERR-INVALID-ACHIEVEMENT (err u503))
(define-constant ERR-NFT-NOT-FOUND (err u504))
(define-constant ERR-TRANSFER-FAILED (err u505))

;; Data Variables
(define-data-var next-badge-id uint u1)
(define-data-var total-achievements-earned uint u0)
(define-data-var badge-metadata-uri (string-ascii 200) "https://cultoken.heritage/badges/")

;; Achievement Type Definitions
(define-map achievement-types (string-ascii 30) {
    name: (string-ascii 50),
    description: (string-ascii 150),
    icon: (string-ascii 50),
    requirement-type: (string-ascii 20),
    requirement-value: uint,
    rarity-level: (string-ascii 15),
    points-reward: uint
})

;; User Achievement Progress
(define-map user-achievements {user: principal, achievement-type: (string-ascii 30)} {
    earned: bool,
    earned-at: uint,
    badge-id: uint,
    progress: uint
})

;; Badge Ownership and Metadata
(define-map badge-details uint {
    owner: principal,
    achievement-type: (string-ascii 30),
    minted-at: uint,
    rarity: (string-ascii 15),
    serial-number: uint
})

;; Tourist progression tracking
(define-map tourist-stats principal {
    total-badges-earned: uint,
    rare-badges-count: uint,
    legendary-badges-count: uint,
    achievement-score: uint,
    first-badge-earned: uint,
    explorer-rank: (string-ascii 20)
})

;; Public Functions

;; Initialize achievement types (called during contract deployment)
(define-public (setup-achievements)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        ;; First-time visitor achievement
        (map-set achievement-types "first-visit" {
            name: "Heritage Explorer",
            description: "Visit your first heritage site and check in",
            icon: "compass",
            requirement-type: "visits",
            requirement-value: u1,
            rarity-level: "common",
            points-reward: u50
        })
        
        ;; Multi-site explorer
        (map-set achievement-types "explorer" {
            name: "Cultural Wanderer", 
            description: "Visit 5 different heritage sites",
            icon: "map",
            requirement-type: "unique-sites",
            requirement-value: u5,
            rarity-level: "uncommon",
            points-reward: u200
        })
        
        ;; Heritage enthusiast
        (map-set achievement-types "enthusiast" {
            name: "Heritage Guardian",
            description: "Visit 15 sites and write 5 reviews",
            icon: "shield",
            requirement-type: "combined",
            requirement-value: u15,
            rarity-level: "rare",
            points-reward: u500
        })
        
        ;; Master traveler
        (map-set achievement-types "master" {
            name: "Cultural Ambassador",
            description: "Visit 25+ sites across different heritage types",
            icon: "crown",
            requirement-type: "diversity",
            requirement-value: u25,
            rarity-level: "legendary",
            points-reward: u1000
        })
        
        ;; Site supporter
        (map-set achievement-types "supporter" {
            name: "Heritage Patron",
            description: "Donate to 3 different heritage site fundraising campaigns",
            icon: "heart",
            requirement-type: "donations",
            requirement-value: u3,
            rarity-level: "rare",
            points-reward: u350
        })
        
        (ok true)
    )
)

;; Check and award achievement (called by main Cultoken contract)
(define-public (check-achievement (user principal) (achievement-type (string-ascii 30)) (current-progress uint))
    (let (
        (achievement-def (unwrap! (map-get? achievement-types achievement-type) ERR-INVALID-ACHIEVEMENT))
        (user-progress (default-to {earned: false, earned-at: u0, badge-id: u0, progress: u0}
                       (map-get? user-achievements {user: user, achievement-type: achievement-type})))
    )
        (asserts! (not (get earned user-progress)) ERR-ALREADY-EARNED)
        
        ;; Update progress
        (map-set user-achievements {user: user, achievement-type: achievement-type}
            (merge user-progress {progress: current-progress}))
        
        ;; Check if requirements are met
        (if (>= current-progress (get requirement-value achievement-def))
            (match (mint-achievement-badge user achievement-type)
                badge-id (ok true)
                error (ok false)
            )
            (ok false)
        )
    )
)

;; Mint achievement badge NFT
(define-private (mint-achievement-badge (recipient principal) (achievement-type (string-ascii 30)))
    (let (
        (badge-id (var-get next-badge-id))
        (achievement-def (unwrap! (map-get? achievement-types achievement-type) ERR-INVALID-ACHIEVEMENT))
        (current-stats (default-to {total-badges-earned: u0, rare-badges-count: u0, legendary-badges-count: u0, 
                                   achievement-score: u0, first-badge-earned: u0, explorer-rank: "novice"}
                       (map-get? tourist-stats recipient)))
    )
        ;; Mint the NFT badge
        (try! (nft-mint? achievement-badge badge-id recipient))
        
        ;; Store badge details
        (map-set badge-details badge-id {
            owner: recipient,
            achievement-type: achievement-type,
            minted-at: stacks-block-height,
            rarity: (get rarity-level achievement-def),
            serial-number: (+ (var-get total-achievements-earned) u1)
        })
        
        ;; Update user achievement status
        (map-set user-achievements {user: recipient, achievement-type: achievement-type} {
            earned: true,
            earned-at: stacks-block-height,
            badge-id: badge-id,
            progress: (get requirement-value achievement-def)
        })
        
        ;; Update tourist statistics
        (let (
            (new-total (+ (get total-badges-earned current-stats) u1))
            (rare-increment (if (is-eq (get rarity-level achievement-def) "rare") u1 u0))
            (legendary-increment (if (is-eq (get rarity-level achievement-def) "legendary") u1 u0))
            (new-score (+ (get achievement-score current-stats) (get points-reward achievement-def)))
            (first-badge (if (is-eq (get total-badges-earned current-stats) u0) stacks-block-height (get first-badge-earned current-stats)))
        )
            (map-set tourist-stats recipient {
                total-badges-earned: new-total,
                rare-badges-count: (+ (get rare-badges-count current-stats) rare-increment),
                legendary-badges-count: (+ (get legendary-badges-count current-stats) legendary-increment),
                achievement-score: new-score,
                first-badge-earned: first-badge,
                explorer-rank: (calculate-explorer-rank new-total new-score)
            })
        )
        
        ;; Update global counters
        (var-set next-badge-id (+ badge-id u1))
        (var-set total-achievements-earned (+ (var-get total-achievements-earned) u1))
        
        ;; Emit achievement event
        (print {
            event-type: "achievement-earned",
            recipient: recipient,
            achievement: achievement-type,
            badge-id: badge-id,
            rarity: (get rarity-level achievement-def),
            points-earned: (get points-reward achievement-def),
            timestamp: stacks-block-height
        })
        
        (ok badge-id)
    )
)

;; Transfer badge NFT
(define-public (transfer-badge (badge-id uint) (sender principal) (recipient principal))
    (let ((badge-info (unwrap! (map-get? badge-details badge-id) ERR-NFT-NOT-FOUND)))
        (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? achievement-badge badge-id sender recipient))
        (map-set badge-details badge-id (merge badge-info {owner: recipient}))
        (ok true)
    )
)

;; Create custom achievement (premium feature)
(define-public (create-custom-achievement 
    (achievement-id (string-ascii 30))
    (name (string-ascii 50)) 
    (description (string-ascii 150))
    (requirement-value uint)
    (points-reward uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set achievement-types achievement-id {
            name: name,
            description: description,
            icon: "custom",
            requirement-type: "custom",
            requirement-value: requirement-value,
            rarity-level: "special",
            points-reward: points-reward
        })
        (ok true)
    )
)

;; Calculate explorer rank based on badges and score
(define-private (calculate-explorer-rank (badges-count uint) (total-score uint))
    (if (>= badges-count u10)
        (if (>= total-score u2000) "master-explorer" "expert-explorer")
        (if (>= badges-count u5)
            (if (>= total-score u500) "skilled-explorer" "active-explorer")
            (if (>= badges-count u1) "novice-explorer" "newcomer")
        )
    )
)

;; Read-only Functions

(define-read-only (get-achievement-type (achievement-id (string-ascii 30)))
    (map-get? achievement-types achievement-id)
)

(define-read-only (get-user-achievement (user principal) (achievement-type (string-ascii 30)))
    (map-get? user-achievements {user: user, achievement-type: achievement-type})
)

(define-read-only (get-badge-details (badge-id uint))
    (map-get? badge-details badge-id)
)

(define-read-only (get-tourist-stats (user principal))
    (map-get? tourist-stats user)
)

(define-read-only (get-badge-owner (badge-id uint))
    (nft-get-owner? achievement-badge badge-id)
)

(define-read-only (get-user-badge-count (user principal))
    (match (map-get? tourist-stats user)
        stats (get total-badges-earned stats)
        u0
    )
)

(define-read-only (get-contract-stats)
    {
        total-badges-minted: (var-get next-badge-id),
        total-achievements-earned: (var-get total-achievements-earned),
        metadata-uri: (var-get badge-metadata-uri)
    }
)

;; Check if user qualifies for specific achievement
(define-read-only (check-qualification (user principal) (achievement-type (string-ascii 30)) (current-progress uint))
    (match (map-get? achievement-types achievement-type)
        achievement-def 
            (let ((user-progress (map-get? user-achievements {user: user, achievement-type: achievement-type})))
                (match user-progress
                    progress (not (get earned progress))
                    (>= current-progress (get requirement-value achievement-def))
                )
            )
        false
    )
)

;; Admin functions
(define-public (update-metadata-uri (new-uri (string-ascii 200)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set badge-metadata-uri new-uri)
        (ok true)
    )
)
