# Cultoken - Heritage Site NFT Platform

A Clarity smart contract for tokenizing heritage sites and landmarks as NFTs with integrated fundraising capabilities.

## Overview

Cultoken enables the creation of NFTs representing heritage sites, monuments, and cultural landmarks. Each token serves as both a digital collectible and a fundraising vehicle for site preservation and maintenance.

## Features

- **Heritage Site NFTs**: Mint unique tokens for landmarks with detailed metadata
- **Geolocation Tracking**: Prevent duplicate sites using latitude/longitude coordinates
- **Integrated Fundraising**: Direct donations to site owners with progress tracking
- **Marketplace Functions**: List and purchase heritage site tokens
- **Donation History**: Track all contributions with timestamps
- **Fundraising Goals**: Set and monitor preservation funding targets

## Contract Functions

### Minting

```clarity
(mint-heritage-site name description location latitude longitude heritage-type year-established fundraising-goal recipient)
```

Mint a new heritage site NFT (owner only).

### Donations

```clarity
(donate-to-site token-id amount)
```

Donate STX to a heritage site's preservation fund.

### Marketplace

```clarity
(list-for-sale token-id price)
(purchase-token token-id)
(unlist-from-sale token-id)
```

Trade heritage site tokens on the integrated marketplace.

### Fundraising

```clarity
(toggle-fundraising token-id)
```

Enable/disable fundraising for a specific site (token owner only).

## Read-Only Functions

- `get-heritage-site`: Get site details by token ID
- `get-site-by-coordinates`: Find site by lat/lng coordinates
- `get-fundraising-progress`: Check funding progress and completion status
- `get-token-donations`: View donation history for a token
- `get-user-donations`: Total donations by a user
- `get-marketplace-listing`: Check if token is for sale

## Usage Example

1. **Deploy Contract**: Deploy to Stacks blockchain
2. **Mint Site**: Create NFT for heritage location with coordinates and funding goal
3. **Enable Fundraising**: Site owner activates donation collection
4. **Accept Donations**: Users contribute STX toward preservation goals
5. **Trade Tokens**: List and purchase heritage site NFTs on marketplace

## Error Codes

- `u100`: Owner only function
- `u101`: Not token owner
- `u102`: Token not found
- `u108`: Site already registered at coordinates
- `u109`: Invalid coordinates
- `u110`: Fundraising not active
- `u111`: Fundraising goal already reached

## Data Structure

Heritage sites store:
- Name, description, location
- GPS coordinates (lat/lng)
- Heritage type and establishment year
- Fundraising goal and current funds
- Owner and creation timestamp

## Security Features

- Ownership verification for all token operations
- Coordinate uniqueness to prevent site duplication
- Metadata freeze functionality
- Secure STX transfers for donations and marketplace
