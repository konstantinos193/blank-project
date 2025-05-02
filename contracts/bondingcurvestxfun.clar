;; Constants
(define-constant TREASURY 'SPAT9BDQ1NQ5B6VNNVS9J5PEH8WXHAEZ3E2Z72AR)
(define-constant PLATFORM 'SP2DRP8QBQPC5KNJ4S5CMJ58XQMT8KSX59P7QV86V)
(define-constant BURN_ADDRESS 'SP000000000000000000002Q6VF78)  ;; Standard burn address
(define-constant BASE_PRICE u199)  ;; Initial price of 199 microSTX
(define-constant SLOPE u1)
(define-constant FEE_PERCENT u5)

;; Token counter
(define-data-var token-counter uint u0)

;; Launch fee (initial 2.45 STX)
(define-data-var launch-fee uint u2450000)

;; Graduation threshold (initial 10,000 STX)
(define-data-var graduation-threshold uint u10000000000)

;; Creator reward (initial 122.40 STX)
(define-data-var creator-reward uint u122400000)

;; Token data structure
(define-map token-data
  {id: uint}
  {
    creator: principal,
    total-supply: uint,
    total-sold: uint,
    initial-purchase: uint,
    name: (string-ascii 32),
    symbol: (string-ascii 8),
    description: (string-ascii 256),
    website: (string-ascii 256),
    twitter: (string-ascii 256),
    telegram: (string-ascii 256),
    logo-url: (string-ascii 512),
    bonded: bool,
    pool-created: bool,
    snapshot-block: uint,
    creation-block: uint,
    last-price: uint,
    price-24h-ago: uint,
    price-7d-ago: uint,
    volume-24h: uint,
    volume-7d: uint,
    txn-count-24h: uint,
    holder-count: uint
  }
)

;; User balances
(define-map balances
  {id: uint, owner: principal}
  {amount: uint}
)

;; Bonding curve STX balances
(define-map curve-balances
  {id: uint}
  {amount: uint}
)

;; Snapshot of holder balances when bonding occurred
(define-map holder-snapshots
  {id: uint, holder: principal}
  {
    amount: uint,
    snapshot-block: uint
  }
)

;; Track if an address is a holder
(define-map is-holder
  {id: uint, holder: principal}
  {is-holder: bool}
)

;; Transaction history
(define-map transactions
  {id: uint, tx-id: uint}
  {
    user: principal,
    amount: uint,
    price: uint,
    total-cost: uint,
    fee: uint,
    tx-type: (string-ascii 4),
    block-height: uint
  }
)

;; Transaction counter
(define-data-var transaction-counter uint u0)

;; TradingView data structure
(define-map price-history
  {id: uint, timeframe: uint, block: uint}  ;; timeframe: 1=1m, 2=15m, 3=1h, 4=4h, 5=1d
  {
    open: uint,
    high: uint,
    low: uint,
    close: uint,
    volume: uint
  }
)

;; Timeframe constants
(define-constant TIMEFRAME_1M u1)
(define-constant TIMEFRAME_15M u2)
(define-constant TIMEFRAME_1H u3)
(define-constant TIMEFRAME_4H u4)
(define-constant TIMEFRAME_1D u5)

;; Block intervals for timeframes
(define-constant BLOCKS_1M u1)    ;; 1 minute = 1 block
(define-constant BLOCKS_15M u15)  ;; 15 minutes = 15 blocks
(define-constant BLOCKS_1H u60)   ;; 1 hour = 60 blocks
(define-constant BLOCKS_4H u240)  ;; 4 hours = 240 blocks
(define-constant BLOCKS_1D u1440) ;; 1 day = 1440 blocks

;; Get price based on total sold
(define-read-only (get-price (total-sold uint))
  (let ((price (* BASE_PRICE (* SLOPE total-sold))))
    (ok {price: price})))

;; Launch token and optionally buy initial tokens
(define-public (launch-token
  (initial-purchase uint)
    (name (string-ascii 32))
    (symbol (string-ascii 8))
    (description (string-ascii 256))
    (website (string-ascii 256))
    (twitter (string-ascii 256))
    (telegram (string-ascii 256))
    (logo-url (string-ascii 512)))
  (let ((creator tx-sender)
        (id (var-get token-counter))
        (total-supply u21000000)
        (price-result (unwrap-panic (get-price u0)))
        (price (get price price-result))
        (total-cost (* price initial-purchase))
        (fee (/ (* total-cost FEE_PERCENT) u100))
        (net-cost (- total-cost fee)))
    (begin
      ;; Validate inputs
      (asserts! (> (len name) u0) (err u400))
      (asserts! (> (len symbol) u0) (err u400))
      (asserts! (> (len description) u0) (err u400))
      (asserts! (<= (len website) u256) (err u400))
      (asserts! (<= (len twitter) u256) (err u400))
      (asserts! (<= (len telegram) u256) (err u400))
      (asserts! (<= (len logo-url) u512) (err u400))
      
      ;; Pay launch fee to treasury
      (try! (stx-transfer? (var-get launch-fee) creator TREASURY))
      
      ;; Only process initial purchase if amount is greater than 0
      (if (> initial-purchase u0)
          (begin
            ;; Buy initial tokens from bonding curve (with 5% fee)
            (try! (stx-transfer? net-cost creator (as-contract tx-sender)))
            (try! (stx-transfer? fee creator TREASURY))
            (map-set balances
              {id: id, owner: creator}
              {amount: initial-purchase})
            (map-set curve-balances
              {id: id}
              {amount: net-cost}))
          true)
      
      (map-set token-data
        {id: id}
        {
          creator: creator,
          total-supply: total-supply,
          total-sold: initial-purchase,
          initial-purchase: initial-purchase,
          name: name,
          symbol: symbol,
          description: description,
          website: website,
          twitter: twitter,
          telegram: telegram,
          logo-url: logo-url,
          bonded: false,
          pool-created: false,
          snapshot-block: u0,
          creation-block: block-height,
          last-price: price,
          price-24h-ago: price,
          price-7d-ago: price,
          volume-24h: u0,
          volume-7d: u0,
          txn-count-24h: u0,
          holder-count: (if (> initial-purchase u0) u1 u0)
        })
      (var-set token-counter (+ id u1))
      (ok {id: id}))))

;; Function to update launch fee
(define-public (update-launch-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender TREASURY) (err u403))
    (asserts! (> new-fee u0) (err u400))
    (var-set launch-fee new-fee)
    (ok true)))

;; Function to update holder list
(define-private (update-holder (id uint) (holder principal) (amount uint))
  (let ((balance (default-to {amount: u0} (map-get? balances {id: id, owner: holder})))
        (new-balance (+ (get amount balance) amount)))
    (begin
      (map-set balances {id: id, owner: holder} {amount: new-balance})
      (map-set is-holder {id: id, holder: holder} {is-holder: true}))))

;; Function to update token stats
(define-private (update-token-stats (id uint) (amount uint) (price uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (volume (* amount price))
        (current-block block-height)
        (blocks-24h u144))  ;; Assuming 1 block = 10 minutes, 24h = 144 blocks
    (begin
      ;; Update price history if 24h has passed
      (if (>= (- current-block (get creation-block token)) blocks-24h)
          (map-set token-data
            {id: id}
            (merge token {
              price-24h-ago: (get last-price token),
              last-price: price
            }))
          true)
      ;; Update volume
      (map-set token-data
        {id: id}
        (merge token {
          volume-24h: (+ (get volume-24h token) volume),
          volume-7d: (+ (get volume-7d token) volume)
        }))
      ;; Update holder count
      (let ((holder-count (get holder-count token)))
        (map-set token-data
          {id: id}
          (merge token {
            holder-count: holder-count
          }))))))

;; Function to record price data for TradingView
(define-private (record-price-data (id uint) (price uint) (amount uint))
  (let ((current-block block-height)
        (volume (* amount price)))
    (begin
      ;; Record for 1m timeframe
      (let ((last-data (map-get? price-history {id: id, timeframe: TIMEFRAME_1M, block: current-block})))
        (if (is-none last-data)
            (map-set price-history
              {id: id, timeframe: TIMEFRAME_1M, block: current-block}
              {
                open: price,
                high: price,
                low: price,
                close: price,
                volume: volume
              })
            (let ((data (unwrap-panic last-data)))
              (map-set price-history
                {id: id, timeframe: TIMEFRAME_1M, block: current-block}
                {
                  open: (get open data),
                  high: (if (> price (get high data)) price (get high data)),
                  low: (if (< price (get low data)) price (get low data)),
                  close: price,
                  volume: (+ (get volume data) volume)
                }))))
      ;; Record for 15m timeframe
      (let ((block-15m (/ current-block BLOCKS_15M)))
        (let ((last-data (map-get? price-history {id: id, timeframe: TIMEFRAME_15M, block: block-15m})))
          (if (is-none last-data)
              (map-set price-history
                {id: id, timeframe: TIMEFRAME_15M, block: block-15m}
                {
                  open: price,
                  high: price,
                  low: price,
                  close: price,
                  volume: volume
                })
              (let ((data (unwrap-panic last-data)))
                (map-set price-history
                  {id: id, timeframe: TIMEFRAME_15M, block: block-15m}
                  {
                    open: (get open data),
                    high: (if (> price (get high data)) price (get high data)),
                    low: (if (< price (get low data)) price (get low data)),
                    close: price,
                    volume: (+ (get volume data) volume)
                  })))))
      ;; Similar for 1h, 4h, and 1d timeframes
      true)))

;; Buy function with bonding logic
(define-public (buy (id uint) (amount uint))
  (let ((buyer tx-sender)
        (token-opt (map-get? token-data {id: id}))
        (token (unwrap-panic token-opt))
    (total-sold (get total-sold token))
        (price-result (unwrap-panic (get-price total-sold)))
        (price (get price price-result))
    (total-cost (* price amount))
    (fee (/ (* total-cost FEE_PERCENT) u100))
    (net-cost (- total-cost fee))
    (new-sold (+ total-sold amount))
        (curve-balance (default-to {amount: u0} (map-get? curve-balances {id: id})))
        (new-curve-balance (+ (get amount curve-balance) net-cost))
        (tx-id (var-get transaction-counter)))
    (begin
      ;; Validate inputs
      (asserts! (> amount u0) (err u400))
      (asserts! (is-some token-opt) (err u404))
      
      ;; Prevent buying if token is bonded
      (asserts! (not (get bonded token)) (err u701))
      (try! (stx-transfer? net-cost buyer (as-contract tx-sender)))
      (try! (stx-transfer? fee buyer TREASURY))

      (update-holder id buyer amount)
      (update-token-stats id amount price)
      (record-price-data id price amount)
      (map-set token-data
        {id: id}
        (merge token {total-sold: new-sold}))
      (map-set curve-balances
        {id: id}
        {amount: new-curve-balance})
      ;; Log transaction
      (map-set transactions
        {id: id, tx-id: tx-id}
        {
          user: buyer,
          amount: amount,
          price: price,
          total-cost: total-cost,
          fee: fee,
          tx-type: "buy",
          block-height: block-height
        })
      ;; Emit event
      (print {
        event: "buy",
        token-id: id,
        tx-id: tx-id,
        user: buyer,
        amount: amount,
        price: price,
        total-cost: total-cost,
        fee: fee,
        block-height: block-height
      })
      (var-set transaction-counter (+ tx-id u1))
      (if (and (not (get bonded token))
               (>= new-curve-balance (var-get graduation-threshold)))
          (begin
            (let ((snapshot-block block-height))
              (map-set token-data
                {id: id}
                (merge token {bonded: true, snapshot-block: snapshot-block}))
              ;; Take snapshot of creator's balance
              (map-set holder-snapshots
                {id: id, holder: (get creator token)}
                {amount: (get amount (unwrap-panic (map-get? balances {id: id, owner: (get creator token)}))), snapshot-block: snapshot-block})
              ;; Send creator reward from contract
              (try! (as-contract (stx-transfer? (var-get creator-reward) (as-contract tx-sender) (get creator token))))
              true))
          true)
      (ok {price: price, total-cost: total-cost}))))

;; Function to update graduation threshold
(define-public (update-graduation-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender TREASURY) (err u403))
    (asserts! (> new-threshold u0) (err u400))
    (var-set graduation-threshold new-threshold)
    (ok true)))

;; Function to update creator reward
(define-public (update-creator-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender TREASURY) (err u403))
    (asserts! (> new-reward u0) (err u400))
    (var-set creator-reward new-reward)
    (ok true)))

;; Get minimum of two numbers
(define-private (get-min (a uint) (b uint))
  (if (< a b)
      a
      b))

;; Get single transaction
(define-read-only (get-transaction (id uint) (tx-id uint))
  (map-get? transactions {id: id, tx-id: tx-id}))

;; Get latest transaction ID
(define-read-only (get-latest-tx-id)
  (var-get transaction-counter))

;; Sell function with transaction logging
(define-public (sell (id uint) (amount uint))
  (let ((seller tx-sender)
        (token-opt (map-get? token-data {id: id}))
        (token (unwrap-panic token-opt))
    (total-sold (get total-sold token))
        (price-result (unwrap-panic (get-price total-sold)))
        (price (get price price-result))
    (total-return (* price amount))
    (fee (/ (* total-return FEE_PERCENT) u100))
    (net-return (- total-return fee))
        (user-balance-opt (map-get? balances {id: id, owner: seller}))
        (user-balance (unwrap-panic user-balance-opt))
        (curve-balance-opt (map-get? curve-balances {id: id}))
        (curve-balance (unwrap-panic curve-balance-opt))
        (new-curve-balance (- (get amount curve-balance) total-return))
        (tx-id (var-get transaction-counter)))
    (begin
      ;; Validate inputs
      (asserts! (> amount u0) (err u400))
      (asserts! (is-some token-opt) (err u404))
      (asserts! (is-some user-balance-opt) (err u404))
      (asserts! (is-some curve-balance-opt) (err u404))
      
      (asserts! (not (get bonded token)) (err u702))
      (asserts! (>= (get amount user-balance) amount) (err u700))
      (asserts! (>= (get amount curve-balance) total-return) (err u704))
      (map-set balances
        {id: id, owner: seller}
        {amount: (- (get amount user-balance) amount)})
      (update-token-stats id amount price)
      (record-price-data id price amount)
      (map-set token-data
        {id: id}
        (merge token {total-sold: (- total-sold amount)}))
      (map-set curve-balances
        {id: id}
        {amount: new-curve-balance})
      ;; Log transaction
      (map-set transactions
        {id: id, tx-id: tx-id}
        {
          user: seller,
          amount: amount,
          price: price,
          total-cost: total-return,
          fee: fee,
          tx-type: "sell",
          block-height: block-height
        })
      ;; Emit event
      (print {
        event: "sell",
        token-id: id,
        tx-id: tx-id,
        user: seller,
        amount: amount,
        price: price,
        total-return: total-return,
        fee: fee,
        block-height: block-height
      })
      (var-set transaction-counter (+ tx-id u1))
      (try! (as-contract (stx-transfer? net-return (as-contract tx-sender) seller)))
      (try! (as-contract (stx-transfer? fee (as-contract tx-sender) TREASURY)))
      (ok {price: price, total-return: total-return}))))

;; Function to get all holders at snapshot
(define-read-only (get-all-holders (id uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id}))))
    (if (not (get bonded token))
        (err u709)
        (ok (map-get? holder-snapshots {id: id, holder: tx-sender})))))

;; Function to get a specific holder's snapshot
(define-read-only (get-holder-snapshot (id uint) (holder principal))
  (let ((token (unwrap-panic (map-get? token-data {id: id}))))
    (if (not (get bonded token))
        (err u709)
        (ok (map-get? holder-snapshots {id: id, holder: holder})))))

;; Function to create pool with bonding curve funds
(define-public (create-pool (id uint) (pool-address principal))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (curve-balance (unwrap-panic (map-get? curve-balances {id: id}))))
    (begin
      (asserts! (is-eq tx-sender PLATFORM) (err u403))
      (asserts! (get bonded token) (err u703))
      (asserts! (> (get amount curve-balance) u0) (err u705))
      (asserts! (is-eq pool-address PLATFORM) (err u400))  ;; Validate pool address
      (try! (stx-transfer? (get amount curve-balance) PLATFORM pool-address))
      (map-set curve-balances
        {id: id}
        {amount: u0})
      (ok true))))

;; Function to prepare pool creation
(define-public (prepare-pool-creation (id uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (curve-balance (unwrap-panic (map-get? curve-balances {id: id}))))
    (begin
      (asserts! (is-eq tx-sender PLATFORM) (err u403))
      (asserts! (get bonded token) (err u703))
      (asserts! (not (get pool-created token)) (err u706))
      (asserts! (> (get amount curve-balance) u0) (err u705))
      (map-set token-data
        {id: id}
        (merge token {pool-created: true}))
      (ok {curve-balance: (get amount curve-balance)}))))

;; Function to confirm pool creation
(define-public (confirm-pool-creation (id uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id}))))
    (begin
      (asserts! (is-eq tx-sender PLATFORM) (err u403))
      (asserts! (get pool-created token) (err u707))
      (map-set curve-balances
        {id: id}
        {amount: u0})
      (ok true))))

;; Function to withdraw STX from curve balance
(define-public (withdraw-curve-funds (id uint) (amount uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (curve-balance (unwrap-panic (map-get? curve-balances {id: id}))))
    (begin
      (asserts! (is-eq tx-sender PLATFORM) (err u403))
      (asserts! (get bonded token) (err u703))
      (asserts! (>= (get amount curve-balance) amount) (err u705))
      (try! (as-contract (stx-transfer? amount (as-contract tx-sender) PLATFORM)))
      (map-set curve-balances
        {id: id}
        {amount: (- (get amount curve-balance) amount)})
      (ok true))))

;; Function to transfer tokens between holders
(define-public (transfer (id uint) (amount uint) (recipient principal))
  (let ((sender tx-sender)
        (token (unwrap-panic (map-get? token-data {id: id})))
        (sender-balance (unwrap-panic (map-get? balances {id: id, owner: sender})))
        (tx-id (var-get transaction-counter)))
    (begin
      ;; Validate inputs
      (asserts! (> amount u0) (err u400))
      (asserts! (not (is-eq sender recipient)) (err u400))  ;; Prevent self-transfer
      
      ;; Prevent transfers if token is bonded
      (asserts! (not (get bonded token)) (err u701))
      (asserts! (>= (get amount sender-balance) amount) (err u700))
      
      ;; Update balances
      (update-holder id sender (- amount))
      (update-holder id recipient amount)
      
      ;; Log transfer transaction
      (map-set transactions
        {id: id, tx-id: tx-id}
        {
          user: sender,
          amount: amount,
          price: u0,
          total-cost: u0,
          fee: u0,
          tx-type: "trns",
          block-height: block-height
        })
      ;; Emit event
      (print {
        event: "transfer",
        token-id: id,
        tx-id: tx-id,
        sender: sender,
        recipient: recipient,
        amount: amount,
        block-height: block-height
      })
      (var-set transaction-counter (+ tx-id u1))
      (ok true))))

;; Function to burn tokens
(define-public (burn (id uint) (amount uint))
  (let ((sender tx-sender)
        (token (unwrap-panic (map-get? token-data {id: id})))
        (sender-balance (unwrap-panic (map-get? balances {id: id, owner: sender})))
        (total-supply (get total-supply token))
        (total-sold (get total-sold token)))
    (begin
      ;; Prevent burning if token is bonded
      (asserts! (not (get bonded token)) (err u701))  ;; Error u701: Token is bonded, trading is frozen
      (asserts! (>= (get amount sender-balance) amount) (err u700))  ;; Error u700: Insufficient balance
      
      ;; Update sender's balance
      (map-set balances
        {id: id, owner: sender}
        {amount: (- (get amount sender-balance) amount)})
      
      ;; Update total supply and total sold
      (map-set token-data
        {id: id}
        (merge token {
          total-supply: (- total-supply amount),
          total-sold: (- total-sold amount)
        }))
      
      ;; Log burn transaction
      (let ((tx-id (var-get transaction-counter)))
        (begin
          (map-set transactions
            {id: id, tx-id: tx-id}
            {
              user: sender,
              amount: amount,
              price: u0,  ;; No price for burns
              total-cost: u0,  ;; No cost for burns
              fee: u0,  ;; No fee for burns
              tx-type: "burn",  ;; Burn type
              block-height: block-height
            })
          ;; Emit event
          (print {
            event: "burn",
            token-id: id,
            tx-id: tx-id,
            sender: sender,
            amount: amount,
            new-supply: (- total-supply amount),
            block-height: block-height
          })
          (var-set transaction-counter (+ tx-id u1))))
      
      (ok true))))

;; Function to get current balance of a holder
(define-read-only (get-balance (id uint) (holder principal))
  (let ((balance (map-get? balances {id: id, owner: holder})))
    (if (is-none balance)
        (ok {amount: u0})
        (ok (unwrap-panic balance)))))

;; Function to get token list with all market data
(define-read-only (get-token-list (offset uint) (limit uint))
  (let ((end-id (var-get token-counter))
        (start-id offset)
        (max-id (if (> (+ offset limit) end-id) end-id (+ offset limit))))
    (ok (list
      (let ((token (unwrap-panic (map-get? token-data {id: start-id})))
            (price-result (unwrap-panic (get-price (get total-sold token))))
            (price (get price price-result))
            (price-24h-ago (get price-24h-ago token))
            (price-7d-ago (get price-7d-ago token)))
        (ok {
          id: start-id,
          name: (get name token),
          symbol: (get symbol token),
          logo-url: (get logo-url token),
          price: price,
          price-change-24h: (if (> price-24h-ago u0)
                               (* (/ (- price price-24h-ago) price-24h-ago) u100)
                               u0),
          price-change-7d: (if (> price-7d-ago u0)
                              (* (/ (- price price-7d-ago) price-7d-ago) u100)
                              u0),
          market-cap: (* price (get total-supply token)),
          volume-24h: (get volume-24h token),
          volume-7d: (get volume-7d token),
          holders: (get holder-count token),
          bonded: (get bonded token),
          ascended: (get pool-created token)
        }))
      (let ((token (unwrap-panic (map-get? token-data {id: (+ start-id u1)})))
            (price-result (unwrap-panic (get-price (get total-sold token))))
            (price (get price price-result))
            (price-24h-ago (get price-24h-ago token))
            (price-7d-ago (get price-7d-ago token)))
        (ok {
          id: (+ start-id u1),
          name: (get name token),
          symbol: (get symbol token),
          logo-url: (get logo-url token),
          price: price,
          price-change-24h: (if (> price-24h-ago u0)
                               (* (/ (- price price-24h-ago) price-24h-ago) u100)
                               u0),
          price-change-7d: (if (> price-7d-ago u0)
                              (* (/ (- price price-7d-ago) price-7d-ago) u100)
                              u0),
          market-cap: (* price (get total-supply token)),
          volume-24h: (get volume-24h token),
          volume-7d: (get volume-7d token),
          holders: (get holder-count token),
          bonded: (get bonded token),
          ascended: (get pool-created token)
        }))
      ;; ... repeat for other tokens ...
    ))))

;; Function to get token count
(define-read-only (get-token-count)
  (ok (var-get token-counter)))

;; Function to get token market data
(define-read-only (get-token-market-data (id uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (price-result (unwrap-panic (get-price (get total-sold token))))
        (price (get price price-result))
        (last-price (get last-price token))
        (price-24h-ago (get price-24h-ago token))
        (price-7d-ago (get price-7d-ago token)))
    (ok {
      id: id,
      name: (get name token),
      symbol: (get symbol token),
      logo-url: (get logo-url token),
      price: price,
      price-change-24h: (if (> price-24h-ago u0)
                           (* (/ (- price price-24h-ago) price-24h-ago) u100)
                           u0),
      price-change-7d: (if (> price-7d-ago u0)
                          (* (/ (- price price-7d-ago) price-7d-ago) u100)
                          u0),
      market-cap: (* price (get total-supply token)),
      volume-24h: (get volume-24h token),
      volume-7d: (get volume-7d token),
      holders: (get holder-count token),
      bonded: (get bonded token),
      ascended: (get pool-created token)
    })))

;; Function to get holder information
(define-read-only (get-holder-info (id uint) (holder principal))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (total-supply (get total-supply token))
        (balance (default-to {amount: u0} (map-get? balances {id: id, owner: holder}))))
    (ok {
      holder: holder,
      amount: (get amount balance),
      percentage: (* (/ (get amount balance) total-supply) u100)
    })))

;; Function to get bonding curve balance
(define-read-only (get-curve-balance (id uint))
  (let ((curve-balance (map-get? curve-balances {id: id})))
    (if (is-none curve-balance)
        (ok {amount: u0})
        (ok (unwrap-panic curve-balance)))))

;; Function to get bonding curve info
(define-read-only (get-bonding-curve-info (id uint))
  (let ((token (unwrap-panic (map-get? token-data {id: id})))
        (total-supply (get total-supply token))
        (curve-balance (default-to {amount: u0} (map-get? curve-balances {id: id}))))
    (ok {
      holder: (as-contract tx-sender),
      amount: (get total-sold token),
      percentage: (* (/ (get total-sold token) total-supply) u100),
      curve-balance: (get amount curve-balance),
      threshold: (var-get graduation-threshold)
    })))

;; Function to get TradingView price data
(define-read-only (get-tradingview-data (id uint) (timeframe uint) (block uint))
  (let ((price-data (map-get? price-history {id: id, timeframe: timeframe, block: block})))
    (if (is-some price-data)
        (ok (unwrap-panic price-data))
        (err u404))))  ;; Return error if no data found for the block

;; Function to get multiple blocks of price data
(define-read-only (get-tradingview-data-range (id uint) (timeframe uint) (start-block uint) (end-block uint))
  (let ((price-data (map-get? price-history {id: id, timeframe: timeframe, block: start-block})))
    (if (is-some price-data)
        (ok (unwrap-panic price-data))
        (err u404))))