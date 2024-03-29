(namespace "free")
(define-keyset "free.testtokenairdrop-admin" (read-keyset "testtokenairdrop-admin"))
(module testtokenairdrop GOVERNANCE
    (use testy [ transfer get-balance transfer-create ])

    (defconst AIRDROP_BANK "testtokenairdrop-bank")
    (defconst AIRDROP_ADMIN "k:56609bf9d1983f0c13aaf3bd3537fe00db65eb15160463bb641530143d4e9bcf")
    (defconst AIRDROP_COUNT "airdrop-count")

    ; --------------------------------------------------------------------------
    ; Utilities
    ; --------------------------------------------------------------------------
    (defcap PRIVATE ()
        true
    )

    (defcap GOVERNANCE ()
        (enforce-one "Any Guard passes" [
            (enforce-guard (at "guard" (coin.details "k:56609bf9d1983f0c13aaf3bd3537fe00db65eb15160463bb641530143d4e9bcf")))
        ])
    )

    (defcap ACCOUNT_GUARD (account:string)
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard
            (at "guard" (coin.details account))
        )
    )

    (defun airdrop-bank-guard () (create-module-guard "airdrop-bank"))


    ; --------------------------------------------------------------------------
    ; Schema
    ; --------------------------------------------------------------------------
    (defschema counts-schema
        @doc "Keeps track of key counts."
        count:integer
    )

    (defschema airdrop-history-schema
        @doc "Stores airdrop history"
        account:string
        amount:decimal
        airdrop-time:time
    )

    ; --------------------------------------------------------------------------
    ; Tables
    ; --------------------------------------------------------------------------
    (deftable airdrop-history-table:{airdrop-history-schema})
    (deftable counts-table:{counts-schema})

    ; --------------------------------------------------------------------------
    ; Init
    ; --------------------------------------------------------------------------
    (defun init()
      (with-capability (GOVERNANCE)
        (insert counts-table AIRDROP_COUNT { "count": 0 })
      )
    )


    (defun bank-acc ()
        (coin.create-account AIRDROP_BANK (airdrop-bank-guard))
        "empty lock succeeded"
     )
    ; --------------------------------------------------------------------------
    ; Functions
    ; --------------------------------------------------------------------------
    (defun get-count:integer (key:string)
        @doc "Gets the count for a key"
        (at "count" (read counts-table key ['count]))
    )

    (defun get-airdrop-history()
        @doc "Returns all the airdrop history."
        (select airdrop-history-table (where "account" (!= "")))
    )

    (defun increase-count (key:string)
        @doc "Increase the count of a key in a table by 1"
        (require-capability (PRIVATE))
        (update counts-table key {"count": (+ 1 (get-count key))})
    )

    (defun airdrop-accounts(accounts:list)
        (with-capability (ACCOUNT_GUARD AIRDROP_ADMIN)
        (with-capability (PRIVATE)
            (map (airdrop-user-account) accounts)
        )
        )
    )

    (defun airdrop-user-account(item:object)
        (enforce (!= (at 'account item) "") "Account field can not be empty.")
        (enforce (> (at 'amount item) 0.0) "Amount must be greater than 0.0")
        (require-capability (PRIVATE))
        (with-capability (BANK_DEBIT)
            (let*
                (
                    (account (at 'account item))
                    (amount (at 'amount item))
                )
                (install-capability (testy.TRANSFER AIRDROP_BANK account amount))
                (transfer-create AIRDROP_BANK account (at 'guard (coin.details account)) amount)
                (increase-count AIRDROP_COUNT)
                (insert airdrop-history-table (int-to-str 10 (+ 1  (get-count AIRDROP_COUNT))){
                    'account: account
                    ,'amount: amount
                    ,'airdrop-time: (at "block-time" (chain-data))
                })
            )
        )
    )

    ; --------------------------------------------------------------------------
    ; Utility
    ; --------------------------------------------------------------------------
    (defun create-airdrop-user-guard (funder:string amount:decimal account:string)
        (with-capability (GOVERNANCE)
            (transfer-create funder account
                (create-BANK_DEBIT-guard) amount)
        )
    )

    (defun require-BANK_DEBIT ()
        (require-capability (BANK_DEBIT))
    )

    (defun create-BANK_DEBIT-guard ()
        (create-user-guard (require-BANK_DEBIT))
    )

    (defcap BANK_DEBIT ()
        true
    )
)


    (create-table airdrop-history-table)
    (create-table counts-table)
    (free.testtokenairdrop.init)
    (free.testtokenairdrop.bank-acc)
