`company-mode' back-end for `haskell-mode' via `ghc-mod'.

Provide context sensitive completion by using information from `ghc-mod'.
Add `company-ghc' to `company-mode' back-ends list.

    (add-to-list 'company-backends 'company-ghc)

or grouped with other back-ends.

    (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
