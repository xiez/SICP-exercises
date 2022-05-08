;;; unserialized access to the bank balance will result to non-repeatable read, which
;;; may cause some surprise if Ben operated a deposit/withdraw immediately afterwards,
;;; the balance is correct, though.
