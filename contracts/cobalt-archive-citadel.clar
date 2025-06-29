;; cobalt-archive-citadel

;; ========== System Constants and Error Management ==========

;; System administration constants
(define-constant vault-administrator-principal tx-sender)
(define-constant maximum-title-character-limit u64)
(define-constant maximum-abstract-character-limit u128)
(define-constant maximum-tag-character-limit u32)
(define-constant maximum-file-size-bytes u1000000000)
(define-constant maximum-tags-per-asset u10)
