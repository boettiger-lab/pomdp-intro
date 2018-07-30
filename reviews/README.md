README.Rmd
================
Carl Boettiger

# README

Reviews are copyright of the reviewers and cannot be shared without
permission. To keep everything in one place, review material is stored
here in encrypted files.

``` r
library(gpg)
```

    ## Found GPG 2.2.8. Using keyring: /Users/cboettig/.gnupg

``` r
keyring <- gpg_list_keys(secret = TRUE)
cboettig <- keyring[1, "id"]
```

Encrypt `.gitignore`’d plain text, allowing only specified users to
decrypt:

``` r
## letter 1
letter1 <- "review-reply.Rmd"
gpg_letter1 <- "review-reply.Rmd.gpg"

msg <- gpg_encrypt(letter1, receiver = cboettig, signer = cboettig)
writeLines(msg, gpg_letter1)
```

Unencrypt committed gpg versions:

``` r
writeLines(gpg_decrypt(gpg_letter1), letter1)
```

Encrypt letter 2

``` r
letter2 <- "review-reply-2.Rmd"
gpg_letter2 <- "review-reply-2.Rmd.gpg"

msg <- gpg_encrypt(letter2, receiver = cboettig, signer = cboettig)
writeLines(msg, gpg_letter2)
```

Unencrypt committed gpg versions:

``` r
writeLines(gpg_decrypt(gpg_letter2), letter2)
```
